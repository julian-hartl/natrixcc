use std::cmp::Ordering;
use std::collections::{BTreeSet, VecDeque};
use std::fmt::{Display, Formatter};
use std::ops::{Range, RangeInclusive};

use cranelift_entity::{EntityList, EntitySet, SecondaryMap};
use daggy::petgraph::prelude::DfsPostOrder;
use daggy::Walker;
use iced_x86::CC_p::p;
use index_vec::IndexVec;
use iter_tools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::{smallvec, SmallVec};
use tracing::debug;

pub use coalescer::Coalescer;

use crate::codegen::machine::{Abi, BasicBlockId, Cfg, Function, Instr, InstrId, InstrOperand, InstrOperandMut, PhysicalRegister, PseudoInstr, Register, Size, VReg};
use crate::codegen::register_allocator::linear_scan::RegAlloc;

mod coalescer;
pub mod linear_scan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstrUid {
    pub bb: BasicBlockId,
    pub instr: InstrId,
}

impl InstrUid {
    pub fn new(bb: BasicBlockId, instr: InstrId) -> Self {
        Self {
            bb,
            instr,
        }
    }
}

impl Display for InstrUid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.bb, self.instr)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ProgPoint {
    Read(InstrNr),
    Write(InstrNr),
}

impl ProgPoint {
    pub fn instr_nr(&self) -> InstrNr {
        match self {
            ProgPoint::Read(instr_nr) => *instr_nr,
            ProgPoint::Write(instr_nr) => *instr_nr,
        }
    }
}

impl PartialOrd for ProgPoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}

impl Ord for ProgPoint {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Read(a), Self::Read(b)) => a.cmp(b),
            (Self::Write(a), Self::Write(b)) => a.cmp(b),
            (Self::Read(a), Self::Write(b)) => {
                if a == b {
                    Ordering::Less
                } else {
                    a.cmp(b)
                }
            }
            (Self::Write(a), Self::Read(b)) => {
                if a == b {
                    Ordering::Greater
                } else {
                    a.cmp(b)
                }
            }
        }
    }
}

impl Display for ProgPoint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgPoint::Read(instr_nr) => write!(f, "{instr_nr}r"),
            ProgPoint::Write(instr_nr) => write!(f, "{instr_nr}w")
        }
    }
}


#[cfg(test)]
mod prog_point_tests {
    use super::*;

    #[test]
    fn before_should_be_less_than_after() {
        assert!(ProgPoint::Read(0.into()) < ProgPoint::Write(0.into()));
    }

    #[test]
    fn after_with_lower_instr_nr_should_be_less_than_with_higher() {
        assert!(ProgPoint::Write(0.into()) < ProgPoint::Write(1.into()));
    }

    #[test]
    fn before_the_next_instruction_should_be_equal_to_after_the_previous_instruction() {
        let before = ProgPoint::Read((0, 1).into());
        let after = ProgPoint::Write((0, 0).into());
        assert_eq!(before, after);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LiveInterval {
    pub start: ProgPoint,
    pub end: ProgPoint,
}

impl LiveInterval {
    pub fn new(start: ProgPoint, end: ProgPoint) -> Self {
        Self {
            start,
            end,
        }
    }
    pub fn contains(&self, pp: &ProgPoint) -> bool {
        self.start <= *pp && *pp <= self.end
    }

    /// Potentially merges two **not** overlapping intervals i,j.
    ///
    /// If j begins right after i ends, then the two intervals are merged into a single one and the result is returned.
    /// Otherwise, `None` is returned.
    pub fn merge(i: &LiveInterval, j: &LiveInterval) -> Option<LiveInterval> {
        match (i.end, j.start) {
            (ProgPoint::Read(i_nr), ProgPoint::Write(j_nr)) if i_nr == j_nr => {
                Some(LiveInterval::new(i.start, j.end))
            }
            _ => None,
        }
    }

    fn merge_in_place(&mut self, other: &LiveInterval) -> bool {
        match Self::merge(self, other) {
            Some(merged) => {
                *self = merged;
                true
            }
            None => false,
        }
    }
}

impl Display for LiveInterval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}", self.start)?;
        write!(f, ", ")?;
        write!(f, "{})", self.end)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Lifetime {
    /// List of intervals in which the register is live.
    ///
    /// Sorted by start point.
    intervals: Vec<LiveInterval>,
}

impl Lifetime {
    pub fn insert(&mut self, interval: LiveInterval) {
        // if interval.start == interval.end && instr_numbering.get_instr_uid(
        //     interval.start.instr_nr()
        // ).unwrap().instr == InstrId::from_raw(0) {
        //     return;
        // }
        let idx = self.intervals.binary_search_by(|i| i.start.cmp(&interval.start));
        match idx {
            Ok(_) => panic!("Overlapping intervals are not allowed"),
            Err(idx) => {
                // if let Some(prev_idx) = idx.checked_sub(1) {
                //     if self.intervals[prev_idx].merge_in_place(&interval) {
                //         return;
                //     }
                // };
                // if let Some(next_interval) = self.intervals.get_mut(idx + 1) {
                //     if next_interval.merge_in_place(&interval) {
                //         return;
                //     }
                // };
                self.intervals.insert(idx, interval);
            }
        };
    }

    pub fn get_interval_live_at(&self, pp: ProgPoint) -> Option<&LiveInterval> {
        let idx = self.get_interval_live_at_idx(pp);
        idx.map(|idx| &self.intervals[idx])
    }

    /// Returns the interval that was live before the interval that contains `pp`.
    pub fn get_previous_interval(&self, pp: ProgPoint) -> Option<&LiveInterval> {
        let idx = self.get_interval_live_at_idx(pp);
        idx.and_then(|idx| self.intervals.get(idx.checked_sub(1)?))
    }

    fn get_interval_live_at_idx(&self, pp: ProgPoint) -> Option<usize> {
        self.intervals.iter().position(|interval| interval.contains(&pp))
    }
}

#[cfg(test)]
mod lifetime_tests {
    #[test]
    fn test1() {
        todo!()
    }
}

impl Display for Lifetime {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (idx, interval) in self.intervals.iter().enumerate() {
            if idx != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", interval)?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

pub type InstrNr = u32;

#[derive(Debug)]
pub struct InstrNumbering {
    toplogical_order: Vec<(BasicBlockId, u32)>,
}

impl InstrNumbering {
    pub fn new<A: Abi>(func: &Function<A>) -> Self {
        let order = func.cfg().topological_order().into_iter().map(
            |bb| (bb, func.basic_blocks[bb].instructions.len_idx().raw())
        ).collect_vec();
        debug!("Created instruction numbering with ordering: {:?}",order);
        Self {
            toplogical_order: order
        }
    }

    pub fn get_instr_uid(&self, instr_nr: InstrNr) -> Option<InstrUid> {
        let mut nr = 0;
        for (bb, instructions_len) in self.toplogical_order.iter().copied() {
            let next_nr = nr + instructions_len;
            if next_nr < instr_nr {
                return Some(InstrUid {
                    bb,
                    instr: InstrId::from_raw(instr_nr - nr),
                });
            }
            nr = next_nr;
        }
        None
    }

    pub fn get_instr_nr(&self, instr_uid: InstrUid) -> Option<InstrNr> {
        let bb_offset = self.get_bb_offset(instr_uid.bb)?;
        Some(bb_offset + instr_uid.instr.raw())
    }

    pub fn get_bb_offset(&self, bb_query: BasicBlockId) -> Option<InstrNr> {
        let mut nr = 0;
        for (bb, instr_len) in self.toplogical_order.iter().copied() {
            if bb == bb_query {
                return Some(nr);
            }
            nr += instr_len;
        }
        None
    }

    pub fn iter(&self) -> InstrNumberingIter<'_> {
        InstrNumberingIter::new(self)
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item=(InstrNr, InstrUid)> + '_ {
        self.iter().enumerate().map(|(nr, instr_uid)| (nr as InstrNr, instr_uid))
    }
}

pub struct InstrNumberingIter<'numbering> {
    instr_id: InstrId,
    bb: usize,
    numbering: &'numbering InstrNumbering,
}

impl<'numbering> InstrNumberingIter<'numbering> {
    pub fn new(numbering: &'numbering InstrNumbering) -> Self {
        Self {
            numbering,
            bb: 0,
            instr_id: 0.into(),
        }
    }
}

impl Iterator for InstrNumberingIter<'_> {
    type Item = InstrUid;

    fn next(&mut self) -> Option<Self::Item> {
        let (bb, instr_len) = self.numbering.toplogical_order.get(self.bb).copied()?;
        if instr_len <= self.instr_id.raw() {
            self.bb += 1;
            self.instr_id = 0.into();
            return self.next();
        }
        let instr_id = self.instr_id;
        self.instr_id += 1;
        Some(InstrUid {
            bb,
            instr: instr_id,
        })
    }
}

#[derive(Debug)]
pub struct LivenessRepr {
    pub instr_numbering: InstrNumbering,
    reg_lifetimes: SecondaryMap<VReg, Lifetime>,
}

impl Display for LivenessRepr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (reg, lifetime) in self.reg_lifetimes.iter() {
            writeln!(f, "{}: {}", reg, lifetime)?;
        }
        Ok(())
    }
}

impl LivenessRepr {
    pub fn new<A: Abi>(func: &Function<A>) -> Self {
        Self {
            reg_lifetimes: SecondaryMap::default(),
            instr_numbering: InstrNumbering::new(func),
        }
    }
}

pub type RegAllocHints<A: Abi> = SmallVec<[A::REG; 2]>;

pub trait RegAllocAlgorithm<'liveness, A: Abi> {
    fn new(liveness_repr: &'liveness LivenessRepr) -> Self;
    fn allocate(&mut self, vreg: VReg, live_interval: LiveInterval, size: Size, hints: RegAllocHints<A>) -> A::REG;
}

struct VRegAllocations<'liveness, A: Abi> {
    map: FxHashMap<(VReg, ProgPoint), A::REG>,
    liveness_repr: &'liveness LivenessRepr,
}

impl<'liveness, A: Abi> VRegAllocations<'liveness, A> {
    pub fn new(liveness_repr: &'liveness LivenessRepr) -> Self {
        Self {
            map: FxHashMap::default(),
            liveness_repr,
        }
    }

    pub fn get_previously_allocated_reg(&self, vreg: VReg, pp: ProgPoint) -> Option<A::REG> {
        let lifetime = &self.liveness_repr.reg_lifetimes[vreg];
        lifetime.get_previous_interval(pp).and_then(
            |interval| self.map.get(&(vreg, interval.start)).copied()
        )
    }

    /// Returns the interval that is live at the given program point.
    pub fn get_live_interval_for_reg(&self, vreg: VReg, at: ProgPoint) -> Option<&LiveInterval> {
        let lifetime = &self.liveness_repr.reg_lifetimes[vreg];
        lifetime.get_interval_live_at(at)
    }

    pub fn get_allocated_reg(&self, vreg: VReg, starting_at: ProgPoint) -> Option<A::REG> {
        let live_interval = self.get_live_interval_for_reg(vreg, starting_at)?;
        self.map.get(&(vreg, live_interval.start)).copied()
    }

    pub fn start_allocation(&mut self, vreg: VReg, at: ProgPoint, reg: A::REG) {
        self.map.insert((vreg, at), reg);
    }
}

pub struct RegisterAllocator<'liveness, 'func, A: Abi, RegAlloc: RegAllocAlgorithm<'liveness, A>> {
    algo: RegAlloc,
    func: &'func mut Function<A>,
    marker: std::marker::PhantomData<A>,
    allocations: VRegAllocations<'liveness, A>,
    liveness_repr: &'liveness LivenessRepr,
}

impl<'liveness, 'func, A: Abi, RegAlloc: RegAllocAlgorithm<'liveness, A>> RegisterAllocator<'liveness, 'func, A, RegAlloc> {
    pub fn new(func: &'func mut Function<A>, liveness_repr: &'liveness LivenessRepr) -> Self {
        Self {
            func,
            algo: RegAlloc::new(liveness_repr),
            marker: std::marker::PhantomData,
            allocations: VRegAllocations::new(liveness_repr),
            liveness_repr,
        }
    }

    pub fn run(mut self) {
        for (instr_nr, instr_uid) in self.liveness_repr.instr_numbering.iter_enumerated() {
            let instr = &mut self.func.basic_blocks[instr_uid.bb].instructions[instr_uid.instr];
            let mut hints: SmallVec<[_; 2]> = smallvec![];
            match instr {
                Instr::Pseudo(instr) => {
                    match instr {
                        PseudoInstr::Copy(_, src) => {
                            if let Some(reg) = match src {
                                Register::Virtual(reg) => self.allocations.get_allocated_reg(*reg, ProgPoint::Read(instr_nr)),
                                Register::Physical(reg) => Some(*reg),
                            } {
                                hints.push(reg);
                            }
                        }
                        PseudoInstr::Ret(_) => {}
                    }
                }
                Instr::Machine(_) => {}
            };
            let output = instr.writes();
            if let Some(vreg) = output.and_then(|reg| reg.try_as_virtual()) {
                if self.allocations.get_allocated_reg(vreg, ProgPoint::Write(instr_nr)).is_some() {
                    continue;
                }
                if let Some(prev_reg) = self.allocations.get_allocated_reg(vreg, ProgPoint::Read(instr_nr)) {
                    hints.push(prev_reg);
                }
                let live_interval = self.allocations.get_live_interval_for_reg(vreg, ProgPoint::Write(
                    instr_nr
                )).unwrap_or_else(|| panic!("{vreg} is not live at {instr_uid}"));
                let size = self.func.vregs[vreg].size;
                debug!("Allocating {vreg} at {instr_uid} with hints: {:?} and size {size}", hints);
                let phys_reg = self.algo.allocate(vreg, live_interval.clone(), size, hints);
                assert_eq!(phys_reg.size(), size, "Register size mismatch");
                debug!("Allocated {} for {} during {}", phys_reg.name(), vreg,live_interval);
                debug_assert_eq!(live_interval.start.instr_nr(), instr_nr);
                self.allocations.start_allocation(vreg, ProgPoint::Write(instr_nr), phys_reg);
            }
        }

        for (instr_nr, instr_uid) in self.liveness_repr.instr_numbering.iter_enumerated() {
            let instr = &mut self.func.basic_blocks[instr_uid.bb].instructions[instr_uid.instr];
            for reg in instr.read_regs_mut() {
                if let Some(vreg) = reg.try_as_virtual() {
                    let phys_reg = self.allocations.get_allocated_reg(vreg, ProgPoint::Read(instr_nr)).unwrap_or_else(
                        || self.allocations.get_previously_allocated_reg(vreg, ProgPoint::Read(instr_nr)).unwrap_or_else(
                            || panic!("{vreg} was not live before reading it at {instr_uid}")
                    ));
                    *reg = Register::Physical(phys_reg);
                }
            }
            for reg in instr.written_regs_mut() {
                if let Some(vreg) = reg.try_as_virtual() {
                    let phys_reg = self.allocations.get_allocated_reg(vreg, ProgPoint::Write(instr_nr)).unwrap_or_else(|| panic!("{vreg} is not live at {}", ProgPoint::Write(instr_nr)));
                    *reg = Register::Physical(phys_reg);
                }
            }
        }
    }
}

impl<A: Abi> Function<A> {
    pub fn liveness_repr(&mut self) -> LivenessRepr {
        #[derive(Default)]
        struct Liveins(FxHashMap<BasicBlockId, FxHashSet<VReg>>);
        impl Liveins {
            fn new() -> Self {
                Self(FxHashMap::default())
            }
            fn insert(&mut self, bb: BasicBlockId, reg: VReg) {
                self.0.entry(
                    bb
                ).or_default().insert(reg);
            }

            fn ensure_exists(&mut self, bb: BasicBlockId) {
                self.0.entry(bb).or_default();
            }

            fn liveins(&self, bb: BasicBlockId) -> impl Iterator<Item=VReg> + '_ {
                self.0[&bb].iter().copied()
            }
        }
        let mut liveins = Liveins::default();
        let mut repr = LivenessRepr::new(self);
        debug!("Starting liveness analysis");
        let mut worklist = self.cfg().dfs_postorder().collect::<VecDeque<_>>();
        let mut visited = FxHashSet::default();
        while let Some(bb_id) = worklist.pop_front() {
            debug!("Looking at {bb_id}");
            if visited.contains(&bb_id) {
                debug!("Already visited");
                continue;
            }
            let mut all_visited = true;
            for succ in self.cfg().successors(bb_id) {
                if !visited.contains(&succ) {
                    debug!("Successor {succ} has not been visited yet. Queueing");
                    all_visited = false;
                    worklist.push_front(succ);
                }
            }
            if !all_visited {
                debug!("Not all successors have been visited. Queueing {bb_id} for later");
                worklist.push_back(bb_id);
                continue;
            }
            debug!("Analysing liveness in {bb_id}");
            visited.insert(bb_id);
            let bb = &self.basic_blocks[bb_id];
            let mut current_intervals = FxHashMap::default();
            let mut undeclared_regs = FxHashSet::default();
            let entry_pp = bb.entry_pp(&repr);
            let exit_pp = bb.exit_pp(&repr);
            for liveout in self.cfg().successors(bb_id).flat_map(|pred| liveins.liveins(pred)) {
                debug!("{liveout} is in liveins of some succ. Extending its lifetime to the end of {bb_id}");
                debug_assert!(current_intervals.insert(
                    liveout,
                    LiveInterval::new(entry_pp, exit_pp),
                ).is_none(), "Liveout {liveout} already in current_intervals");
            }
            let mut instr_nr = exit_pp.instr_nr();
            for instr in bb.instructions.iter().rev() {
                let out = instr.writes();
                if let Some(reg) = out.and_then(
                    |reg| reg.try_as_virtual()
                ) {
                    undeclared_regs.remove(&reg);
                    let mut interval = current_intervals.remove(&reg).unwrap_or_else(|| {
                        debug!("Creating new interval for {reg} at {instr_nr}");
                        LiveInterval::new(ProgPoint::Write(instr_nr), ProgPoint::Write(instr_nr))
                    });
                    debug!("Finished interval for {reg} at {instr_nr}");
                    interval.start = ProgPoint::Write(instr_nr);
                    repr.reg_lifetimes[reg].insert(interval);
                }
                let read = instr.reads();
                for reg in read {
                    let Register::Virtual(reg) = reg else {
                        continue;
                    };
                    undeclared_regs.insert(reg);
                    current_intervals.entry(reg).or_insert_with(
                        || {
                            debug!("Setting interval end of {reg} to {instr_nr}");
                            LiveInterval::new(ProgPoint::Read(instr_nr), ProgPoint::Read(instr_nr))
                        }
                    );
                }
                if let Some(val) = instr_nr.checked_sub(1) {
                    instr_nr = val;
                }
            }
            liveins.ensure_exists(bb_id);

            for undeclared_reg in undeclared_regs {
                debug!("Inserting {undeclared_reg} in liveins set of {bb_id} and extending its lifetime to the start of the basic block");
                liveins.insert(bb_id, undeclared_reg);
                current_intervals.get_mut(&undeclared_reg).unwrap().start = entry_pp;
            }

            for (reg, interval) in current_intervals {
                repr.reg_lifetimes[reg].insert(interval);
            }
        }

        debug!("{}", repr);
        repr
    }
}
