use std::{
    cmp::Ordering,
    collections::{
        VecDeque,
    },
    fmt::{
        Display,
        Formatter,
    },
};

use cranelift_entity::{
    SecondaryMap,
};
use daggy::{
    Walker,
};
use iter_tools::Itertools;
use rustc_hash::{
    FxHashMap,
    FxHashSet,
};
use smallvec::{
    smallvec,
    SmallVec,
};
use tracing::debug;

pub use coalescer::Coalescer;

use crate::codegen::{
    machine::{
        abi::{
            calling_convention::Slot,
            CallingConvention,
        },
        function::{
            Function,
        },
        instr::{
            Instr,
            PseudoInstr,
        },
        InstrId,
        isa::PhysicalRegister,
        reg::{
            Register,
            VReg,
        },
        Size,
    },
};
use crate::codegen::machine::function::BasicBlockId;
use crate::codegen::machine::TargetMachine;

mod coalescer;
pub mod linear_scan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InstrUid {
    pub bb: BasicBlockId,
    pub instr: InstrId,
}

impl InstrUid {
    pub fn new(bb: BasicBlockId, instr: InstrId) -> Self {
        Self { bb, instr }
    }
}

impl Display for InstrUid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.bb, self.instr)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProgPoint {
    Read(InstrNr),
    Write(InstrNr),
}

impl ProgPoint {
    pub const fn instr_nr(&self) -> InstrNr {
        match self {
            Self::Write(nr) | Self::Read(nr) => *nr,
        }
    }
}

impl Default for ProgPoint {
    fn default() -> Self {
        Self::Read(0)
    }
}

impl PartialOrd for ProgPoint {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ProgPoint {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Read(a), Self::Read(b)) | (Self::Write(a), Self::Write(b)) => a.cmp(b),
            (Self::Read(a), Self::Write(b)) => {
                if a <= b {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            }
            (Self::Write(a), Self::Read(b)) => {
                if a < b {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            }
        }
    }
}

impl Display for ProgPoint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgPoint::Read(nr) => write!(f, "{}r", nr),
            ProgPoint::Write(nr) => write!(f, "{}w", nr),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Lifetime {
    pub start: ProgPoint,
    pub end: ProgPoint,
}

impl Lifetime {
    pub fn new(start: InstrNr, end: ProgPoint) -> Self {
        Self {
            start: ProgPoint::Write(start),
            end,
        }
    }
    pub fn contains(&self, pp: ProgPoint) -> bool {
        self.start <= pp && pp <= self.end
    }

    /// Returns true if the two lifetimes overlap.
    ///
    /// Two lifetimes l, j overlap if the intersection of their ranges is not empty.
    /// This is the case iff.
    /// - l.start <= j.end
    /// and
    /// - j.start <= l.end
    ///
    /// Overlaps are **symmetric**.
    pub fn are_overlapping(l: &Self, j: &Self) -> bool {
        l.start <= j.end && j.start <= l.end
    }

    pub fn overlaps_with(&self, other: &Self) -> bool {
        Self::are_overlapping(self, other)
    }
}

impl Display for Lifetime {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}", self.start)?;
        write!(f, ", ")?;
        write!(f, "{})", self.end)?;
        Ok(())
    }
}

#[cfg(test)]
mod prog_point_tests {
    use super::*;

    #[test]
    fn ord_should_be_correct() {
        let inputs = [
            (ProgPoint::Read(0), ProgPoint::Read(1), Ordering::Less),
            (ProgPoint::Read(0), ProgPoint::Write(1), Ordering::Less),
            (ProgPoint::Write(0), ProgPoint::Read(1), Ordering::Less),
            (ProgPoint::Write(0), ProgPoint::Write(1), Ordering::Less),
            (ProgPoint::Read(0), ProgPoint::Read(0), Ordering::Equal),
            (ProgPoint::Write(0), ProgPoint::Write(0), Ordering::Equal),
            (ProgPoint::Read(0), ProgPoint::Write(0), Ordering::Less),
            (ProgPoint::Write(0), ProgPoint::Read(0), Ordering::Greater),
        ];

        for (a, b, expected) in inputs {
            assert_eq!(a.cmp(&b), expected);
        }
    }
}

#[cfg(test)]
mod lifetime_tests {
    use crate::codegen::register_allocator::{
        Lifetime,
        ProgPoint,
    };

    #[test]
    fn lifetimes_overlap() {
        let inputs = [
            // Both lifetimes are the same
            (
                Lifetime::new(0, ProgPoint::Read(2)),
                Lifetime::new(0, ProgPoint::Read(2)),
                true,
            ),
            // The second lifetime is within the first one
            (
                Lifetime::new(0, ProgPoint::Read(2)),
                Lifetime::new(0, ProgPoint::Read(1)),
                true,
            ),
            // The lifetimes do not overlap
            (
                Lifetime::new(0, ProgPoint::Read(1)),
                Lifetime::new(2, ProgPoint::Read(3)),
                false,
            ),
            // The lifetimes overlap at one point
            (
                Lifetime::new(0, ProgPoint::Write(2)),
                Lifetime::new(2, ProgPoint::Read(3)),
                true,
            ),
            // The lifetimes are the same but with different ProgPoints
            (
                Lifetime::new(0, ProgPoint::Write(2)),
                Lifetime::new(0, ProgPoint::Read(2)),
                true,
            ),
        ];
        for (l1, l2, should_overlap) in inputs {
            // Overlaps are symmetric
            assert_eq!(
                l1.overlaps_with(&l2),
                should_overlap,
                "{:?} and {:?} should overlap: {}",
                l1,
                l2,
                should_overlap
            );
            assert_eq!(
                l2.overlaps_with(&l1),
                should_overlap,
                "{:?} and {:?} should overlap: {}",
                l2,
                l1,
                should_overlap
            );
        }
    }

    #[test]
    fn lifetimes_contain() {
        let inputs = [
            // The lifetime contains the ProgPoint
            (
                Lifetime::new(0, ProgPoint::Read(2)),
                ProgPoint::Read(1),
                true,
            ),
            // The lifetime does not contain the ProgPoint
            (
                Lifetime::new(0, ProgPoint::Read(1)),
                ProgPoint::Write(2),
                false,
            ),
            // The lifetime contains the ProgPoint at the interval end
            (
                Lifetime::new(0, ProgPoint::Write(2)),
                ProgPoint::Write(2),
                true,
            ),
            // The lifetime does not contain the ProgPoint at the interval start
            (
                Lifetime::new(0, ProgPoint::Read(1)),
                ProgPoint::Write(0),
                true,
            ),
        ];
        for (lifetime, pp, should_contain) in inputs {
            assert_eq!(
                lifetime.contains(pp),
                should_contain,
                "{:?} should contain {:?}: {}",
                lifetime,
                pp,
                should_contain
            );
        }
    }
}

pub type InstrNr = u32;

#[derive(Debug)]
pub struct InstrNumbering {
    toplogical_order: Vec<(BasicBlockId, u32)>,
}

impl InstrNumbering {
    pub fn new<TM: TargetMachine>(func: &Function<TM>) -> Self {
        debug!("Creating instruction numbering");
        let order = func
            .cfg()
            .ordered()
            .into_iter()
            .map(|bb| (bb, func.basic_blocks[bb].instructions.len_idx().raw()))
            .collect_vec();
        debug!("Created instruction numbering with ordering: {:?}", order);
        Self {
            toplogical_order: order,
        }
    }

    pub fn get_instr_uid(&self, instr_nr: InstrNr) -> Option<InstrUid> {
        let mut nr = 0;
        for (bb, instructions_len) in self.toplogical_order.iter().copied() {
            let next_nr = nr + instructions_len;
            if nr <= instr_nr && instr_nr < next_nr {
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

    pub fn end_of_bb(&self, bb: BasicBlockId) -> Option<InstrNr> {
        let offset = self.get_bb_offset(bb)?;
        let len = self
            .toplogical_order
            .iter()
            .find(|(bb_id, _)| *bb_id == bb)?
            .1;
        Some(offset + len)
    }

    pub fn iter(&self) -> InstrNumberingIter<'_> {
        InstrNumberingIter::new(self)
    }

    pub fn iter_enumerated(&self) -> impl Iterator<Item = (InstrNr, InstrUid)> + '_ {
        self.iter()
            .enumerate()
            .map(|(nr, instr_uid)| (nr as InstrNr, instr_uid))
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
    defs: SecondaryMap<VReg, InstrNr>,
    /// Map of register to a sorted list of instruction numbers where the register is used
    uses: SecondaryMap<VReg, Vec<InstrNr>>,
}

impl LivenessRepr {
    pub fn display<'func, 'liveness, A: TargetMachine>(
        &'liveness self,
        func: &'func Function<A>,
    ) -> LivenessReprDisplay<'func, 'liveness, A> {
        LivenessReprDisplay(self, func)
    }
}

struct LivenessReprDisplay<'func, 'liveness, A: TargetMachine>(&'liveness LivenessRepr, &'func Function<A>);

impl<A: TargetMachine> Display for LivenessReprDisplay<'_, '_, A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (reg, lifetime) in self
            .0
            .defs
            .keys()
            .map(|reg| (reg, self.0.lifetime(reg, self.1)))
        {
            writeln!(f, "{}: {}", reg, lifetime)?;
        }
        Ok(())
    }
}

impl LivenessRepr {
    pub fn new<A: TargetMachine>(func: &Function<A>) -> Self {
        debug!("Creating liveness representation");
        Self {
            defs: SecondaryMap::default(),
            uses: SecondaryMap::default(),
            instr_numbering: InstrNumbering::new(func),
        }
    }

    pub fn record_def(&mut self, reg: VReg, instr_nr: InstrNr) {
        self.defs[reg] = instr_nr;
    }

    pub fn record_use(&mut self, reg: VReg, instr_nr: InstrNr) {
        let insert_at = self.uses[reg]
            .binary_search(&instr_nr)
            .unwrap_or_else(|x| x);
        self.uses[reg].insert(insert_at, instr_nr);
    }

    pub fn lifetime<A: TargetMachine>(&self, reg: VReg, func: &Function<A>) -> Lifetime {
        let start = self.defs[reg];
        let end = self.last_use(reg).unwrap_or(start);
        let end_uid = self.instr_numbering.get_instr_uid(end).unwrap();
        if func.basic_blocks[end_uid.bb].instructions[end_uid.instr].is_phi() {
            if let Some(second_last_use_in_bb) = Some(self.second_last_use(reg).unwrap_or(start))
                .and_then(|instr_nr| self.instr_numbering.get_instr_uid(instr_nr))
                .map(|uid| uid.bb)
            {
                let end = self
                    .instr_numbering
                    .end_of_bb(second_last_use_in_bb)
                    .unwrap();
                return Lifetime::new(start, ProgPoint::Write(end));
            }
        }
        Lifetime::new(start, ProgPoint::Read(end))
    }

    pub fn second_last_use(&self, reg: VReg) -> Option<InstrNr> {
        self.uses[reg].iter().copied().rev().nth(1)
    }

    pub fn last_use(&self, reg: VReg) -> Option<InstrNr> {
        self.uses[reg].last().copied()
    }
}

pub type RegAllocHints<TM> = SmallVec<[<TM as TargetMachine>::Reg; 2]>;

#[derive(Debug, Clone)]
pub struct RegAllocVReg {
    pub id: VReg,
    pub size: Size,
    pub lifetime: Lifetime,
}

pub trait RegAllocAlgorithm<'liveness, A: TargetMachine> {
    fn new(liveness_repr: &'liveness LivenessRepr) -> Self;
    fn allocate_arbitrary(&mut self, vreg: &RegAllocVReg, hints: RegAllocHints<A>) -> A::Reg;

    fn try_allocate_fixed(&mut self, vreg: &RegAllocVReg, reg: A::Reg) -> bool;

    fn try_evict(&mut self, reg: A::Reg) -> bool;

    /// Returns true if a register was evicted
    fn allocate_fixed_or_evict(&mut self, vreg: &RegAllocVReg, reg: A::Reg) -> bool {
        if self.try_allocate_fixed(vreg, reg) {
            return false;
        }
        assert!(
            self.try_evict(reg),
            "{} was ensured to be allocated, but it was not",
            reg.name()
        );
        assert!(
            self.try_allocate_fixed(vreg, reg),
            "{} was ensured to be evicted, but it was not",
            reg.name()
        );
        true
    }
}

struct VRegAllocations<'liveness, A: TargetMachine> {
    map: FxHashMap<VReg, A::Reg>,
    liveness_repr: &'liveness LivenessRepr,
}

impl<'liveness, A: TargetMachine> VRegAllocations<'liveness, A> {
    pub fn new(liveness_repr: &'liveness LivenessRepr) -> Self {
        Self {
            map: FxHashMap::default(),
            liveness_repr,
        }
    }

    pub fn get_allocated_reg(&self, vreg: VReg) -> Option<A::Reg> {
        self.map.get(&vreg).copied()
    }

    pub fn start_allocation(&mut self, vreg: VReg, reg: A::Reg) {
        self.map.insert(vreg, reg);
    }
}

pub struct RegisterAllocator<'liveness, 'func, A: TargetMachine, RegAlloc: RegAllocAlgorithm<'liveness, A>> {
    algo: RegAlloc,
    func: &'func mut Function<A>,
    marker: std::marker::PhantomData<A>,
    allocations: VRegAllocations<'liveness, A>,
    liveness_repr: &'liveness LivenessRepr,
}

impl<'liveness, 'func, TM: TargetMachine, RegAlloc: RegAllocAlgorithm<'liveness, TM>>
    RegisterAllocator<'liveness, 'func, TM, RegAlloc>
{
    pub fn new(func: &'func mut Function<TM>, liveness_repr: &'liveness LivenessRepr) -> Self {
        Self {
            func,
            algo: RegAlloc::new(liveness_repr),
            marker: std::marker::PhantomData,
            allocations: VRegAllocations::new(liveness_repr),
            liveness_repr,
        }
    }

    pub fn run(mut self) {
        self.insert_fixed_locations_for_function_params();
        let mut worklist = self
            .liveness_repr
            .instr_numbering
            .iter()
            .filter_map(|instr_uid| {
                let instr = &self.func.basic_blocks[instr_uid.bb].instructions[instr_uid.instr];
                instr.writes().and_then(|reg| reg.try_as_virtual())
            })
            .collect::<VecDeque<_>>();
        while let Some(vreg) = worklist.pop_front() {
            let instr_nr = self.liveness_repr.defs[vreg];
            let instr_uid = self
                .liveness_repr
                .instr_numbering
                .get_instr_uid(instr_nr)
                .unwrap_or_else(|| panic!("No instr uid for {}", instr_nr));
            let instr = &self.func.basic_blocks[instr_uid.bb].instructions[instr_uid.instr];
            let mut hints: SmallVec<[_; 2]> = smallvec![];
            match instr {
                Instr::Pseudo(instr) => match instr {
                    PseudoInstr::Copy(_, src) => {
                        if let Some(reg) = match src {
                            Register::Virtual(reg) => self.allocations.get_allocated_reg(*reg),
                            Register::Physical(reg) => Some(*reg),
                        } {
                            hints.push(reg);
                        }
                    }
                    PseudoInstr::Ret(_) => {}
                    PseudoInstr::Phi(_, _) => {}
                    PseudoInstr::Def(_) => {}
                },
                Instr::Machine(_) => {}
            };
            if self.allocations.get_allocated_reg(vreg).is_some() {
                continue;
            }
            let lifetime = self.liveness_repr.lifetime(vreg, &self.func);
            let vreg_info = &self.func.vregs[vreg];
            let size = vreg_info.size;
            let alloc_vreg = RegAllocVReg {
                id: vreg,
                size,
                lifetime: lifetime.clone(),
            };
            let phys_reg = match vreg_info.fixed {
                None => match vreg_info.tied_to {
                    None => {
                        debug!(
                            "Allocating {vreg} at {instr_uid} with hints: {:?} and size {size}",
                            hints
                        );

                        Some(self.algo.allocate_arbitrary(&alloc_vreg, hints))
                    }
                    Some(tied_to) => {
                        debug!(
                            "{vreg} is tied to {tied_to}. Trying to put it in the same register"
                        );
                        assert!(
                            !lifetime
                                .overlaps_with(&self.liveness_repr.lifetime(tied_to, &self.func)),
                            "Tied register {tied_to} overlaps with {vreg}"
                        );
                        let allocated_reg = self.allocations.get_allocated_reg(tied_to);
                        match allocated_reg {
                            None => {
                                debug!("Tied register {tied_to} is not allocated yet.");
                                None
                            }
                            Some(allocated_reg) => {
                                debug!("Tied register {tied_to} is allocated at {}. Trying to allocate {vreg} there", allocated_reg.name());
                                if self
                                    .algo
                                    .allocate_fixed_or_evict(&alloc_vreg, allocated_reg)
                                {
                                    debug!("Evicted {} to allocate {vreg}", allocated_reg.name());
                                }
                                Some(allocated_reg)
                            }
                        }
                    }
                },
                Some(fixed) => {
                    debug!(
                        "Allocating {vreg} at {instr_uid} in fixed register {}",
                        fixed.name()
                    );
                    self.algo.allocate_fixed_or_evict(&alloc_vreg, fixed);
                    Some(fixed)
                }
            };
            if let Some(phys_reg) = phys_reg {
                assert_eq!(phys_reg.size(), size, "Register size mismatch");
                debug!("Allocated {} for {vreg}", phys_reg.name());
                self.allocations.start_allocation(vreg, phys_reg);
            } else {
                debug!("No register available for {vreg}. Retrying later");
                worklist.push_back(vreg);
            }
        }

        for (instr_nr, instr_uid) in self.liveness_repr.instr_numbering.iter_enumerated() {
            let instr = &mut self.func.basic_blocks[instr_uid.bb].instructions[instr_uid.instr];
            for reg in instr.read_regs_mut() {
                if let Some(vreg) = reg.try_as_virtual() {
                    debug!(
                        "Replacing {} with its physical register at {}",
                        vreg, instr_uid
                    );
                    let phys_reg = self
                        .allocations
                        .get_allocated_reg(vreg)
                        .unwrap_or_else(|| panic!("{vreg} was not allocated"));
                    *reg = Register::Physical(phys_reg);
                }
            }
            for reg in instr.written_regs_mut() {
                if let Some(vreg) = reg.try_as_virtual() {
                    debug!(
                        "Replacing {} with its physical register at {}",
                        vreg, instr_uid
                    );
                    let phys_reg = self
                        .allocations
                        .get_allocated_reg(vreg)
                        .unwrap_or_else(|| panic!("{vreg} was not allocated"));
                    *reg = Register::Physical(phys_reg);
                }
            }
        }
    }

    fn insert_fixed_locations_for_function_params(&mut self) {
        let slots = TM::CallingConvention::parameter_slots(
            self.func
                .params
                .iter()
                .map(|param| self.func.vregs[*param].size),
        )
        .collect_vec();
        for (arg, slot) in self.func.params.iter().copied().zip(slots) {
            match slot {
                Slot::Register(reg) => {
                    self.func.vregs[arg].fixed = Some(reg);
                }
                Slot::Stack => unimplemented!(),
            }
        }
    }
}

impl<TM: TargetMachine> Function<TM> {
    pub fn liveness_repr(&mut self) -> LivenessRepr {
        #[derive(Default)]
        struct Liveins(FxHashMap<BasicBlockId, FxHashSet<VReg>>);
        impl Liveins {
            fn new() -> Self {
                Self(FxHashMap::default())
            }
            fn insert(&mut self, bb: BasicBlockId, reg: VReg) {
                self.0.entry(bb).or_default().insert(reg);
            }

            fn ensure_exists(&mut self, bb: BasicBlockId) {
                self.0.entry(bb).or_default();
            }

            fn liveins(&self, bb: BasicBlockId) -> impl Iterator<Item = VReg> + '_ {
                self.0[&bb].iter().copied()
            }
        }
        let liveins = Liveins::default();

        let mut repr = LivenessRepr::new(self);
        debug!("Starting liveness analysis");
        let worklist = self.cfg().dfs_postorder().collect::<VecDeque<_>>();
        let mut visited = FxHashSet::default();
        for bb_id in self.cfg().ordered().into_iter().rev() {
            debug!("Looking at {bb_id}");
            // if visited.contains(&bb_id) {
            //     debug!("Already visited");
            //     continue;
            // }
            // let mut all_visited = true;
            // for succ in self.cfg().successors(bb_id) {
            //     if succ != bb_id && !visited.contains(&succ) {
            //         debug!("Successor {succ} has not been visited yet. Queueing");
            //         all_visited = false;
            //         worklist.push_front(succ);
            //     }
            // }
            // if !all_visited {
            //     debug!("Not all successors have been visited. Queueing {bb_id} for later");
            //     worklist.push_back(bb_id);
            //     continue;
            // }
            debug!("Analysing liveness in {bb_id}");
            visited.insert(bb_id);
            let bb = &self.basic_blocks[bb_id];
            // let mut current_intervals = FxHashMap::default();
            // let mut undeclared_regs = FxHashSet::default();
            let entry_pp = bb.entry_pp(&repr);
            let exit_pp = bb.exit_pp(&repr.instr_numbering);
            // for liveout in self.cfg().successors(bb_id).flat_map(|pred| liveins.liveins(pred)) {
            //     debug!("{liveout} is in liveins of some succ. Extending its lifetime to the end of {bb_id}");
            //     current_intervals.insert(
            //         liveout,
            //         Lifetime::new(entry_pp, exit_pp),
            //     );
            // }
            let mut instr_nr = exit_pp.instr_nr();
            for instr in bb.instructions.iter().rev() {
                let out = instr.writes();
                if let Some(reg) = out.and_then(|reg| reg.try_as_virtual()) {
                    // undeclared_regs.remove(&reg);
                    // let mut interval = current_intervals.remove(&reg).unwrap_or_else(|| {
                    //     debug!("Creating new interval for {reg} at {instr_nr}");
                    //     Lifetime::new(ProgPoint::Write(instr_nr), ProgPoint::Write(instr_nr))
                    // });
                    // debug!("Finished interval for {reg} at {instr_nr}");
                    repr.record_def(reg, instr_nr);
                }
                let read = instr.reads();
                for reg in read {
                    let Register::Virtual(reg) = reg else {
                        continue;
                    };
                    // undeclared_regs.insert(reg);
                    // current_intervals.entry(reg).or_insert_with(
                    //     || {
                    //         debug!("Setting interval end of {reg} to {instr_nr}");
                    //         Lifetime::new(ProgPoint::Read(instr_nr), ProgPoint::Read(instr_nr))
                    //     }
                    // );
                    repr.record_use(reg, instr_nr);
                }
                if let Some(val) = instr_nr.checked_sub(1) {
                    instr_nr = val;
                }
            }
            // liveins.ensure_exists(bb_id);

            // for undeclared_reg in undeclared_regs {
            //     debug!("Inserting {undeclared_reg} in liveins set of {bb_id} and extending its lifetime to the start of the basic block");
            //     liveins.insert(bb_id, undeclared_reg);
            //     current_intervals.get_mut(&undeclared_reg).unwrap().start = entry_pp;
            // }
            //
            // for (reg, interval) in current_intervals {
            //     repr.reg_lifetimes[reg].insert(interval);
            // }
        }

        debug!("{}", repr.display(self));
        repr
    }
}
