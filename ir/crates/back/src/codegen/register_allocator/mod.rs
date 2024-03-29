use std::{
    cmp::Ordering,
    collections::VecDeque,
    fmt::{
        Display,
        Formatter,
    },
};

pub use coalescer::Coalescer;
pub use cranelift_entity::SecondaryMap;
pub use daggy::Walker;
pub use iter_tools::Itertools;
use rustc_hash::{
    FxHashMap,
    FxHashSet,
};
use smallvec::{
    smallvec,
    SmallVec,
};
use tracing::debug;

use crate::codegen::machine::{
    abi::{
        calling_convention::Slot,
        CallingConvention,
    },
    function::{
        BasicBlockId,
        Function,
    },
    instr::{
        Instr,
        PseudoInstr,
    },
    isa::PhysicalRegister,
    reg::{
        Register,
        VReg,
    },
    InstrId,
    Size,
    TargetMachine,
};

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
    /// List of ranges in which the variable is live, sorted by start point.
    pub ranges: SmallVec<[LiveRange; 3]>,
}

impl Lifetime {
    /// Adds a new range to the lifetime, potentially merging it with existing ranges.
    ///
    /// Keeps the ranges sorted by start point.
    pub fn add_range(&mut self, range: LiveRange) {
        // todo: improve performance
        let mut new_ranges = SmallVec::new();
        let mut added = false;
        for existing_range in &self.ranges {
            if LiveRange::are_adjacent(existing_range, &range) {
                let new_range = LiveRange {
                    start: std::cmp::min(existing_range.start, range.start),
                    end: std::cmp::max(existing_range.end, range.end),
                };
                new_ranges.push(new_range);
                added = true;
            } else {
                new_ranges.push(existing_range.clone());
            }
        }
        if !added {
            new_ranges.push(range);
        }
        new_ranges.sort_by(|a: &LiveRange, b| a.start.cmp(&b.start));
        self.ranges = new_ranges;
    }

    pub fn start(&self) -> ProgPoint {
        self.ranges
            .first()
            .map(|range| range.start)
            .expect("Lifetime has no ranges")
    }

    pub fn overlaps_with(&self, other: &Self) -> bool {
        self.ranges.iter().any(|range| {
            other
                .ranges
                .iter()
                .any(|other_range| LiveRange::are_overlapping(range, other_range))
        })
    }

    pub fn contains(&self, pp: ProgPoint) -> bool {
        self.ranges.iter().any(|range| range.contains(pp))
    }
}

impl Display for Lifetime {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (index, range) in self.ranges.iter().enumerate() {
            write!(f, "{}", range)?;
            if index < self.ranges.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct LiveRange {
    pub start: ProgPoint,
    pub end: ProgPoint,
}

impl LiveRange {
    pub fn new(start: ProgPoint, end: ProgPoint) -> Self {
        Self { start, end }
    }
    pub fn contains(&self, pp: ProgPoint) -> bool {
        self.start <= pp && pp <= self.end
    }

    /// Returns true if the two live ranges overlap.
    ///
    /// Two live ranges l, j overlap if the intersection of their ranges is not empty.
    /// This is the case iff.
    /// - l.start <= j.end
    /// and
    /// - j.start <= l.end
    ///
    /// Overlaps are **symmetric**.
    pub fn are_overlapping(l: &Self, j: &Self) -> bool {
        l.start <= j.end && j.start <= l.end
    }

    pub fn are_adjacent(l: &Self, j: &Self) -> bool {
        let are_adjacent = |j: &Self, l: &Self| match (l.end, j.start) {
            (ProgPoint::Read(a), ProgPoint::Write(b)) => a == b,
            (ProgPoint::Write(a), ProgPoint::Read(b)) => a + 1 == b,
            _ => false,
        };
        are_adjacent(l, j) || are_adjacent(j, l)
    }

    pub fn overlaps_with(&self, other: &Self) -> bool {
        Self::are_overlapping(self, other)
    }
}

impl Display for LiveRange {
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
        LiveRange,
        ProgPoint,
    };

    #[test]
    fn lifetimes_overlap() {
        let inputs = [
            // Both lifetimes are the same
            (
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Read(2)),
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Read(2)),
                true,
            ),
            // The second lifetime is within the first one
            (
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Read(2)),
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Read(1)),
                true,
            ),
            // The lifetimes do not overlap
            (
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Read(1)),
                LiveRange::new(ProgPoint::Write(2), ProgPoint::Read(3)),
                false,
            ),
            // The lifetimes overlap at one point
            (
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Write(2)),
                LiveRange::new(ProgPoint::Write(2), ProgPoint::Read(3)),
                true,
            ),
            // The lifetimes are the same but with different ProgPoints
            (
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Write(2)),
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Read(2)),
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
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Read(2)),
                ProgPoint::Read(1),
                true,
            ),
            // The lifetime does not contain the ProgPoint
            (
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Read(1)),
                ProgPoint::Write(2),
                false,
            ),
            // The lifetime contains the ProgPoint at the interval end
            (
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Write(2)),
                ProgPoint::Write(2),
                true,
            ),
            // The lifetime does not contain the ProgPoint at the interval start
            (
                LiveRange::new(ProgPoint::Write(0), ProgPoint::Read(1)),
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
        // todo: write test for this
        Some(offset + len - 1)
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

#[derive(Default, Debug)]
pub struct LiveSets(FxHashMap<BasicBlockId, FxHashSet<VReg>>);
impl LiveSets {
    fn insert(&mut self, bb: BasicBlockId, reg: VReg) {
        self.0.entry(bb).or_default().insert(reg);
    }

    fn remove(&mut self, bb: BasicBlockId, reg: VReg) {
        self.0.entry(bb).or_default().remove(&reg);
    }

    fn get(&self, bb: BasicBlockId) -> impl Iterator<Item = VReg> + '_ {
        self.0
            .get(&bb)
            .map(|regs| regs.iter().copied())
            .into_iter()
            .flatten()
    }
}

#[derive(Debug)]
pub struct LivenessRepr {
    pub instr_numbering: InstrNumbering,
    pub lifetimes: SecondaryMap<VReg, Lifetime>,
}

impl LivenessRepr {
    pub fn new<A: TargetMachine>(func: &Function<A>) -> Self {
        debug!("Creating liveness representation");
        Self {
            instr_numbering: InstrNumbering::new(func),
            lifetimes: SecondaryMap::default(),
        }
    }

    pub fn add_range(&mut self, vreg: VReg, range: LiveRange) {
        self.lifetimes[vreg].add_range(range);
    }
}

impl Display for LivenessRepr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (vreg, lifetime) in self.lifetimes.iter() {
            writeln!(f, "{}: {}", vreg, lifetime)?;
        }
        Ok(())
    }
}

pub type RegAllocHints<TM> = SmallVec<[<TM as TargetMachine>::Reg; 2]>;

#[derive(Debug, Clone)]
pub struct RegAllocVReg {
    pub id: VReg,
    pub size: Size,
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

pub struct RegisterAllocator<
    'liveness,
    'func,
    A: TargetMachine,
    RegAlloc: RegAllocAlgorithm<'liveness, A>,
> {
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
        let mut worklist = VecDeque::new();
        for instr_uid in self.liveness_repr.instr_numbering.iter() {
            if instr_uid.instr == InstrId::new(0) {
                let bb = instr_uid.bb;
                for (phi, _) in self.func.basic_blocks[bb].phis.iter() {
                    if let Some(vreg) = phi.try_as_virtual() {
                        worklist.push_back((vreg, None));
                    }
                }
            }
            let instr = &self.func.basic_blocks[instr_uid.bb].instructions[instr_uid.instr];
            if let Some(writes) = instr.writes() {
                if let Some(vreg) = writes.try_as_virtual() {
                    worklist.push_back((vreg, Some(instr_uid)));
                }
            }
        }
        while let Some((vreg, instr_uid)) = worklist.pop_front() {
            if self.allocations.get_allocated_reg(vreg).is_some() {
                continue;
            }
            let mut hints: SmallVec<[_; 2]> = smallvec![];
            if let Some(instr_uid) = instr_uid {
                let instr = &self.func.basic_blocks[instr_uid.bb].instructions[instr_uid.instr];
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
                        PseudoInstr::Def(_) => {}
                    },
                    Instr::Machine(_) => {}
                };
            }
            let lifetime = &self.liveness_repr.lifetimes[vreg];
            let vreg_info = &self.func.vregs[vreg];
            let size = vreg_info.size;
            let alloc_vreg = RegAllocVReg { id: vreg, size };
            let phys_reg = match vreg_info.fixed {
                None => match vreg_info.tied_to {
                    None => {
                        debug!(
                            "Allocating {vreg} at {:?} with hints: {:?} and size {size}",
                            instr_uid, hints
                        );

                        Some(self.algo.allocate_arbitrary(&alloc_vreg, hints))
                    }
                    Some(tied_to) => {
                        debug!(
                            "{vreg} is tied to {tied_to}. Trying to put it in the same register"
                        );
                        assert!(
                            !lifetime.overlaps_with(&self.liveness_repr.lifetimes[tied_to]),
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
                        "Allocating {vreg} at {:?} in fixed register {}",
                        instr_uid,
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
                worklist.push_back((vreg, instr_uid));
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
        #[derive(Default, Clone)]
        struct IncompleteLiveRange {
            start: Option<ProgPoint>,
            end: Option<ProgPoint>,
        }

        impl IncompleteLiveRange {
            fn set_start(&mut self, pp: ProgPoint) {
                self.start = Some(pp);
            }
            fn maybe_set_end(&mut self, pp: ProgPoint) {
                if self.end.is_none() {
                    self.end = Some(pp);
                }
            }
        }

        let mut repr = LivenessRepr::new(self);
        debug!("Starting liveness analysis");
        let mut live_sets = LiveSets::default();
        for bb_id in self.cfg().ordered().into_iter().rev() {
            debug!("Analysing liveness in {bb_id}");
            let bb = &self.basic_blocks[bb_id];
            let entry_pp = bb.entry_pp(&repr.instr_numbering);
            let exit_pp = bb.exit_pp(&repr.instr_numbering);
            let mut local_live_ranges = SecondaryMap::default();
            for succ in self.cfg().successors(bb_id) {
                let liveset = live_sets.get(succ).collect::<SmallVec<[_; 3]>>();
                for liveout in liveset {
                    live_sets.insert(bb_id, liveout);
                    local_live_ranges[liveout] = Some(IncompleteLiveRange {
                        start: None,
                        end: Some(exit_pp),
                    });
                }
            }
            let mut instr_nr = exit_pp.instr_nr();
            for instr in bb.instructions.iter().rev() {
                let out = instr.writes();
                if let Some(reg) = out.and_then(|reg| reg.try_as_virtual()) {
                    live_sets.remove(bb_id, reg);
                    local_live_ranges[reg]
                        .get_or_insert_default()
                        .set_start(ProgPoint::Write(instr_nr));
                }
                let read = instr.reads();
                for reg in read {
                    let Register::Virtual(reg) = reg else {
                        continue;
                    };
                    live_sets.insert(bb_id, reg);
                    local_live_ranges[reg]
                        .get_or_insert_default()
                        .maybe_set_end(ProgPoint::Read(instr_nr));
                }
                if let Some(val) = instr_nr.checked_sub(1) {
                    instr_nr = val;
                }
            }
            for (def, _) in &bb.phis {
                if let Some(def) = def.try_as_virtual() {
                    live_sets.remove(bb_id, def);
                    local_live_ranges[def]
                        .get_or_insert_default()
                        .set_start(entry_pp);
                }
            }
            for (vreg, range) in local_live_ranges
                .iter()
                .filter_map(|(vreg, range)| range.as_ref().map(|range| (vreg, range)))
            {
                let start = range.start.unwrap_or(entry_pp);
                let range = LiveRange::new(start, range.end.unwrap_or(start));
                repr.add_range(vreg, range);
            }
        }

        debug!("{:?}", live_sets);

        debug!("{}", repr);
        repr
    }
}
