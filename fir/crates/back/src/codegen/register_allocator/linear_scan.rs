use iter_tools::Itertools;

use crate::codegen::machine::{Abi, PhysicalRegister, Size, VReg};
use crate::codegen::register_allocator::{LiveInterval, LivenessRepr, ProgPoint, RegAllocAlgorithm, RegAllocHints};

pub struct RegAlloc<'liveness, A: Abi> {
    /// Liveness representation.
    liveness_repr: &'liveness LivenessRepr,
    /// List of free registers.
    free_regs: Vec<A::REG>,
    /// List of active live intervals, sorted by end point.
    active: Vec<(LiveInterval, A::REG)>,
}

impl<'liveness, A: Abi> RegAllocAlgorithm<'liveness, A> for RegAlloc<'liveness, A> {
    fn new(liveness_repr: &'liveness LivenessRepr) -> Self {
        let free_regs = A::REG::all().iter().copied().collect_vec();
        Self {
            liveness_repr,
            free_regs,
            active: Vec::new(),
        }
    }
    fn allocate(&mut self, _: VReg, vreg_interval: LiveInterval, size: Size, hints: RegAllocHints<A>) -> A::REG {
        self.expire_old_intervals(vreg_interval.start);
        let reg = hints.into_iter().find(
            |hint| self.free_regs.contains(hint)
        ).unwrap_or_else(|| self.find_reg_with_size(size).expect("No free register available"));
        self.insert_active(vreg_interval, reg);
        reg
    }
}

impl<A: Abi> RegAlloc<'_, A> {

    /// Inserts a new live interval into the active list.
    fn insert_active(&mut self, interval: LiveInterval, reg: A::REG) {
        self.free_regs.retain(|r| r != &reg);
        let idx = self.active.iter().position(|(i, _)| i.end >= interval.end);
        if let Some(idx) = idx {
            self.active.insert(idx, (interval, reg));
        } else {
            self.active.push((interval, reg));
        }
    }

    fn expire_old_intervals(&mut self, pp: ProgPoint) {
        self.active.retain(|(i, reg)| {
            if i.contains(&pp) {
                true
            } else {
                self.free_regs.push(*reg);
                false
            }
        });
    }

    /// Finds a free register with the given size
    fn find_reg_with_size(&self, size: Size) -> Option<A::REG> {
        for reg in &self.free_regs {
            if reg.size() == size {
                return Some(*reg);
            } 
        }
        None
    }
}