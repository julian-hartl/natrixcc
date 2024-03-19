use iter_tools::Itertools;

use crate::codegen::machine::{Abi, PhysicalRegister, Size};
use crate::codegen::register_allocator::{Lifetime, LivenessRepr, ProgPoint, RegAllocAlgorithm, RegAllocHints, RegAllocVReg};

pub struct RegAlloc<'liveness, A: Abi> {
    /// Liveness representation.
    liveness_repr: &'liveness LivenessRepr,
    /// List of free registers.
    free_regs: Vec<A::REG>,
    active: Vec<(Lifetime, A::REG)>,
    inactive: Vec<(Lifetime, A::REG)>,
}

impl<'liveness, A: Abi> RegAllocAlgorithm<'liveness, A> for RegAlloc<'liveness, A> {
    fn new(liveness_repr: &'liveness LivenessRepr) -> Self {
        let free_regs = A::REG::all().iter().copied().collect_vec();
        Self {
            liveness_repr,
            free_regs,
            active: Vec::new(),
            inactive: Vec::new(),
        }
    }
    fn allocate_arbitrary(&mut self, vreg: &RegAllocVReg, hints: RegAllocHints<A>) -> A::REG {
        self.set_cursor(vreg.lifetime.start);
        let reg = hints.into_iter().find(
            |hint| self.is_free(*hint)
        ).unwrap_or_else(|| self.find_reg_with_size(vreg.size).expect("No free register available"));
        self.insert_active(vreg.lifetime.clone(), reg);
        reg
    }

    fn try_allocate_fixed(&mut self, vreg: &RegAllocVReg, reg: A::REG) -> bool {
        self.set_cursor(vreg.lifetime.start);
        if self.is_free(reg) {
            self.insert_active(vreg.lifetime.clone(), reg);
            true
        } else {
            false
        }
    }

    fn try_evict(&mut self, reg: A::REG) -> bool {
        if self.is_free(reg) {
            return false;
        }
        let i = self.active.iter().position(|(_, r)| r == &reg).expect("Register not found in active list even though it is not free");
        self.remove_active(i);
        true
    }
}

impl<A: Abi> RegAlloc<'_, A> {
    /// Configures the algorithm to view allocation state at the given program point.
    fn set_cursor(&mut self, at: ProgPoint) {
        self.expire_old_intervals(at);
        self.reactivate_intervals(at);
    }


    /// Inserts a new live interval into the active list.
    fn insert_active(&mut self, interval: Lifetime, reg: A::REG) {
        self.free_regs.retain(|r| !reg.interferes_with(*r));
        // let idx = self.active.iter().position(|(i, _)| i.end >= interval.end);
        // if let Some(idx) = idx {
        //     self.active.insert(idx, (interval, reg));
        // } else {
        //     self.active.push((interval, reg));
        // }
        self.active.push((interval, reg));
    }

    fn remove_active(&mut self, i: usize) -> (Lifetime, A::REG) {
        let (lifetime, reg) = self.active.remove(i);
        self.free_regs.push(reg);
        for subreg in reg.subregs().into_iter().flatten().copied() {
            self.free_regs.push(subreg);
        }
        (lifetime, reg)
    }

    fn insert_inactive(&mut self, interval: Lifetime, reg: A::REG) {
        self.inactive.push((interval, reg));
    }

    fn remove_inactive(&mut self, i: usize) -> (Lifetime, A::REG) {
        self.inactive.remove(i)
    }

    fn expire_old_intervals(&mut self, pp: ProgPoint) {
        let mut i = 0;
        while i < self.active.len() {
            let (interval, _) = &self.active[i];
            if !interval.contains(pp) {
                let (interval, reg) = self.remove_active(i);
                self.insert_inactive(interval, reg);
            } else {
                i += 1;
            }
        }
    }

    fn reactivate_intervals(&mut self, pp: ProgPoint) {
        let mut i = 0;
        while i < self.inactive.len() {
            let (interval, _) = &self.inactive[i];
            if interval.contains(pp) {
                let (interval, reg) = self.remove_inactive(i);
                self.insert_active(interval, reg);
            } else {
                i += 1;
            }
        }
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

    fn is_free(&self, reg: A::REG) -> bool {
        self.free_regs.contains(&reg)
    }
}
