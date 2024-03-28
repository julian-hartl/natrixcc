use iter_tools::Itertools;
use tracing::debug;

use crate::codegen::{
    machine::{
        isa::PhysicalRegister,
        Size,
        TargetMachine,
    },
    register_allocator::{
        Lifetime,
        LivenessRepr,
        ProgPoint,
        RegAllocAlgorithm,
        RegAllocHints,
        RegAllocVReg,
    },
};
use crate::codegen::machine::TargetMachine;

#[derive(Debug)]
pub struct RegAlloc<'liveness, TM: TargetMachine> {
    /// Liveness representation.
    liveness_repr: &'liveness LivenessRepr,
    /// List of free registers.
    free_regs: Vec<TM::Reg>,
    active: Vec<(Lifetime, TM::Reg)>,
    inactive: Vec<(Lifetime, TM::Reg)>,
}

impl<'liveness, TM: TargetMachine> RegAllocAlgorithm<'liveness, TM> for RegAlloc<'liveness, TM> {
    fn new(liveness_repr: &'liveness LivenessRepr) -> Self {
        let free_regs = TM::Reg::all()
            .iter()
            .copied()
            .filter(PhysicalRegister::is_gp)
            .collect_vec();
        debug!("Available registers: {:?}", free_regs);
        Self {
            liveness_repr,
            free_regs,
            active: Vec::new(),
            inactive: Vec::new(),
        }
    }
    fn allocate_arbitrary(&mut self, vreg: &RegAllocVReg, hints: RegAllocHints<TM>) -> TM::Reg {
        self.set_cursor(vreg.lifetime.start);
        let reg = hints
            .into_iter()
            .find(|hint| self.is_free(*hint))
            .unwrap_or_else(|| {
                self.find_reg_with_size(vreg.size).unwrap_or_else(|| {
                    debug!("Active: {:?}", self.active);
                    debug!("Inactive: {:?}", self.inactive);
                    panic!("No free register available for size {}", vreg.size)
                })
            });
        self.insert_active(vreg.lifetime.clone(), reg);
        reg
    }

    fn try_allocate_fixed(&mut self, vreg: &RegAllocVReg, reg: TM::Reg) -> bool {
        self.set_cursor(vreg.lifetime.start);
        if self.is_free(reg) {
            self.insert_active(vreg.lifetime.clone(), reg);
            true
        } else {
            false
        }
    }

    fn try_evict(&mut self, reg: TM::Reg) -> bool {
        if self.is_free(reg) {
            return false;
        }
        let i = self
            .active
            .iter()
            .position(|(_, r)| r == &reg)
            .expect("Register not found in active list even though it is not free");
        self.remove_active(i);
        true
    }
}

impl<TM: TargetMachine> RegAlloc<'_, TM> {
    /// Configures the algorithm to view allocation state at the given program point.
    fn set_cursor(&mut self, at: ProgPoint) {
        debug!("Setting cursor to {}", at);
        self.expire_old_intervals(at);
        self.reactivate_intervals(at);
    }

    /// Inserts a new live interval into the active list.
    fn insert_active(&mut self, interval: Lifetime, reg: TM::Reg) {
        self.free_regs.retain(|r| {
            if reg.interferes_with(*r) {
                debug!("Removing {} from free list", r.name());
                false
            } else {
                true
            }
        });
        // let idx = self.active.iter().position(|(i, _)| i.end >= interval.end);
        // if let Some(idx) = idx {
        //     self.active.insert(idx, (interval, reg));
        // } else {
        //     self.active.push((interval, reg));
        // }
        self.active.push((interval, reg));
    }

    fn remove_active(&mut self, i: usize) -> (Lifetime, TM::Reg) {
        let (lifetime, reg) = self.active.remove(i);
        debug!("Removing active interval: {}", lifetime);
        for reg in reg.regclass().filter(|r| reg.interferes_with(*r)) {
            self.free_reg(reg);
        }
        (lifetime, reg)
    }

    fn free_reg(&mut self, reg: TM::Reg) {
        debug!("Freeing register {}", reg.name());
        self.free_regs.push(reg);
    }

    fn insert_inactive(&mut self, interval: Lifetime, reg: TM::Reg) {
        debug!("Inserting inactive interval: {}", interval);
        self.inactive.push((interval, reg));
    }

    fn remove_inactive(&mut self, i: usize) -> (Lifetime, TM::Reg) {
        let (lifetime, reg) = self.inactive.remove(i);
        debug!("Removing inactive interval: {}", lifetime);
        (lifetime, reg)
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
    fn find_reg_with_size(&self, size: Size) -> Option<TM::Reg> {
        for reg in &self.free_regs {
            if reg.size() == size {
                return Some(*reg);
            }
        }
        None
    }

    fn is_free(&self, reg: TM::Reg) -> bool {
        self.free_regs.contains(&reg)
    }
}
