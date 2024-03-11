use crate::codegen::machine::{Abi, PhysicalRegister, Size, VReg};
use crate::codegen::register_allocator::{InstrNr, LiveInterval, LivenessRepr, RegAllocAlgorithm};

pub struct RegAlloc<'liveness, A: Abi> {
    liveness_repr: &'liveness LivenessRepr<A>,
    /// List of free registers.
    free_regs: Vec<A::REG>,
    /// List of active live intervals, sorted by end point.
    active: Vec<(LiveInterval, A::REG)>,
}

impl<'liveness, A: Abi> RegAllocAlgorithm<'liveness, A> for RegAlloc<'liveness, A> {
    fn new(liveness_repr: &'liveness LivenessRepr<A>) -> Self {
        Self {
            liveness_repr,
            free_regs: A::REG::all().to_vec(),
            active: Vec::new(),
        }
    }

    fn allocate(&mut self, vreg: VReg, size: Size, hint: Option<A::REG>) -> A::REG {
        let vreg_interval = self.liveness_repr.vreg_intervals[vreg].clone().unwrap();
        self.expire_old_intervals(vreg_interval.start);
        self.update_active_phys_regs(vreg_interval.start);
        let reg = hint.and_then(
            |hint| if self.free_regs.contains(&hint) {
                Some(hint)
            } else {
                None
            }
        ).unwrap_or_else(|| self.find_reg_with_size(size).expect("No free register available"));
        self.insert_active(vreg_interval, reg);
        reg
    }
}

impl<A: Abi> RegAlloc<'_, A> {
    
    fn update_active_phys_regs(&mut self, instr_nr: InstrNr) {
        for (reg, interval) in &self.liveness_repr.phys_reg_intervals {
            if interval.contains(&instr_nr) {
                self.insert_active(interval.clone(), *reg);
            }
        }
    }
    
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

    fn expire_old_intervals(&mut self, instr_nr: InstrNr) {
        self.active.retain(|(i, reg)| {
            if i.contains(&instr_nr) {
                true
            } else {
                self.free_regs.push(*reg);
                false
            }
        });
    }

    /// Finds a register with best matching size
    fn find_reg_with_size(&self, size: Size) -> Option<A::REG> {
        let mut best_fit: Option<A::REG> = None;
        for reg in &self.free_regs {
            if reg.size() >= size {
                if let Some(best) = best_fit {
                    if reg.size() < best.size() {
                        best_fit = Some(*reg);
                    }
                } else {
                    best_fit = Some(*reg);
                }
            }
        }
        best_fit
    }
}