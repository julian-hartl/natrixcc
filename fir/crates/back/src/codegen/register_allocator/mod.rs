use std::fmt::Display;
use std::ops::Range;

use cranelift_entity::SecondaryMap;
use rustc_hash::FxHashMap;
use tracing::debug;

pub use coalescer::Coalescer;

use crate::codegen::machine::{Abi, Function, Instr, InstrId, InstrInsMut, InstrOperand, PhysicalRegister, PseudoInstr, Register, Size, VReg};

mod coalescer;
pub mod linear_scan;

type InstrNr = u32;
pub type LiveInterval = Range<InstrNr>;

#[derive(Debug)]
pub struct LivenessRepr<A: Abi> {
    vreg_intervals: SecondaryMap<VReg, Option<LiveInterval>>,
    phys_reg_intervals: FxHashMap<A::REG, LiveInterval>,
}

impl<A: Abi> Display for LivenessRepr<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "LivenessRepr")?;
        writeln!(f, "Instr numbering")?;
        writeln!(f, "Virtual register intervals")?;
        for (vreg, interval) in self.vreg_intervals.iter() {
            writeln!(f, "{}: {:?}", vreg, interval.as_ref().unwrap())?;
        }
        writeln!(f, "Physical register intervals")?;
        for (reg, interval) in &self.phys_reg_intervals {
            writeln!(f, "{}: {:?}", reg.name(), interval)?;
        }
        Ok(())
    }
}

impl<A: Abi> LivenessRepr<A> {
    pub fn new() -> Self {
        Self {
            vreg_intervals: SecondaryMap::default(),
            phys_reg_intervals: FxHashMap::default(),
        }
    }
}

pub trait RegAllocAlgorithm<'liveness, A: Abi> {
    fn new(liveness_repr: &'liveness LivenessRepr<A>) -> Self;

    fn allocate(&mut self, vreg: VReg, size: Size, hint: Option<A::REG>) -> A::REG;
}

pub struct RegisterAllocator<'liveness, 'func, A: Abi, RegAlloc: RegAllocAlgorithm<'liveness, A>> {
    algo: RegAlloc,
    liveness_repr: &'liveness LivenessRepr<A>,
    func: &'func mut Function<A>,
    marker: std::marker::PhantomData<A>,
    vreg_to_reg: SecondaryMap<VReg, Option<A::REG>>,
}

impl<'liveness, 'func, A: Abi, RegAlloc: RegAllocAlgorithm<'liveness, A>> RegisterAllocator<'liveness, 'func, A, RegAlloc> {
    pub fn new(func: &'func mut Function<A>, liveness_repr: &'liveness LivenessRepr<A>) -> Self {
        Self {
            func,
            algo: RegAlloc::new(liveness_repr),
            liveness_repr,
            marker: std::marker::PhantomData,
            vreg_to_reg: SecondaryMap::new(),
        }
    }

    pub fn run(mut self) {
        let instructions = self.func.ordered_instructions().into_iter().collect::<Vec<_>>();
        for (_, instr_id) in instructions {
            let instr = &mut self.func.instructions[instr_id];
            for operand in instr.ins_mut() {
                match operand {
                    InstrInsMut::Reg(reg) => {
                        if let Register::Virtual(vreg) = reg {
                            *reg = Register::Physical(self.vreg_to_reg[*vreg].expect("vreg not allocated before use"));
                        }
                    }
                    // InstrInsMut::Imm(_) => {}
                }
            }
            let hint = match instr {
                Instr::Pseudo(instr) => {
                    match instr {
                        PseudoInstr::Copy(_, src) => {
                            src.try_as_physical()
                        }
                    }
                }
                Instr::Machine(_) => None,
            };
            let output = instr.out_mut();
            if let Some(reg) = output {
                if let Register::Virtual(vreg) = reg {
                    let vreg = *vreg;
                    let phys_reg = match self.vreg_to_reg[vreg] {
                        None => {
                            let size = self.func.vregs[vreg].size;
                            let phys_reg = self.algo.allocate(vreg, size, hint);
                            debug!("Allocated {} for {}", phys_reg.name(), vreg);
                            self.vreg_to_reg[vreg] = Some(phys_reg);
                            phys_reg
                        }
                        Some(phys_reg) => phys_reg,
                    };
                    *reg = Register::Physical(phys_reg);
                }
            }
        }
    }
}

impl<A: Abi> Function<A> {
    pub fn liveness_repr(&self) -> LivenessRepr<A> {
        let mut repr = LivenessRepr::new();
        debug!("Starting liveness analysis");
        for (nr, (_, instr_id)) in self.ordered_instructions().into_iter().enumerate() {
            let nr = InstrNr::try_from(nr).expect("Too many instructions");
            let instr = &self.instructions[instr_id];
            let out = instr.out();
            if let Some(reg) = out {
                debug!("Starting liveness interval of {} at {}", reg, nr);
                match reg {
                    Register::Physical(reg) => {
                        repr.phys_reg_intervals.entry(reg).or_insert(
                            0..nr
                        ).end = nr;
                    }
                    Register::Virtual(reg) => {
                        match &mut repr.vreg_intervals[reg] {
                            Some(interval) => {
                                interval.end = nr;
                            }
                            None => {
                                repr.vreg_intervals[reg] = Some(nr..nr);
                            }
                        }
                    }
                }
            }
            let operands = instr.ins();
            for operand in operands {
                match operand {
                    InstrOperand::Reg(reg) => {
                        debug!("Increasing end of live interval for {reg} to {nr}");
                        let interval = match reg {
                            Register::Virtual(reg) => {
                                repr.vreg_intervals[reg].as_mut().expect("Register is used before declaration")
                            }
                            Register::Physical(reg) => {
                                repr.phys_reg_intervals.entry(reg).or_insert(
                                    // todo: check whether reg is used as function argument
                                    0..nr
                                )
                            }
                        };
                        interval.end = nr;
                    }
                    InstrOperand::Imm(_) => {}
                    InstrOperand::Label(_) => {}
                }
            }
        }
        debug!("{}", repr);
        repr
    }
}
