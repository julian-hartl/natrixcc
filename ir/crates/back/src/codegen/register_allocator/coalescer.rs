use tracing::{debug, info};

use crate::codegen::machine::{Abi, Instr, Module, PseudoInstr, TargetMachine};

pub struct Coalescer<'module, TM: TargetMachine> {
    module: &'module mut Module<TM>,
}

impl<'module, TM: TargetMachine> Coalescer<'module, TM> {
    pub fn new(module: &'module mut Module<TM>) -> Self {
        Self {
            module,
        }
    }

    pub fn run(&mut self) {
        info!("Running register coalescer");
        for (_, function) in &mut self.module.functions {
            for bb in &mut function.basic_blocks {
                let mut instructions_to_remove = Vec::new();
                for (instr_id, instr) in bb.instructions.iter_enumerated() {
                    match instr {
                        Instr::Pseudo(instr) => {
                            match instr {
                                PseudoInstr::Copy(dest, src) => {
                                    if dest == src {
                                        debug!("Removing redundant copy: {:?}", instr);
                                        instructions_to_remove.push(instr_id);
                                    }
                                }
                                PseudoInstr::Ret(_) => {}
                                PseudoInstr::Phi(dest, operands) => {
                                    if operands.iter().all(|op| op == dest) {
                                        debug!("Removing redundant phi: {:?}", instr);
                                        instructions_to_remove.push(instr_id);
                                    } 
                                }
                                PseudoInstr::Def(_) => {}
                            }
                        }
                        Instr::Machine(_) => {}
                    }
                }
                for (removed, instr_id) in instructions_to_remove.into_iter().enumerate() {
                    bb.instructions.remove(instr_id - removed);
                }
            }
        }
    }
}