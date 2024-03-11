use tracing::{debug, info};
use crate::codegen::machine::{Abi, Instr, Module, PseudoInstr};

pub struct Coalescer<'module, A: Abi> {
    module: &'module mut Module<A>,
}

impl<'module, A: Abi> Coalescer<'module, A> {
    pub fn new(module: &'module mut Module<A>) -> Self {
        Self {
            module,
        }
    }

    pub fn run(&mut self) {
        info!("Running register coalescer");
        for (_, function) in &mut self.module.functions {
            let ordered_instrs = function.ordered_instructions().into_iter().collect::<Vec<_>>();
            let mut instructions_to_remove = Vec::new();
            for (bb_id, instr_id) in ordered_instrs {
                let instr = &mut function.instructions[instr_id];
                match instr {
                    Instr::Pseudo(instr) => {
                        match instr {
                            PseudoInstr::Copy(dest, src) => {
                                if dest == src {
                                    debug!("Removing redundant copy: {:?}", instr);
                                    instructions_to_remove.push((bb_id, instr_id));
                                }
                            }
                        }
                    }
                    Instr::Machine(_) => {}
                }
            }
            for (bb_id, instr_id) in instructions_to_remove {
                function.remove_instruction(bb_id, instr_id);
            }
        }
    }
}