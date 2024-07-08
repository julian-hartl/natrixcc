use fusion_compiler::Idx;

use crate::mir::{basic_block::BasicBlockIdx, optimizations::MIRPass, InstructionKind, MIR};

pub struct UnreachableCodeElimination;

impl MIRPass for UnreachableCodeElimination {
    fn run(&mut self, mir: &mut MIR) -> u32 {
        let mut changes = 0;

        for function in mir.functions.iter_mut() {
            let mut basic_blocks_to_remove = vec![];
            let predecessors = function.predecessors(&mir.basic_blocks);

            for bb in function.basic_blocks.iter().copied() {
                if bb == BasicBlockIdx::first() {
                    continue;
                }
                if predecessors
                    .get_immediate(bb)
                    .map(|i| i.len())
                    .unwrap_or_default()
                    == 0
                {
                    tracing::debug!("Found unreachable basic block {}, removing it", bb);
                    basic_blocks_to_remove.push(bb);
                    changes += 1;
                }
            }
            let successors = function.successors(&mir.basic_blocks);
            for bb_idx in basic_blocks_to_remove.into_iter() {
                // Only iterate over the immediate successors, because that is the only place where phi nodes can reference the basic block
                if let Some(successors) = successors.get_immediate(bb_idx) {
                    for successor in successors {
                        let successor = mir.basic_blocks.get_mut_or_panic(*successor);
                        for instruction_idx in successor.instructions.iter_mut() {
                            let instruction = function.instructions.get_mut(*instruction_idx);
                            match &mut instruction.kind {
                                InstructionKind::Binary { .. } => {}
                                InstructionKind::Unary { .. } => {}
                                InstructionKind::Value(_) => {}
                                InstructionKind::Call { .. } => {}
                                InstructionKind::Phi(phi) => {
                                    phi.operands.retain(|(from, _)| *from != bb_idx);
                                }
                            }
                        }
                    }
                }
                mir.basic_blocks.remove(bb_idx);
                function.basic_blocks.retain(|bb| *bb != bb_idx);
            }
        }

        changes
    }
}
