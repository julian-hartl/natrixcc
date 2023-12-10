use crate::mir::{InstructionKind, MIR, TerminatorKind};
use crate::mir::optimizations::MIRPass;

pub struct BranchElimination;


impl MIRPass for BranchElimination {
    fn run(&mut self, mir: &mut MIR) -> u32 {
        let mut changes = 0;
        for function in mir.functions.iter_mut() {
            let mut basic_blocks_to_remove = Vec::new();
            let predecessors = function.predecessors(&mir.basic_blocks);
            for bb_idx in function.basic_blocks.iter().copied() {
                let bb = mir.basic_blocks.get_mut(bb_idx).as_mut();
                match bb {
                    None => continue,
                    Some(bb) => {
                        if let Some(terminator) = bb.terminator.as_mut() {

                            match &terminator.kind {
                                TerminatorKind::Return { .. } => {}
                                TerminatorKind::Jump(target_idx) => {
                                    let target_idx = *target_idx;
                                    if predecessors.get_immediate(target_idx).map(|i| i.len()).unwrap_or_default() == 1 {
                                        tracing::info!("Found a jump to a basic block with a single predecessor, merging basic blocks {:?} and {:?}", bb_idx, target_idx);
                                        let target = mir.basic_blocks.get_or_panic(target_idx).clone();
                                        let bb = mir.basic_blocks.get_mut_or_panic(bb_idx);
                                        bb.append(target);
                                        basic_blocks_to_remove.push((target_idx, bb_idx));
                                        changes += 1;
                                    }
                                }
                                TerminatorKind::SwitchInt { value, cases, default } => {
                                    if let Some(value) = value.as_i64() {
                                        tracing::info!("Found a switch on a constant value, replacing with a jump to the target");
                                        let case = cases.iter().find(|(case_value, _)| *case_value as i64 == value);
                                        let target = case.map(|c| c.1).unwrap_or(*default);
                                        terminator.kind = TerminatorKind::Jump(target);
                                        changes += 1;
                                    }
                                }
                                TerminatorKind::Unresolved  => {}
                            }
                        }

                    }
                }
            }
            let successors = function.successors(&mir.basic_blocks);
            for (bb_idx, replaced_with) in basic_blocks_to_remove {
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
                                    phi.operands = phi.operands.iter().copied().map(|(from, instruction_idx)| {
                                        if from == bb_idx {
                                            (replaced_with, instruction_idx)
                                        } else {
                                            (from, instruction_idx)
                                        }
                                    }).collect();


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
