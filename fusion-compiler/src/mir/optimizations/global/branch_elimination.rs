use crate::mir::{MIR, TerminatorKind};
use crate::mir::optimizations::MIRPass;

pub struct BranchElimination;


impl MIRPass for BranchElimination {
    fn run(&mut self, mir: &mut MIR) -> u32 {
        let mut changes = 0;
        for function in mir.functions.iter_mut() {
            let mut basic_blocks_to_remove = Vec::new();
            let predecessors = function.predecessors();
            for bb_idx in function.basic_blocks.indices() {
                let bb = function.basic_blocks.get(bb_idx);
                match bb {
                    None => continue,
                    Some(bb) => {
                        match &bb.terminator.kind {
                            TerminatorKind::Return { .. } => {}
                            TerminatorKind::Jump(target_idx) => {
                                let target_idx = *target_idx;
                                if predecessors.get_immediate(target_idx).map(|i| i.len()).unwrap_or_default() == 1 {
                                    tracing::info!("Found a jump to a basic block with a single predecessor, merging basic blocks {:?} and {:?}", bb_idx, target_idx);
                                    let target = function.basic_blocks.get_or_panic(target_idx).clone();
                                    let bb = function.basic_blocks.get_mut_or_panic(bb_idx);
                                    bb.append(target);
                                    basic_blocks_to_remove.push(target_idx);
                                    changes += 1;
                                }
                            }
                            TerminatorKind::SwitchInt { value, cases, default } => {
                                if let Some(value) = value.as_i64() {
                                    tracing::info!("Found a switch on a constant value, replacing with a jump to the target");
                                    let case = cases.iter().find(|(case_value, _)| *case_value as i64 == value);
                                    let target = case.map(|c| c.1).unwrap_or(*default);
                                    let bb = function.basic_blocks.get_mut_or_panic(bb_idx);
                                    bb.terminator.kind = TerminatorKind::Jump(target);
                                    changes += 1;
                                }
                            }
                            TerminatorKind::Unresolved |
                            TerminatorKind::Unreachable => {}
                        }

                    }
                }
            }
            for bb_idx in basic_blocks_to_remove {
                function.basic_blocks.remove(bb_idx);
            }
        }
        changes
    }
}
