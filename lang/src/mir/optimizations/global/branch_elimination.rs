use std::collections::HashMap;

use itertools::Itertools;

use crate::mir::{
    basic_block::BasicBlockIdx, optimizations::MIRPass, InstructionKind, TerminatorKind, MIR,
};

/// Branch elimination is a global optimization that removes branches that are not needed.
///
/// This can happen in the following cases:
/// - A jump to a basic block with a single predecessor can be merged with the predecessor
/// - A switch on a constant value can be replaced with a jump to the target, which then may be merged with the predecessor again
pub struct BranchElimination;

// todo: we should replace this graph with the dominator tree as the dominator tree contains all paths that can be merged

struct AppendixGraph {
    edges: HashMap<BasicBlockIdx, BasicBlockIdx>,
}

impl AppendixGraph {
    pub fn new() -> Self {
        Self {
            edges: HashMap::new(),
        }
    }

    pub fn add_edge(&mut self, from: BasicBlockIdx, to: BasicBlockIdx) {
        assert!(self.edges.insert(from, to).is_none(), "There should only be one edge from a basic block to another basic block as only basic blocks with exactly one predecessor can be merged");
    }

    pub fn get_paths(&self) -> Vec<Vec<BasicBlockIdx>> {
        let mut all_paths = Vec::new();

        // Step 1: Generate all paths
        for (from, to) in self.edges.iter() {
            let mut path = vec![*from];
            let mut current = *to;
            while let Some(next) = self.edges.get(&current) {
                path.push(current);
                current = *next;
            }
            path.push(current);
            all_paths.push(path);
        }

        let mut paths = all_paths.clone();
        // Step 2: Filter out subset paths
        paths.retain(|path| {
            !all_paths
                .iter()
                .any(|other_path| Self::is_subset_path(path, other_path))
        });

        paths
    }

    // Helper function to determine if one path is a subset of another
    fn is_subset_path(subset: &[BasicBlockIdx], superset: &[BasicBlockIdx]) -> bool {
        if subset.len() >= superset.len() || subset.is_empty() {
            return false;
        }

        let subset_first = subset.first().unwrap();
        let subset_last = subset.last().unwrap();

        for window in superset.windows(subset.len()) {
            if window.first() == Some(subset_first) && window.last() == Some(subset_last) {
                return true;
            }
        }

        false
    }
}

impl MIRPass for BranchElimination {
    fn run(&mut self, mir: &mut MIR) -> u32 {
        let mut changes = 0;
        for function in mir.functions.iter_mut() {
            let mut appendix_graph = AppendixGraph::new();
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
                                    if predecessors
                                        .get_immediate(target_idx)
                                        .map(|i| i.len())
                                        .unwrap_or_default()
                                        == 1
                                    {
                                        appendix_graph.add_edge(bb_idx, target_idx);
                                    }
                                }
                                TerminatorKind::SwitchInt {
                                    value,
                                    cases,
                                    default,
                                } => {
                                    if let Some(value) = value.as_i32() {
                                        tracing::info!("Found a switch on a constant value, replacing with a jump to the target");
                                        let case = cases
                                            .iter()
                                            .find(|(case_value, _)| *case_value as i32 == value);
                                        let target = case.map(|c| c.1).unwrap_or(*default);
                                        terminator.kind = TerminatorKind::Jump(target);
                                        changes += 1;
                                    }
                                }
                                TerminatorKind::Unresolved => {}
                            }
                        }
                    }
                }
            }
            let successors = function.successors(&mir.basic_blocks);
            for path in appendix_graph.get_paths() {
                tracing::info!("Found a path that can be merged: {:?}", path);
                let mut is_first_iteration = true;
                for (bb_to_append_idx, bb_idx) in path.iter().copied().rev().tuple_windows() {
                    tracing::info!("Merging basic blocks {} and {}", bb_idx, bb_to_append_idx);
                    let bb_to_append = mir.basic_blocks.get_or_panic(bb_to_append_idx).clone();
                    let bb = mir.basic_blocks.get_mut_or_panic(bb_idx);
                    bb.append(bb_to_append);
                    changes += 1;
                    // We only need to do this for the first block as all other blocks only have one predecessor and therefore no phi nodes
                    if is_first_iteration {
                        is_first_iteration = false;
                        // Only iterate over the immediate successors, because that is the only place where phi nodes can reference the basic block
                        if let Some(successors) = successors.get_immediate(bb_to_append_idx) {
                            for successor in successors {
                                let successor = mir.basic_blocks.get_mut_or_panic(*successor);
                                for instruction_idx in successor.instructions.iter_mut() {
                                    let instruction =
                                        function.instructions.get_mut(*instruction_idx);
                                    match &mut instruction.kind {
                                        InstructionKind::Binary { .. } => {}
                                        InstructionKind::Unary { .. } => {}
                                        InstructionKind::Value(_) => {}
                                        InstructionKind::Call { .. } => {}
                                        InstructionKind::Phi(phi) => {
                                            phi.operands = phi
                                                .operands
                                                .iter()
                                                .copied()
                                                .map(|(from, instruction_idx)| {
                                                    if from == bb_to_append_idx {
                                                        (bb_idx, instruction_idx)
                                                    } else {
                                                        (from, instruction_idx)
                                                    }
                                                })
                                                .collect();
                                        }
                                    }
                                }
                            }
                        }
                    }
                    mir.basic_blocks.remove(bb_to_append_idx);
                    function.basic_blocks.retain(|bb| *bb != bb_to_append_idx);
                }
            }
        }
        changes
    }
}
