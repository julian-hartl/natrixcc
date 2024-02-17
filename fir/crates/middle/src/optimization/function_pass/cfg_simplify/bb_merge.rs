use crate::optimization::FunctionPass;
use rustc_hash::FxHashMap;
use crate::cfg::BasicBlockId;
use crate::FunctionId;

use crate::instruction::InstrKind;
use crate::module::Module;

/// Merge two basic blocks a & b if:
/// 1. a has only one successor b
/// 2. b has only one predecessor a
///
/// Merging a & b means:
/// 1. Append all instructions in b to a
/// 2. Override a's terminator with b's terminator
/// 3. Remove b from the function
/// 4. Update all references to b to a
/// - Update all references to b in the function's basic blocks
/// - Update all references to b in [`crate::instruction::ValueData::defined_in`]
#[derive(Default)]
pub struct Pass {}

impl FunctionPass for Pass {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionId) -> usize {
        let mut appendix_graph = AppendixGraph::default();
        let function = &mut module.functions[function];
        let mut merged = 0;
        for (a, a_data) in function.cfg.basic_blocks.iter_enumerated() {
            let Some(a_data) = a_data else { continue; };
            let successors = a_data.successors();
            if successors.len() != 1 {
                continue;
            }
            let b = successors[0];
            let b_data = function.cfg.basic_blocks[b].as_ref().unwrap();
            let b_preds = b_data.predecessors(&function.cfg);
            if b_preds.len() != 1 {
                continue;
            }
            appendix_graph.add_edge(a, b);
        }
        for path in appendix_graph.get_paths() {
            assert!(path.len() >= 2);
            let trailing_bb = path.last().copied().unwrap();
            let trailing_bb_data = function.cfg.basic_blocks[trailing_bb].take().unwrap();
            let trailing_bb_successors = trailing_bb_data.successors();
            let mut instructions_to_append = Vec::new();
            let terminator = trailing_bb_data.terminator;
            for bb in &path[1..path.len() - 1] {
                let bb_data = function.cfg.basic_blocks[*bb].take().unwrap();
                instructions_to_append.extend(bb_data.instructions);
            }
            let leading_bb = path.first().copied().unwrap();
            let leading_bb_data = function.cfg.basic_blocks[leading_bb].as_mut().unwrap();
            instructions_to_append.extend(trailing_bb_data.instructions);
            leading_bb_data.instructions.extend(instructions_to_append);
            leading_bb_data.terminator = terminator;
            merged += path.len();
            for value in &mut function.cfg.values_ctx {
                if path.contains(&value.defined_in) {
                    value.defined_in = leading_bb;
                }
            }
            for bb in trailing_bb_successors {
                let bb_data = function.cfg.basic_blocks[bb].as_ref().unwrap();
                for instr in bb_data.instructions.iter().copied() {
                    let instr = &mut function.cfg.instructions[instr];
                    if let InstrKind::Phi(phi) = &mut instr.kind {
                        for incoming in &mut phi.incoming {
                            if path.contains(&incoming.source) {
                                incoming.source = leading_bb;
                            }
                        }
                    }
                }
            }
        }
        merged
    }
}

// todo: we should replace this graph with the dominator tree as the dominator tree contains all paths that can be merged

#[derive(Default)]
struct AppendixGraph {
    edges: FxHashMap<BasicBlockId, BasicBlockId>,
}

impl AppendixGraph {
    pub fn add_edge(&mut self, from: BasicBlockId, to: BasicBlockId) {
        assert!(self.edges.insert(from, to).is_none(), "There should only be one edge from a basic block to another basic block as only basic blocks with exactly one predecessor can be merged");
    }

    pub fn get_paths(&self) -> Vec<Vec<BasicBlockId>> {
        let mut all_paths = Vec::new();

        // Step 1: Generate all paths
        for (from, to) in &self.edges {
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
            !all_paths.iter().any(|other_path| Self::is_subset_path(path, other_path))
        });

        paths
    }

    //. Helper function to determine if one path is a subset of another
    fn is_subset_path(subset: &[BasicBlockId], superset: &[BasicBlockId]) -> bool {
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
