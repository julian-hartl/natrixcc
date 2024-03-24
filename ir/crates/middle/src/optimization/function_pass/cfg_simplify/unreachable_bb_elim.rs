use tracing::{debug, trace};
use crate::{FunctionId, Instr};
use crate::instruction::{InstrKind, OpInstr};
use crate::module::Module;
use crate::optimization::function_pass::FunctionPass;

/// # Unreachable Basic Block Elimination
///
/// ## What is Unreachable Basic Block Elimination?
///
/// Unreachable Basic Block Elimination is a pass that removes basic blocks that are unreachable from the entry block.
///
/// This reduces the final binary size and can make other passes more effective.
///
/// ## How does this pass work?
///
/// This pass iterates over all basic blocks and checks if they are reachable from the entry block. If they are not, they are removed.
///
/// We say a basic block `b` is reachable from the entry block if there is a path from the entry block to `b`.
///
/// Once again, we make use of the [DomTree][`crate::cfg::DomTree`].
/// All reachable basic blocks are dominated by the entry block, and therefore we can find all unreachable basic blocks
/// by finding all basic blocks that are not dominated by the entry block.
///
/// *Note* that this pass also **removes** any unnecessary basic block arguments, that evolve from the removal of the unreachable basic blocks.
///
///
/// ## Example
///
/// ```text
/// fun @test() {
/// bb0:
///   br bb2(i32 0)
/// bb1:
///   br bb2(i32 1)
/// bb2(i32 v0):
///   ret v0
/// }
/// ```
///
/// After one run of the pass, the function will look like this:
///
/// ```text
/// fun @test() {
/// bb0:
///   v0 = i32 0
///   br bb2
/// bb2:
///   ret v0
/// }
/// ```
///
/// ## Costs of this pass
///
/// This pass needs to compute the [DomTree][`crate::cfg::DomTree`] for the function, which has a time complexity of O(n^2).
///
#[derive(Default)]
pub struct Pass {}

impl crate::optimization::Pass for Pass {
    fn name(&self) -> &'static str {
        "unreachable_bb_elim"
    }
}

impl FunctionPass for Pass {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionId) -> usize {
        let function = &mut module.functions[function];
        let domtree = function.cfg.dom_tree();
        let entry_block = function.cfg.entry_block();
        // Find all unreachable basic blocks.
        // A basic block is unreachable iff it is not dominated by the entry block.
        let unreachable_bbs = function
            .cfg
            .basic_block_ids()
            .filter(|&bb| !domtree.dominates(entry_block, bb))
            .collect::<Vec<_>>();
        if unreachable_bbs.is_empty() {
            return 0;
        }
        trace!("Removing {} unreachable basic blocks: {:?}", unreachable_bbs.len(), unreachable_bbs);
        let removed = unreachable_bbs.len();
        for bb in unreachable_bbs {
            // Remove the unreachable block from the cfg
            debug!("Removing {bb}");
            let successors = function.cfg.successors(bb).collect::<Vec<_>>();
            function.cfg.remove_basic_block(bb);
            for successor_id in successors {
                // Check if we can remove basic block arguments from one of the unreachable block's successors
                // This is the case if the successor now only has one predecessor
                let pred_id = {
                    let mut preds = function.cfg.predecessors(successor_id);
                    let Some(pred_id) = preds.next() else { continue; };
                    if preds.next().is_some() {
                        continue;
                    }
                    pred_id
                };
                debug!("Removing jump target argument list from {pred_id}");
                // Clear the jump target argument list of the predecessor
                let pred = function.cfg.basic_block_mut(pred_id);
                let Some(branch_args) = pred.update_terminator(|terminator| {
                    terminator.clear_args(successor_id)
                }) else {
                    continue;
                };
                debug!("Removing basic block arguments from {successor_id}");
                let branch_args = branch_args.collect::<Vec<_>>();
                // Remove the basic block arguments from the successor
                let successors_args = function.cfg.basic_block_mut(successor_id).clear_arguments().collect::<Vec<_>>();
                for (argument, op) in successors_args.into_iter().zip(branch_args) {
                    let ty = function.cfg.vreg_ty_cloned(argument);
                    // To keep the program consistent, we need to insert move instruction for each argument
                    // They look like this:
                    // <previous argument> = <previous value argument in branch instruction>
                    function.cfg.basic_block_mut(pred_id).append_instruction(ty, InstrKind::Op(OpInstr {
                        value: argument,
                        op,
                    }));
                }
            }
        }
        removed
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;
    use crate::cfg;
    use crate::optimization::{CFGSimplifyPipelineConfig, Pipeline};
    use crate::optimization::PipelineConfig;
    use crate::test::{assert_module_is_equal_to_src, create_test_module_from_source};

    #[test]
    #[traced_test]
    fn should_work_on_cfg_without_bb_args() {
        let mut module = create_test_module_from_source(r#"
            fun void @test() {
            bb0:
              v0 = bool 1;
              br bb2;
            bb1:
              v1 = i32 7;
              br bb2;
            bb2:
              v3 = i32 8;
              ret void;
            }
        "#);
        module.optimize(PipelineConfig::cfg_simplify_only(CFGSimplifyPipelineConfig::unreachable_bb_elim_only()));
        assert_module_is_equal_to_src(&module, "
            fun void @test() {
            bb0:
                v0 = bool 1;
                br bb2;
            bb2:
                v3 = i32 8;
                ret void;
            }
            ")
    }

    #[test]
    fn should_work_on_cfg_with_bb_args() {
        let mut module = create_test_module_from_source(r#"
            fun i32 @test() {
            bb0:
              v0 = i32 8;
              br bb2(v0);
            bb1:
              v1 = i32 7;
              br bb2(v1);
            bb2(i32 v4):
              v3 = i32 v4;
              ret i32 v3;
            }
        "#);
        module.optimize(PipelineConfig::cfg_simplify_only(CFGSimplifyPipelineConfig::unreachable_bb_elim_only()));
        assert_module_is_equal_to_src(&module, "
            fun i32 @test() {
            bb0:
                v0 = i32 8;
                v4 = i32 8;
                br bb2;
            bb2:
              v3 = i32 v4;
              ret i32 v3;
            }
        ")
    }
}
