use itertools::Itertools;
use tracing::{
    debug,
    trace,
};

use crate::{
    instruction::{
        InstrKind,
        OpInstr,
    },
    module::Module,
    optimization::function_pass::FunctionPass,
    FunctionRef,
    Instr,
};

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
#[derive(Default)]
pub struct Pass {}

impl crate::optimization::Pass for Pass {
    fn name(&self) -> &'static str {
        "unreachable_bb_elim"
    }
}

impl FunctionPass for Pass {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionRef) -> usize {
        let function = &mut module.functions[function];
        let domtree = function.cfg.dom_tree();
        let entry_block = function.cfg.entry_block_ref();
        // Find all unreachable basic blocks.
        // A basic block is unreachable iff it is not dominated by the entry block.
        let unreachable_bbs = function
            .cfg
            .basic_blocks
            .keys()
            .filter(|&bb| !domtree.dominates(entry_block, bb))
            .collect_vec();
        if unreachable_bbs.is_empty() {
            return 0;
        }
        debug!(
            "Removing {} unreachable basic blocks: {:?}",
            unreachable_bbs.len(),
            unreachable_bbs
        );
        let removed = unreachable_bbs.len();
        for bb_ref in unreachable_bbs {
            // Remove the unreachable block from the cfg
            let bb = &function.cfg.basic_blocks[bb_ref];
            trace!("Removing {bb}");
            let successors = function.cfg.successors(bb_ref).collect_vec();
            function.cfg.remove_basic_block(bb_ref);
            for successor_ref in successors {
                // Check if we can remove basic block arguments from one of the unreachable block's successors
                // This is the case if the successor now only has one predecessor
                let pred_id = {
                    let mut preds = function.cfg.predecessors(successor_ref);
                    let Some(pred_id) = preds.next() else {
                        continue;
                    };
                    if preds.next().is_some() {
                        continue;
                    }
                    pred_id
                };
                // Clear the jump target argument list of the predecessor
                let pred = &mut function.cfg.basic_blocks[pred_id];
                trace!("Removing jump target argument list from {pred}");
                let Some(branch_args) =
                    pred.update_terminator(|terminator| terminator.clear_args(successor_ref))
                else {
                    continue;
                };
                let branch_args = branch_args.collect_vec();
                let successor = &function.cfg.basic_blocks[successor_ref];
                trace!("Removing basic block arguments from {successor}");
                // Remove the basic block arguments from the successor
                let successors_args = function.cfg.basic_blocks[successor_ref]
                    .clear_arguments()
                    .collect_vec();
                for (argument, op) in successors_args.into_iter().zip(branch_args) {
                    let ty = function.cfg.basic_block_args[argument].ty.clone();
                    // To keep the program consistent, we need to insert move instruction for each argument
                    // They look like this:
                    // <previous argument> = <previous value argument in branch instruction>
                    let instr_id = function.cfg.add_instruction(
                        pred_id,
                        ty,
                        InstrKind::Op(OpInstr { op }),
                        "todo".into(),
                    );
                    todo!("Replace references to 'argument' with 'instr_id' in the function");
                }
            }
        }
        removed
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::{
        cfg,
        optimization::{
            CFGSimplifyPipelineConfig,
            Pipeline,
            PipelineConfig,
        },
        test::{
            assert_module_is_equal_to_src,
            create_test_module_from_source,
        },
    };

    #[test]
    #[traced_test]
    fn should_work_on_cfg_without_bb_args() {
        let mut module = create_test_module_from_source(
            r#"
            fun void @test() {
            bb0:
              bool %0 = true;
              br bb2;
            bb1:
              i32 %1 = 7i32;
              br bb2;
            bb2:
              i32 %3 = 8i32;
              ret;
            }
        "#,
        );
        module.optimize(PipelineConfig::cfg_simplify_only(
            CFGSimplifyPipelineConfig::unreachable_bb_elim_only(),
        ));
        assert_module_is_equal_to_src(
            &module,
            "
            fun void @test() {
            bb0:
                bool %0 = true;
                br bb2;
            bb2:
                i32 %3 = 8i32;
                ret;
            }
            ",
        )
    }

    #[test]
    fn should_work_on_cfg_with_bb_args() {
        let mut module = create_test_module_from_source(
            r#"
            fun i32 @test() {
            bb0:
              i32 %0 = 8i32;
              br bb2(%0);
            bb1:
              i32 %1 = 7i32;
              br bb2(%1);
            bb2(i32 %4):
              i32 %3 = %4;
              ret %3;
            }
        "#,
        );
        module.optimize(PipelineConfig::cfg_simplify_only(
            CFGSimplifyPipelineConfig::unreachable_bb_elim_only(),
        ));
        assert_module_is_equal_to_src(
            &module,
            "
            fun i32 @test() {
            bb0:
                i32 %0 = 8i32;
                i32 %4 = 8i32;
                br bb2;
            bb2:
              i32 %3 = %4;
              ret %3;
            }
        ",
        )
    }
}
