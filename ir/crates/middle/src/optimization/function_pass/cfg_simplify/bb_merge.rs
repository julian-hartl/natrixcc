use itertools::Itertools;
use tracing::debug;

use crate::{
    module::Module,
    optimization::FunctionPass,
    FunctionRef,
};

/// # Basic block merge
///
/// ## What is basic block merge?
///
/// Basic block merge is (like its name implies) applied to merge basic blocks that have exactly one predecessor and one successor.
///
/// This is useful, because it can reduce the number of basic blocks and therefore the number of jumps that need to be executed.
///
/// ## How does this pass work?
///
/// This pass makes use of the [DomTree][`crate::cfg::DomTree`] to find paths in the control flow graph that can be merged. See the documentation of the [DomTree][`crate::cfg::DomTree`] for more information.
///
/// Merging basic block `b` into basic block `a` means that we append `b`'s instructions to `a`,
/// override `a`'s terminator with `b`'s terminator and remove `b` from the control flow graph.
///
/// ### Preconditions for merging basic block `b` into basic block `a`
/// pred(b) = 1, succ(b) = 1 idom(b) = a
/// Let `a` and `b` be two basic blocks. We can merge `b` into `a` if the following conditions are met:
///
/// - |pred(b)| = 1: The number of predecessors of `b` is exactly 1
/// If |pred(b)| > 1, we cannot merge `b` into `a` because `b` can be run without running `a`.
/// - |succ(a)| = 1: The number of successors of `a` is exactly 1
/// If |succ(a)| > 1, we cannot merge `b` into `a` because `a` can be run without running `b`.
/// - idom(b) = a: The immediate dominator of `b` is `a`
/// If idom(b) != a, we cannot merge `b` into `a` because some other basic block `c` is run before `b` but after `a`.
///
/// ## Example
///
/// ```text
/// fun @test() {
/// bb0:
///   br bb1
/// bb1:
///   br bb2
/// bb2:
///   ret
/// }
/// ```
///
/// After one run of the pass, the function will look like this:
///
/// ```text
/// fun @test() {
/// bb0:
///   br bb2
/// bb2:
///   ret
/// }
/// ```
///
/// After another run of the pass, the function will look like this:
///
/// ```text
/// fun @test() {
/// bb0:
///   ret
/// }
/// ```
///
/// ## Costs of this pass
///
/// This pass needs to recompute the dominator tree for every merge operation.
///
/// Therefore, it is relatively expensive to run.
#[derive(Default)]
pub struct Pass {}

impl crate::optimization::Pass for Pass {
    fn name(&self) -> &'static str {
        "bb_merge"
    }
}

impl FunctionPass for Pass {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionRef) -> usize {
        let function = &mut module.functions[function];
        let mut worklist = function.cfg.basic_blocks.keys().collect_vec();
        let mut merged = 0;
        while let Some(b_id) = worklist.pop() {
            let cfg = &mut function.cfg;
            if cfg.predecessors(b_id).count() != 1 {
                continue;
            }
            let domtree = cfg.dom_tree();
            let Some(a_id) = domtree.idom(b_id) else {
                continue;
            };
            if cfg.successors(a_id).count() != 1 {
                continue;
            }
            let b = cfg
                .remove_basic_block(b_id)
                .expect("Basic block does not exist");
            let a = &mut cfg.basic_blocks[a_id];
            debug!("Merging {b} into {a}");
            a.append_instructions(b.instructions.into_iter());
            a.update_terminator(|term| *term = b.terminator.unwrap());
            cfg.recompute_successors(a_id);
            merged += 1;
        }

        merged
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        optimization::{
            CFGSimplifyPipelineConfig,
            PipelineConfig,
        },
        test::{
            assert_module_is_equal_to_src,
            create_test_module_from_source,
        },
    };

    #[test]
    fn should_merge_in_more_complex_cfg() {
        let mut module = create_test_module_from_source(
            "
            fun void @test() {
            bb0:
                bool %0 = true;
                condbr %0 bb1, bb2;
            bb1:
                i32 %1 = add 1i32, 2i32;
                br bb4;
            bb2:
                i32 %2 = add 3i32, 4i32;
                br bb3;
            bb3:
                i32 %3 = add 5i32, 6i32;
                br bb5;
            bb4:
                i32 %4 = add 7i32, 8i32;
                br bb5;
            bb5:
                i32 %5 = add 9i32, 10i32;
                ret;
            }
",
        );
        dbg!(module.to_string());
        module.optimize(PipelineConfig::cfg_simplify_only(
            CFGSimplifyPipelineConfig::bb_merge_only(),
        ));
        assert_module_is_equal_to_src(
            &module,
            "
        fun void @test() {
        bb0:
            bool %0 = true;
            condbr true bb1, bb2;
        bb1:
            i32 %1 = 3i32;
            i32 %4 = 15i32;
            br bb5;
        bb2:
            i32 %2 = 7i32;
            i32 %3 = 11i32;
            br bb5;
        bb5:
            i32 %5 = 19i32;
            ret;
        }
",
        );
    }
}
