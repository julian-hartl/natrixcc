use crate::cfg::{BasicBlockId, BranchTerm, TerminatorKind};
use crate::FunctionId;
use crate::instruction::{Const, Op};
use crate::module::Module;
use crate::optimization::basic_block_pass;

/// # Jump threading optimization pass
///
/// ## What is jump threading?
///
/// Jump threading is applied to convert conditional branches with a constant condition into unconditional branches.
///
/// This is useful, because otherwise we would need to check that condition at runtime which brings overhead.
///
/// It lays the foundation for other optimizations, such as [merging of basic blocks][`super::bb_merge::Pass`].
///
/// ## How does this pass work?
///
/// 1. Check the kind of the terminator
/// 2. If it is not a conditional branch, we're done
/// 3. Otherwise, we check whether the condition is a constant
/// 4. If so, we can evaluate the constant. Note that any other value than `0` will evaluate to `true`
/// 5. Depending on the constant, we choose the new jump target and replace the conditional branch with an unconditional branch
/// 6. Update the successors of the basic block - this is currently implemented by just recomputing the successors,
/// but could be optimized as we know the exact successors after the optimization
/// 
/// ## Example
/// 
/// ```text
/// fun @test() {
/// bb0:
///   condbr i1 1, bb1, bb2
/// bb1:
///   ret
/// bb2:
///   ret    
/// }
/// ```
/// 
/// After the optimization, the function will look like this:
///
/// ```text
/// fun @test() {
/// bb0:
///   br bb1
/// bb1:
///   ret
/// bb2:
///   ret    
/// }
/// ```
/// 
/// ## Costs of this pass
///
/// This optimization only needs to check the terminator of the basic block and therefore is relatively cheap to run. 
///
#[derive(Default)]
pub struct Pass {}

impl basic_block_pass::BasicBlockPass for Pass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: FunctionId, basic_block: BasicBlockId) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let bb = cfg.basic_block_mut(basic_block);
        let changes = bb.update_terminator(|term| {
            match &mut term.kind {
                TerminatorKind::Ret(_) |
                TerminatorKind::Branch(_) => 0,
                TerminatorKind::CondBranch(condbr_term) => {
                    match &condbr_term.cond {
                        Op::Const(const_val) => {
                            let is_true_branch = match const_val {
                                // It is important to not check equality with one, as any other value than 0 is evaluated to true at runtime.
                                // For example: 2 => true, -1 => true, 0 => false
                                Const::Int { value, .. } => *value != 0,
                            };
                            term.kind = TerminatorKind::Branch(BranchTerm::new(
                                if is_true_branch { condbr_term.true_target.clone() } else { condbr_term.false_target.clone() }
                            ));
                            1
                        }
                        Op::Value(_) => 0,
                    }
                }
            }
        });
        // Only recompute successors if we have changed anything as
        // recompute_successors does not check whether any updates have occurred 
        if changes > 0 {
            cfg.recompute_successors(basic_block);
        }
        changes
    }
}
