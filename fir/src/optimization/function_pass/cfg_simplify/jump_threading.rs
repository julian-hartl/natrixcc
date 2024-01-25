use crate::cfg::{BasicBlock, BranchTerm, TerminatorKind, UnCondBrTerm};
use crate::function::Function;
use crate::instruction::{Const, Op};
use crate::module::Module;
use crate::optimization::basic_block_pass;

#[derive(Default)]
pub struct Pass {}

impl basic_block_pass::BasicBlockPass for Pass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: Function, basic_block: BasicBlock) -> usize {
        let bb = module.functions[function].cfg.basic_blocks[basic_block].as_mut().unwrap();
        match &mut bb.terminator.kind {
            TerminatorKind::Ret(_) => 0,
            TerminatorKind::Branch(branch) => {
                match branch {
                    BranchTerm::UnCond(_) => 0,
                    BranchTerm::Cond(cond_branch) => {
                        match &cond_branch.cond {
                            Op::Const(const_val) => {
                                let is_true_branch = match const_val {
                                    Const::Int { value, .. } => *value == 1,
                                };
                                if is_true_branch {
                                    *branch = BranchTerm::UnCond(UnCondBrTerm::new(cond_branch.true_target));
                                } else {
                                    *branch = BranchTerm::UnCond(UnCondBrTerm::new(cond_branch.false_target));
                                }
                                1
                            }
                            Op::Value(_) => 0,
                        }
                    }
                }
            }
        }
    }
}