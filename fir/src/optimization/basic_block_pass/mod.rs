use crate::cfg::BasicBlock;
use crate::function::Function;
use crate::module::Module;
use crate::optimization::function_pass::FunctionPass;

pub mod constant_fold;
pub mod cse;
pub mod copy_propagation;
pub mod trivial_phi_elim;

pub trait BasicBlockPass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: Function, basic_block: BasicBlock) -> usize;
}

impl<T> FunctionPass for T
    where T: BasicBlockPass
{
    fn run_on_function(&mut self, module: &mut Module, function: Function) -> usize {
        let mut changes = 0;
        for basic_block in module.functions[function].cfg.basic_blocks.indices() {
            if module.functions[function].cfg.basic_blocks[basic_block].is_none() {
                continue;
            }
            changes += self.run_on_basic_block(module, function, basic_block);
        }
        changes
    }
}
