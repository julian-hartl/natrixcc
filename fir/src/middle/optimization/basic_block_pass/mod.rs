use crate::middle::optimization::FunctionPass;
use crate::middle::cfg::BasicBlockId;
use crate::middle::FunctionId;
use crate::middle::module::Module;

pub mod constant_fold;
pub mod cse;
pub mod copy_propagation;
pub mod trivial_phi_elim;

pub trait BasicBlockPass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: FunctionId, basic_block: BasicBlockId) -> usize;
}

impl<T> FunctionPass for T
    where T: BasicBlockPass
{
    fn run_on_function(&mut self, module: &mut Module, function: FunctionId) -> usize {
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
