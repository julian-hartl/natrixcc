use crate::cfg::BasicBlock;
use crate::function::Function;
use crate::module::Module;

pub mod constant_fold;

pub trait BasicBlockPass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: Function, basic_block: BasicBlock) -> usize;

    fn run_on_function(&mut self, module: &mut Module, function: Function) -> usize {
        let mut changes = 0;
        for basic_block in module.functions[function].cfg.basic_blocks.indices() {
            changes += self.run_on_basic_block(module,function, basic_block);
        }
        changes
    }
}
