use crate::{
    cfg::BasicBlockId,
    module::Module,
    optimization::{
        FunctionPass,
        Pass,
    },
    FunctionId,
};

pub mod constant_fold;
pub mod copy_propagation;
pub mod cse;

pub trait BasicBlockPass: Pass {
    fn run_on_basic_block(
        &mut self,
        module: &mut Module,
        function: FunctionId,
        basic_block: BasicBlockId,
    ) -> usize;
}

impl<T> FunctionPass for T
where
    T: BasicBlockPass,
{
    fn run_on_function(&mut self, module: &mut Module, function: FunctionId) -> usize {
        let mut changes = 0;
        let basic_blocks = module.functions[function]
            .cfg
            .basic_block_ids()
            .collect::<Vec<_>>();
        for basic_block in basic_blocks {
            changes += self.run_on_basic_block(module, function, basic_block);
        }
        changes
    }
}
