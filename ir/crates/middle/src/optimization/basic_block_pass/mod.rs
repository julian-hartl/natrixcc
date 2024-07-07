use itertools::Itertools;

use crate::{
    cfg::BasicBlockRef,
    module::Module,
    optimization::{
        FunctionPass,
        Pass,
    },
    FunctionRef,
};

pub mod constant_fold;
pub mod copy_propagation;
pub mod cse;

pub trait BasicBlockPass: Pass {
    fn run_on_basic_block(
        &mut self,
        module: &mut Module,
        function: FunctionRef,
        bb_ref: BasicBlockRef,
    ) -> usize;
}

impl<T> FunctionPass for T
where
    T: BasicBlockPass,
{
    fn run_on_function(&mut self, module: &mut Module, function: FunctionRef) -> usize {
        let mut changes = 0;
        let basic_blocks = module.functions[function]
            .cfg
            .basic_blocks
            .keys()
            .collect_vec();
        for basic_block in basic_blocks {
            changes += self.run_on_basic_block(module, function, basic_block);
        }
        changes
    }
}
