use crate::function::{Function, FunctionData};
use crate::module::Module;
use crate::optimization::basic_block_pass::BasicBlockPass;
use crate::optimization::basic_block_pass::constant_fold::ConstantFoldPass;

mod basic_block_pass;

pub struct OptimizationPipeline<'m> {
    module: &'m mut Module,
}

impl <'a> OptimizationPipeline<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self {
            module,
        }
    }

    pub fn run(&mut self) {
        let mut changes = 1;
        while changes > 0 {
            changes = 0;
            for function in self.module.functions.indices() {
                changes += self.run_on_function(function);
            }
        }
    }

    fn run_on_function(&mut self, function: Function) -> usize {
        let mut changes = 0;
        let mut passes = vec![
            Box::new(ConstantFoldPass {}),
        ];
        for pass in passes.iter_mut() {
            changes += pass.run_on_function(&mut self.module, function);
        }
        changes
    }
}
