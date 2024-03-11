use crate::codegen::machine::{Backend, FunctionBuilder, Module};

#[derive(Debug)]
pub struct Builder<'module, B: Backend> {
    module: &'module mut firc_middle::Module,
    mtbb: Module<B::ABI>,
}

impl<'module, B: Backend> Builder<'module, B> {
    pub fn new(module: &'module mut firc_middle::Module) -> Self {
        Self {
            module,
            mtbb: Module::default(),
        }
    }

    pub fn build(mut self) -> Module<B::ABI> {
        for (_, function) in &mut self.module.functions {
            let builder = FunctionBuilder::<B>::new();
            self.mtbb.functions.push(
                builder.build(function)
            );
        }
        self.mtbb
    }
}
