use crate::codegen::machine::{Backend, FunctionBuilder, Module, TargetMachine};

#[derive(Debug)]
pub struct Builder<'module, TM: TargetMachine> {
    module: &'module mut firc_middle::Module,
    mtbb: Module<TM>,
}

impl<'module, TM: TargetMachine> Builder<'module, TM> {
    pub fn new(module: &'module mut firc_middle::Module) -> Self {
        Self {
            module,
            mtbb: Module::default(),
        }
    }

    pub fn build(mut self) -> Module<TM> {
        for (_, function) in &mut self.module.functions {
            let builder = FunctionBuilder::<TM::Backend>::new();
            self.mtbb.functions.push(
                builder.build(function)
            );
        }
        self.mtbb
    }
}
