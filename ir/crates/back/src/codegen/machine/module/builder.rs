use crate::codegen::machine::{
    backend::Backend,
    function::builder::FunctionBuilder,
    Module,
    TargetMachine,
};

#[derive(Debug)]
pub struct Builder<'module, TM: TargetMachine> {
    module: &'module mut natrix_middle::Module,
    mtbb: Module<TM>,
}

impl<'module, TM: TargetMachine> Builder<'module, TM> {
    pub fn new(module: &'module mut natrix_middle::Module) -> Self {
        Self {
            module,
            mtbb: Module::default(),
        }
    }

    pub fn build(mut self) -> Module<TM> {
        for (_, function) in &mut self.module.functions {
            let builder = FunctionBuilder::<TM>::new();
            self.mtbb.functions.push(builder.build(function));
        }
        self.mtbb
    }
}
