use crate::codegen::x86_64::function_builder::FunctionBuilder;
use crate::codegen::x86_64::{FunctionData, Module};
use crate::codegen::x86_64::register_allocator::{FastRegisterAllocator, RegisterAllocator};

type MirModule = crate::module::Module;

#[derive(Debug)]
pub struct ModuleBuilder<'a> {
    pub mir_module: &'a MirModule,
    module: Module,
}

impl <'a> ModuleBuilder<'a> {
    pub fn new(mir_module: &'a MirModule) -> Self {
        Self {
            mir_module,
            module: Module::default(),
        }
    }

    pub fn build(mut self) -> Module {
        for mir_function in &self.mir_module.functions {
            let function = Self::build_function(mir_function);
            self.module.functions.push(function);
        }
        self.module
    }

    fn build_function(mir_function: &crate::function::FunctionData) -> FunctionData {
        let function_builder = FunctionBuilder::new(mir_function);
        function_builder.build()
    }
}
