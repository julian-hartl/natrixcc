use crate::{cfg, Function, Module, Type};

pub struct FrontBridge {
    module: firc_front::Module,
}

impl FrontBridge {
    pub fn new(module: firc_front::Module) -> Self {
        Self { module }
    }
    
    pub fn bridge(mut self) -> Module {
        let mut module = Module::default();
        for function in self.module.functions {
            let function = Self::bridge_function(function);
            module.functions.push(function);
        }
        module
    }

    fn bridge_function(front_f: firc_front::module::Function) -> Function {
        let mut function = Function::new(front_f.name, front_f.args.into_iter().map(
            |arg| arg.ty.into()
        ).collect::<Vec<_>>(), front_f.ret_ty.into());
        let mut cfg_builder = cfg::Builder::new(&mut function); 
        for basic_block in front_f.basic_blocks {
            cfg_builder.start_bb();
            for instruction in basic_block.instructions {
                todo!()
            }
        }
        function
    }
}

impl From<firc_front::module::Type> for Type {
    fn from(value: firc_front::module::Type) -> Self {
        match value {
            firc_front::module::Type::U8 => Self::U8,
            firc_front::module::Type::U16 => Self::U16,
            firc_front::module::Type::U32 => Self::U32,
            firc_front::module::Type::U64 => Self::U64,
            firc_front::module::Type::I8 => Self::I8,
            firc_front::module::Type::I16 => Self::I16,
            firc_front::module::Type::I32 => Self::I32,
            firc_front::module::Type::I64 => Self::I64,
        }
    }
}