use crate::{FrontBridge, Function, FunctionId, Module, Type};

pub fn create_test_function() -> Function {
    Function::new("test".to_string(), vec![], Type::I32)
}

pub fn create_test_module() -> (Module, FunctionId) {
    let mut module = Module::default();
    let function = module.functions.push(create_test_function());
    (module, function)
}

pub fn create_test_module_from_source(source: &str) -> Module {
    let module = firc_front::parse(source).unwrap();
    FrontBridge::new(module).bridge()
}
