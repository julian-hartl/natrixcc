use index_vec::index_vec;
use crate::{Function, FunctionId, Module};

pub fn create_test_function() -> Function {
    Function::new("test".to_string(), index_vec![], Type::I32)
}

pub fn create_test_module() -> (Module, FunctionId) {
    let mut module = Module::default();
    let function = module.functions.push(create_test_function());
    (module, function)
}
