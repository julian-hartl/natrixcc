use index_vec::index_vec;
use crate::function::{Function, FunctionData};
use crate::module::Module;
use crate::ty::Type;

pub fn create_test_function() -> FunctionData {
    FunctionData::new("test".to_string(), index_vec![], Type::I32)
}

pub fn create_test_module() -> (Module, Function) {
    let mut module = Module::default();
    let function = module.functions.push(create_test_function());
    (module, function)
}
