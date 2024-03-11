use crate::{Function, FunctionId, Module, Type};

pub fn create_test_function() -> Function {
    Function::new("test".to_string(), vec![], Type::I32)
}

pub fn create_test_module() -> (Module, FunctionId) {
    let mut module = Module::default();
    let function = module.functions.push(create_test_function());
    (module, function)
}

pub fn create_test_module_from_source(source: &str) -> Module {
    firc_front::parse(source).unwrap().into()
}

pub fn assert_module_is_equal_to_src(module: &Module, expected: &str) {
    let mod2: Module = firc_front::parse(expected).unwrap().into();
    let actual = module.to_string();
    let expected = mod2.to_string();
    assert_eq!(actual, expected);
}
