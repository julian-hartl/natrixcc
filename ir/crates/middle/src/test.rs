use crate::{
    Function,
    FunctionRef,
    Module,
    Type,
};

pub fn create_test_function() -> Function {
    Function::new("test".to_string(), vec![], Type::I32)
}

pub fn create_test_module() -> (Module, FunctionRef) {
    let mut module = Module::default();
    let function = module.functions.insert(create_test_function());
    (module, function)
}

pub fn create_test_module_from_source(source: &str) -> Module {
    natrix_front::parse(source)
        .expect("Failed to parse source")
        .into()
}

pub fn assert_module_is_equal_to_src(module: &Module, expected: &str) {
    let mod2: Module = natrix_front::parse(expected).unwrap().into();
    let actual = module.to_string();
    let expected = mod2.to_string();
    assert_eq!(actual, expected);
}
