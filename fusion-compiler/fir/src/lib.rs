#[deny(unused_must_use)]
use index_vec::index_vec;

use crate::function::{Function, FunctionData};
use crate::module::Module;
use crate::ty::Type;

mod cfg;
mod module;
mod instruction;
mod ty;
mod function;
mod cfg_builder;
mod optimization;
mod codegen;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}

#[cfg(test)]
fn create_test_function() -> FunctionData {
    let mut function = FunctionData::new("test".to_string(), index_vec![], Type::I32);
    function
}

#[cfg(test)]
fn create_test_module() -> (Module, Function) {
    let mut module = Module::default();
    let function = module.functions.push(create_test_function());
    (module, function)
}


