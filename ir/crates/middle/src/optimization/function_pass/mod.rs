pub mod cfg_simplify;
pub mod constant_propagation;
pub mod dead_code_elimination;

use crate::{
    module::Module,
    optimization::Pass,
    FunctionRef,
};

pub trait FunctionPass: Pass {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionRef) -> usize;
}
