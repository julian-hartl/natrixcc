pub mod dead_code_elimination;
pub mod constant_propagation;
pub mod cfg_simplify;

use crate::middle::FunctionId;
use crate::middle::module::Module;

pub trait FunctionPass {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionId) -> usize;
}
