pub mod dead_code_elimination;
pub mod constant_propagation;
pub mod cfg_simplify;

use crate::FunctionId;
use crate::module::Module;
use crate::optimization::Pass;

pub trait FunctionPass: Pass{
    fn run_on_function(&mut self, module: &mut Module, function: FunctionId) -> usize;
}
