pub mod dead_code_elimination;
pub mod constant_propagation;
pub mod cfg_simplify;

use crate::function::Function;
use crate::module::Module;

pub trait FunctionPass {
    fn run_on_function(&mut self, module: &mut Module, function: Function) -> usize;
}
