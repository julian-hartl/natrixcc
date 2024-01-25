use index_vec::IndexVec;

use crate::function::{Function, FunctionData};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Module {
    pub functions: IndexVec<Function, FunctionData>,
}
