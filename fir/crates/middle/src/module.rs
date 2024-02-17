use index_vec::IndexVec;
use crate::{FunctionId, Function};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct Module {
    pub functions: IndexVec<FunctionId, Function>,
}
