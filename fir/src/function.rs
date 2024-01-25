use index_vec::IndexVec;

use crate::cfg::Cfg;
use crate::cfg::ValueId;
use crate::ty::Type;

index_vec::define_index_type! {
    pub struct Function = usize;
}

index_vec::define_index_type! {
    pub struct Param = usize;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionData {
    pub name: String,
    pub params: IndexVec<Param, ParamData>,
    pub ret_ty: Type,
    pub cfg: Cfg,
}


impl FunctionData {
    pub fn new(name: String, params: IndexVec<Param, ParamData>, ret_ty: Type) -> Self {
        Self {
            name,
            params,
            ret_ty,
            cfg: Cfg::new(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParamData {
    pub id: ValueId,
    pub ty: Type,
}
