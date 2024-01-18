use index_vec::IndexVec;

use crate::cfg::CFG;
use crate::cfg::VarId;
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
    pub cfg: CFG,
}


impl FunctionData {
    pub fn new(name: String, params: IndexVec<Param, ParamData>, ret_ty: Type) -> Self {
        Self {
            name,
            params,
            ret_ty,
            cfg: CFG::default(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParamData {
    pub id: VarId,
    pub ty: Type,
}
