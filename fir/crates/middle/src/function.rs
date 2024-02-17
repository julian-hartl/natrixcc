use index_vec::IndexVec;
use crate::cfg::{Cfg, ValueId};
use crate::Value;
use crate::ty::Type;
index_vec::define_index_type! {
    pub struct FunctionId = usize;
}

index_vec::define_index_type! {
    pub struct Param = usize;
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: IndexVec<Param, ParamData>,
    pub ret_ty: Type,
    pub cfg: Cfg,
}


impl Function {
    pub fn new(name: String, params: IndexVec<Param, ParamData>, ret_ty: Type) -> Self {
        Self {
            name,
            params,
            ret_ty,
            cfg: Cfg::new(),
        }
    }

    pub fn get_value_type(&self, value: Value) -> &Type {
        &self.cfg.values_ctx[value].ty
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParamData {
    pub id: ValueId,
    pub ty: Type,
}
