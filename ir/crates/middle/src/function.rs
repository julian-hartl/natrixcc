use std::fmt::{
    Display,
    Formatter,
};

use slotmap::new_key_type;

use crate::{
    cfg::Cfg,
    ty::Type,
};

new_key_type! {
    pub struct FunctionRef;
}

pub type Symbol = String;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Type>,
    pub ret_ty: Type,
    pub cfg: Cfg,
}

impl Function {
    pub fn new(name: String, params: Vec<Type>, ret_ty: Type) -> Self {
        Self {
            name,
            params,
            ret_ty,
            cfg: Cfg::new(),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {} @{}(", self.ret_ty, self.name)?;
        for (index, param) in self.params.iter().enumerate() {
            write!(f, "{param}")?;
            if index < self.params.len() - 1 {
                write!(f, ", ")?;
            }
        }
        write!(f, ")")?;
        writeln!(f, " {{")?;
        write!(f, "{}", self.cfg)?;
        writeln!(f, "}}")?;
        Ok(())
    }
}
