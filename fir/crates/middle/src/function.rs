use index_vec::IndexVec;
use rustc_hash::FxHashMap;

use crate::cfg::Cfg;
use crate::instruction::ValueData;
use crate::ty::Type;
use crate::Value;

index_vec::define_index_type! {
    pub struct FunctionId = usize;
}

pub type Symbol = String;

pub type SymbolTable = FxHashMap<Value, Symbol>;

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Type>,
    pub ret_ty: Type,
    pub cfg: Cfg,
    pub symbol_table: SymbolTable,
    pub values_ctx: IndexVec<Value, ValueData>,
}


impl Function {
    pub fn new(name: String, params: Vec<Type>, ret_ty: Type) -> Self {
        Self {
            name,
            params,
            ret_ty,
            cfg: Cfg::new(),
            symbol_table: SymbolTable::default(),
            values_ctx: IndexVec::new(),
        }
    }

    pub fn get_value_type(&self, value: Value) -> &Type {
        &self.values_ctx[value].ty
    }
    
    pub fn write_to(&self, w: &mut impl std::fmt::Write) -> std::fmt::Result {
        write!(w, "fun {} @{} (", self.ret_ty, self.name)?;
        for (index, param) in self.params.iter().enumerate() {
            write!(w, "{param}")?;
            if index < self.params.len() - 1{
                write!(w, ", ")?;
            }
        }
        writeln!(w, " {{")?;
        self.cfg.write_to(w, self)?;
        writeln!(w, " }}")?;
        Ok(())
    }
    
    pub fn write_to_string(&self) -> Result<String, std::fmt::Error> {
        let mut buf = String::new();
        self.write_to(&mut buf)?;
        Ok(buf)
    }
}
