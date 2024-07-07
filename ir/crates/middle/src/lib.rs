#![feature(impl_trait_in_assoc_type)]
#![feature(type_alias_impl_trait)]

use derive_more::From;
pub use front_bridge::FrontBridge;
pub use function::{
    Function,
    FunctionRef,
};
pub use instruction::{
    Instr,
    InstrKind,
};
pub use module::Module;
pub use ty::Type;

use crate::cfg::{
    BBArgRef,
    Cfg,
    InstrRef,
};

pub mod cfg;
pub mod function;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, From)]
pub enum Value {
    Instr(InstrRef),
    BBArg(BBArgRef),
}

impl Value {
    pub fn display<'cfg>(&self, cfg: &'cfg Cfg) -> ValueDisplay<'cfg> {
        ValueDisplay(cfg, *self)
    }
}

struct ValueDisplay<'cfg>(&'cfg Cfg, Value);

impl<'cfg> std::fmt::Display for ValueDisplay<'cfg> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.1 {
            Value::Instr(instr) => write!(f, "{}", self.0.instructions[instr]),
            Value::BBArg(arg) => write!(f, "{}", self.0.bb_args[arg]),
        }
    }
}

pub mod instruction;

pub mod module;
pub mod optimization;

mod analysis;
mod front_bridge;
#[cfg(test)]
pub mod test;
pub mod ty;
