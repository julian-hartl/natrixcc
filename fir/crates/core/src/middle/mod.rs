pub mod cfg;
pub use cfg::ValueId;
pub mod function;

pub use function::{Function, FunctionId};

index_vec::define_index_type! {
    pub struct Value = usize;
}

pub mod instruction;
pub use instruction::{InstrId, InstrKind, Instr};
mod optimization;
pub mod module;
pub use module::{Module};
mod analysis;
