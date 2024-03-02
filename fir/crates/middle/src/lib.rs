#![feature(impl_trait_in_assoc_type)]
#![feature(type_alias_impl_trait)]
pub mod cfg;
pub mod function;

pub use function::{Function, FunctionId};

index_vec::define_index_type! {
    pub struct Value = usize;
    
    DISPLAY_FORMAT = "%{}";
}

pub mod instruction;
pub use instruction::{ InstrKind, Instr};
pub mod optimization;
pub mod module;
pub use module::{Module};
mod analysis;
#[cfg(test)]
pub mod test;
pub mod ty;
mod front_bridge;
pub use front_bridge::FrontBridge;

pub use ty::{Type};
