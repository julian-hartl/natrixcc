#![feature(impl_trait_in_assoc_type)]
#![feature(type_alias_impl_trait)]

use cranelift_entity::entity_impl;
pub use front_bridge::FrontBridge;
pub use function::{
    Function,
    FunctionId,
};
pub use instruction::{
    Instr,
    InstrKind,
};
pub use module::Module;
pub use ty::Type;

pub mod cfg;
pub mod function;

#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Default)]
pub struct VReg(u32);
entity_impl!(VReg, "v");
pub mod instruction;

pub mod module;
pub mod optimization;

mod analysis;
mod front_bridge;
#[cfg(test)]
pub mod test;
pub mod ty;
mod front_bridge;
mod verifier;

pub use verifier::Verifier;

