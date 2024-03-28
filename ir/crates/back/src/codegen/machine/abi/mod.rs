use std::fmt::Debug;

pub use calling_convention::CallingConvention;

use crate::codegen::machine::isa::PhysicalRegister;

pub mod calling_convention;