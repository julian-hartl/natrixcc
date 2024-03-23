use crate::codegen::machine::{Abi, PhysicalRegister};

pub mod x86_64;

pub enum Endianness {
    Little,
    Big
}

pub enum Architecture {
    X86_64,
}
//
// pub trait Architecture {
//     fn get_calling_convention(&self) -> &dyn CallingConvention;
// }

// pub trait CallingConvention {
//
//     type Abi: Abi;
//
//     fn get_return_register(&self, size: Size) -> Self::Abi::Reg;
// }
