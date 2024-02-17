use crate::codegen::machine;
use crate::codegen::machine::{Abi, PhysicalRegister};
use crate::ty::Type;

pub mod x86_64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Size {
    Byte,
    Word,
    DWord,
    QWord,
}

impl Size {
    pub fn from_ty(ty: &Type) -> Self {
        let bit_width = ty.size() * 8;
        Self::from_bit_width(bit_width)
    }

    pub fn from_bit_width(bit_width: u32) -> Self {
        if bit_width <= 8 {
            Self::Byte
        } else if bit_width <= 16 {
            Self::Word
        } else if bit_width <= 32 {
            Self::DWord
        } else if bit_width <= 64 {
            Self::QWord
        } else {
            panic!("Invalid bit width: {}", bit_width)
        }
    }
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
