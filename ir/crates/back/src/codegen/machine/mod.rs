use std::fmt::Debug;

pub use backend::Backend;
pub use function::{
    Function,
    FunctionId,
};
pub use instr::{
    Instr,
    InstrId,
};
pub use isa::{
    MachInstr,
    PhysicalRegister,
};
pub use module::Module;
use natrix_middle::ty::Type;
pub use reg::{
    Register,
    VRegRef,
};

use crate::codegen::machine::{
    abi::CallingConvention,
    asm::Assembler,
};

pub mod abi;
pub mod asm;
pub mod isa;
pub mod module;
pub mod reg;

pub mod function;
pub mod instr;

pub mod backend;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Display)]
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

    pub fn bit_width(&self) -> u32 {
        match self {
            Size::Byte => 8,
            Size::Word => 16,
            Size::DWord => 32,
            Size::QWord => 64,
        }
    }
}

impl PartialOrd for Size {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.bit_width().partial_cmp(&other.bit_width())
    }
}

impl From<&Type> for Size {
    fn from(value: &Type) -> Self {
        Self::from_ty(value)
    }
}

pub enum Endianness {
    Little,
    Big,
}

pub enum Architecture {
    X86_64,
}

pub trait TargetMachine: Debug + Default + Copy + Clone + PartialEq + Eq {
    type Reg: PhysicalRegister;

    type Instr: MachInstr<TM = Self>;

    type CallingConvention: CallingConvention<Reg = Self::Reg>;

    type Backend: Backend<TM = Self>;

    type Assembler: Assembler<TM = Self>;

    fn endianness() -> Endianness;

    fn arch() -> Architecture;
}
