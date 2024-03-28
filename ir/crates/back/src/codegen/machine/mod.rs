use std::fmt::Debug;

use cranelift_entity::EntityRef;

pub use abi::Abi;
pub use function::{
    Function,
    FunctionId,
};
pub use instr::{
    Instr,
    InstrId,
};
use isa::{
    Instr as MInstr,
    PhysicalRegister,
};
pub use isa::Isa;
pub use module::Module;
use natrix_middle::ty::Type;
pub use reg::{
    Register,
    VReg,
};

use crate::codegen::machine::asm::Assembler;

pub mod abi;
pub mod asm;
pub mod isa;
pub mod module;
pub mod reg;

pub mod function;
pub mod instr;

pub mod backend;
pub use backend::Backend;

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

pub trait TargetMachine {
    type Abi: Abi<Reg = <Self::Isa as Isa>::Reg>;

    type Isa: Isa;

    type Backend: Backend<Isa=Self::Isa>;

    type ASSEMBLER: Assembler<TM=Self>;

    fn endianness() -> Endianness;

    fn arch() -> Architecture;
}
