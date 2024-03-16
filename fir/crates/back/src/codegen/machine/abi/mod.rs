use std::fmt::Debug;
use std::hash::Hash;

pub use calling_convention::CallingConvention;

use crate::codegen::machine;
use crate::codegen::machine::asm::Assembler;

pub mod calling_convention;

pub trait Abi: Debug + Default + Clone + PartialEq + Eq + Hash {
    type I: machine::MachineInstr<Abi=Self>;

    type REG: machine::PhysicalRegister + 'static + Hash + Copy;

    type ASSEMBLER: Assembler<Self>;

    type CallingConvention: CallingConvention<Reg=Self::REG>;

    fn get_assembler() -> Self::ASSEMBLER {
        Self::ASSEMBLER::new()
    }
}

