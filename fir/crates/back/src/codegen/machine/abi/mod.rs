pub mod calling_convention;
pub use calling_convention::CallingConvention;

use std::fmt::Debug;
use std::hash::Hash;

use crate::codegen::machine;
use crate::codegen::machine::asm::Assembler;

pub trait Abi: Debug + Default + Clone + PartialEq +  Eq {
    type I: machine::MachineInstr<Abi=Self>;

    type REG: machine::PhysicalRegister + 'static + Hash;

    type ASSEMBLER: Assembler<Self>;

    type CallingConvention: CallingConvention<Reg=Self::REG>;

    fn get_assembler() -> Self::ASSEMBLER {
        Self::ASSEMBLER::new()
    }
}

