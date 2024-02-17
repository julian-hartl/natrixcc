use std::fmt::Debug;

use crate::codegen::machine;
use crate::codegen::machine::asm::Assembler;

pub trait Abi: Debug + Default + Clone + PartialEq +  Eq {
    type I: machine::Instr;

    type REG: machine::PhysicalRegister;

    type ASSEMBLER: Assembler<Self>;

    fn get_assembler() -> Self::ASSEMBLER {
        Self::ASSEMBLER::new()
    }
}
