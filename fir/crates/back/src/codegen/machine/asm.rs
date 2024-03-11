use crate::codegen::machine::{Abi, BasicBlockId};

pub trait Assembler<A: Abi> {
    fn new() -> Self;
    
    fn begin_basic_block(&mut self, bb_id: BasicBlockId);

    fn assemble(&mut self, instr: &A::I);

    fn finish(self) -> Vec<u8>;

    fn format(&self) -> String;
}
