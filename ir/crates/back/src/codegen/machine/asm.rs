use crate::codegen::machine::{
    function::cfg::BasicBlockId,
    isa::Isa,
    TargetMachine,
};

pub trait Assembler {
    type TM: TargetMachine;
    
    fn new(base_addr: u64) -> Self;

    fn begin_basic_block(&mut self, bb_id: BasicBlockId);

    fn assemble(&mut self, instr: &<<Self::TM as TargetMachine>::Isa as Isa>::Instr);

    fn finish(self) -> Vec<u8>;

    fn format(&self) -> String;
}
