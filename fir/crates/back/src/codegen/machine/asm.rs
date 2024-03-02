use crate::codegen::machine::Abi;

pub trait Assembler<A: Abi> {
    fn new() -> Self;

    fn assemble(&mut self, instr: &A::I);

    fn finish(self) -> Vec<u8>;

    fn format(&self) -> String;

    fn save<P: AsRef<std::path::Path>>(&self, path: P) -> std::io::Result<()> {
        std::fs::write(path, self.format())
    }
}
