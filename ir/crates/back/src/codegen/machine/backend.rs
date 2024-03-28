use std::fmt::Debug;

use smallvec::SmallVec;

use crate::codegen::{
    machine::{
        Abi,
        function::{
            builder::{
                MatchedPattern,
                PatternIn,
            },
            Function,
        },
    },
    selection_dag::Immediate,
};
use crate::codegen::machine::Instr;
use crate::codegen::machine::isa::Isa;
type Reg<B: Backend> = <B::Isa as Isa>::Reg;
type BackInstr<B: Backend> = <B::Isa as Isa>::Instr;
pub trait Backend {
    type Isa: Isa;

    type P: Pattern<Isa = Self::Isa>;
    

    fn patterns() -> &'static [Self::P];

    fn mov(dest: Reg<Self>, src: Reg<Self>) -> BackInstr<Self>;

    fn mov_imm(dest: Reg<Self>, imm: Immediate) -> BackInstr<Self>;

    fn ret() -> BackInstr<Self>;

    fn new() -> Self;
}

pub trait Pattern: Sized + Debug + Clone + PartialEq + Eq + 'static {
    type Isa: Isa;

    fn in_(&self) -> PatternIn;

    fn into_instr(
        self,
        function: &mut Function<Self::Isa>,
        matched: MatchedPattern<Self::Isa>,
    ) -> SmallVec<[Instr<<Self as Pattern>::Isa>; 2]>;
}
