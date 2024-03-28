use std::fmt::Debug;

use smallvec::SmallVec;

use crate::codegen::{
    machine::{
        function::{
            builder::{
                MatchedPattern,
                PatternIn,
            },
            Function,
        },
        MachInstr,
        TargetMachine,
    },
    selection_dag::Immediate,
};
use crate::codegen::machine::Instr;

type Reg<B: Backend> = <B::TM as TargetMachine>::Reg;
type BackInstr<B: Backend> = <B::TM as TargetMachine>::Instr;
pub trait Backend {
    type TM: TargetMachine;

    type P: Pattern<TM = Self::TM>;

    fn patterns() -> &'static [Self::P];

    fn mov(dest: Reg<Self>, src: Reg<Self>) -> BackInstr<Self>;

    fn mov_imm(dest: Reg<Self>, imm: Immediate) -> BackInstr<Self>;

    fn ret() -> BackInstr<Self>;

    fn new() -> Self;
}

pub trait Pattern: Sized + Debug + Clone + PartialEq + Eq + 'static {
    type TM: TargetMachine;

    fn in_(&self) -> PatternIn;

    fn into_instr(
        self,
        function: &mut Function<Self::TM>,
        matched: MatchedPattern<Self::TM>,
    ) -> SmallVec<[Instr<Self::TM>; 2]>;
}
