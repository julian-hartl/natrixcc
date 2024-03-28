use std::{
    fmt::Debug,
    hash::Hash,
};

use smallvec::SmallVec;

use crate::codegen::machine::{
    instr::InstrOperand,
    reg::Register,
    Size,
};
use crate::codegen::machine::TargetMachine;

pub trait PhysicalRegister: Debug + Clone + Copy + PartialEq + Eq + Sized + Hash + 'static {
    fn name(&self) -> &'static str;

    fn all() -> &'static [Self];

    fn is_gp(&self) -> bool;

    fn size(&self) -> Size;

    fn into_unicorn_emu_reg(self) -> impl Into<i32>;

    /// Returns the sub registers of this register.
    ///
    /// E.g. on x86-64, the sub registers of RAX are EAX, AX, AH and AL.
    fn subregs(&self) -> Option<&'static [Self]>;

    fn superregs(&self) -> Option<&'static [Self]>;

    fn regclass(&self) -> impl Iterator<Item=Self>
        where
            Self: 'static,
    {
        self.subregs()
            .into_iter()
            .flatten()
            .copied()
            .chain(self.superregs().into_iter().flatten().copied())
            .chain(std::iter::once(*self))
    }

    fn has_subreg(&self, other: Self) -> bool
        where
            Self: 'static,
    {
        self.subregs()
            .map_or(false, |subregs| subregs.contains(&other))
    }

    fn interferes_with(self, other: Self) -> bool
        where
            Self: 'static,
    {
        if self == other {
            return true;
        }
        if self.has_subreg(other) || other.has_subreg(self) {
            return true;
        }
        false
    }
}

pub trait MachInstr: Debug + PartialEq + Eq + Clone {
    type TM: TargetMachine;
    fn name(&self) -> &'static str;

    fn writes(&self) -> Option<Register<Self::TM>>;

    fn reads(&self) -> SmallVec<[Register<Self::TM>; 2]>;

    fn operands(&self) -> SmallVec<[InstrOperand<Self::TM>; 3]>;

    fn written_regs_mut(&mut self) -> SmallVec<[&mut Register<Self::TM>; 1]>;

    fn read_regs_mut(&mut self) -> SmallVec<[&mut Register<Self::TM>; 2]>;
}
