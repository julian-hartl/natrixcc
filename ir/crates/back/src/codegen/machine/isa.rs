use std::{
    fmt::Debug,
    hash::Hash,
};

use smallvec::SmallVec;

use crate::codegen::{
    machine::{
        instr::InstrOperand,
        reg::Register,
        Abi,
        Size,
    },
};

pub trait Isa {
    type Reg: PhysicalRegister + 'static + Hash + Copy + Clone;

    type Instr: Instr;
}

pub trait PhysicalRegister: Debug + Clone + Copy + PartialEq + Eq + Sized {
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

    fn regclass(&self) -> impl Iterator<Item = Self>
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

pub trait Instr: Debug + PartialEq + Eq + Clone {
    type Isa: Isa;
    fn name(&self) -> &'static str;

    fn writes(&self) -> Option<Register<Self::Isa>>;

    fn reads(&self) -> SmallVec<[Register<Self::Isa>; 2]>;

    fn operands(&self) -> SmallVec<[InstrOperand<Self::Isa>; 3]>;

    fn written_regs_mut(&mut self) -> SmallVec<[&mut Register<Self::Isa>; 1]>;

    fn read_regs_mut(&mut self) -> SmallVec<[&mut Register<Self::Isa>; 2]>;
}
