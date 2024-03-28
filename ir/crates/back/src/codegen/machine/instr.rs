use std::fmt::{
    Display,
    Formatter,
};

use smallvec::{
    smallvec,
    SmallVec,
};

use crate::codegen::{
    machine::Register,
    selection_dag::Immediate,
};
use crate::codegen::machine::function::BasicBlockId;
use crate::codegen::machine::isa::MachInstr as MInstr;
use crate::codegen::machine::TargetMachine;

index_vec::define_index_type! {
    pub struct InstrId = u32;

    DISPLAY_FORMAT = "instr{}";
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr<TM: TargetMachine> {
    Pseudo(PseudoInstr<TM>),
    Machine(TM::Instr),
}

impl<TM: TargetMachine> Instr<TM> {
    pub fn name(&self) -> &'static str {
        match self {
            Instr::Pseudo(pseudo) => pseudo.name(),
            Instr::Machine(machine) => machine.name(),
        }
    }

    pub fn reads(&self) -> SmallVec<[Register<TM>; 2]> {
        match self {
            Instr::Pseudo(pseudo) => pseudo.reads(),
            Instr::Machine(machine) => machine.reads(),
        }
    }

    pub fn reads_implicitly(&self) -> SmallVec<[Register<TM>; 2]> {
        let writes = self.writes();
        let reg_operands = self
            .operands()
            .iter()
            .filter_map(|operand| {
                if let InstrOperand::Reg(reg) = operand {
                    Some(*reg)
                } else {
                    None
                }
            })
            .collect::<SmallVec<[_; 2]>>();
        let mut implicit_reads = SmallVec::new();
        for read in self.reads() {
            if !reg_operands.contains(&read) && writes.map_or(true, |writes| writes != read) {
                implicit_reads.push(read);
            }
        }
        implicit_reads
    }

    pub fn writes(&self) -> Option<Register<TM>> {
        match self {
            Instr::Pseudo(pseudo) => pseudo.writes(),
            Instr::Machine(machine) => machine.writes().map(|reg| reg.into()),
        }
    }

    pub fn operands(&self) -> SmallVec<[InstrOperand<TM>; 3]> {
        match self {
            Instr::Pseudo(pseudo) => pseudo.operands(),
            Instr::Machine(machine) => machine.operands(),
        }
    }

    pub fn written_regs_mut(&mut self) -> SmallVec<[&mut Register<TM>; 1]> {
        match self {
            Instr::Pseudo(pseudo) => pseudo.written_regs_mut(),
            Instr::Machine(machine) => machine.written_regs_mut(),
        }
    }
    pub fn read_regs_mut(&mut self) -> SmallVec<[&mut Register<TM>; 2]> {
        match self {
            Instr::Pseudo(pseudo) => pseudo.read_regs_mut(),
            Instr::Machine(machine) => machine.read_regs_mut(),
        }
    }
    pub fn try_as_machine(&self) -> Option<&TM::Instr> {
        match self {
            Instr::Pseudo(_) => None,
            Instr::Machine(machine) => Some(machine),
        }
    }

}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InstrOperand<TM: TargetMachine> {
    Reg(Register<TM>),
    Imm(Immediate),
    Label(BasicBlockId),
}

#[derive(Debug)]
pub enum InstrOperandMut<'a, TM: TargetMachine> {
    Reg(&'a mut Register<TM>),
    Imm(&'a mut Immediate),
    Label(&'a mut BasicBlockId),
}

impl<TM: TargetMachine> Display for InstrOperand<TM> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(reg) => write!(f, "{}", reg),
            Self::Imm(imm) => write!(f, "{}", imm),
            Self::Label(label) => write!(f, "{}", label),
        }
    }
}

impl<'op, TM: TargetMachine> From<&'op mut InstrOperand<TM>> for InstrOperandMut<'op, TM> {
    fn from(value: &'op mut InstrOperand<TM>) -> Self {
        match value {
            InstrOperand::Reg(reg) => InstrOperandMut::Reg(reg),
            InstrOperand::Imm(imm) => InstrOperandMut::Imm(imm),
            InstrOperand::Label(label) => InstrOperandMut::Label(label),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PseudoInstr<TM: TargetMachine> {
    Copy(Register<TM>, Register<TM>),
    Ret(Option<InstrOperand<TM>>),
    Def(Register<TM>),
}

impl<TM: TargetMachine> PseudoInstr<TM> {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Copy(_, _) => "COPY",
            Self::Ret(_) => "RET",
            Self::Def(_) => "DEF",
        }
    }

    pub fn reads(&self) -> SmallVec<[Register<TM>; 2]> {
        match self {
            Self::Copy(_, to) => {
                smallvec![*to,]
            }
            Self::Ret(value) => {
                let mut reads = smallvec![];
                if let Some(InstrOperand::Reg(reg)) = value {
                    reads.push(*reg)
                }
                reads
            }
            Self::Def(_) => {
                smallvec![]
            }
        }
    }

    pub fn operands(&self) -> SmallVec<[InstrOperand<TM>; 3]> {
        match self {
            Self::Copy(dest, src) => {
                smallvec![InstrOperand::Reg(*dest), InstrOperand::Reg(*src),]
            }
            Self::Ret(value) => match value {
                None => smallvec![],
                Some(value) => smallvec![value.clone()],
            },
            Self::Def(reg) => {
                smallvec![InstrOperand::Reg(*reg),]
            }
        }
    }

    pub fn written_regs_mut(&mut self) -> SmallVec<[&mut Register<TM>; 1]> {
        match self {
            Self::Copy(dest, _) => {
                smallvec![dest,]
            }
            Self::Ret(_) => {
                smallvec![]
            }
            Self::Def(reg) => {
                smallvec![reg,]
            }
        }
    }

    pub fn read_regs_mut(&mut self) -> SmallVec<[&mut Register<TM>; 2]> {
        match self {
            Self::Copy(_, src) => {
                smallvec![src,]
            }
            Self::Ret(value) => {
                let mut reads = smallvec![];
                if let Some(InstrOperand::Reg(reg)) = value {
                    reads.push(reg)
                }
                reads
            }
            Self::Def(_) => {
                smallvec![]
            }
        }
    }

    pub fn writes(&self) -> Option<Register<TM>> {
        match self {
            Self::Copy(dest, _) => Some(*dest),
            Self::Ret(_) => None,
            Self::Def(dest) => Some(*dest),
        }
    }
}
