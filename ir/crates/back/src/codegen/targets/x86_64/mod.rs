use smallvec::{
    smallvec,
    SmallVec,
};
use strum::VariantArray;

use machine::instr::Instr as MInstr;
use natrix_middle::instruction::CmpOp;

use crate::codegen::{
    machine,
    machine::{
        Architecture,
        backend,
        Endianness,
        function::{
            BasicBlockId,
            builder::{
                MatchedPattern,
                PatternInOperand,
                PatternInOutput,
            },
            Function,
        },
        instr::{
            InstrOperand,
            PseudoInstr,
        },
        isa::PhysicalRegister as MachPhysicalRegister,
        Size,
        TargetMachine,
    },
    selection_dag::Immediate,
};
use crate::codegen::machine::function::builder::PatternIn;
use crate::codegen::targets::calling_convention::systemv::SystemV;

mod asm;

#[derive(Debug, Default, Copy, Clone, Eq, PartialEq)]
pub struct Target;

impl TargetMachine for Target {
    type Reg = PhysicalRegister;
    type Instr = Instr;
    type CallingConvention = SystemV;
    type Backend = Backend;
    type Assembler = asm::Assembler;

    fn endianness() -> Endianness {
        Endianness::Little
    }

    fn arch() -> Architecture {
        Architecture::X86_64
    }
}

pub type Register = machine::Register<Target>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumTryAs, IntoStaticStr)]
pub enum CC {
    Eq,
    Gt,
}

impl From<CmpOp> for CC {
    fn from(op: CmpOp) -> Self {
        match op {
            CmpOp::Eq => Self::Eq,
            CmpOp::Gt => Self::Gt,
            _ => unimplemented!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, EnumTryAs, IntoStaticStr)]
pub enum Instr {
    SUB32ri {
        dest: Register,
        immediate: Immediate,
    },
    SUB32rr {
        dest: Register,
        src: Register,
    },
    ADD32ri {
        dest: Register,
        immediate: Immediate,
    },
    ADD32rr {
        dest: Register,
        src: Register,
    },
    MOV8ri {
        dest: Register,
        immediate: Immediate,
    },
    MOV8rr {
        dest: Register,
        src: Register,
    },
    MOV16ri {
        dest: Register,
        immediate: Immediate,
    },
    MOV16rr {
        dest: Register,
        src: Register,
    },
    MOV32ri {
        dest: Register,
        immediate: Immediate,
    },
    MOV32rr {
        dest: Register,
        src: Register,
    },
    MOV64ri {
        dest: Register,
        immediate: Immediate,
    },
    MOV64rr {
        dest: Register,
        src: Register,
    },
    CMP32rr {
        lhs: Register,
        rhs: Register,
    },
    CMP32ri {
        lhs: Register,
        rhs: Immediate,
    },
    CMP8ri {
        lhs: Register,
        rhs: Immediate,
    },
    SETCC {
        dest: Register,
        cc: CC,
    },
    JMP {
        target: BasicBlockId,
    },
    JCC {
        cc: CC,
        target: BasicBlockId,
    },
    RET,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, VariantArray)]
pub enum Pattern {
    Mov8ri,
    Mov8rr,
    Mov16ri,
    Mov16rr,
    Mov32ri,
    Mov32rr,
    Mov64ri,
    Mov64rr,
    Sub32ri,
    Sub32rr,
    Add32ri,
    Add32rr,
    Cmp32rreq,
    Cmp32rigt,
    CondJmp,
    Jmp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoStaticStr, VariantArray, Hash)]
pub enum PhysicalRegister {
    AL,
    AH,
    AX,
    EAX,
    RAX,
    BL,
    BH,
    BX,
    EBX,
    RBX,
    CL,
    CH,
    CX,
    ECX,
    RCX,
    DL,
    DH,
    DX,
    EDX,
    RDX,
    SIL,
    SI,
    ESI,
    RSI,
    DIL,
    DI,
    EDI,
    RDI,
    R8L,
    R8W,
    R8D,
    R8,
    R9L,
    R9W,
    R9D,
    R9,
    EFLAGS,
}

impl MachPhysicalRegister for PhysicalRegister {
    fn name(&self) -> &'static str {
        self.into()
    }

    fn all() -> &'static [Self] {
        Self::VARIANTS
    }

    fn is_gp(&self) -> bool {
        match self {
            Self::EFLAGS => false,
            _ => true,
        }
    }

    fn size(&self) -> Size {
        match self {
            Self::AL
            | Self::AH
            | Self::BL
            | Self::BH
            | Self::CL
            | Self::CH
            | Self::DL
            | Self::DH
            | Self::SIL
            | Self::DIL
            | Self::R8L
            | Self::R9L => Size::Byte,
            Self::AX
            | Self::BX
            | Self::CX
            | Self::DX
            | Self::SI
            | Self::DI
            | Self::R8W
            | Self::R9W => Size::Word,
            Self::EAX
            | Self::ECX
            | Self::EDX
            | Self::EBX
            | Self::ESI
            | Self::EDI
            | Self::R8D
            | Self::R9D
            | Self::EFLAGS => Size::DWord,
            Self::RAX
            | Self::RBX
            | Self::RCX
            | Self::RDX
            | Self::R8
            | Self::R9
            | Self::RSI
            | Self::RDI => Size::QWord,
        }
    }

    fn into_unicorn_emu_reg(self) -> impl Into<i32> {
        use unicorn_engine::RegisterX86;
        match self {
            Self::AL => RegisterX86::AL,
            Self::AH => RegisterX86::AH,
            Self::AX => RegisterX86::AX,
            Self::EAX => RegisterX86::EAX,
            Self::RAX => RegisterX86::RAX,
            Self::BL => RegisterX86::BL,
            Self::BH => RegisterX86::BH,
            Self::BX => RegisterX86::BX,
            Self::EBX => RegisterX86::EBX,
            Self::RBX => RegisterX86::RBX,
            Self::CL => RegisterX86::CL,
            Self::CH => RegisterX86::CH,
            Self::CX => RegisterX86::CX,
            Self::ECX => RegisterX86::ECX,
            Self::RCX => RegisterX86::RCX,
            Self::DL => RegisterX86::DL,
            Self::DH => RegisterX86::DH,
            Self::DX => RegisterX86::DX,
            Self::EDX => RegisterX86::EDX,
            Self::RDX => RegisterX86::RDX,
            Self::SIL => RegisterX86::SIL,
            Self::SI => RegisterX86::SI,
            Self::ESI => RegisterX86::ESI,
            Self::RSI => RegisterX86::RSI,
            Self::DIL => RegisterX86::DIL,
            Self::DI => RegisterX86::DI,
            Self::EDI => RegisterX86::EDI,
            Self::RDI => RegisterX86::RDI,
            Self::R8L => RegisterX86::R8B,
            Self::R8W => RegisterX86::R8W,
            Self::R8D => RegisterX86::R8D,
            Self::R8 => RegisterX86::R8,
            Self::R9L => RegisterX86::R9B,
            Self::R9W => RegisterX86::R9W,
            Self::R9D => RegisterX86::R9D,
            Self::R9 => RegisterX86::R9,
            Self::EFLAGS => RegisterX86::EFLAGS,
        }
    }

    fn subregs(&self) -> Option<&'static [Self]> {
        match self {
            Self::AL | Self::AH => None,
            Self::AX => Some(&[Self::AL, Self::AH]),
            Self::EAX => Some(&[Self::AX, Self::AL, Self::AH]),
            Self::RAX => Some(&[Self::EAX, Self::AX, Self::AL, Self::AH]),
            Self::BL | Self::BH => None,
            Self::BX => Some(&[Self::BL, Self::BH]),
            Self::EBX => Some(&[Self::BX, Self::BL, Self::BH]),
            Self::RBX => Some(&[Self::EBX, Self::BX, Self::BL, Self::BH]),
            Self::CL | Self::CH => None,
            Self::CX => Some(&[Self::CL, Self::CH]),
            Self::ECX => Some(&[Self::CX, Self::CL, Self::CH]),
            Self::RCX => Some(&[Self::ECX, Self::CX, Self::CL, Self::CH]),
            Self::DL | Self::DH => None,
            Self::DX => Some(&[Self::DL, Self::DH]),
            Self::EDX => Some(&[Self::DX, Self::DL, Self::DH]),
            Self::RDX => Some(&[Self::EDX, Self::DX, Self::DL, Self::DH]),
            Self::SIL => None,
            Self::SI => Some(&[Self::SIL]),
            Self::ESI => Some(&[Self::SI, Self::SIL]),
            Self::RSI => Some(&[Self::ESI, Self::SI, Self::SIL]),
            Self::DIL => None,
            Self::DI => Some(&[Self::DIL]),
            Self::EDI => Some(&[Self::DI, Self::DIL]),
            Self::RDI => Some(&[Self::EDI, Self::DI, Self::DIL]),
            Self::R8L => None,
            Self::R8W => Some(&[Self::R8L]),
            Self::R8D => Some(&[Self::R8W, Self::R8L]),
            Self::R8 => Some(&[Self::R8D, Self::R8W, Self::R8L]),
            Self::R9L => None,
            Self::R9W => Some(&[Self::R9L]),
            Self::R9D => Some(&[Self::R9W, Self::R9L]),
            Self::R9 => Some(&[Self::R9D, Self::R9W, Self::R9L]),
            Self::EFLAGS => None,
        }
    }

    fn superregs(&self) -> Option<&'static [Self]> {
        match self {
            PhysicalRegister::AL | PhysicalRegister::AH => Some(&[
                PhysicalRegister::AX,
                PhysicalRegister::EAX,
                PhysicalRegister::RAX,
            ]),
            PhysicalRegister::AX => Some(&[PhysicalRegister::EAX, PhysicalRegister::RAX]),
            PhysicalRegister::EAX => Some(&[PhysicalRegister::RAX]),
            PhysicalRegister::RAX => None,
            PhysicalRegister::BL | PhysicalRegister::BH => Some(&[
                PhysicalRegister::BX,
                PhysicalRegister::EBX,
                PhysicalRegister::RBX,
            ]),
            PhysicalRegister::BX => Some(&[PhysicalRegister::EBX, PhysicalRegister::RBX]),
            PhysicalRegister::EBX => Some(&[PhysicalRegister::RBX]),
            PhysicalRegister::RBX => None,
            PhysicalRegister::CL | PhysicalRegister::CH => Some(&[
                PhysicalRegister::CX,
                PhysicalRegister::ECX,
                PhysicalRegister::RCX,
            ]),
            PhysicalRegister::CX => Some(&[PhysicalRegister::ECX, PhysicalRegister::RCX]),
            PhysicalRegister::ECX => Some(&[PhysicalRegister::RCX]),
            PhysicalRegister::RCX => None,
            PhysicalRegister::DL | PhysicalRegister::DH => Some(&[
                PhysicalRegister::DX,
                PhysicalRegister::EDX,
                PhysicalRegister::RDX,
            ]),
            PhysicalRegister::DX => Some(&[PhysicalRegister::EDX, PhysicalRegister::RDX]),
            PhysicalRegister::EDX => Some(&[PhysicalRegister::RDX]),
            PhysicalRegister::RDX => None,
            PhysicalRegister::SIL => Some(&[
                PhysicalRegister::SI,
                PhysicalRegister::ESI,
                PhysicalRegister::RSI,
            ]),
            PhysicalRegister::SI => Some(&[PhysicalRegister::ESI, PhysicalRegister::RSI]),
            PhysicalRegister::ESI => Some(&[PhysicalRegister::RSI]),
            PhysicalRegister::RSI => None,
            PhysicalRegister::DIL => Some(&[
                PhysicalRegister::DI,
                PhysicalRegister::EDI,
                PhysicalRegister::RDI,
            ]),
            PhysicalRegister::DI => Some(&[PhysicalRegister::EDI, PhysicalRegister::RDI]),
            PhysicalRegister::EDI => Some(&[PhysicalRegister::RDI]),
            PhysicalRegister::RDI => None,
            PhysicalRegister::R8L => Some(&[
                PhysicalRegister::R8W,
                PhysicalRegister::R8D,
                PhysicalRegister::R8,
            ]),
            PhysicalRegister::R8W => Some(&[PhysicalRegister::R8D, PhysicalRegister::R8]),
            PhysicalRegister::R8D => Some(&[PhysicalRegister::R8]),
            PhysicalRegister::R8 => None,
            PhysicalRegister::R9L => Some(&[
                PhysicalRegister::R9W,
                PhysicalRegister::R9D,
                PhysicalRegister::R9,
            ]),
            PhysicalRegister::R9W => Some(&[PhysicalRegister::R9D, PhysicalRegister::R9]),
            PhysicalRegister::R9D => Some(&[PhysicalRegister::R9]),
            PhysicalRegister::R9 => None,
            PhysicalRegister::EFLAGS => None,
        }
    }
}


impl machine::isa::MachInstr for Instr {
    type TM = Target;

    fn name(&self) -> &'static str {
        self.into()
    }

    fn writes(&self) -> Option<Register> {
        match self {
            Self::MOV8ri { dest, .. }
            | Self::MOV8rr { dest, .. }
            | Self::MOV16ri { dest, .. }
            | Self::MOV16rr { dest, .. }
            | Self::MOV32ri { dest, .. }
            | Self::MOV32rr { dest, .. }
            | Self::MOV64ri { dest, .. }
            | Self::MOV64rr { dest, .. } => Some(*dest),
            Self::SUB32ri { dest, .. } => Some(*dest),
            Self::SUB32rr { dest, .. } => Some(*dest),
            Self::ADD32ri { dest, .. } => Some(*dest),
            Self::ADD32rr { dest, .. } => Some(*dest),
            Self::JMP { .. } => None,
            Self::RET => None,
            Self::CMP32rr { .. } => Some(Register::Physical(PhysicalRegister::EFLAGS)),
            Self::CMP32ri { .. } => Some(Register::Physical(PhysicalRegister::EFLAGS)),
            Self::CMP8ri { .. } => Some(Register::Physical(PhysicalRegister::EFLAGS)),
            Self::SETCC { dest, .. } => Some(*dest),
            Self::JCC { .. } => None,
        }
    }

    fn reads(&self) -> SmallVec<[machine::Register<Self::TM>; 2]> {
        match self {
            Self::MOV8ri { .. }
            | Self::MOV16ri { .. }
            | Self::MOV32ri { .. }
            | Self::MOV64ri { .. } => smallvec![],
            Self::MOV8rr { src, .. }
            | Self::MOV16rr { src, .. }
            | Self::MOV32rr { src, .. }
            | Self::MOV64rr { src, .. } => smallvec![*src],
            Self::SUB32ri { dest, .. } => smallvec![*dest],
            Self::SUB32rr { src, dest } => smallvec![*src, *dest],
            Self::ADD32ri { dest, .. } => smallvec![*dest],
            Self::ADD32rr { src, dest } => smallvec![*src, *dest],
            Self::JMP { .. } => smallvec![],
            Self::RET => smallvec![],
            Self::CMP32rr { lhs, rhs } => smallvec![*lhs, *rhs],
            Self::CMP32ri { lhs, .. } => smallvec![*lhs],
            Self::CMP8ri { lhs, .. } => smallvec![*lhs],
            Self::SETCC { .. } => smallvec![Register::Physical(PhysicalRegister::EFLAGS)],
            Self::JCC { .. } => smallvec![Register::Physical(PhysicalRegister::EFLAGS)],
        }
    }

    fn operands(&self) -> SmallVec<[InstrOperand<Self::TM>; 3]> {
        match self {
            Self::MOV8ri { dest, immediate }
            | Self::MOV16ri { dest, immediate }
            | Self::MOV32ri { dest, immediate }
            | Self::MOV64ri { dest, immediate } => {
                smallvec![InstrOperand::Reg(*dest), InstrOperand::Imm(*immediate)]
            }
            Self::MOV8rr { dest, src }
            | Self::MOV16rr { dest, src }
            | Self::MOV32rr { dest, src }
            | Self::MOV64rr { dest, src } => {
                smallvec![InstrOperand::Reg(*dest), InstrOperand::Reg(*src)]
            }
            Self::SUB32ri { dest, immediate } => {
                smallvec![InstrOperand::Reg(*dest), InstrOperand::Imm(*immediate)]
            }
            Self::SUB32rr { dest, src } => {
                smallvec![InstrOperand::Reg(*dest), InstrOperand::Reg(*src)]
            }
            Self::ADD32ri { dest, immediate } => {
                smallvec![InstrOperand::Reg(*dest), InstrOperand::Imm(*immediate)]
            }
            Self::ADD32rr { dest, src } => {
                smallvec![InstrOperand::Reg(*dest), InstrOperand::Reg(*src)]
            }
            Self::JMP { target } => smallvec![InstrOperand::Label(*target)],
            Self::RET => smallvec![],
            Self::CMP32rr { lhs, rhs } => {
                smallvec![InstrOperand::Reg(*lhs), InstrOperand::Reg(*rhs)]
            }
            Self::CMP32ri { lhs, .. } => smallvec![InstrOperand::Reg(*lhs),],
            Self::CMP8ri { lhs, .. } => smallvec![InstrOperand::Reg(*lhs),],
            Self::SETCC { dest, .. } => smallvec![InstrOperand::Reg(*dest)],
            Self::JCC { target, .. } => smallvec![InstrOperand::Label(*target),],
        }
    }
    fn written_regs_mut(&mut self) -> SmallVec<[&mut machine::Register<Self::TM>; 1]> {
        match self {
            Self::MOV8ri { dest, .. }
            | Self::MOV8rr { dest, .. }
            | Self::MOV16ri { dest, .. }
            | Self::MOV16rr { dest, .. }
            | Self::MOV32ri { dest, .. }
            | Self::MOV32rr { dest, .. }
            | Self::MOV64ri { dest, .. }
            | Self::MOV64rr { dest, .. } => smallvec![dest],
            Self::SUB32ri { dest, .. } => smallvec![dest],
            Self::SUB32rr { dest, .. } => smallvec![dest],
            Self::ADD32ri { dest, .. } => smallvec![dest],
            Self::ADD32rr { dest, .. } => smallvec![dest],
            Self::JMP { .. } => smallvec![],
            Self::RET => smallvec![],
            Self::CMP32rr { .. } => smallvec![],
            Self::CMP32ri { .. } => smallvec![],
            Self::CMP8ri { .. } => smallvec![],
            Self::SETCC { dest, .. } => smallvec![dest],
            Self::JCC { .. } => smallvec![],
        }
    }

    fn read_regs_mut(&mut self) -> SmallVec<[&mut machine::Register<Self::TM>; 2]> {
        match self {
            Self::MOV8ri { .. }
            | Self::MOV16ri { .. }
            | Self::MOV32ri { .. }
            | Self::MOV64ri { .. } => smallvec![],
            Self::MOV8rr { src, .. }
            | Self::MOV16rr { src, .. }
            | Self::MOV32rr { src, .. }
            | Self::MOV64rr { src, .. } => smallvec![src],
            Self::SUB32ri { dest, .. } => smallvec![dest],
            Self::SUB32rr { src, dest } => smallvec![src, dest],
            Self::ADD32ri { dest, .. } => smallvec![dest],
            Self::ADD32rr { src, dest } => smallvec![src, dest],
            Self::JMP { .. } => smallvec![],
            Self::RET => smallvec![],
            Self::CMP32rr { lhs, rhs } => smallvec![lhs, rhs],
            Self::CMP32ri { lhs, .. } => smallvec![lhs],
            Self::CMP8ri { lhs, .. } => smallvec![lhs],
            Self::SETCC { .. } => smallvec![],
            Self::JCC { .. } => smallvec![],
        }
    }
}

impl backend::Pattern for Pattern {
    type TM = Target;

    fn in_(&self) -> PatternIn {
        match self {
            Self::Mov8ri => PatternIn::Mov(
                PatternInOutput::Reg(Size::Byte),
                PatternInOperand::Imm(Size::Byte),
            ),
            Self::Mov8rr => PatternIn::Mov(
                PatternInOutput::Reg(Size::Byte),
                PatternInOperand::Reg(Size::Byte),
            ),
            Self::Mov16ri => PatternIn::Mov(
                PatternInOutput::Reg(Size::Word),
                PatternInOperand::Imm(Size::Word),
            ),
            Self::Mov16rr => PatternIn::Mov(
                PatternInOutput::Reg(Size::Word),
                PatternInOperand::Reg(Size::Word),
            ),
            Self::Mov32ri => PatternIn::Mov(
                PatternInOutput::Reg(Size::DWord),
                PatternInOperand::Imm(Size::DWord),
            ),
            Self::Mov32rr => PatternIn::Mov(
                PatternInOutput::Reg(Size::DWord),
                PatternInOperand::Reg(Size::DWord),
            ),
            Self::Mov64ri => PatternIn::Mov(
                PatternInOutput::Reg(Size::QWord),
                PatternInOperand::Imm(Size::QWord),
            ),
            Self::Mov64rr => PatternIn::Mov(
                PatternInOutput::Reg(Size::QWord),
                PatternInOperand::Reg(Size::QWord),
            ),
            Self::Sub32ri => PatternIn::Sub(
                PatternInOutput::Reg(Size::DWord),
                PatternInOperand::Reg(Size::DWord),
                PatternInOperand::Imm(Size::DWord),
            ),
            Self::Sub32rr => PatternIn::Sub(
                PatternInOutput::Reg(Size::DWord),
                PatternInOperand::Reg(Size::DWord),
                PatternInOperand::Reg(Size::DWord),
            ),
            Self::Add32ri => PatternIn::Add(
                PatternInOutput::Reg(Size::DWord),
                PatternInOperand::Reg(Size::DWord),
                PatternInOperand::Imm(Size::DWord),
            ),
            Self::Add32rr => PatternIn::Add(
                PatternInOutput::Reg(Size::DWord),
                PatternInOperand::Reg(Size::DWord),
                PatternInOperand::Reg(Size::DWord),
            ),
            Self::Jmp => PatternIn::Br,
            Self::Cmp32rreq => PatternIn::Cmp(
                PatternInOutput::Reg(Size::Byte),
                CmpOp::Eq,
                PatternInOperand::Reg(Size::DWord),
                PatternInOperand::Reg(Size::DWord),
            ),
            Self::Cmp32rigt => PatternIn::Cmp(
                PatternInOutput::Reg(Size::Byte),
                CmpOp::Gt,
                PatternInOperand::Reg(Size::DWord),
                PatternInOperand::Imm(Size::DWord),
            ),
            Self::CondJmp => PatternIn::CondBr(PatternInOperand::Reg(Size::Byte)),
        }
    }

    fn into_instr(
        self,
        function: &mut Function<Self::TM>,
        matched: MatchedPattern<Self::TM>,
    ) -> SmallVec<[MInstr<Self::TM>; 2]> {
        match self {
            Self::Mov8rr | Self::Mov16rr | Self::Mov32rr | Self::Mov64rr => {
                let pattern = matched.try_as_mov().unwrap();
                let dest = *pattern.dest.try_as_reg().unwrap();
                let src = *pattern.src.try_as_reg().unwrap();
                smallvec![MInstr::Pseudo(PseudoInstr::Copy(dest, src))]
            }
            Self::Mov8ri | Self::Mov16ri | Self::Mov32ri | Self::Mov64ri => {
                let pattern = matched.try_as_mov().unwrap();
                let dest = *pattern.dest.try_as_reg().unwrap();
                let immediate = pattern.src.try_as_imm().copied().unwrap();
                smallvec![match self {
                    Self::Mov8ri => MInstr::Machine(Instr::MOV8ri { dest, immediate }),
                    Self::Mov16ri => MInstr::Machine(Instr::MOV16ri { dest, immediate }),
                    Self::Mov32ri => MInstr::Machine(Instr::MOV32ri { dest, immediate }),
                    Self::Mov64ri => MInstr::Machine(Instr::MOV64ri { dest, immediate }),
                    _ => unreachable!(),
                }]
            }
            Self::Sub32ri => {
                let pattern = matched.try_as_sub().unwrap();
                let dest = *pattern.dest.try_as_reg().unwrap();
                let lhs = *pattern.lhs.try_as_reg().unwrap();
                let rhs = pattern.rhs.try_as_imm().copied().unwrap();
                smallvec![
                    MInstr::Pseudo(PseudoInstr::Copy(dest, lhs)),
                    MInstr::Machine(Instr::SUB32ri {
                        dest,
                        immediate: rhs,
                    })
                ]
            }
            Self::Sub32rr => {
                let pattern = matched.try_as_sub().unwrap();
                let dest = *pattern.dest.try_as_reg().unwrap();
                let lhs = *pattern.lhs.try_as_reg().unwrap();
                let rhs = *pattern.rhs.try_as_reg().unwrap();
                smallvec![
                    MInstr::Machine(Instr::MOV32rr { dest, src: lhs }),
                    MInstr::Machine(Instr::SUB32rr { dest, src: rhs })
                ]
            }
            Self::Add32ri => {
                let pattern = matched.try_as_add().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let lhs = pattern.lhs.try_as_reg().unwrap().clone();
                let rhs = pattern.rhs.try_as_imm().cloned().unwrap();
                smallvec![
                    MInstr::Pseudo(PseudoInstr::Copy(dest.clone(), lhs)),
                    MInstr::Machine(Instr::ADD32ri {
                        dest,
                        immediate: rhs,
                    })
                ]
            }
            Self::Add32rr => {
                let pattern = matched.try_as_add().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let lhs = pattern.lhs.try_as_reg().unwrap().clone();
                let rhs = pattern.rhs.try_as_reg().unwrap().clone();
                smallvec![
                    MInstr::Machine(Instr::MOV32rr {
                        dest: dest.clone(),
                        src: lhs,
                    }),
                    MInstr::Machine(Instr::ADD32rr { dest, src: rhs })
                ]
            }

            Self::Jmp => {
                let target = matched.try_as_br().unwrap().target;
                smallvec![MInstr::Machine(Instr::JMP { target })]
            }
            Self::Cmp32rreq => {
                let matched = matched.try_as_cmp().unwrap();
                let lhs = *matched.lhs.try_as_reg().unwrap();
                let rhs = *matched.rhs.try_as_reg().unwrap();
                let dest = *matched.dest.try_as_reg().unwrap();
                let cc = matched.cmp_op.into();
                smallvec![
                    MInstr::Machine(Instr::CMP32rr { lhs, rhs }),
                    MInstr::Machine(Instr::SETCC { dest, cc })
                ]
            }
            Self::Cmp32rigt => {
                let matched = matched.try_as_cmp().unwrap();
                let lhs = *matched.lhs.try_as_reg().unwrap();
                let rhs = matched.rhs.try_as_imm().copied().unwrap();
                let dest = *matched.dest.try_as_reg().unwrap();
                let cc = CC::Gt;
                smallvec![
                    MInstr::Machine(Instr::CMP32ri { lhs, rhs }),
                    MInstr::Machine(Instr::SETCC { dest, cc })
                ]
            }
            Self::CondJmp => {
                let matched = matched.try_as_cond_br().unwrap();
                let cond = *matched.cond.try_as_reg().unwrap();
                let true_target = matched.true_target;
                let false_target = matched.false_target;
                let cc = CC::Eq;
                let cmp_instr = match cond.size(function) {
                    Size::Byte => Instr::CMP8ri {
                        lhs: cond,
                        rhs: Immediate::from(0u8),
                    },
                    Size::Word => unimplemented!(),
                    Size::DWord => Instr::CMP32ri {
                        lhs: cond,
                        rhs: Immediate::from(0u32),
                    },
                    Size::QWord => unimplemented!(),
                };
                smallvec![
                    MInstr::Machine(cmp_instr),
                    MInstr::Machine(Instr::JCC {
                        cc,
                        target: false_target,
                    }),
                    MInstr::Machine(Instr::JMP {
                        target: true_target,
                    })
                ]
            }
        }
    }
}

#[derive(Default)]
pub struct Backend {}

impl machine::Backend for Backend {
    type TM = Target;
    type P = Pattern;

    fn patterns() -> &'static [Self::P] {
        Self::P::VARIANTS
    }
    fn mov(dest: PhysicalRegister, src: PhysicalRegister) -> Instr {
        let dest_size = dest.size();
        assert_eq!(dest_size, src.size());
        let dest = Register::Physical(dest);
        let src = Register::Physical(src);
        match dest_size {
            Size::Byte => Instr::MOV8rr { dest, src },
            Size::Word => Instr::MOV16rr { dest, src },
            Size::DWord => Instr::MOV32rr { dest, src },
            Size::QWord => Instr::MOV64rr { dest, src },
        }
    }

    fn mov_imm(dest: PhysicalRegister, imm: Immediate) -> Instr {
        let dest = Register::Physical(dest);
        match imm.size {
            Size::Byte => Instr::MOV8ri {
                dest,
                immediate: imm,
            },
            Size::Word => Instr::MOV16ri {
                dest,
                immediate: imm,
            },
            Size::DWord => Instr::MOV32ri {
                dest,
                immediate: imm,
            },
            Size::QWord => Instr::MOV64ri {
                dest,
                immediate: imm,
            },
        }
    }

    fn ret() -> Instr {
        Instr::RET
    }

    fn new() -> Self {
        Self {}
    }
}
