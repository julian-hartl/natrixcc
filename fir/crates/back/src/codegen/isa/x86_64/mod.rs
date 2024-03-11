use iced_x86::code_asm::CodeAssembler;
use smallvec::{SmallVec, smallvec};
use strum::VariantArray;

use firc_middle::ty::Type;

use crate::codegen::machine;
use crate::codegen::machine::{Instr, InstrOperand, InstrInsMut, MatchedPattern, PatternInOperand, PatternInOutput, PseudoInstr, Size, BasicBlockId};
use crate::codegen::machine::abi::calling_convention::ParameterSlot;
use crate::codegen::machine::abi::CallingConvention;
use crate::codegen::selection_dag::{DWord, Immediate, MachineOp};

mod asm;

pub type Register = machine::Register<Abi>;

#[derive(Debug, Clone, PartialEq, Eq, EnumTryAs, IntoStaticStr)]
pub enum X86Instr {
    SUB32RI {
        dest: Register,
        immediate: DWord,
    },
    SUB32RR {
        dest: Register,
        src: Register,
    },
    ADD32RI {
        dest: Register,
        immediate: DWord,
    },
    ADD32RR {
        dest: Register,
        src: Register,
    },
    MOV32RI {
        dest: Register,
        immediate: DWord,
    },
    MOV32RR {
        dest: Register,
        src: Register,
    },
    JMP {
        target: BasicBlockId,
    },
    RET,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, VariantArray)]
pub enum Pattern {
    MOV32RI,
    MOV32RR,
    SUB32RI,
    SUB32RR,
    ADD32RI,
    ADD32RR,
    RET_VOID,
    RET_OP,
    JMP,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoStaticStr, VariantArray, Hash)]
pub enum PhysicalRegister {
    EAX,
    ECX,
    EDX,
    EBX,
    ESP,
    EBP,
    ESI,
    EDI,
    R8D,
    R9D,
}

impl machine::PhysicalRegister for PhysicalRegister {
    fn name(&self) -> &'static str {
        self.into()
    }

    fn all() -> &'static [Self] {
        Self::VARIANTS
    }

    fn is_gp(&self) -> bool {
        match self {
            Self::EAX | Self::ECX | Self::EDX | Self::EBX | Self::ESP | Self::EBP | Self::ESI | Self::EDI | Self::R8D | Self::R9D => true,
        }
    }

    fn size(&self) -> Size {
        match self {
            Self::EAX | Self::ECX | Self::EDX | Self::EBX | Self::ESP | Self::EBP | Self::ESI | Self::EDI | Self::R8D | Self::R9D => Size::DWord,
        }
    }
}


impl machine::MachineInstr for X86Instr {
    type Abi = Abi;

    fn name(&self) -> &'static str {
        self.into()
    }

    fn out(&self) -> Option<Register> {
        match self {
            Self::MOV32RI { dest, .. } => Some(*dest),
            Self::MOV32RR { dest, .. } => Some(*dest),
            Self::SUB32RI { dest, .. } => Some(*dest),
            Self::SUB32RR { dest, .. } => Some(*dest),
            Self::ADD32RI { dest, .. } => Some(*dest),
            Self::ADD32RR { dest, .. } => Some(*dest),
            Self::JMP { .. } => None,
            Self::RET => None,
        }
    }

    fn out_mut(&mut self) -> Option<&mut machine::Register<Self::Abi>> {
        match self {
            Self::MOV32RI { dest, .. } => Some(dest),
            Self::MOV32RR { dest, .. } => Some(dest),
            Self::SUB32RI { dest, .. } => Some(dest),
            Self::SUB32RR { dest, .. } => Some(dest),
            Self::ADD32RI { dest, .. } => Some(dest),
            Self::ADD32RR { dest, .. } => Some(dest),
            Self::JMP { .. } => None,
            Self::RET => None,
        }
    }

    fn ins(&self) -> SmallVec<[InstrOperand<Self::Abi>; 2]> {
        match self {
            Self::MOV32RI { immediate, .. } => smallvec![InstrOperand::Imm(Immediate::DWord(immediate.clone()))],
            Self::MOV32RR { src, .. } => smallvec![InstrOperand::Reg(*src)],
            Self::SUB32RI { immediate, .. } => smallvec![InstrOperand::Imm(Immediate::DWord(immediate.clone()))],
            Self::SUB32RR { src, .. } => smallvec![InstrOperand::Reg(*src)],
            Self::ADD32RI { immediate, .. } => smallvec![InstrOperand::Imm(Immediate::DWord(immediate.clone()))],
            Self::ADD32RR { src, .. } => smallvec![InstrOperand::Reg(*src)],
            Self::JMP { target} => smallvec![InstrOperand::Label(*target)],
            Self::RET => smallvec![],
        }
    }

    fn ins_mut(&mut self) -> SmallVec<[InstrInsMut<'_, Self::Abi>; 2]> {
        match self {
            Self::MOV32RI { .. } => smallvec![],
            Self::MOV32RR { src, .. } => smallvec![InstrInsMut::Reg(src)],
            Self::SUB32RI { .. } => smallvec![],
            Self::SUB32RR { src, .. } => smallvec![InstrInsMut::Reg(src)],
            Self::ADD32RI { .. } => smallvec![],
            Self::ADD32RR { src, .. } => smallvec![InstrInsMut::Reg(src)],
            Self::JMP { .. } => smallvec![],
            Self::RET => smallvec![],
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Abi {}

impl machine::Abi for Abi {
    type I = X86Instr;
    type ASSEMBLER = asm::Assembler;

    type REG = PhysicalRegister;
    type CallingConvention = SystemV;
}

pub struct SystemV;

impl CallingConvention for SystemV {
    type Reg = PhysicalRegister;

    fn parameter_slots(params: impl Iterator<Item=Type>) -> impl Iterator<Item=ParameterSlot<Self::Reg>> {
        let mut used_regs = 0;
        params.map(move |ty| {
            let slot = if used_regs < 6 {
                let reg = match (used_regs, Size::from_ty(&ty)) {
                    (0, Size::DWord) => PhysicalRegister::EDI,
                    (1, Size::DWord) => PhysicalRegister::ESI,
                    (2, Size::DWord) => PhysicalRegister::EDX,
                    (3, Size::DWord) => PhysicalRegister::ECX,
                    (4, Size::DWord) => PhysicalRegister::R8D,
                    (5, Size::DWord) => PhysicalRegister::R9D,
                    _ => panic!("Too many parameters"),
                };
                used_regs += 1;
                ParameterSlot::Register(reg)
            } else {
                ParameterSlot::Stack
            };
            slot
        })
    }
}

impl machine::Pattern for Pattern {
    type ABI = Abi;

    fn in_(&self) -> machine::PatternIn {
        match self {
            Self::MOV32RI =>
                machine::PatternIn::Mov(PatternInOutput::Reg(Size::DWord), PatternInOperand::Imm(Size::DWord)),
            Self::MOV32RR => machine::PatternIn::Mov(PatternInOutput::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord)),
            Self::SUB32RI => machine::PatternIn::Sub(PatternInOutput::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord), PatternInOperand::Imm(Size::DWord)),
            Self::SUB32RR => machine::PatternIn::Sub(PatternInOutput::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord)),
            Self::ADD32RI => machine::PatternIn::Add(PatternInOutput::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord), PatternInOperand::Imm(Size::DWord)),
            Self::ADD32RR => machine::PatternIn::Add(PatternInOutput::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord)),
            Self::JMP => machine::PatternIn::Br,
            Self::RET_VOID => {
                machine::PatternIn::Ret(None)
            }
            Self::RET_OP => {
                machine::PatternIn::Ret(Some(PatternInOperand::Reg(Size::DWord)))
            }
        }
    }

    fn into_instr(self, in_: MatchedPattern<Self::ABI>) -> SmallVec<[Instr<Self::ABI>; 2]> {
        match self {
            Self::MOV32RR => {
                let pattern = in_.try_as_mov().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let src = pattern.src.try_as_reg().unwrap().clone();
                smallvec![
                    Instr::Machine(X86Instr::MOV32RR {
                        dest,
                        src
                    })
                ]
            }
            Self::MOV32RI => {
                let pattern = in_.try_as_mov().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let immediate = pattern.src.try_as_imm().cloned().unwrap().try_as_d_word().unwrap();
                smallvec![
                    Instr::Machine(X86Instr::MOV32RI {
                        dest,
                        immediate ,
                    })
                ]
            }
            Self::SUB32RI => {
                let pattern = in_.try_as_sub().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let lhs = pattern.lhs.try_as_reg().unwrap().clone();
                let rhs = pattern.rhs.try_as_imm().cloned().unwrap().try_as_d_word().unwrap();
                smallvec![
                    Instr::Pseudo(PseudoInstr::Copy(dest.clone(), lhs)),
                    Instr::Machine(X86Instr::SUB32RI {
                        dest,
                        immediate: rhs,
                    })
                ]
            }
            Self::SUB32RR => {
                let pattern = in_.try_as_sub().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let lhs = pattern.lhs.try_as_reg().unwrap().clone();
                let rhs = pattern.rhs.try_as_reg().unwrap().clone();
                smallvec![
                    Instr::Machine(X86Instr::MOV32RR {
                        dest: dest.clone(),
                        src: lhs,
                    }),
                    Instr::Machine(X86Instr::SUB32RR {
                        dest,
                        src: rhs,
                    })
                ]
            }
            Self::ADD32RI => {
                let pattern = in_.try_as_add().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let lhs = pattern.lhs.try_as_reg().unwrap().clone();
                let rhs = pattern.rhs.try_as_imm().cloned().unwrap().try_as_d_word().unwrap();
                smallvec![
                    Instr::Pseudo(PseudoInstr::Copy(dest.clone(), lhs)),
                    Instr::Machine(X86Instr::ADD32RI {
                        dest,
                        immediate: rhs,
                    })
                ]
            }
            Self::ADD32RR => {
                let pattern = in_.try_as_add().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let lhs = pattern.lhs.try_as_reg().unwrap().clone();
                let rhs = pattern.rhs.try_as_reg().unwrap().clone();
                smallvec![
                    Instr::Machine(X86Instr::MOV32RR {
                        dest: dest.clone(),
                        src: lhs,
                    }),
                    Instr::Machine(X86Instr::ADD32RR {
                        dest,
                        src: rhs,
                    })
                ]
            }
            Self::RET_VOID => {
                smallvec![
                    Instr::Machine(X86Instr::RET)
                ]
            }
            Self::RET_OP => {
                let value = *in_.try_as_ret().cloned().unwrap().value.unwrap().try_as_reg().unwrap();
                smallvec![
                    Instr::Machine(X86Instr::MOV32RR {
                        dest: Register::Physical(PhysicalRegister::EAX),
                        src: value,
                    }),
                    Instr::Machine(X86Instr::RET)
                ]
            }
            Self::JMP => {
                let target = in_.try_as_br().unwrap().target;
                smallvec![
                    Instr::Machine(X86Instr::JMP {
                        target
                    })
                ]
            }
        }
    }
}

#[derive(Default)]
pub struct Backend {}


impl machine::Backend for Backend {
    type ABI = Abi;
    type P = Pattern;

    fn patterns() -> &'static [Self::P] {
        Self::P::VARIANTS
    }

    fn expand_pseudo_instruction(instr: &PseudoInstr<Self::ABI>) -> SmallVec<[Instr<Self::ABI>; 2]> {
        match instr {
            PseudoInstr::Copy(dest, src) => {
                smallvec![
                    Instr::Machine(X86Instr::MOV32RR {
                        dest: *dest,
                        src: *src,
                    })
                ]
            }
        }
    }

    fn new() -> Self {
        Self {}
    }
}
