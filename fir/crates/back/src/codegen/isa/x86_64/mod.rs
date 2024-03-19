use smallvec::{SmallVec, smallvec};
use strum::VariantArray;

use firc_middle::instruction::CmpOp;
use firc_middle::ty::Type;

use crate::codegen::machine;
use crate::codegen::machine::{BasicBlockId, Function, Instr, InstrOperand, MatchedPattern, PatternInOperand, PatternInOutput, PseudoInstr, Size};
use crate::codegen::machine::abi::calling_convention::Slot;
use crate::codegen::machine::abi::CallingConvention;
use crate::codegen::selection_dag::Immediate;

mod asm;

pub type Register = machine::Register<Abi>;

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
            _ => unimplemented!()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, EnumTryAs, IntoStaticStr)]
pub enum X86Instr {
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
    MOV32ri {
        dest: Register,
        immediate: Immediate,
    },
    MOV32rr {
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
    Mov32ri,
    Mov32rr,
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

impl machine::PhysicalRegister for PhysicalRegister {
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
            Self::AL | Self::AH | Self::BL | Self::BH | Self::CL | Self::CH | Self::DL | Self::DH | Self::SIL | Self::DIL | Self::R8L | Self::R9L => Size::Byte,
            Self::AX | Self::BX | Self::CX | Self::DX | Self::SI | Self::DI | Self::R8W | Self::R9W => Size::Word,
            Self::EAX | Self::ECX | Self::EDX | Self::EBX | Self::ESI | Self::EDI | Self::R8D | Self::R9D | Self::EFLAGS => Size::DWord,
            Self::RAX | Self::RBX | Self::RCX | Self::RDX | Self::R8 | Self::R9 | Self::RSI | Self::RDI => Size::QWord,
        }
    }

    fn subregs(&self) -> Option<&'static [Self]> {
        match self {
            Self::AL |
            Self::AH => None,
            Self::AX => Some(&[Self::AL, Self::AH]),
            Self::EAX => Some(&[Self::AX, Self::AL, Self::AH]),
            Self::RAX => Some(&[Self::EAX, Self::AX, Self::AL, Self::AH]),
            Self::BL |
            Self::BH => None,
            Self::BX => Some(&[Self::BL, Self::BH]),
            Self::EBX => Some(&[Self::BX, Self::BL, Self::BH]),
            Self::RBX => Some(&[Self::EBX, Self::BX, Self::BL, Self::BH]),
            Self::CL |
            Self::CH => None,
            Self::CX => Some(&[Self::CL, Self::CH]),
            Self::ECX => Some(&[Self::CX, Self::CL, Self::CH]),
            Self::RCX => Some(&[Self::ECX, Self::CX, Self::CL, Self::CH]),
            Self::DL |
            Self::DH => None,
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
}

impl machine::MachineInstr for X86Instr {
    type Abi = Abi;

    fn name(&self) -> &'static str {
        self.into()
    }

    fn writes(&self) -> Option<Register> {
        match self {
            Self::MOV32ri { dest, .. } => Some(*dest),
            Self::MOV32rr { dest, .. } => Some(*dest),
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

    fn reads(&self) -> SmallVec<[machine::Register<Self::Abi>; 2]> {
        match self {
            Self::MOV32ri { .. } => smallvec![],
            Self::MOV32rr { src, .. } => smallvec![*src],
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

    fn operands(&self) -> SmallVec<[InstrOperand<Self::Abi>; 3]> {
        match self {
            Self::MOV32ri { dest, immediate } => smallvec![
                InstrOperand::Reg(*dest),
                InstrOperand::Imm(*immediate)
            ],
            Self::MOV32rr { dest, src } => smallvec![
                InstrOperand::Reg(*dest),
                InstrOperand::Reg(*src)
            ],
            Self::SUB32ri { dest, immediate } => smallvec![
                InstrOperand::Reg(*dest),
                InstrOperand::Imm(*immediate)
            ],
            Self::SUB32rr { dest, src } => smallvec![
                InstrOperand::Reg(*dest),
                InstrOperand::Reg(*src)
            ],
            Self::ADD32ri { dest, immediate } => smallvec![
                InstrOperand::Reg(*dest),
                InstrOperand::Imm(*immediate)
            ],
            Self::ADD32rr { dest, src } => smallvec![
                InstrOperand::Reg(*dest),
                InstrOperand::Reg(*src)
            ],
            Self::JMP { target } => smallvec![
                InstrOperand::Label(*target)
            ],
            Self::RET => smallvec![],
            Self::CMP32rr { lhs, rhs } => smallvec![
                InstrOperand::Reg(*lhs),
                InstrOperand::Reg(*rhs)
            ],
            Self::CMP32ri { lhs, .. } => smallvec![
                InstrOperand::Reg(*lhs),
            ],
            Self::CMP8ri { lhs, .. } => smallvec![
                InstrOperand::Reg(*lhs),
            ],
            Self::SETCC { dest, .. } => smallvec![
                InstrOperand::Reg(*dest)
            ],
            Self::JCC { target, .. } => smallvec![
                InstrOperand::Label(*target),
            ]
        }
    }
    fn written_regs_mut(&mut self) -> SmallVec<[&mut machine::Register<Self::Abi>; 1]> {
        match self {
            Self::MOV32ri { dest, .. } => smallvec![dest],
            Self::MOV32rr { dest, .. } => smallvec![dest],
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

    fn read_regs_mut(&mut self) -> SmallVec<[&mut machine::Register<Self::Abi>; 2]> {
        match self {
            Self::MOV32ri { .. } => smallvec![],
            Self::MOV32rr { src, .. } => smallvec![src],
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

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
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

    fn parameter_slots(params: impl Iterator<Item=Type>) -> impl Iterator<Item=Slot<Self::Reg>> {
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
                Slot::Register(reg)
            } else {
                Slot::Stack
            };
            slot
        })
    }

    fn return_slot(size: Size) -> Slot<Self::Reg> {
        match size {
            Size::DWord => Slot::Register(PhysicalRegister::EAX),
            _ => unimplemented!()
        }
    }
}

impl machine::Pattern for Pattern {
    type ABI = Abi;

    fn in_(&self) -> machine::PatternIn {
        match self {
            Self::Mov32ri =>
                machine::PatternIn::Mov(PatternInOutput::Reg(Size::DWord), PatternInOperand::Imm(Size::DWord)),
            Self::Mov32rr => machine::PatternIn::Mov(PatternInOutput::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord)),
            Self::Sub32ri => machine::PatternIn::Sub(PatternInOutput::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord), PatternInOperand::Imm(Size::DWord)),
            Self::Sub32rr => machine::PatternIn::Sub(PatternInOutput::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord)),
            Self::Add32ri => machine::PatternIn::Add(PatternInOutput::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord), PatternInOperand::Imm(Size::DWord)),
            Self::Add32rr => machine::PatternIn::Add(PatternInOutput::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord), PatternInOperand::Reg(Size::DWord)),
            Self::Jmp => machine::PatternIn::Br,
            Self::Cmp32rreq => machine::PatternIn::Cmp(
                PatternInOutput::Reg(Size::Byte),
                CmpOp::Eq,
                PatternInOperand::Reg(Size::DWord),
                PatternInOperand::Reg(Size::DWord),
            ),
            Self::Cmp32rigt => machine::PatternIn::Cmp(
                PatternInOutput::Reg(Size::Byte),
                CmpOp::Gt,
                PatternInOperand::Reg(Size::DWord),
                PatternInOperand::Imm(Size::DWord),
            ),
            Self::CondJmp => machine::PatternIn::CondBr(
                PatternInOperand::Reg(Size::Byte),
            )
        }
    }

    fn into_instr(self, function: &mut Function<Self::ABI>, matched: MatchedPattern<Self::ABI>) -> SmallVec<[Instr<Self::ABI>; 2]> {
        match self {
            Self::Mov32rr => {
                let pattern = matched.try_as_mov().unwrap();
                let dest = *pattern.dest.try_as_reg().unwrap();
                let src = *pattern.src.try_as_reg().unwrap();
                smallvec![
                    Instr::Pseudo(PseudoInstr::Copy (dest, src))
                ]
            }
            Self::Mov32ri => {
                let pattern = matched.try_as_mov().unwrap();
                let dest = *pattern.dest.try_as_reg().unwrap();
                let immediate = pattern.src.try_as_imm().copied().unwrap();
                smallvec![
                    Instr::Machine(X86Instr::MOV32ri {
                        dest,
                        immediate ,
                    })
                ]
            }
            Self::Sub32ri => {
                let pattern = matched.try_as_sub().unwrap();
                let dest = *pattern.dest.try_as_reg().unwrap();
                let lhs = *pattern.lhs.try_as_reg().unwrap();
                let rhs = pattern.rhs.try_as_imm().copied().unwrap();
                smallvec![
                    Instr::Pseudo(PseudoInstr::Copy(dest, lhs)),
                    Instr::Machine(X86Instr::SUB32ri {
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
                    Instr::Machine(X86Instr::MOV32rr {
                        dest,
                        src: lhs,
                    }),
                    Instr::Machine(X86Instr::SUB32rr {
                        dest,
                        src: rhs,
                    })
                ]
            }
            Self::Add32ri => {
                let pattern = matched.try_as_add().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let lhs = pattern.lhs.try_as_reg().unwrap().clone();
                let rhs = pattern.rhs.try_as_imm().cloned().unwrap();
                smallvec![
                    Instr::Pseudo(PseudoInstr::Copy(dest.clone(), lhs)),
                    Instr::Machine(X86Instr::ADD32ri {
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
                    Instr::Machine(X86Instr::MOV32rr {
                        dest: dest.clone(),
                        src: lhs,
                    }),
                    Instr::Machine(X86Instr::ADD32rr {
                        dest,
                        src: rhs,
                    })
                ]
            }

            Self::Jmp => {
                let target = matched.try_as_br().unwrap().target;
                smallvec![
                    Instr::Machine(X86Instr::JMP {
                        target
                    })
                ]
            }
            Self::Cmp32rreq => {
                let matched = matched.try_as_cmp().unwrap();
                let lhs = *matched.lhs.try_as_reg().unwrap();
                let rhs = *matched.rhs.try_as_reg().unwrap();
                let dest = *matched.dest.try_as_reg().unwrap();
                let cc = matched.cmp_op.into();
                smallvec![
                    Instr::Machine(
                        X86Instr::CMP32rr {
                            lhs,
                            rhs,
                        }
                    ),
                    Instr::Machine(
                        X86Instr::SETCC {
                            dest,
                            cc,
                        }
                    )
                ]
            }
            Self::Cmp32rigt => {
                let matched = matched.try_as_cmp().unwrap();
                let lhs = *matched.lhs.try_as_reg().unwrap();
                let rhs = matched.rhs.try_as_imm().copied().unwrap();
                let dest = *matched.dest.try_as_reg().unwrap();
                let cc = CC::Gt;
                smallvec![
                    Instr::Machine(
                        X86Instr::CMP32ri {
                            lhs,
                            rhs,
                        }
                    ),
                    Instr::Machine(
                        X86Instr::SETCC {
                            dest,
                            cc,
                        }
                    )
                ]
            }
            Self::CondJmp => {
                let matched = matched.try_as_cond_br().unwrap();
                let cond = *matched.cond.try_as_reg().unwrap();
                let true_target = matched.true_target;
                let false_target = matched.false_target;
                let cc = CC::Eq;
                let cmp_instr = match cond.size(function) {
                    Size::Byte => X86Instr::CMP8ri {
                        lhs: cond,
                        rhs: Immediate::Byte(0u8.into()),
                    },
                    Size::Word => unimplemented!(),
                    Size::DWord => X86Instr::CMP32ri {
                        lhs: cond,
                        rhs: Immediate::DWord(0.into()),
                    },
                    Size::QWord => unimplemented!(),
                };
                smallvec![
                    Instr::Machine(
                        cmp_instr
                    ),
                    Instr::Machine(
                        X86Instr::JCC {
                            cc,
                            target: false_target,
                        }
                    ),
                    Instr::Machine(
                        X86Instr::JMP {
                            target: true_target,
                        }
                    )
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
    fn mov(dest: machine::Register<Self::ABI>, src: machine::Register<Self::ABI>) -> X86Instr {
        X86Instr::MOV32rr {
            dest,
            src,
        }
    }

    fn mov_imm(dest: machine::Register<Self::ABI>, imm: Immediate) -> X86Instr {
        assert_eq!(imm.size(), Size::DWord);
        X86Instr::MOV32ri {
            dest,
            immediate: imm,
        }
    }

    fn ret() -> X86Instr {
        X86Instr::RET
    }

    fn new() -> Self {
        Self {}
    }
}
