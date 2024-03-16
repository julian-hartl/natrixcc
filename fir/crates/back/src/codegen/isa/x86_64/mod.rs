use smallvec::{SmallVec, smallvec};
use strum::VariantArray;

use firc_middle::instruction::CmpOp;
use firc_middle::ty::Type;

use crate::codegen::machine;
use crate::codegen::machine::{BasicBlockId, Instr, InstrOperand, MatchedPattern, PatternInOperand, PatternInOutput, PseudoInstr, Size};
use crate::codegen::machine::abi::calling_convention::Slot;
use crate::codegen::machine::abi::CallingConvention;
use crate::codegen::selection_dag::Immediate;

mod asm;

pub type Register = machine::Register<Abi>;

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
    SETCC {
        dest: Register,
        cc: CmpOp,
    },
    JMP {
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
    RetVoid,
    RetOp,
    Jmp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, IntoStaticStr, VariantArray, Hash)]
pub enum PhysicalRegister {
    AL,
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
            Self::AL |
            Self::EAX | Self::ECX | Self::EDX | Self::EBX | Self::ESP | Self::EBP | Self::ESI | Self::EDI | Self::R8D | Self::R9D => true,
            Self::EFLAGS => false,
        }
    }

    fn size(&self) -> Size {
        match self {
            Self::AL => Size::Byte,
            Self::EAX | Self::ECX | Self::EDX | Self::EBX | Self::ESP | Self::EBP | Self::ESI | Self::EDI | Self::R8D | Self::R9D | Self::EFLAGS => Size::DWord,
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
            Self::SETCC { dest, .. } => Some(*dest)
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
            Self::SETCC { .. } => smallvec![Register::Physical(PhysicalRegister::EFLAGS)]
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
            Self::SETCC { dest, .. } => smallvec![
                InstrOperand::Reg(*dest)
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
            Self::SETCC { dest, .. } => smallvec![dest],
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
            Self::SETCC { .. } => smallvec![]
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
            Self::RetVoid => {
                machine::PatternIn::Ret(None)
            }
            Self::RetOp => {
                machine::PatternIn::Ret(Some(PatternInOperand::Reg(Size::DWord)))
            }
            Self::Cmp32rreq => machine::PatternIn::Cmp(
                PatternInOutput::Reg(Size::Byte),
                CmpOp::Eq,
                PatternInOperand::Reg(Size::DWord),
                PatternInOperand::Reg(Size::DWord),
            )
        }
    }

    fn into_instr(self, in_: MatchedPattern<Self::ABI>) -> SmallVec<[Instr<Self::ABI>; 2]> {
        match self {
            Self::Mov32rr => {
                let pattern = in_.try_as_mov().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let src = pattern.src.try_as_reg().unwrap().clone();
                smallvec![
                    Instr::Machine(X86Instr::MOV32rr {
                        dest,
                        src
                    })
                ]
            }
            Self::Mov32ri => {
                let pattern = in_.try_as_mov().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let immediate = pattern.src.try_as_imm().cloned().unwrap();
                smallvec![
                    Instr::Machine(X86Instr::MOV32ri {
                        dest,
                        immediate ,
                    })
                ]
            }
            Self::Sub32ri => {
                let pattern = in_.try_as_sub().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let lhs = pattern.lhs.try_as_reg().unwrap().clone();
                let rhs = pattern.rhs.try_as_imm().cloned().unwrap();
                smallvec![
                    Instr::Pseudo(PseudoInstr::Copy(dest.clone(), lhs)),
                    Instr::Machine(X86Instr::SUB32ri {
                        dest,
                        immediate: rhs,
                    })
                ]
            }
            Self::Sub32rr => {
                let pattern = in_.try_as_sub().unwrap();
                let dest = pattern.dest.try_as_reg().unwrap().clone();
                let lhs = pattern.lhs.try_as_reg().unwrap().clone();
                let rhs = pattern.rhs.try_as_reg().unwrap().clone();
                smallvec![
                    Instr::Machine(X86Instr::MOV32rr {
                        dest: dest.clone(),
                        src: lhs,
                    }),
                    Instr::Machine(X86Instr::SUB32rr {
                        dest,
                        src: rhs,
                    })
                ]
            }
            Self::Add32ri => {
                let pattern = in_.try_as_add().unwrap();
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
                let pattern = in_.try_as_add().unwrap();
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
            Self::RetVoid => {
                smallvec![
                    Instr::Machine(X86Instr::RET)
                ]
            }
            Self::RetOp => {
                let value = *in_.try_as_ret().cloned().unwrap().value.unwrap().try_as_reg().unwrap();
                smallvec![
                    Instr::Pseudo(PseudoInstr::Ret(InstrOperand::Reg(value)))
                ]
            }
            Self::Jmp => {
                let target = in_.try_as_br().unwrap().target;
                smallvec![
                    Instr::Machine(X86Instr::JMP {
                        target
                    })
                ]
            }
            Self::Cmp32rreq => {
                let matched = in_.try_as_cmp().unwrap();
                let lhs = *matched.lhs.try_as_reg().unwrap();
                let rhs = *matched.rhs.try_as_reg().unwrap();
                let dest = *matched.dest.try_as_reg().unwrap();
                let cc = matched.cmp_op;
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
