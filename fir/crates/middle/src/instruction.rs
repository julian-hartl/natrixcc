use std::fmt::{Display, Formatter};
use std::ops::Sub;

use strum_macros::{Display, EnumTryAs};

use crate::{Type, VReg};
use crate::cfg::{BasicBlockId, Cfg};

/// An instruction in a basic block.
///
/// Currently, an instruction only holds a **single** field.
/// We do this as we will be adding more fields to the instruction in the future, such as metadata.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Instr {
    pub ty: Type,
    pub kind: InstrKind,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum InstrIdentifyingKey {
    Sub(Op, Op),
}

impl Instr {
    pub const fn new(ty: Type, kind: InstrKind) -> Self {
        Self { ty, kind }
    }

    pub fn identifying_key(&self) -> Option<InstrIdentifyingKey> {
        match &self.kind {
            InstrKind::Sub(instr) => Some(InstrIdentifyingKey::Sub(instr.lhs.clone(), instr.rhs.clone())),
            _ => None,
        }
    }

    pub fn display<'a>(&'a self, cfg: &'a Cfg) -> InstrDisplay {
        InstrDisplay(cfg, self)
    }

    pub const fn produced_value(&self) -> Option<VReg> {
        match &self.kind {
            InstrKind::Alloca(instr) => Some(instr.value),
            InstrKind::Op(op) => Some(op.value),
            InstrKind::Sub(instr) => Some(instr.value),
            InstrKind::Load(instr) => Some(instr.dest),
            InstrKind::Store(_) => None,
            InstrKind::Cmp(instr) => Some(instr.value),
            InstrKind::Add(instr) => Some(instr.value),
        }
    }
}

pub struct InstrDisplay<'a>(&'a Cfg, &'a Instr);

impl Display for InstrDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.1.kind {
            InstrKind::Alloca(instr) => {
                write!(f, "{} = alloca {}", instr.value, self.1.ty)?;
                if instr.num_elements > 1 {
                    write!(f, ", {}", instr.num_elements)?;
                }
            }
            InstrKind::Sub(instr) => {
                write!(f, "{} = sub {} {}, {}", instr.value, self.1.ty, instr.lhs, instr.rhs)?;
            }
            InstrKind::Add(instr) => {
                write!(f, "{} = add {} {}, {}", instr.value, self.1.ty, instr.lhs, instr.rhs)?;
            }
            InstrKind::Op(instr) => {
                write!(f, "{} = {} {}", instr.value, self.1.ty, instr.op)?;
            }
            InstrKind::Store(instr) => {
                write!(f, "store {} {}, ptr {}", self.1.ty, instr.value, instr.dest)?;
            }
            InstrKind::Load(instr) => {
                write!(f, "{} = load {} ptr {}", instr.dest, self.1.ty, instr.source)?;
            }
            InstrKind::Cmp(instr) => {
                write!(f, "{} = icmp {} {} {}, {}", instr.value, self.1.ty, instr.op, instr.lhs, instr.rhs)?;
            }
        };
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VRegData {
    pub ty: Type,
    pub defined_in: BasicBlockId,
}

#[derive(Debug, Clone, Eq, PartialEq, EnumTryAs)]
pub enum InstrKind {
    Alloca(AllocaInstr),
    Store(StoreInstr),
    Load(LoadInstr),
    Op(OpInstr),
    Sub(BinOpInstr),
    Add(BinOpInstr),
    Cmp(CmpInstr),
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AllocaInstr {
    pub value: VReg,
    pub num_elements: u32,
}

impl AllocaInstr {
    pub fn new(value: VReg, num_elements: Option<u32>) -> Self {
        Self {
            value,
            num_elements: num_elements.unwrap_or(1),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StoreInstr {
    pub dest: VReg,
    pub value: Op,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LoadInstr {
    pub dest: VReg,
    pub source: Op,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BinOpInstr {
    pub value: VReg,
    pub lhs: Op,
    pub rhs: Op,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OpInstr {
    pub value: VReg,
    pub op: Op,
}


#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Op {
    Const(Const),
    Value(VReg),
}

impl From<VReg> for Op {
    fn from(value: VReg) -> Self {
        Self::Value(value)
    }
}

impl Op {
    pub fn referenced_value(&self) -> Option<VReg> {
        match self {
            Op::Const(_) => None,
            Op::Value(value) => {
                Some(*value)
            }
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Const(c) => write!(f, "{}", c),
            Op::Value(l) => write!(f, "{}", *l),
        }
    }
}


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Const {
    Int(i64),
}

impl Const {
    pub fn sub(self, other: Const, ty: Type) -> Option<Self> {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => {
                let res = lhs.checked_sub(rhs)?;
                // todo: check if result has overflown
                Some(Self::Int(res))
            }
        }
    }

    pub fn add(self, other: Const, ty: Type) -> Option<Self> {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => {
                let res = lhs.checked_add(rhs)?;
                // todo: check if result has overflown
                Some(Self::Int(res))
            }
        }
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CmpInstr {
    pub value: VReg,
    pub op: CmpOp,
    pub lhs: Op,
    pub rhs: Op,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Display)]
pub enum CmpOp {
    #[strum(serialize = "eq")]
    Eq,
    #[strum(serialize = "gt")]
    Gt,
    // Ne,
    // Slt,
    // Sle,
    // Sgt,
    // Sge,
}


#[cfg(test)]
mod tests {
    mod instruction_type {
        use crate::cfg;
        use crate::cfg::{RetTerm, TerminatorKind};
        use crate::instruction::{Const, Op};
        use crate::test::create_test_function;
        use crate::ty::Type;

        #[test]
        fn test_sub_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            let vreg = cfg_builder.sub(Type::I8, Op::Const(Const::Int(0)), Op::Const(Const::Int(1)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.vreg_ty(vreg).clone(), Type::I8);
        }

        #[test]
        fn test_alloca_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            let alloca_value = cfg_builder.alloca(Type::I8, None);
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.vreg_ty(alloca_value).clone(), Type::Ptr(Box::new(Type::I8)));
        }

        #[test]
        fn test_op_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            let place = cfg_builder.op(Type::I8, Op::Const(Const::Int(0)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.vreg_ty(place).clone(), Type::I8);
        }

        #[test]
        fn test_store_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            let bb = cfg_builder.start_bb();
            let alloca_value = cfg_builder.alloca(Type::I8, None);
            cfg_builder.store(Type::I8, alloca_value, Op::Const(Const::Int(0)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.basic_block(bb).instructions.as_ref().unwrap()[1].produced_value(), None);
        }

        #[test]
        fn test_load_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            let bb = cfg_builder.start_bb();
            let alloca_value = cfg_builder.alloca(Type::I8, None);
            let instr = cfg_builder.load(Type::I8, alloca_value.into());
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.vreg_ty(instr).clone(), Type::I8);
        }
    }
}
