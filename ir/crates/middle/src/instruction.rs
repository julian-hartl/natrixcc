use std::fmt::{
    Display,
    Formatter,
};

use smallvec::{
    smallvec,
    SmallVec,
};
use strum_macros::{
    Display,
    EnumTryAs,
};

use crate::{
    cfg::{
        BasicBlockRef,
        Cfg,
        InstrRef,
    },
    Type,
    Value,
};

/// An instruction in a basic block.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Instr {
    pub id: InstrRef,
    pub defined_in: BasicBlockRef,
    pub ty: Type,
    pub kind: InstrKind,
    /// Uniqye symbol for debugging purposes.
    pub symbol: String,
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.symbol)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum InstrIdentifyingKey {
    Sub(Op, Op),
}

impl Instr {
    pub fn new(ty: Type, kind: InstrKind, bb: BasicBlockRef, id: InstrRef, symbol: String) -> Self {
        Self {
            id,
            defined_in: bb,
            ty,
            kind,
            symbol,
        }
    }

    pub fn identifying_key(&self) -> Option<InstrIdentifyingKey> {
        match &self.kind {
            InstrKind::Sub(instr) => Some(InstrIdentifyingKey::Sub(
                instr.lhs.clone(),
                instr.rhs.clone(),
            )),
            _ => None,
        }
    }

    pub fn value(&self) -> Value {
        Value::Instr(self.id)
    }

    pub fn display<'a>(&'a self, cfg: &'a Cfg) -> InstrDisplay {
        InstrDisplay(cfg, self)
    }

    pub fn used(&self) -> SmallVec<[&Op; 2]> {
        match &self.kind {
            InstrKind::Alloca(_) => smallvec![],
            InstrKind::Op(op) => smallvec![&op.op],
            InstrKind::Sub(instr) | InstrKind::Add(instr) => smallvec![&instr.lhs, &instr.rhs],
            InstrKind::Load(instr) => smallvec![&instr.source],
            InstrKind::Store(instr) => smallvec![&instr.value],
            InstrKind::Cmp(instr) => smallvec![&instr.lhs, &instr.rhs],
        }
    }
}

pub struct InstrDisplay<'a>(&'a Cfg, &'a Instr);

impl Display for InstrDisplay<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let instr = self.1;
        write!(f, "{} {} = ", instr.ty, instr)?;
        match &instr.kind {
            InstrKind::Alloca(alloca_instr) => {
                write!(f, "alloca {}", alloca_instr.ty)?;
                if alloca_instr.num_elements > 1 {
                    write!(f, ", {}", alloca_instr.num_elements)?;
                }
            }
            InstrKind::Sub(sub_instr) => {
                write!(
                    f,
                    "sub {}, {}",
                    sub_instr.lhs.display(self.0),
                    sub_instr.rhs.display(self.0)
                )?;
            }
            InstrKind::Add(add_instr) => {
                write!(
                    f,
                    "add {}, {}",
                    add_instr.lhs.display(self.0),
                    add_instr.rhs.display(self.0)
                )?;
            }
            InstrKind::Op(op_instr) => {
                write!(f, "{}", op_instr.op.display(self.0))?;
            }
            InstrKind::Store(store_instr) => {
                write!(
                    f,
                    "store {}, {}",
                    store_instr.value.display(self.0),
                    store_instr.dest.display(self.0)
                )?;
            }
            InstrKind::Load(load_instr) => {
                write!(f, "load {}", load_instr.source.display(self.0))?;
            }
            InstrKind::Cmp(cmp_instr) => {
                write!(
                    f,
                    "cmp {} {}, {}",
                    cmp_instr.op,
                    cmp_instr.lhs.display(self.0),
                    cmp_instr.rhs.display(self.0)
                )?;
            }
        };
        Ok(())
    }
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
    pub num_elements: u32,
    pub ty: Type,
}

impl AllocaInstr {
    pub fn new(ty: Type, num_elements: Option<u32>) -> Self {
        Self {
            num_elements: num_elements.unwrap_or(1),
            ty,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StoreInstr {
    pub dest: Value,
    pub value: Op,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LoadInstr {
    pub source: Op,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BinOpInstr {
    pub lhs: Op,
    pub rhs: Op,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OpInstr {
    pub op: Op,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, EnumTryAs)]
pub enum Op {
    Const(Const),
    Value(Value),
}

impl From<Value> for Op {
    fn from(value: Value) -> Self {
        Self::Value(value)
    }
}

impl Op {
    pub fn referenced_value(&self) -> Option<Value> {
        match self {
            Op::Const(_) => None,
            Op::Value(value) => Some(*value),
        }
    }

    pub fn display<'cfg>(&self, cfg: &'cfg Cfg) -> OpDisplay<'cfg, '_> {
        OpDisplay { cfg, op: self }
    }
}

pub struct OpDisplay<'cfg, 'op> {
    cfg: &'cfg Cfg,
    op: &'op Op,
}

impl Display for OpDisplay<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.op {
            Op::Const(c) => write!(f, "{}{}", c, c.ty()),
            Op::Value(l) => write!(f, "{}", l.display(self.cfg)),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Const {
    Int(Type, i64),
}

impl Const {
    pub fn cmp(self, other: Const, op: CmpOp) -> Option<Self> {
        match (self, other) {
            (Self::Int(lty, lhs), Self::Int(rty, rhs)) => {
                assert_eq!(lty, rty, "comparison of different types");
                let res = match op {
                    CmpOp::Eq => (lhs == rhs) as i64,
                    CmpOp::Gt => (lhs > rhs) as i64,
                };
                Some(Self::Int(Type::Bool, res))
            }
        }
    }

    pub fn sub(self, other: Const) -> Option<Self> {
        match (self, other) {
            (Self::Int(lty, lhs), Self::Int(rty, rhs)) => {
                assert_eq!(lty, rty, "subtraction of different types");
                let res = lhs.checked_sub(rhs)?;
                // todo: check if result has overflown
                Some(Self::Int(lty, res))
            }
        }
    }

    pub fn add(self, other: Const) -> Option<Self> {
        match (self, other) {
            (Self::Int(lty, lhs), Self::Int(rty, rhs)) => {
                assert_eq!(lty, rty, "addition of different types");
                let res = lhs.checked_add(rhs)?;
                Some(Self::Int(lty, res))
            }
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            Const::Int(ty, _) => ty.clone(),
        }
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(_, value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CmpInstr {
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
        use crate::{
            cfg,
            cfg::{
                RetTerm,
                TerminatorKind,
            },
            instruction::{
                Const,
                Op,
            },
            test::create_test_function,
            ty::Type,
        };

        #[test]
        fn test_sub_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb("".into());
            let instr_ref = cfg_builder.sub(
                "v0".into(),
                Type::I8,
                Op::Const(Const::Int(Type::I8, 0)),
                Op::Const(Const::Int(Type::I8, 1)),
            );
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            assert_eq!(function.cfg.instructions[instr_ref].ty, Type::I8);
        }

        #[test]
        fn test_alloca_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb("".into());
            let alloca_instr_ref = cfg_builder.alloca("v0".into(), Type::I8, None);
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            assert_eq!(
                function.cfg.instructions[alloca_instr_ref].ty,
                Type::Ptr(Box::new(Type::I8))
            );
        }

        #[test]
        fn test_op_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb("".into());
            let op_instr_ref =
                cfg_builder.op("v0".into(), Type::I8, Op::Const(Const::Int(Type::I8, 0)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            assert_eq!(function.cfg.instructions[op_instr_ref].ty, Type::I8);
        }

        #[test]
        fn test_store_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            let bb_ref = cfg_builder.start_bb("".into());
            let alloca_instr_ref = cfg_builder.alloca("v0".into(), Type::I8, None);
            cfg_builder.store(
                "v1".into(),
                alloca_instr_ref.into(),
                Op::Const(Const::Int(Type::I8, 0)),
            );
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        }

        #[test]
        fn test_load_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            let bb = cfg_builder.start_bb("".into());
            let alloca_value = cfg_builder.alloca("v0".into(), Type::I8, None);
            let instr = cfg_builder.load("v1".into(), Type::I8, Op::Value(alloca_value.into()));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            assert_eq!(function.cfg.instructions[instr].ty, Type::I8);
        }
    }
}
