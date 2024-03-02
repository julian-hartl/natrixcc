use std::fmt::{Display, Formatter};
use std::ops::Sub;

use strum_macros::{Display, EnumTryAs};

use crate::{Function, Type, Value};
use crate::cfg::BasicBlockId;

/// An instruction in a basic block.
///
/// Currently, an instruction only holds a **single** field.
/// We do this as we will be adding more fields to the instruction in the future, such as metadata.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Instr {
    pub kind: InstrKind,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum InstrIdentifyingKey {
    Sub(Op, Op),
}

impl Instr {
    pub const fn new(kind: InstrKind) -> Self {
        Self { kind }
    }

    pub fn identifying_key(&self) -> Option<InstrIdentifyingKey> {
        match &self.kind {
            InstrKind::Sub(instr) => Some(InstrIdentifyingKey::Sub(instr.lhs.clone(), instr.rhs.clone())),
            _ => None,
        }
    }

    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &Function) -> std::fmt::Result {
        match &self.kind {
            InstrKind::Alloca(instr) => {
                write!(writer, "{} = alloca {}", instr.value, instr.ty)?;
                if instr.num_elements > 1 {
                    write!(writer, ", {}", instr.num_elements)?;
                }
                if instr.alignment != instr.ty.alignment() {
                    write!(writer, ", align {}", instr.alignment)?;
                }
            }
            InstrKind::Sub(instr) => {
                let ty = function.get_value_type(instr.value);
                write!(writer, "{} = sub {} ", instr.value, ty)?;
                instr.lhs.write_to(writer, function)?;
                write!(writer, ", ")?;
                instr.rhs.write_to(writer, function)?;
            }
            InstrKind::Op(instr) => {
                let ty = function.get_value_type(instr.value);
                write!(writer, "{} = {}", instr.value, ty)?;
                write!(writer, " ")?;
                instr.op.write_to(writer, function)?;
            }
            InstrKind::Store(instr) => {
                write!(writer, "store {} ", instr.value.ty(function))?;
                instr.value.write_to(writer, function)?;
                write!(writer, ", ptr {}", instr.dest)?;
            }
            InstrKind::Load(instr) => {
                write!(writer, "{} = load {} ", instr.dest, function.get_value_type(instr.dest))?;
                write!(writer, "ptr {}", instr.source)?;
            }
            InstrKind::ICmp(instr) => {
                let dest_ty = function.get_value_type(instr.value);
                write!(writer, "{} = icmp {} {} ", instr.value, dest_ty, instr.condition)?;
                instr.op1.write_to(writer, function)?;
                write!(writer, ", ")?;
                instr.op2.write_to(writer, function)?;
            }
        }
        writeln!(writer)?;
        Ok(())
    }

    pub const fn produced_value(&self) -> Option<Value> {
        match &self.kind {
            InstrKind::Alloca(instr) => Some(instr.value),
            InstrKind::Op(op) => Some(op.value),
            InstrKind::Sub(instr) => Some(instr.value),
            InstrKind::Load(instr) => Some(instr.dest),
            InstrKind::Store(_) => None,
            InstrKind::ICmp(instr) => Some(instr.value),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ValueData {
    pub id: Value,
    pub ty: Type,
    pub defined_in: BasicBlockId,
}

#[derive(Debug, Clone, Eq, PartialEq, EnumTryAs)]
pub enum InstrKind {
    Alloca(AllocaInstr),
    Store(StoreInstr),
    Load(LoadInstr),
    Op(OpInstr),
    Sub(SubInstr),
    ICmp(ICmpInstr),
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AllocaInstr {
    pub value: Value,
    pub ty: Type,
    pub num_elements: u32,
    pub alignment: u32,
}

impl AllocaInstr {
    pub fn new(value: Value, ty: Type, num_elements: Option<u32>, alignment: Option<u32>) -> Self {
        Self {
            value,
            alignment: alignment.unwrap_or(ty.alignment()),
            ty,
            num_elements: num_elements.unwrap_or(1),
        }
    }

    pub fn total_size(&self) -> u32 {
        self.ty.size() * self.num_elements
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StoreInstr {
    pub dest: Value,
    pub value: Op,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LoadInstr {
    pub dest: Value,
    pub source: Value,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SubInstr {
    pub value: Value,
    pub lhs: Op,
    pub rhs: Op,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OpInstr {
    pub value: Value,
    pub op: Op,
}


#[derive(Debug, Clone, Eq, PartialEq, Hash)]
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
    pub fn ty(&self, function: &Function) -> Type {
        match self {
            Op::Const(c) => c.ty(),
            Op::Value(place) => function.values_ctx[*place].ty.clone()
        }
    }

    pub fn referenced_value(&self) -> Option<Value> {
        match self {
            Op::Const(_) => None,
            Op::Value(value) => {
                Some(*value)
            }
        }
    }

    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &Function) -> std::fmt::Result {
        match self {
            Op::Const(c) => write!(writer, "{}", c),
            Op::Value(l) => write!(writer, "{}", *l),
        }
    }
}


#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Const {
    Int {
        value: i64,
        ty: Type,
    }
}

impl Const {
    pub const fn bool(value: bool) -> Self {
        Self::Int {
            value: if value { 1 } else { 0 },
            ty: Type::Bool,
        }
    }

    pub fn i8(value: i8) -> Self {
        Self::Int {
            value: value as i64,
            ty: Type::I8,
        }
    }

    pub fn i16(value: i16) -> Self {
        Self::Int {
            value: value as i64,
            ty: Type::I16,
        }
    }

    pub fn i32(value: i32) -> Self {
        Self::Int {
            value: value as i64,
            ty: Type::I32,
        }
    }

    pub fn i64(value: i64) -> Self {
        Self::Int {
            value,
            ty: Type::I64,
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            Self::Int { ty, .. } => ty.clone(),
        }
    }

    pub fn can_be_substituted_by(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int { value: lhs, .. }, Self::Int { value: rhs, .. }) => lhs == rhs
        }
    }
}

impl Sub for Const {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Int {
                value: lhs,
                ty,
            }, Self::Int {
                value: rhs,
                ..
            }) => Self::Int {
                // todo: check for overflow and respect signed or not
                value: lhs - rhs,
                ty,
            },
            _ => panic!("invalid sub operands"),
        }
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int {
                value,
                ..
            } => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ICmpInstr {
    pub value: Value,
    pub condition: ICmpCond,
    pub op1: Op,
    pub op2: Op,
}

#[derive(Debug, Clone, Eq, PartialEq, Display)]
pub enum ICmpCond {
    #[strum(serialize = "eq")]
    Eq,
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
            let value = cfg_builder.sub(None, Op::Const(Const::i8(0)), Op::Const(Const::i8(1)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.values_ctx[value].ty, Type::I8);
        }

        #[test]
        fn test_alloca_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            let alloca_value = cfg_builder.alloca(None, Type::I8, None, None);
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.values_ctx[alloca_value].ty, Type::Ptr(Box::new(Type::I8)));
        }

        #[test]
        fn test_op_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            let place = cfg_builder.op(None, Op::Const(Const::i8(0)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.values_ctx[place].ty, Type::I8);
        }

        #[test]
        fn test_store_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            let bb = cfg_builder.start_bb();
            let alloca_value = cfg_builder.alloca(None, Type::I8, None, None);
            cfg_builder.store(alloca_value, Op::Const(Const::i8(0)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.basic_block(bb).instructions[1].produced_value(), None);
        }

        #[test]
        fn test_load_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            let bb = cfg_builder.start_bb();
            let alloca_value = cfg_builder.alloca(None, Type::I8, None, None);
            let instr = cfg_builder.load(None, alloca_value);
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.values_ctx[instr].ty, Type::I8);
        }
    }
}
