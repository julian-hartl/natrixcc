use std::ops::Sub;
use std::fmt::{Display, Formatter};
use crate::middle::cfg::{BasicBlockId, ValueId};
use crate::middle::function::Function;
use crate::middle::Value;
use crate::ty::Type;
index_vec::define_index_type! {
    pub struct InstrId = usize;
}


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

    pub fn validate(&self, function: &Function) -> Result<(), String> {
        match &self.kind {
            InstrKind::Alloca(instr) => {
                if instr.num_elements == 0 {
                    return Err("alloca instruction must have at least one element".to_string());
                }
                if instr.alignment % instr.ty.alignment() != 0 {
                    return Err("alloca instruction alignment must be a multiple of the type's alignment".to_string());
                }
            }
            InstrKind::Sub(instr) => {
                let valid_types = [Type::I8, Type::I32];
                if !valid_types.contains(&instr.lhs.ty(function)) || !valid_types.contains(&instr.rhs.ty(function)) {
                    return Err(format!("sub instruction operands must be one of {:?}, found {} and {}", valid_types, instr.lhs.ty(function), instr.rhs.ty(function)));
                }
                if instr.lhs.ty(function) != instr.rhs.ty(function) {
                    return Err(format!("sub instruction operands must have the same type, found {} and {}", instr.lhs.ty(function), instr.rhs.ty(function)));
                }
            }
            InstrKind::Op(_) => {}
            InstrKind::Store(instr) => {
                let place_ty = &function.cfg.values_ctx[instr.pointer].ty;
                match place_ty {
                    Type::Ptr(inner_ty) => {
                        if inner_ty.as_ref() != &instr.value.ty(function) {
                            return Err(format!("store instructions must store to pointers of the same type, found {} and {}", inner_ty, instr.value.ty(function)));
                        }
                    }
                    _ => return Err(format!("store instructions must store to pointers, found {}", place_ty)),
                }
            }
            InstrKind::Load(instr) => {
                let place_ty = &function.cfg.values_ctx[instr.source].ty;
                match place_ty {
                    Type::Ptr(_) => {}
                    _ => return Err(format!("load instructions must load from pointers, found {}", place_ty)),
                }
            }
            InstrKind::Phi(_) => {}
            InstrKind::ICmp(_) => {}
        }
        Ok(())
    }

    pub fn identifying_key(&self) -> Option<InstrIdentifyingKey> {
        match &self.kind {
            InstrKind::Sub(instr) => Some(InstrIdentifyingKey::Sub(instr.lhs.clone(), instr.rhs.clone())),
            _ => None,
        }
    }

    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &Function) -> std::fmt::Result {
        if let Some(place) = self.produced_value() {
            write!(writer, "{} = ", function.cfg.values_ctx[place])?;
        }
        match &self.kind {
            InstrKind::Alloca(instr) => {
                write!(writer, "alloca {}", instr.ty)?;
                if instr.num_elements > 1 {
                    write!(writer, ", {}", instr.num_elements)?;
                }
                if instr.alignment != instr.ty.alignment() {
                    write!(writer, ", align {}", instr.alignment)?;
                }
            }
            InstrKind::Sub(instr) => {
                write!(writer, "sub {} ", instr.lhs.ty(function))?;
                instr.lhs.write_to(writer, function)?;
                write!(writer, ", ")?;
                instr.rhs.write_to(writer, function)?;
            }
            InstrKind::Op(op_instr) => {
                op_instr.write_to(writer, function)?;
            }
            InstrKind::Store(store_instr) => {
                write!(writer, "store {} ", store_instr.value.ty(function))?;
                store_instr.value.write_to(writer, function)?;
                write!(writer, ", {} ", function.cfg.values_ctx[store_instr.pointer].ty)?;
                write!(writer, "{}", function.cfg.values_ctx[store_instr.pointer])?;
            }
            InstrKind::Load(load_instr) => {
                write!(writer, "load {}, ", function.cfg.values_ctx[load_instr.value].ty)?;
                let source_place = &function.cfg.values_ctx[load_instr.source];
                write!(writer, "{} {}", source_place.ty, source_place)?;
            }
            InstrKind::Phi(phi_instr) => {
                write!(writer, "phi {}", function.cfg.values_ctx[phi_instr.value].ty)?;
                for (i, incoming) in phi_instr.incoming.iter().enumerate() {
                    if i > 0 {
                        write!(writer, ",")?;
                    }
                    write!(writer, " [ ")?;
                    incoming.op.write_to(writer, function)?;
                    write!(writer, ", {} ]", incoming.source)?;
                }
            }
            InstrKind::ICmp(instr) => {
                write!(writer, "icmp {} {} ", instr.condition, instr.op1.ty(function))?;
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
            InstrKind::Load(instr) => Some(instr.value),
            InstrKind::Store(_) => None,
            InstrKind::Phi(instr) => Some(instr.value),
            InstrKind::ICmp(instr) => Some(instr.value),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ValueData {
    pub id: ValueId,
    pub ty: Type,
    pub defined_in: BasicBlockId,
    /// Versions the [`ValueData::id`].
    pub version: Option<usize>,
}

impl ValueData {
    pub fn increment_version(&mut self) -> usize {
        let current_version = self.version.unwrap_or(0);
        self.version = Some(current_version + 1);
        current_version
    }
}

impl Display for ValueData {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        if let Some(version) = self.version {
            write!(f, ".{}", version)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PlaceKind {
    Local(InstrId)
}

#[derive(Debug, Clone, Eq, PartialEq, EnumTryAs)]
pub enum InstrKind {
    Alloca(AllocaInstr),
    Store(StoreInstr),
    Load(LoadInstr),
    Op(OpInstr),
    Sub(SubInstr),
    Phi(PhiInstr),
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
    pub pointer: Value,
    pub value: Op,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LoadInstr {
    pub value: Value,
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

impl OpInstr {
    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &Function) -> std::fmt::Result {
        write!(writer, "{}", self.op.ty(function))?;
        write!(writer, " ")?;
        self.op.write_to(writer, function)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Op {
    Const(Const),
    Value(Value),
}

impl Op {
    pub fn ty(&self, function: &Function) -> Type {
        match self {
            Op::Const(c) => c.ty(),
            Op::Value(place) => function.cfg.values_ctx[*place].ty.clone()
        }
    }

    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &Function) -> std::fmt::Result {
        match self {
            Op::Const(c) => write!(writer, "{}", c),
            Op::Value(l) => write!(writer, "{}", function.cfg.values_ctx[*l]),
        }
    }
}


#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Const {
    Int {
        value: i64,
        width: u32,
        signed: bool,
    }
}

impl Const {
    pub const fn i1(value: bool) -> Self {
        Self::Int {
            value: if value { 1 } else { 0 },
            width: 1,
            signed: true,
        }
    }

    pub fn i8(value: i8) -> Self {
        Self::Int {
            value: value as i64,
            width: 8,
            signed: true,
        }
    }

    pub fn i16(value: i16) -> Self {
        Self::Int {
            value: value as i64,
            width: 16,
            signed: true,
        }
    }

    pub fn i32(value: i32) -> Self {
        Self::Int {
            value: value as i64,
            width: 32,
            signed: true,
        }
    }

    pub fn i64(value: i64) -> Self {
        Self::Int {
            value,
            width: 64,
            signed: true,
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            Self::Int {
                value: _,
                width,
                signed,
            } => Type::Int {
                width: *width,
                signed: *signed,
            },
        }
    }
}

impl Sub for Const {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Self::Int {
                value: lhs,
                width,
                signed,
            }, Self::Int {
                value: rhs,
                ..
            }) => Self::Int {
                // todo: check for overflow and respect signed or not
                value: lhs - rhs,
                width,
                signed,
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
pub struct PhiInstr {
    pub value: Value,
    pub incoming: Vec<Incoming>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Incoming {
    pub op: Op,
    pub source: BasicBlockId,
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
    mod validate {
        use crate::middle::cfg;
        use crate::middle::cfg::ValueId;
        use crate::middle::cfg::{RetTerm, TerminatorKind};
        use crate::middle::instruction::{Const, Op};
        use crate::test_utils::create_test_function;
        use crate::ty::Type;

        #[test]
        fn test_invalid_sub_instruction() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            let instr = cfg_builder.sub(Some(ValueId::Named("target".to_string())), Op::Const(Const::i8(0)), Op::Const(Const::i32(1)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert_eq!(instr, Err("sub instruction operands must have the same type, found i8 and i32".to_string()));
        }

        #[test]
        fn test_valid_sub_instructions() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            assert!(cfg_builder.sub(
                Some(ValueId::Named("target.2".to_string())),
                Op::Const(Const::i8(0)),
                Op::Const(Const::i8(1)),
            ).is_ok());
            assert!(cfg_builder.sub(
                Some(ValueId::Named("target.2".to_string())),
                Op::Const(Const::i32(0)),
                Op::Const(Const::i32(1)),
            ).is_ok());
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        }

        #[test]
        fn test_invalid_store_of_wrong_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();

            let (alloca_value, _alloca_instr) = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            let instr = cfg_builder.store(alloca_value, Op::Const(Const::i32(0)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert_eq!(instr, Err("store instructions must store to pointers of the same type, found i8 and i32".to_string()));
        }

        #[test]
        fn test_invalid_store_to_non_pointer() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();

            let (place, _) = cfg_builder.op(None, Op::Const(Const::i32(0))).unwrap();
            let instr = cfg_builder.store(place, Op::Const(Const::i32(0)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert_eq!(instr, Err("store instructions must store to pointers, found i32".to_string()));
        }

        #[test]
        fn test_valid_store_of_same_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();

            let (alloca_value, _) = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            let instr = cfg_builder.store(alloca_value, Op::Const(Const::i8(0)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert!(instr.is_ok());
        }

        #[test]
        fn test_invalid_load_from_non_pointer() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();

            let (place, _)= cfg_builder.op(None, Op::Const(Const::i32(0))).unwrap();
            let instr = cfg_builder.load(None, place);
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert_eq!(instr, Err("load instructions must load from pointers, found i32".to_string()));
        }

        #[test]
        fn test_valid_load_from_pointer() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();

             let (alloca_value, _) = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            let instr = cfg_builder.load(None, alloca_value);
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert!(instr.is_ok());
        }
    }

    mod instruction_type {
        use crate::middle::cfg;
        use crate::middle::cfg::{RetTerm, TerminatorKind};
        use crate::middle::instruction::{Const, Op};
        use crate::test_utils::create_test_function;
        use crate::ty::Type;

        #[test]
        fn test_sub_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            let (value, _) = cfg_builder.sub(None, Op::Const(Const::i8(0)), Op::Const(Const::i8(1))).unwrap();
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.values_ctx[value].ty, Type::I8);
        }

        #[test]
        fn test_alloca_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            let (alloca_value, _) = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.values_ctx[alloca_value].ty, Type::Ptr(Box::new(Type::I8)));
        }

        #[test]
        fn test_op_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            let (place, _) = cfg_builder.op(None, Op::Const(Const::i8(0))).unwrap();
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.values_ctx[place].ty, Type::I8);
        }

        #[test]
        fn test_store_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            let (alloca_value, _) = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            let instr = cfg_builder.store(alloca_value, Op::Const(Const::i8(0))).unwrap();
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.target_place_data(instr), None);
        }

        #[test]
        fn test_load_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = cfg::Builder::new(&mut function);
            cfg_builder.start_bb();
            let (alloca_value, _) = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            let instr = cfg_builder.load(None, alloca_value).unwrap();
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.target_place_data(instr).unwrap().ty, Type::I8);
        }
    }
}
