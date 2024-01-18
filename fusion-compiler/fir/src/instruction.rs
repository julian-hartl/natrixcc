use std::any::Any;
use std::fmt::{Display, Formatter};
use std::ops::Sub;

use crate::function::FunctionData;
use crate::cfg::{BasicBlock, VarId};
use crate::ty::Type;

index_vec::define_index_type! {
    pub struct Instr = usize;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct InstrData {
    pub kind: InstrKind,
}

impl InstrData {
    pub fn new(kind: InstrKind) -> Self {
        Self { kind }
    }

    pub fn validate(&self, function: &FunctionData) -> Result<(), String> {
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
                let place_ty = &function.cfg.places[instr.place].ty;
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
                let place_ty = &function.cfg.places[instr.source].ty;
                match place_ty {
                    Type::Ptr(_) => {}
                    _ => return Err(format!("load instructions must load from pointers, found {}", place_ty)),
                }
            }
            InstrKind::Phi(_) => {}
        }
        Ok(())
    }

    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &FunctionData) -> std::fmt::Result {
        if let Some(place) = self.target_place() {
            write!(writer, "{} = ", function.cfg.places[place])?;
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
                write!(writer, ", {} ", function.cfg.places[store_instr.place].ty)?;
                write!(writer, "{}", function.cfg.places[store_instr.place])?;
            }
            InstrKind::Load(load_instr) => {
                write!(writer, "load {}, ", function.cfg.places[load_instr.place].ty)?;
                let source_place = &function.cfg.places[load_instr.source];
                write!(writer, "{} {}", source_place.ty, source_place)?;
            }
            InstrKind::Phi(phi_instr) => {
                write!(writer, "phi {}", function.cfg.places[phi_instr.place].ty)?;
                for (i, incoming) in phi_instr.incoming.iter().enumerate() {
                    if i > 0 {
                        write!(writer, ",")?;
                    }
                    write!(writer, " [ ")?;
                    incoming.op.write_to(writer, function)?;
                    write!(writer, ", {} ]", incoming.source)?;
                }
            }
        }
        writeln!(writer)?;
        Ok(())
    }

    pub fn target_place(&self) -> Option<Place> {
        match &self.kind {
            InstrKind::Alloca(instr) => Some(instr.place),
            InstrKind::Op(op) => Some(op.place),
            InstrKind::Sub(instr) => Some(instr.place),
            InstrKind::Load(instr) => Some(instr.place),
            InstrKind::Store(_) => None,
            InstrKind::Phi(instr) => Some(instr.place),
        }
    }
}

index_vec::define_index_type! {
    pub struct Place = usize;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PlaceData {
    pub id: VarId,
    pub ty: Type,
    pub kind: PlaceKind,
    /// Versions the [`PlaceData::id`].
    pub version: Option<usize>,
}

impl PlaceData {
    pub fn increment_version(&mut self) -> usize{
        let current_version = self.version.unwrap_or(0);
        self.version = Some(current_version + 1);
        current_version
    }
}

impl Display for PlaceData {
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
    Local(Instr)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum InstrKind {
    Alloca(AllocaInstr),
    Store(StoreInstr),
    Load(LoadInstr),
    Op(OpInstr),
    Sub(SubInstr),
    Phi(PhiInstr),
}


impl InstrKind {
    pub fn as_op_instr(&self) -> Option<&OpInstr> {
        match self {
            InstrKind::Op(op_instr) => Some(op_instr),
            _ => None,
        }
    }

    pub fn as_sub_instr(&self) -> Option<&SubInstr> {
        match self {
            InstrKind::Sub(sub_instr) => Some(sub_instr),
            _ => None,
        }
    }

    pub fn as_alloca_instr(&self) -> Option<&AllocaInstr> {
        match self {
            InstrKind::Alloca(alloca_instr) => Some(alloca_instr),
            _ => None,
        }
    }

    pub fn as_store_instr(&self) -> Option<&StoreInstr> {
        match self {
            InstrKind::Store(store_instr) => Some(store_instr),
            _ => None,
        }
    }

    pub fn as_load_instr(&self) -> Option<&LoadInstr> {
        match self {
            InstrKind::Load(load_instr) => Some(load_instr),
            _ => None,
        }
    }

    pub fn as_phi_instr(&self) -> Option<&PhiInstr> {
        match self {
            InstrKind::Phi(phi_instr) => Some(phi_instr),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct AllocaInstr {
    pub place: Place,
    pub ty: Type,
    pub num_elements: usize,
    pub alignment: usize,
}

impl AllocaInstr {
    pub fn new(place: Place, ty: Type, num_elements: Option<usize>, alignment: Option<usize>) -> Self {
        Self {
            place,
            alignment: alignment.unwrap_or(ty.alignment()),
            ty,
            num_elements: num_elements.unwrap_or(1),
        }
    }

    pub fn total_size(&self) -> usize {
        self.ty.size() * self.num_elements
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StoreInstr {
    pub place: Place,
    pub value: Op,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LoadInstr {
    pub place: Place,
    pub source: Place,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SubInstr {
    pub place: Place,
    pub lhs: Op,
    pub rhs: Op,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct OpInstr {
    pub place: Place,
    pub op: Op,
}

impl OpInstr {
    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &FunctionData) -> std::fmt::Result {
        write!(writer, "{}", self.op.ty(function))?;
        write!(writer, " ")?;
        self.op.write_to(writer, function)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Op {
    Const(Const),
    Place(Place),
}

impl Op {
    pub fn ty(&self, function: &FunctionData) -> Type {
        match self {
            Op::Const(c) => c.ty(),
            Op::Place(place) => function.cfg.places[*place].ty.clone()
        }
    }

    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &FunctionData) -> std::fmt::Result {
        match self {
            Op::Const(c) => write!(writer, "{}", c),
            Op::Place(l) => write!(writer, "{}", function.cfg.places[*l]),
        }
    }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Const {
    I8(i8),
    I32(i32),
}

impl Const {
    pub fn ty(&self) -> Type {
        match self {
            Const::I32(_) => Type::I32,
            Const::I8(_) => Type::I8,
        }
    }
}

impl Sub for Const {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Const::I8(l), Const::I8(r)) => Const::I8(l - r),
            (Const::I32(l), Const::I32(r)) => Const::I32(l - r),
            _ => panic!("invalid sub operands"),
        }
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::I8(i) => write!(f, "{}", i),
            Const::I32(i) => write!(f, "{}", i),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct PhiInstr {
    pub place: Place,
    pub incoming: Vec<Incoming>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Incoming {
    pub op: Op,
    pub source: BasicBlock,
}


#[cfg(test)]
mod tests {
    mod validate {
        use crate::cfg::{BasicBlockData, RetTerm, TerminatorKind};
        use crate::cfg_builder::CFGBuilder;
        use crate::create_test_function;
        use crate::cfg::VarId;
        use crate::instruction::{Const, Op};
        use crate::ty::Type;

        #[test]
        fn test_invalid_sub_instruction() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();
            let instr  = cfg_builder.sub(Some(VarId::Named("target".to_string())), Op::Const(Const::I8(0)), Op::Const(Const::I32(1)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert_eq!(instr, Err("sub instruction operands must have the same type, found i8 and i32".to_string()));
        }

        #[test]
        fn test_valid_sub_instructions() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();
            assert!(cfg_builder.sub(
                Some(VarId::Named("target.2".to_string())),
                Op::Const(Const::I8(0)),
                Op::Const(Const::I8(1)),
            ).is_ok());
            assert!(cfg_builder.sub(
                Some(VarId::Named("target.2".to_string())),
                Op::Const(Const::I32(0)),
                Op::Const(Const::I32(1)),
            ).is_ok());
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        }

        #[test]
        fn test_invalid_store_of_wrong_type() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();

            let alloca_instr = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            let alloca_instr = cfg_builder.func.cfg.instructions[alloca_instr].kind.as_alloca_instr().unwrap();
            let instr = cfg_builder.store(alloca_instr.place, Op::Const(Const::I32(0)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert_eq!(instr, Err("store instructions must store to pointers of the same type, found i8 and i32".to_string()));
        }

        #[test]
        fn test_invalid_store_to_non_pointer() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();

            let instr = cfg_builder.op(None, Op::Const(Const::I32(0))).unwrap();
            let place = cfg_builder.func.cfg.target_place(instr).unwrap();
            let instr = cfg_builder.store(place, Op::Const(Const::I32(0)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert_eq!(instr, Err("store instructions must store to pointers, found i32".to_string()));
        }

        #[test]
        fn test_valid_store_of_same_type() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();

            let alloca_instr = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            let alloca_instr = cfg_builder.func.cfg.instructions[alloca_instr].kind.as_alloca_instr().unwrap();
            let instr = cfg_builder.store(alloca_instr.place, Op::Const(Const::I8(0)));
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert!(instr.is_ok());
        }

        #[test]
        fn test_invalid_load_from_non_pointer() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();

            let instr = cfg_builder.op(None, Op::Const(Const::I32(0))).unwrap();
            let place = cfg_builder.func.cfg.target_place(instr).unwrap();
            let instr = cfg_builder.load(None, place);
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert_eq!(instr, Err("load instructions must load from pointers, found i32".to_string()));
        }

        #[test]
        fn test_valid_load_from_pointer() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();

            let alloca_instr = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            let alloca_instr = cfg_builder.func.cfg.instructions[alloca_instr].kind.as_alloca_instr().unwrap();
            let instr = cfg_builder.load(None, alloca_instr.place);
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));

            assert!(instr.is_ok());
        }
    }

    mod instruction_type {
        use crate::cfg::{BasicBlockData, RetTerm, TerminatorKind};
        use crate::cfg_builder::CFGBuilder;
        use crate::create_test_function;
        use crate::instruction::{Const, Op};
        use crate::ty::Type;

        #[test]
        fn test_sub_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();
            let instr = cfg_builder.sub(None, Op::Const(Const::I8(0)), Op::Const(Const::I8(1))).unwrap();
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.target_place_data(instr).unwrap().ty, Type::I8);
        }

        #[test]
        fn test_alloca_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();
            let instr = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.target_place_data(instr).unwrap().ty, Type::Ptr(Box::new(Type::I8)));
            assert_eq!(function.cfg.instructions[instr].kind.as_alloca_instr().unwrap().ty, Type::I8);
        }

        #[test]
        fn test_op_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();
            let instr = cfg_builder.op(None, Op::Const(Const::I8(0))).unwrap();
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.target_place_data(instr).unwrap().ty, Type::I8);
        }

        #[test]
        fn test_store_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();
            let alloca_instr = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            let alloca_instr = cfg_builder.func.cfg.instructions[alloca_instr].kind.as_alloca_instr().unwrap();
            let instr = cfg_builder.store(alloca_instr.place, Op::Const(Const::I8(0))).unwrap();
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.target_place_data(instr), None);
        }

        #[test]
        fn test_load_instruction_type() {
            let mut function = create_test_function();
            let mut cfg_builder = CFGBuilder::new(&mut function);
            cfg_builder.start_bb();
            let alloca_instr = cfg_builder.alloca(None, Type::I8, None, None).unwrap();
            let alloca_instr = cfg_builder.func.cfg.instructions[alloca_instr].kind.as_alloca_instr().unwrap();
            let instr = cfg_builder.load(None, alloca_instr.place).unwrap();
            cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
            drop(cfg_builder);
            assert_eq!(function.cfg.target_place_data(instr).unwrap().ty, Type::I8);
        }
    }
}
