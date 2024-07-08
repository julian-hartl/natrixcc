use fusion_compiler::{idx, Idx, IdxVec};

use crate::mir;

pub(crate) mod builder;

pub type Places = IdxVec<PlaceIdx, Place>;

/// The lower intermediate representation.
#[derive(Debug)]
pub struct LIR {
    pub functions: IdxVec<FunctionIdx, Function>,
    pub basic_blocks: IdxVec<BasicBlockIdx, BasicBlock>,
    pub places: Places,
}

impl LIR {
    pub fn new() -> Self {
        Self {
            functions: IdxVec::new(),
            basic_blocks: IdxVec::new(),
            places: IdxVec::new(),
        }
    }
}

idx!(FunctionIdx);

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub parameters: Vec<PlaceIdx>,
    pub basic_blocks: Vec<BasicBlockIdx>,
}

#[derive(Debug, Default)]
pub struct BasicBlock {
    pub instructions: Vec<Instruction>,
    pub terminator: Option<Terminator>,
}

idx!(BasicBlockIdx);

#[derive(Debug)]
pub struct Instruction {
    pub kind: InstructionKind,
}

#[derive(Debug)]
pub enum InstructionKind {
    Add {
        target: PlaceIdx,
        lhs: Operand,
        rhs: Operand,
    },
    Sub {
        target: PlaceIdx,
        lhs: Operand,
        rhs: Operand,
    },
    Gt {
        target: PlaceIdx,
        lhs: Operand,
        rhs: Operand,
    },
    AllocInit {
        target: PlaceIdx,
        value: Operand,
    },
    // Move {
    //     target: PlaceIdx,
    //     source: Operand,
    // },
    AddrOf {
        target: PlaceIdx,
        source: PlaceIdx,
    },
}

#[derive(Debug)]
pub enum Terminator {
    Return { value: Option<Operand> },
    Jump { target: BasicBlockIdx },
}

#[derive(Debug)]
pub struct Operand {
    pub ty: Type,
    pub kind: OperandKind,
}

#[derive(Debug)]
pub enum OperandKind {
    Deref(PlaceIdx),
    Const(ConstOp),
}

#[derive(Debug)]
pub enum ConstOp {
    Int32(i32),
    // Int32(i32),
    // Int16(i16),
    // Int8(i8),
}

/// A place to store something.
#[derive(Debug)]
pub struct Place {
    pub idx: PlaceIdx,
    pub ty: Type,
}

idx!(PlaceIdx);
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Type {
    Int32,
    Int8,
}

impl Type {
    pub fn layout(&self) -> Layout {
        match self {
            Type::Int32 => Layout {
                size: 4,
                alignment: 4,
            },
            Type::Int8 => Layout {
                size: 1,
                alignment: 1,
            },
        }
    }
}
impl From<mir::Type> for Type {
    fn from(value: mir::Type) -> Self {
        match value {
            mir::Type::Int => Type::Int32,
            mir::Type::Bool => Type::Int8,
            mir::Type::Void => todo!(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Layout {
    pub size: usize,
    pub alignment: usize,
}
