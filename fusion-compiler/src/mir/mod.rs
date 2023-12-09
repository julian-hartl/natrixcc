use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};

#[allow(unused)]
pub use builder::MIRBuilder;
use fusion_compiler::{bug, idx, Idx, IdxVec};
#[allow(unused)]
pub use writer::MIRWriter;

use crate::{ast, compilation_unit};

mod builder;
mod writer;
pub mod optimizations;


#[derive(Debug, Copy, Clone)]
pub enum Type {
    Bool,
    Int,
    Void,
}

impl From<compilation_unit::Type> for Type {
    fn from(value: crate::typings::Type) -> Self {
        match value {
            compilation_unit::Type::Bool => Self::Bool,
            compilation_unit::Type::Int => Self::Int,
            compilation_unit::Type::Void => Self::Void,
            compilation_unit::Type::Unresolved | compilation_unit::Type::Error => bug!("Unresolved type")
        }
    }
}

#[derive(Debug)]
pub struct MIR {
    pub functions: IdxVec<FunctionIdx, Function>,
}


impl MIR {
    pub fn new(
        functions: IdxVec<FunctionIdx, Function>,
    ) -> Self {
        Self {
            functions,
        }
    }
}

idx!(FunctionIdx);

pub type Instructions = IdxVec<InstructionIdx, Instruction>;
pub type BasicBlocks = IdxVec<BasicBlockIdx, Option<BasicBlock>>;

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub return_type: Type,
    pub parameters: Vec<Type>,
    pub basic_blocks: BasicBlocks,
    pub instructions: Instructions,
}

impl Function {
    pub fn new_basic_block(&mut self) -> BasicBlockIdx {
        self.basic_blocks.push_with_index(|idx| Some(BasicBlock::new(idx)))
    }
}


idx!(BasicBlockIdx);

impl Display for BasicBlockIdx {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{}", self.as_index())
    }
}


#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub instructions: Vec<InstructionIdx>,
    terminator: Terminator,
    pub idx: BasicBlockIdx,
}

impl BasicBlock {
    pub fn new(
        idx: BasicBlockIdx,
    ) -> Self {
        Self {
            instructions: vec![],
            terminator: Terminator::new(TerminatorKind::Unresolved),
            idx
        }
    }

    pub fn has_instructions(&self) -> bool {
        !self.instructions.is_empty()
    }

    pub fn is_terminated(&self) -> bool {
        match self.terminator.kind {
            TerminatorKind::Unresolved => false,
            _ => true,
        }
    }

    pub fn set_terminator(&mut self, kind: TerminatorKind) {
        tracing::debug!("Setting terminator of {:?} to {:?}",self.idx , kind);
        self.terminator = Terminator::new(kind);
    }

    /// Sets the terminator if it is not already set.
    pub fn maybe_set_terminator(&mut self,  kind: TerminatorKind) {
        if !self.is_terminated() {
            self.set_terminator(kind);
        }
    }


    /// Appends `other` to `self`.
    ///
    /// 1. Adds the arguments of `other` to `self`
    /// 2. Replaces all references to the arguments of `other` with the arguments of `self`
    /// 3. Appends the instructions of `other` to `self`
    /// 4. Replaces `self.terminator` with `other.terminator`
    pub fn append(&mut self, other: Self) {
        self.instructions.extend(other.instructions);
        self.terminator = other.terminator;
    }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Value {
    InstructionRef(InstructionIdx),
    ConstantInt(i64),
    Void,
}

impl Value {
    pub fn is_constant(&self) -> bool {
        matches!(self, Self::ConstantInt(_) | Self::Void)
    }

    pub fn as_instruction_ref(&self) -> Option<InstructionIdx> {
        match self {
            Self::InstructionRef(idx) => Some(*idx),
            _ => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Self::ConstantInt(value) => Some(*value),
            _ => None,
        }
    }

    /// Replaces the value if it is not equal to the new value.
    ///
    /// Returns true if the value was replaced.
    pub fn replace_if_not_equal(&mut self, value: Value) -> bool {
        if value != *self {
            *self = value;
            true
        } else {
            false
        }
    }

    pub fn replace_with_new_reference_from_copies(&mut self, copies: &HashMap<InstructionIdx, InstructionIdx>) -> bool {
        match self {
            Self::InstructionRef(idx) => {
                if let Some(new_reference) = copies.get(idx) {
                    *self = Self::InstructionRef(*new_reference);
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn replace_with_new_reference(&mut self, old: InstructionIdx, new: InstructionIdx) -> bool {
        match self {
            Self::InstructionRef(idx) => {
                if *idx == old {
                    *self = Self::InstructionRef(new);
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}


idx!(InstructionIdx);

#[derive(Debug)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub ty: Type,
}

impl Instruction {
    pub fn new(kind: InstructionKind, ty: Type) -> Self {
        Self {
            kind,
            ty,
        }
    }

    pub fn is_pure(&self) -> bool {
        match &self.kind {
            InstructionKind::Binary { .. } => true,
            InstructionKind::Unary { .. } => true,
            InstructionKind::Value(_) => true,
            InstructionKind::Call { .. } => false,
            InstructionKind::Phi { .. } => false,
        }
    }

    pub fn update_refs(&mut self, old_idx: InstructionIdx, new_idx: InstructionIdx) {
        match &mut self.kind {
            InstructionKind::Binary { lhs, rhs, .. } => {
                lhs.replace_with_new_reference(old_idx, new_idx);
                rhs.replace_with_new_reference(old_idx, new_idx);
            }
            InstructionKind::Unary { operand, .. } => {
                operand.replace_with_new_reference(old_idx, new_idx);
            }
            InstructionKind::Value(value) => {
                value.replace_with_new_reference(old_idx, new_idx);
            }
            InstructionKind::Call { arguments, .. } => {
                for arg in arguments.iter_mut() {
                    arg.replace_with_new_reference(old_idx, new_idx);
                }
            }
            InstructionKind::Phi(phi) => {
                for index in phi.operands.iter_mut() {
                    if *index == old_idx {
                        *index = new_idx;
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum InstructionKind {
    Binary {
        operator: Binop,
        lhs: Value,
        rhs: Value,
    },
    Unary {
        operator: Unop,
        operand: Value,
    },
    Value(Value),
    Call {
        function_idx: FunctionIdx,
        arguments: Vec<Value>,
    },
    Phi(PhiNode),
}

impl InstructionKind {
    pub fn as_phi(&self) -> Option<&PhiNode> {
        match self {
            Self::Phi(phi) => Some(phi),
            _ => None,
        }
    }

    pub fn as_phi_mut(&mut self) -> Option<&mut PhiNode> {
        match self {
            Self::Phi(phi) => Some(phi),
            _ => None,
        }
    }
}

type Operands = Vec<InstructionIdx>;

#[derive(Debug, Clone)]
pub struct PhiNode {
    pub operands: Operands,
}

impl PhiNode {
    pub fn operandless() -> Self {
        Self {
            operands: Operands::new(),
        }
    }
}

impl Deref for PhiNode {
    type Target = Operands;

    fn deref(&self) -> &Self::Target {
        &self.operands
    }
}

impl DerefMut for PhiNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.operands
    }
}

#[derive(Debug)]
pub enum Unop {
    Neg,
    Not,
}

impl From<ast::UnOpKind> for Unop {
    fn from(value: ast::UnOpKind) -> Self {
        match value {
            ast::UnOpKind::Minus => Self::Neg,
            ast::UnOpKind::BitwiseNot => Self::Not,
        }
    }
}

impl Display for Unop {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Neg => write!(f, "neg"),
            Self::Not => write!(f, "not"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
}

impl From<ast::BinOpKind> for Binop {
    fn from(value: ast::BinOpKind) -> Self {
        match value {
            ast::BinOpKind::Plus => Self::Add,
            ast::BinOpKind::Minus => Self::Sub,
            ast::BinOpKind::Multiply => Self::Mul,
            ast::BinOpKind::Divide => Self::Div,
            ast::BinOpKind::Power => unimplemented!(),
            ast::BinOpKind::Modulo => Self::Mod,
            ast::BinOpKind::BitwiseAnd => Self::And,
            ast::BinOpKind::BitwiseOr => Self::Or,
            ast::BinOpKind::BitwiseXor => Self::Xor,
            ast::BinOpKind::Equals => Self::Eq,
            ast::BinOpKind::NotEquals => Self::Neq,
            ast::BinOpKind::LessThan => Self::Lt,
            ast::BinOpKind::LessThanOrEqual => Self::Leq,
            ast::BinOpKind::GreaterThan => Self::Gt,
            ast::BinOpKind::GreaterThanOrEqual => Self::Geq,
        }
    }
}

impl Display for Binop {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::Mul => write!(f, "mul"),
            Self::Div => write!(f, "div"),
            Self::Mod => write!(f, "mod"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::Shl => write!(f, "shl"),
            Self::Shr => write!(f, "shr"),
            Self::Eq => write!(f, "eq"),
            Self::Neq => write!(f, "neq"),
            Self::Lt => write!(f, "lt"),
            Self::Leq => write!(f, "leq"),
            Self::Gt => write!(f, "gt"),
            Self::Geq => write!(f, "geq"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Terminator {
    pub kind: TerminatorKind,
}

impl Terminator {
    pub fn new(kind: TerminatorKind) -> Self {
        Self {
            kind,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TerminatorKind {
    Return {
        value: Value,
    },
    Jump(BasicBlockIdx),
    SwitchInt {
        value: Value,
        cases: Vec<(i32, BasicBlockIdx)>,
        default: BasicBlockIdx,
    },
    Unresolved,
    /// Marks a basic block as unreachable.
    ///
    /// This is for example used for an unresolved break statement.
    Unreachable,
}

