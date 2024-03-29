use std::{
    fmt::{
        Display,
        Formatter,
    },
    io::Write,
};

use anyhow::Result;

use crate::{
    ast::{
        BinOpKind,
        BinOperator,
        UnOpKind,
        UnOperator,
    },
    typings::Type,
};

pub struct CAst {
    pub items: Vec<CItem>,
}

impl CAst {
    pub fn new(items: Vec<CItem>) -> Self {
        Self { items }
    }
}

#[derive(Debug, Clone)]
pub enum CType {
    Int,
    Bool,
    Void,
}

impl Display for CType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return match self {
            CType::Int => write!(f, "int"),
            CType::Bool => write!(f, "bool"),
            CType::Void => write!(f, "void"),
        };
    }
}

impl TryFrom<&Type> for CType {
    type Error = ();

    fn try_from(value: &Type) -> Result<Self, Self::Error> {
        return match value {
            Type::Int => Ok(CType::Int),
            Type::Bool => Ok(CType::Bool),
            Type::Void => Ok(CType::Void),
            Type::Unresolved => Err(()),
            Type::Error => Err(()),
        };
    }
}

#[derive(Debug)]
pub enum CItem {
    Macro(String, String),
    FunctionDecl(CFunctionDecl),
    FunctionImpl(CFunctionImpl),
    VarDecl(CVarDecl),
}

#[derive(Debug)]
pub struct CFunctionDecl {
    pub name: String,
    pub return_type: CType,
    pub parameters: Vec<CParameter>,
}

#[derive(Debug)]
pub struct CFunctionImpl {
    pub name: String,
    pub return_type: CType,
    pub parameters: Vec<CParameter>,
    pub body: Vec<CStmt>,
}

#[derive(Debug)]
pub struct CParameter {
    pub name: String,
    pub ty: CType,
}

#[derive(Debug, Clone)]
pub enum CStmt {
    Break,
    VarDecl(CVarDecl),
    Return(CReturn),
    If(CIfStmt),
    While(CWhile),
    Expr(CExpr),
    Block(CBlock),
}

#[derive(Debug, Clone)]
pub struct CBlock {
    pub statements: Vec<CStmt>,
}

#[derive(Debug, Clone)]
pub struct CVarDecl {
    pub name: String,
    pub ty: CType,
    pub initializer: Option<CExpr>,
}

#[derive(Debug, Clone)]
pub struct CAssignExpr {
    pub name: String,
    pub expr: Box<CExpr>,
}

#[derive(Debug, Clone)]
pub struct CReturn {
    pub expr: Option<CExpr>,
}

#[derive(Debug, Clone)]
pub struct CIfStmt {
    pub condition: CExpr,
    pub then_block: CBlock,
    pub else_block: Option<CBlock>,
}

#[derive(Debug, Clone)]
pub struct CWhile {
    pub condition: CExpr,
    pub body: CBlock,
}

#[derive(Debug, Clone)]
pub enum CExpr {
    Assign(CAssignExpr),
    Number(CNumber),
    Bool(CBool),
    Var(CVarExpr),
    Unary(CUnaryExpr),
    Binary(CBinaryExpr),
    Call(CCallExpr),
    Parenthesized(Box<CExpr>),
}

#[derive(Debug, Clone)]
pub struct CNumber {
    pub value: i64,
}

#[derive(Debug, Clone)]
pub struct CBool {
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct CVarExpr {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct CUnaryExpr {
    pub operator: CUnOperator,
    pub expr: Box<CExpr>,
}

#[derive(Debug, Clone)]
pub enum CUnOperator {
    Minus,
    BitwiseNot,
}

impl Display for CUnOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return match self {
            CUnOperator::Minus => write!(f, "-"),
            CUnOperator::BitwiseNot => write!(f, "~"),
        };
    }
}

impl TryFrom<&UnOperator> for CUnOperator {
    type Error = ();

    fn try_from(value: &UnOperator) -> Result<Self, Self::Error> {
        return match &value.kind {
            UnOpKind::Minus => Ok(CUnOperator::Minus),
            UnOpKind::BitwiseNot => Ok(CUnOperator::BitwiseNot),
        };
    }
}

#[derive(Debug, Clone)]
pub struct CBinaryExpr {
    pub operator: CBinOperator,
    pub left: Box<CExpr>,
    pub right: Box<CExpr>,
}

#[derive(Debug, Clone)]
pub enum CBinOperator {
    Plus,
    Minus,
    Multiply,
    Modulo,
    Divide,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

impl Display for CBinOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return match self {
            CBinOperator::Plus => write!(f, "+"),
            CBinOperator::Minus => write!(f, "-"),
            CBinOperator::Multiply => write!(f, "*"),
            CBinOperator::Divide => write!(f, "/"),
            CBinOperator::Equals => write!(f, "=="),
            CBinOperator::NotEquals => write!(f, "!="),
            CBinOperator::LessThan => write!(f, "<"),
            CBinOperator::GreaterThan => write!(f, ">"),
            CBinOperator::BitwiseAnd => write!(f, "&"),
            CBinOperator::BitwiseOr => write!(f, "|"),
            CBinOperator::BitwiseXor => write!(f, "^"),
            CBinOperator::LessThanOrEqual => write!(f, "<="),
            CBinOperator::GreaterThanOrEqual => write!(f, ">="),
            CBinOperator::Modulo => write!(f, "%"),
        };
    }
}

impl TryFrom<&BinOperator> for CBinOperator {
    type Error = ();

    fn try_from(value: &BinOperator) -> Result<Self, Self::Error> {
        return match &value.kind {
            BinOpKind::Plus => Ok(CBinOperator::Plus),
            BinOpKind::Minus => Ok(CBinOperator::Minus),
            BinOpKind::Multiply => Ok(CBinOperator::Multiply),
            BinOpKind::Divide => Ok(CBinOperator::Divide),
            BinOpKind::Equals => Ok(CBinOperator::Equals),
            BinOpKind::NotEquals => Ok(CBinOperator::NotEquals),
            BinOpKind::LessThan => Ok(CBinOperator::LessThan),
            BinOpKind::GreaterThan => Ok(CBinOperator::GreaterThan),
            BinOpKind::BitwiseAnd => Ok(CBinOperator::BitwiseAnd),
            BinOpKind::BitwiseOr => Ok(CBinOperator::BitwiseOr),
            BinOpKind::BitwiseXor => Ok(CBinOperator::BitwiseXor),
            BinOpKind::LessThanOrEqual => Ok(CBinOperator::LessThanOrEqual),
            BinOpKind::GreaterThanOrEqual => Ok(CBinOperator::GreaterThanOrEqual),
            BinOpKind::Modulo => Ok(CBinOperator::Modulo),
            BinOpKind::Power => Err(()),
        };
    }
}

#[derive(Debug, Clone)]
pub struct CCallExpr {
    pub name: String,
    pub arguments: Vec<CExpr>,
}
