use std::collections::HashMap;

#[allow(unused)]
pub use builder::HIRBuilder;
#[allow(unused)]
pub use writer::HIRWriter;

use crate::ast::{BinOpKind, UnOpKind};
use crate::compilation_unit::{FunctionIdx, Type, VariableIdx};

mod builder;
mod writer;

#[derive(Debug)]
pub struct HIR {
    pub functions: HashMap<FunctionIdx, Vec<HIRStmt>>,
}

#[derive(Debug)]
pub struct HIRStmt {
    pub kind: HIRStmtKind,
}

#[derive(Debug)]
pub enum HIRStmtKind {
    Loop {
        body: Vec<HIRStmt>,
    },
    If {
        condition: HIRExpr,
        then_body: Vec<HIRStmt>,
        else_body: Vec<HIRStmt>,
    },
    Block {
        body: Vec<HIRStmt>,
    },
    Return {
        expr: HIRExpr,
    },
    Expr {
        expr: HIRExpr,
    },
    Decl {
        variable_idx: VariableIdx,
        initializer: Option<HIRExpr>,
    },
    Assign {
        lhs: VariableIdx,
        rhs: HIRExpr,
    },
    Break,
}

#[derive(Debug, Clone)]
pub struct HIRExpr {
    pub kind: HIRExprKind,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum HIRExprKind {
    Binary {
        operator: BinOpKind,
        lhs: Box<HIRExpr>,
        rhs: Box<HIRExpr>,
    },
    Unary {
        operator: UnOpKind,
        operand: Box<HIRExpr>,
    },
    Call {
        function_idx: FunctionIdx,
        arguments: Vec<HIRExpr>,
    },
    Number(i64),
    Bool(bool),
    Var(VariableIdx),
    Unit,
}
