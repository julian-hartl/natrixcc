use std::fmt::Write;

use anyhow::Result;
use fusion_compiler::Idx;

use crate::{
    compilation_unit::GlobalScope,
    hir::{HIRExpr, HIRExprKind, HIRStmt, HIRStmtKind, HIR},
};

pub struct HIRWriter<W> {
    _phantom: std::marker::PhantomData<W>,
}

impl<W> HIRWriter<W>
where
    W: Write,
{
    pub fn write(writer: &mut W, hir: &HIR, global_scope: &GlobalScope) -> Result<()> {
        for (function_id, body) in &hir.functions {
            let function = global_scope.functions.get(*function_id);
            writeln!(writer, "func {}(", function.name)?;
            for arg_id in function.parameters.iter() {
                let arg = global_scope.variables.get(*arg_id);
                write!(writer, "{}", arg.name)?;
                write!(writer, ": ")?;
                write!(writer, "{}", arg.ty)?;
                if arg_id.as_index() != function.parameters.len() - 1 {
                    write!(writer, ", ")?;
                }
            }
            writeln!(writer, ") -> {} {{", function.return_type)?;
            for statement in body {
                Self::write_statement(writer, statement, global_scope)?;
            }
            writeln!(writer, "}}")?;
        }
        Ok(())
    }

    fn write_statement(
        writer: &mut W,
        statement: &HIRStmt,
        global_scope: &GlobalScope,
    ) -> Result<()> {
        match &statement.kind {
            HIRStmtKind::Loop { body } => {
                writeln!(writer, "loop {{")?;
                for stmt in body {
                    Self::write_statement(writer, stmt, global_scope)?;
                }
                writeln!(writer, "}}")?;
            }
            HIRStmtKind::If {
                condition,
                then_body,
                else_body,
            } => {
                write!(writer, "if ")?;
                Self::write_expression(writer, condition, global_scope)?;
                writeln!(writer, " {{")?;
                for stmt in then_body {
                    Self::write_statement(writer, stmt, global_scope)?;
                }
                writeln!(writer, "}} else {{")?;
                for stmt in else_body {
                    Self::write_statement(writer, stmt, global_scope)?;
                }
                writeln!(writer, "}}")?;
            }
            HIRStmtKind::Block { body } => {
                writeln!(writer, "{{")?;
                for stmt in body {
                    Self::write_statement(writer, stmt, global_scope)?;
                }
                writeln!(writer, "}}")?;
            }
            HIRStmtKind::Return { expr } => {
                write!(writer, "return ")?;
                Self::write_expression(writer, expr, global_scope)?;
                writeln!(writer)?;
            }
            HIRStmtKind::Expr { expr } => {
                Self::write_expression(writer, expr, global_scope)?;
                writeln!(writer)?;
            }
            HIRStmtKind::Decl {
                variable_idx,
                initializer,
            } => {
                let variable = global_scope.variables.get(*variable_idx);
                write!(writer, "let {}: {}", variable.name, variable.ty)?;
                if let Some(initializer) = initializer {
                    write!(writer, " = ")?;
                    Self::write_expression(writer, initializer, global_scope)?;
                }
                writeln!(writer)?;
            }
            HIRStmtKind::Assign { lhs, rhs } => {
                let variable = global_scope.variables.get(*lhs);
                write!(writer, "{} = ", variable.name)?;
                Self::write_expression(writer, rhs, global_scope)?;
                writeln!(writer)?;
            }
            HIRStmtKind::Break => {
                writeln!(writer, "break")?;
            }
        }
        Ok(())
    }

    fn write_expression(
        writer: &mut W,
        expression: &HIRExpr,
        global_scope: &GlobalScope,
    ) -> Result<()> {
        match &expression.kind {
            HIRExprKind::Binary { operator, lhs, rhs } => {
                Self::write_expression(writer, lhs.as_ref(), global_scope)?;
                write!(writer, " {} ", operator)?;
                Self::write_expression(writer, rhs.as_ref(), global_scope)?;
            }
            HIRExprKind::Unary { operator, operand } => {
                write!(writer, "{}", operator)?;
                Self::write_expression(writer, operand.as_ref(), global_scope)?;
            }
            HIRExprKind::Call {
                function_idx,
                arguments,
            } => {
                let function = global_scope.functions.get(*function_idx);
                write!(writer, "{}(", function.name)?;
                for (arg_idx, arg) in arguments.iter().enumerate() {
                    Self::write_expression(writer, arg, global_scope)?;
                    if arg_idx != arguments.len() - 1 {
                        write!(writer, ", ")?;
                    }
                }
                write!(writer, ")")?;
            }
            HIRExprKind::Number(value) => {
                write!(writer, "{}", value)?;
            }
            HIRExprKind::Bool(value) => {
                write!(writer, "{}", value)?;
            }
            HIRExprKind::Var(variable_idx) => {
                let variable = global_scope.variables.get(*variable_idx);
                write!(writer, "{}", variable.name)?;
            }
            HIRExprKind::Unit => {
                write!(writer, "()")?;
            }
        }
        Ok(())
    }
}
