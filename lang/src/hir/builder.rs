use std::{cell::Cell, collections::HashMap};

use fusion_compiler::bug;

use crate::{
    ast::{Ast, ExprId, ExprKind, ItemKind, StmtId, StmtKind},
    compilation_unit::{GlobalScope, VariableIdx},
    hir::{HIRExpr, HIRExprKind, HIRStmt, HIRStmtKind, Type, HIR},
};

struct Ctx {
    stmts: Vec<HIRStmt>,
}

impl Ctx {
    fn new() -> Self {
        Self { stmts: vec![] }
    }

    fn with_capacity(capacity: usize) -> Self {
        Self {
            stmts: Vec::with_capacity(capacity),
        }
    }
}

pub struct HIRBuilder {
    hir: HIR,
    temp_var_counter: Cell<usize>,
}

impl HIRBuilder {
    pub fn new() -> Self {
        Self {
            hir: HIR {
                functions: HashMap::new(),
            },
            temp_var_counter: Cell::new(0),
        }
    }

    pub fn build(mut self, ast: &Ast, global_scope: &mut GlobalScope) -> HIR {
        for item in ast.items.iter() {
            match &item.kind {
                ItemKind::Stmt(_) => unimplemented!(),
                ItemKind::Function(function_decl) => {
                    let mut ctx = Ctx::new();
                    for stmt_id in function_decl.body.iter() {
                        self.build_stmt(*stmt_id, ast, global_scope, &mut ctx);
                    }
                    self.hir.functions.insert(function_decl.idx, ctx.stmts);
                }
            }
        }
        self.hir
    }

    fn build_stmt(
        &self,
        stmt_id: StmtId,
        ast: &Ast,
        global_scope: &mut GlobalScope,
        ctx: &mut Ctx,
    ) {
        let stmt = ast.query_stmt(stmt_id);
        let kind = match &stmt.kind {
            StmtKind::Expr(expr) => {
                let expr = self.build_expr(*expr, ast, global_scope, ctx);
                HIRStmtKind::Expr { expr }
            }
            StmtKind::Let(let_stmt) => {
                let expr = self.build_expr(let_stmt.initializer, ast, global_scope, ctx);
                HIRStmtKind::Decl {
                    variable_idx: let_stmt.variable_idx,
                    initializer: Some(expr),
                }
            }
            StmtKind::While(while_stmt) => {
                let condition = self.build_expr(while_stmt.condition, ast, global_scope, ctx);
                let mut while_body_ctx = Ctx::with_capacity(while_stmt.body.len());
                for stmt_id in while_stmt.body.iter() {
                    self.build_stmt(*stmt_id, ast, global_scope, &mut while_body_ctx);
                }
                let body = vec![HIRStmt {
                    kind: HIRStmtKind::If {
                        condition,
                        then_body: while_body_ctx.stmts,
                        else_body: vec![HIRStmt {
                            kind: HIRStmtKind::Break,
                        }],
                    },
                }];
                HIRStmtKind::Loop { body }
            }
            StmtKind::Return(return_stmt) => {
                let expr = return_stmt
                    .return_value
                    .as_ref()
                    .copied()
                    .map(|expr_id| self.build_expr(expr_id, ast, global_scope, ctx))
                    .unwrap_or(HIRExpr {
                        kind: HIRExprKind::Unit,
                        ty: Type::Void,
                    });
                HIRStmtKind::Return { expr }
            }
        };
        ctx.stmts.push(HIRStmt { kind });
    }

    fn build_expr(
        &self,
        expr_id: ExprId,
        ast: &Ast,
        global_scope: &mut GlobalScope,
        ctx: &mut Ctx,
    ) -> HIRExpr {
        let expr = ast.query_expr(expr_id);
        let kind = match &expr.kind {
            ExprKind::Number(number_expr) => HIRExprKind::Number(number_expr.number),
            ExprKind::Binary(binary_expr) => {
                let lhs = self.build_expr(binary_expr.left, ast, global_scope, ctx);
                let rhs = self.build_expr(binary_expr.right, ast, global_scope, ctx);
                HIRExprKind::Binary {
                    operator: binary_expr.operator.kind.clone(),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
            ExprKind::Unary(unary_expr) => {
                let operand = self.build_expr(unary_expr.operand, ast, global_scope, ctx);
                HIRExprKind::Unary {
                    operator: unary_expr.operator.kind.clone(),
                    operand: Box::new(operand),
                }
            }
            ExprKind::Parenthesized(parenthesized_expr) => {
                self.build_expr(parenthesized_expr.inner, ast, global_scope, ctx)
                    .kind
            }
            ExprKind::Variable(variable_expr) => HIRExprKind::Var(variable_expr.variable_idx),
            ExprKind::Boolean(boolean_expr) => HIRExprKind::Bool(boolean_expr.value),
            ExprKind::Call(call_expr) => {
                let arguments = call_expr
                    .arguments
                    .iter()
                    .map(|expr_id| self.build_expr(*expr_id, ast, global_scope, ctx))
                    .collect();
                HIRExprKind::Call {
                    function_idx: call_expr.function_idx,
                    arguments,
                }
            }
            ExprKind::Assignment(assignment_expr) => {
                let stmt = HIRStmt {
                    kind: HIRStmtKind::Assign {
                        lhs: assignment_expr.variable_idx,
                        rhs: self.build_expr(assignment_expr.expression, ast, global_scope, ctx),
                    },
                };
                ctx.stmts.push(stmt);
                HIRExprKind::Var(assignment_expr.variable_idx)
            }
            ExprKind::Block(block_expr) => {
                let mut block_ctx = Ctx::new();
                for stmt_id in block_expr.stmts.iter() {
                    self.build_stmt(*stmt_id, ast, global_scope, &mut block_ctx);
                }
                let expr_kind = if matches!(expr.ty, Type::Void) {
                    HIRExprKind::Unit
                } else {
                    let last_stmt = block_ctx.stmts.last_mut().unwrap();
                    let expr = match &last_stmt.kind {
                        HIRStmtKind::Expr { expr } => expr.clone(),
                        _ => bug!("ICE: Last statement in block expression is not an expression"),
                    };
                    let temp_variable_idx =
                        self.declare_next_temp_var(global_scope, expr.ty.clone());
                    ctx.stmts.push(HIRStmt {
                        kind: HIRStmtKind::Decl {
                            variable_idx: temp_variable_idx,
                            initializer: None,
                        },
                    });
                    *last_stmt = HIRStmt {
                        kind: HIRStmtKind::Assign {
                            lhs: temp_variable_idx,
                            rhs: expr,
                        },
                    };
                    HIRExprKind::Var(temp_variable_idx)
                };
                ctx.stmts.push(HIRStmt {
                    kind: HIRStmtKind::Block {
                        body: block_ctx.stmts,
                    },
                });
                expr_kind
            }
            ExprKind::If(if_expr) => {
                let condition = self.build_expr(if_expr.condition, ast, global_scope, ctx);
                let mut then_ctx = Ctx::new();
                for stmt_id in &if_expr.then_branch.stmts {
                    self.build_stmt(*stmt_id, ast, global_scope, &mut then_ctx);
                }
                let mut else_ctx = Ctx::new();
                if let Some(else_branch) = &if_expr.else_branch {
                    for stmt_id in else_branch.body.iter() {
                        self.build_stmt(*stmt_id, ast, global_scope, &mut else_ctx);
                    }
                }
                let expr_kind = if matches!(expr.ty, Type::Void) {
                    HIRExprKind::Unit
                } else {
                    let temp_variable_idx =
                        self.declare_next_temp_var(global_scope, expr.ty.clone());
                    ctx.stmts.push(HIRStmt {
                        kind: HIRStmtKind::Decl {
                            variable_idx: temp_variable_idx,
                            initializer: None,
                        },
                    });
                    let then_expr = match then_ctx.stmts.last_mut().unwrap().kind {
                        HIRStmtKind::Expr { ref mut expr } => expr.clone(),
                        _ => bug!("ICE: Last statement in then branch of if expression is not an expression"),
                    };
                    let else_expr = match else_ctx.stmts.last_mut().unwrap().kind {
                        HIRStmtKind::Expr { ref mut expr } => expr.clone(),
                        _ => bug!("ICE: Last statement in else branch of if expression is not an expression"),
                    };
                    *then_ctx.stmts.last_mut().unwrap() = HIRStmt {
                        kind: HIRStmtKind::Assign {
                            lhs: temp_variable_idx,
                            rhs: then_expr,
                        },
                    };
                    *else_ctx.stmts.last_mut().unwrap() = HIRStmt {
                        kind: HIRStmtKind::Assign {
                            lhs: temp_variable_idx,
                            rhs: else_expr,
                        },
                    };
                    HIRExprKind::Var(temp_variable_idx)
                };
                ctx.stmts.push(HIRStmt {
                    kind: HIRStmtKind::If {
                        condition,
                        then_body: then_ctx.stmts,
                        else_body: else_ctx.stmts,
                    },
                });
                expr_kind
            }
            ExprKind::Error(_) => bug!("ICE: Error expression in HIR builder"),
        };
        HIRExpr {
            kind,
            ty: expr.ty.clone(),
        }
    }

    fn declare_next_temp_var(&self, global_scope: &mut GlobalScope, ty: Type) -> VariableIdx {
        global_scope.declare_variable(&self.next_temp_variable(), ty, false, false)
    }

    fn next_temp_variable(&self) -> String {
        let temp_variable_idx = self.temp_var_counter.get();
        self.temp_var_counter.set(temp_variable_idx + 1);
        format!("%{}", temp_variable_idx)
    }
}
