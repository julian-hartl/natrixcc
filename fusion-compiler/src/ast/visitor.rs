use termion::color::{Fg, Reset};

use crate::ast::{AssignExpr, Ast, BinaryExpr, BlockExpr, BoolExpr, CallExpr, Expr, ExprId, ExprKind, FuncExpr, FunctionDeclaration, IfExpr, ItemId, ItemKind, LetStmt, NumberExpr, ParenthesizedExpr, RecExpr, ReturnStmt, Stmt, StmtId, StmtKind, UnaryExpr, VarExpr, WhileStmt};
use crate::ast::printer::ASTPrinter;
use crate::text::span::TextSpan;

pub trait ASTVisitor {
    fn visit_item(&mut self, ast: &mut Ast, item: ItemId) {
        self.visit_item_default(ast, item);
    }

    fn visit_item_default(&mut self, ast: &mut Ast, item: ItemId) {
        let item = ast.query_item(item).clone();
        match &item.kind {
            ItemKind::Stmt(stmt) => {
                self.visit_statement(ast, *stmt);
            }
        }
    }

    fn do_visit_statement(&mut self, ast: &mut Ast, statement: StmtId) {
        let statement = ast.query_stmt(statement).clone();
        match &statement.kind {
            StmtKind::Expr(expr) => {
                self.visit_expression(ast, *expr);
            }
            StmtKind::Let(expr) => {
                self.visit_let_statement(ast, expr, &statement);
            }
            StmtKind::While(stmt) => {
                self.visit_while_statement(ast, &stmt);
            }
            StmtKind::Return(stmt) => {
                self.visit_return_statement(ast, &stmt);
            }
        }
    }

    fn visit_func_expr(&mut self, ast: &mut Ast, func_expr: &FuncExpr, expr_id: ExprId);

    fn visit_return_statement(&mut self, ast: &mut Ast, return_statement: &ReturnStmt) {
        if let Some(expr) = &return_statement.return_value {
            self.visit_expression(ast, *expr);
        }
    }

    fn visit_while_statement(&mut self, ast: &mut Ast, while_statement: &WhileStmt) {
        self.visit_expression(ast, while_statement.condition);
        self.visit_expression(ast, while_statement.body);
    }
    fn visit_block_expr(&mut self, ast: &mut Ast, block_expr: &BlockExpr, expr: &Expr) {
        for stmt in &block_expr.stmts {
            self.visit_statement(ast, *stmt);
        }
    }

    fn visit_if_expression(&mut self, ast: &mut Ast, if_expr: &IfExpr, expr: &Expr) {
        self.visit_expression(ast, if_expr.condition);
        self.visit_expression(ast, if_expr.then_branch);
        if let Some(else_branch) = &if_expr.else_branch {
            self.visit_expression(ast, else_branch.expr);
        }
    }
    fn visit_let_statement(&mut self, ast: &mut Ast, let_statement: &LetStmt, stmt: &Stmt);
    fn visit_statement(&mut self, ast: &mut Ast, statement: StmtId) {
        self.do_visit_statement(ast, statement);
    }
    fn do_visit_expression(&mut self, ast: &mut Ast, expression: ExprId) {
        let expression = ast.query_expr(expression).clone();
        match &expression.kind {
            ExprKind::Number(number) => {
                self.visit_number_expression(ast, number, &expression);
            }
            ExprKind::Binary(expr) => {
                self.visit_binary_expression(ast, expr, &expression);
            }
            ExprKind::Parenthesized(expr) => {
                self.visit_parenthesized_expression(ast, expr, &expression);
            }
            ExprKind::Error(span) => {
                self.visit_error(ast, span);
            }
            ExprKind::Variable(expr) => {
                self.visit_variable_expression(ast, expr, &expression);
            }
            ExprKind::Unary(expr) => {
                self.visit_unary_expression(ast, expr, &expression);
            }
            ExprKind::Assignment(expr) => {
                self.visit_assignment_expression(ast, expr, &expression);
            }
            ExprKind::Boolean(expr) => {
                self.visit_boolean_expression(ast, expr, &expression);
            }
            ExprKind::Call(expr) => {
                self.visit_call_expression(ast, expr, &expression);
            }
            ExprKind::If(expr) => {
                self.visit_if_expression(ast, expr, &expression);
            }
            ExprKind::Block(block_expr) => {
                self.visit_block_expr(ast, &block_expr, &expression);
            }
            ExprKind::Func(func_expr) => {
                self.visit_func_expr(ast, func_expr, expression.id);
            }
            ExprKind::Rec(expr) => {
                self.visit_rec_expression(ast, expr, expression.id);
            }
        }
    }

    fn visit_rec_expression(&mut self, ast: &mut Ast, expr: &RecExpr, expr_id: ExprId) ;

    fn visit_call_expression(&mut self, ast: &mut Ast, call_expression: &CallExpr, expr: &Expr) {
        for argument in &call_expression.arguments {
            self.visit_expression(ast, *argument);
        }
    }
    fn visit_expression(&mut self, ast: &mut Ast, expression: ExprId) {
        self.do_visit_expression(ast, expression);
    }

    fn visit_assignment_expression(&mut self, ast: &mut Ast, assignment_expression: &AssignExpr, expr: &Expr) {
        self.visit_expression(ast, assignment_expression.expression);
    }

    fn visit_variable_expression(&mut self, ast: &mut Ast, variable_expression: &VarExpr, expr: &Expr);

    fn visit_number_expression(&mut self, ast: &mut Ast, number: &NumberExpr, expr: &Expr);

    fn visit_boolean_expression(&mut self, ast: &mut Ast, boolean: &BoolExpr, expr: &Expr);

    fn visit_error(&mut self, ast: &mut Ast, span: &TextSpan);

    fn visit_unary_expression(&mut self, ast: &mut Ast, unary_expression: &UnaryExpr, expr: &Expr);

    fn visit_binary_expression(&mut self, ast: &mut Ast, binary_expression: &BinaryExpr, expr: &Expr) {
        self.visit_expression(ast, binary_expression.left);
        self.visit_expression(ast, binary_expression.right);
    }
    fn visit_parenthesized_expression(&mut self, ast: &mut Ast, parenthesized_expression: &ParenthesizedExpr, expr: &Expr) {
        self.visit_expression(ast, parenthesized_expression.expression);
    }
}


