use termion::color::{Fg, Reset};
use crate::ast::{Ast, ASTAssignmentExpression, ASTBinaryExpression, ASTBlockStatement, ASTBooleanExpression, ASTCallExpression, ASTExpression, ASTExpressionKind, ASTExprId, ASTFuncDeclStatement, ASTIfStatement, ASTLetStatement, ASTNumberExpression, ASTParenthesizedExpression, ASTReturnStatement, ASTStatement, ASTStatementKind, ASTStmtId, ASTUnaryExpression, ASTVariableExpression, ASTWhileStatement};
use crate::text::span::TextSpan;
use crate::ast::printer::ASTPrinter;

pub trait ASTVisitor {

    fn get_ast(&self) -> &Ast;

    fn do_visit_statement(&mut self, statement: &ASTStmtId) {
        let statement = self.get_ast().query_stmt(statement).clone();
        match &statement.kind {
            ASTStatementKind::Expression(expr) => {
                self.visit_expression(expr);
            }
            ASTStatementKind::Let(expr) => {
                self.visit_let_statement(expr);
            }
            ASTStatementKind::If(stmt) => {
                self.visit_if_statement(stmt);
            }
            ASTStatementKind::Block(stmt) => {
                self.visit_block_statement(stmt);
            }
            ASTStatementKind::While(stmt) => {
                self.visit_while_statement(stmt);
            }
            ASTStatementKind::FuncDecl(stmt) => {
                self.visit_func_decl_statement(stmt);
            }
            ASTStatementKind::Return(stmt) => {
                self.visit_return_statement(stmt);
            }
        }
    }

    fn visit_func_decl_statement(&mut self, func_decl_statement: &ASTFuncDeclStatement);

    fn visit_return_statement(&mut self, return_statement: &ASTReturnStatement) {
        if let Some(expr) = &return_statement.return_value {
            self.visit_expression(expr);
        }
    }

    fn visit_while_statement(&mut self, while_statement: &ASTWhileStatement) {
        self.visit_expression(&while_statement.condition);
        self.visit_statement(&while_statement.body);

    }
    fn visit_block_statement(&mut self, block_statement: &ASTBlockStatement) {
        for statement in &block_statement.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_if_statement(&mut self, if_statement: &ASTIfStatement) {
        self.visit_expression(&if_statement.condition);
        self.visit_statement(&if_statement.then_branch);
        if let Some(else_branch) = &if_statement.else_branch {
            self.visit_statement(&else_branch.else_statement);
        }
    }
    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement);
    fn visit_statement(&mut self, statement: &ASTStmtId) {
        self.do_visit_statement(statement);
    }
    fn do_visit_expression(&mut self, expression: &ASTExprId) {
        let expression = self.get_ast().query_expr(expression).clone();
        match &expression.kind {
            ASTExpressionKind::Number(number) => {
                self.visit_number_expression(number, &expression);
            }
            ASTExpressionKind::Binary(expr) => {
                self.visit_binary_expression(expr, &expression);
            }
            ASTExpressionKind::Parenthesized(expr) => {
                self.visit_parenthesized_expression(expr, &expression);
            }
            ASTExpressionKind::Error(span) => {
                self.visit_error(span);
            }
            ASTExpressionKind::Variable(expr) => {
                self.visit_variable_expression(expr, &expression);
            }
            ASTExpressionKind::Unary(expr) => {
                self.visit_unary_expression(expr, &expression);
            }
            ASTExpressionKind::Assignment(expr) => {
                self.visit_assignment_expression(expr, &expression);
            }
            ASTExpressionKind::Boolean(expr) => {
                self.visit_boolean_expression(expr, &expression);
            }
            ASTExpressionKind::Call(expr) => {
                self.visit_call_expression(expr, &expression);
            }

        }
    }
    fn visit_call_expression(&mut self, call_expression: &ASTCallExpression, expr: &ASTExpression) {
        for argument in &call_expression.arguments {
            self.visit_expression(argument);
        }
    }
    fn visit_expression(&mut self, expression: &ASTExprId) {
        self.do_visit_expression(expression);
    }

    fn visit_assignment_expression(&mut self, assignment_expression: &ASTAssignmentExpression, expr: &ASTExpression) {
        self.visit_expression(&assignment_expression.expression);
    }

    fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression, expr: &ASTExpression);

    fn visit_number_expression(&mut self, number: &ASTNumberExpression, expr: &ASTExpression);

    fn visit_boolean_expression(&mut self, boolean: &ASTBooleanExpression, expr: &ASTExpression);

    fn visit_error(&mut self, span: &TextSpan);

    fn visit_unary_expression(&mut self, unary_expression: &ASTUnaryExpression, expr: &ASTExpression);

    fn visit_binary_expression(&mut self, binary_expression: &ASTBinaryExpression, expr: &ASTExpression) {
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
    }
    fn visit_parenthesized_expression(&mut self, parenthesized_expression: &ASTParenthesizedExpression, expr: &ASTExpression) {
        self.visit_expression(&parenthesized_expression.expression);
    }
}


