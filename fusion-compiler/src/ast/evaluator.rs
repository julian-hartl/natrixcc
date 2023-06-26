use std::collections::HashMap;
use std::env::var;
use std::fmt::format;

use crate::ast::{Ast, AssignExpr, BinaryExpr, BinOpKind, BlockExpr, BoolExpr, CallExpr, Expr, FunctionDeclaration, IfExpr, LetStmt, NumberExpr, ParenthesizedExpr, UnaryExpr, UnOpKid, VarExpr, WhileStmt, Stmt};
use crate::ast::visitor::ASTVisitor;
use crate::compilation_unit::{GlobalScope, VariableIdx};
use crate::text::span::TextSpan;

pub struct Frame {
    variables: HashMap<VariableIdx, i64>,
}

impl Frame {
    fn new() -> Self {
        Self {
            variables: HashMap::new()
        }
    }

    fn insert(&mut self, idx: VariableIdx, value: i64) {
        self.variables.insert(idx, value);
    }

    fn get(&self, idx: &VariableIdx) -> Option<&i64> {
        self.variables.get(idx)
    }
}

pub struct Frames {
    frames: Vec<Frame>,
}

impl Frames {
    fn new() -> Self {
        Self {
            frames: vec![Frame::new()]
        }
    }

    fn push(&mut self) {
        self.frames.push(Frame::new());
    }

    fn pop(&mut self) {
        self.frames.pop();
    }

    fn update(&mut self, idx: VariableIdx, value: i64) {
        for frame in self.frames.iter_mut().rev() {
            if frame.variables.contains_key(&idx) {
                frame.insert(idx, value);
                return;
            }
        }
    }

    fn insert(&mut self, idx: VariableIdx, value: i64) {
        self.frames.last_mut().unwrap().insert(idx, value);
    }

    fn get(&self, idx: &VariableIdx) -> Option<&i64> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(idx) {
                return Some(value);
            }
        }
        None
    }
}

pub struct ASTEvaluator<'a> {
    pub last_value: Option<i64>,
    pub frames: Frames,
    pub global_scope: &'a GlobalScope,
}

impl<'a> ASTEvaluator<'a> {
    pub fn new(
        global_scope: &'a GlobalScope,
    ) -> Self {
        Self { last_value: None, frames: Frames::new(), global_scope }
    }

    fn eval_boolean_instruction<F>(&self, instruction: F) -> i64 where F: FnOnce() -> bool {
        let result = instruction();
        if result {
            1
        } else {
            0
        }
    }

    fn push_frame(&mut self) {
        self.frames.push();
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
    }
}

impl<'a> ASTVisitor for ASTEvaluator<'a> {

    fn visit_function_declaration(&mut self, ast: &mut Ast, func_decl_statement: &FunctionDeclaration) {}

    fn visit_while_statement(&mut self, ast: &mut Ast,while_statement: &WhileStmt) {
        self.push_frame();
        self.visit_expression(ast, while_statement.condition);
        while self.last_value.unwrap() != 0 {
            self.visit_expression(ast, while_statement.body);
            self.visit_expression(ast, while_statement.condition);
        }
        self.pop_frame();
    }

    fn visit_block_expr(&mut self, ast: &mut Ast, block_statement: &BlockExpr, expr: &Expr) {
        self.push_frame();
        for statement in &block_statement.stmts {
            self.visit_statement(ast, *statement);
        }
        self.pop_frame();
    }

    fn visit_if_expression(&mut self, ast: &mut Ast, if_statement: &IfExpr, expr: &Expr) {
        self.push_frame();
        self.visit_expression(ast, if_statement.condition);
        if self.last_value.unwrap() != 0 {
            self.push_frame();
            self.visit_expression(ast, if_statement.then_branch);
            self.pop_frame();
        } else {
            if let Some(else_branch) = &if_statement.else_branch {
                self.push_frame();
                self.visit_expression(ast, else_branch.expr);
                self.pop_frame();
            }
        }
        self.pop_frame();
    }

    fn visit_let_statement(&mut self,ast: &mut Ast,let_statement: &LetStmt, stmt: &Stmt) {
        self.visit_expression(ast, let_statement.initializer);
        self.frames.insert(let_statement.variable_idx, self.last_value.unwrap());
    }

    fn visit_call_expression(&mut self, ast: &mut Ast,call_expression: &CallExpr, expr: &Expr) {
        let function_idx = self.global_scope.lookup_function(&call_expression.identifier.span.literal).unwrap();
        let function = self.global_scope.functions.get(function_idx);
        let mut arguments = Vec::new();
        for argument in &call_expression.arguments {
            self.visit_expression(ast, *argument);
            arguments.push(self.last_value.unwrap());
        }
        self.push_frame();
        for (argument, param) in arguments.iter().zip(function.parameters.iter()) {
            self.frames.insert(*param, *argument);
        }

        self.visit_statement(ast, function.body);
        self.pop_frame();
    }

    fn visit_assignment_expression(&mut self,ast: &mut Ast, assign_expr: &AssignExpr, expr: &Expr) {
        self.visit_expression(ast, assign_expr.expression);
        self.frames.update(assign_expr.variable_idx, self.last_value.unwrap());
    }

    fn visit_variable_expression(&mut self, ast: &mut Ast,var_expr: &VarExpr, expr: &Expr) {
        let identifier = &var_expr.identifier.span.literal;
        self.last_value = Some(*self.frames.get(&var_expr.variable_idx).expect(format!("Variable {} not found", identifier).as_str()));
    }

    fn visit_number_expression(&mut self,ast: &mut Ast, number: &NumberExpr, expr: &Expr) {
        self.last_value = Some(number.number);
    }


    fn visit_boolean_expression(&mut self,ast: &mut Ast, boolean: &BoolExpr, expr: &Expr) {
        self.last_value = Some(boolean.value as i64);
    }

    fn visit_error(&mut self, ast: &mut Ast,span: &TextSpan) {
        todo!()
    }

    fn visit_unary_expression(&mut self,ast: &mut Ast, unary_expression: &UnaryExpr, expr: &Expr) {
        self.visit_expression(ast, unary_expression.operand);
        let operand = self.last_value.unwrap();
        self.last_value = Some(match unary_expression.operator.kind {
            UnOpKid::Minus => -operand,
            UnOpKid::BitwiseNot => !operand,
        });
    }

    fn visit_binary_expression(&mut self,ast: &mut Ast, binary_expr: &BinaryExpr, expr: &Expr) {
        self.visit_expression(ast, binary_expr.left);
        let left = self.last_value.unwrap();
        self.visit_expression(ast, binary_expr.right);
        let right = self.last_value.unwrap();
        self.last_value = Some(match binary_expr.operator.kind {
            BinOpKind::Plus => left + right,
            BinOpKind::Minus => left - right,
            BinOpKind::Multiply => left * right,
            BinOpKind::Divide => left / right,
            BinOpKind::Power => left.pow(right as u32),
            BinOpKind::BitwiseAnd => left & right,
            BinOpKind::BitwiseOr => left | right,
            BinOpKind::BitwiseXor => left ^ right,
            BinOpKind::Equals => if left == right { 1 } else { 0 },
            BinOpKind::NotEquals => self.eval_boolean_instruction(|| left != right),
            BinOpKind::LessThan => self.eval_boolean_instruction(|| left < right),
            BinOpKind::LessThanOrEqual => self.eval_boolean_instruction(|| left <= right),
            BinOpKind::GreaterThan => self.eval_boolean_instruction(|| left > right),
            BinOpKind::GreaterThanOrEqual => self.eval_boolean_instruction(|| left >= right),
        });
    }

    fn visit_parenthesized_expression(&mut self,ast: &mut Ast, parenthesized_expression: &ParenthesizedExpr, expr: &Expr) {
        self.visit_expression(ast, parenthesized_expression.expression);
    }
}