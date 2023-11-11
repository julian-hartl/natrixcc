use std::collections::HashMap;
use std::env::var;
use std::fmt::format;
use fusion_compiler::Idx;

use crate::ast::{AssignExpr, Ast, BinaryExpr, BinOpKind, BlockExpr, BoolExpr, CallExpr, Expr, ExprId, FunctionDeclaration, IfExpr, ItemId, LetStmt, NumberExpr, ParenthesizedExpr, Stmt, UnaryExpr, UnOpKind, VarExpr, WhileStmt};
use crate::ast::visitor::ASTVisitor;
use crate::compilation_unit::{FunctionIdx, GlobalScope, VariableIdx};
use crate::text::span::TextSpan;
use crate::typings::Type;
#[derive(Debug)]
pub struct Frame {
    variables: HashMap<VariableIdx, Value>,
}

impl Frame {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn insert(&mut self, idx: VariableIdx, value: Value) {
        self.variables.insert(idx, value);
    }

    fn get(&self, idx: &VariableIdx) -> Option<&Value> {
        self.variables.get(idx)
    }
}

#[derive(Debug)]
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

    fn update(&mut self, idx: VariableIdx, value: Value) {
        for frame in self.frames.iter_mut().rev() {
            if frame.get(&idx).is_some() {
                frame.insert(idx, value);
                return;
            }
        }
    }

    fn insert(&mut self, idx: VariableIdx, value: Value) {
        self.frames.last_mut().unwrap().insert(idx, value);
    }

    fn get(&self, idx: &VariableIdx) -> Option<&Value> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(idx) {
                return Some(value);
            }
        }
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Number(i64),
    Boolean(bool),
    Function(FunctionIdx),
}

impl Value {
    pub fn expect_boolean(&self) -> bool {
        match self {
            Value::Boolean(value) => *value,
            _ => panic!("Expected boolean value")
        }
    }

    pub fn expect_number(&self) -> i64 {
        match self {
            Value::Number(value) => *value,
            _ => panic!("Expected number value")
        }
    }

    pub fn expect_function(&self) -> FunctionIdx {
        match self {
            Value::Function(value) => *value,
            _ => panic!("Expected function value")
        }
    }
}


pub struct ASTEvaluator<'a> {
    pub last_value: Option<Value>,
    pub frames: Frames,
    pub global_scope: &'a GlobalScope,
}

impl<'a> ASTEvaluator<'a> {
    pub fn new(
        global_scope: &'a GlobalScope,
    ) -> Self {
        Self { last_value: None, frames: Frames::new(), global_scope }
    }

    fn push_frame(&mut self) {
        self.frames.push();
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
    }

    fn expect_last_value(&self) -> Value {
        *self.last_value.as_ref().expect("Expected last value to be set")
    }
}

impl<'a> ASTVisitor for ASTEvaluator<'a> {
    fn visit_func_decl(&mut self, ast: &mut Ast, func_decl: &FunctionDeclaration, item_id: ItemId) {
    }

    fn visit_while_statement(&mut self, ast: &mut Ast, while_statement: &WhileStmt) {
        self.push_frame();
        self.visit_expression(ast, while_statement.condition);
        while self.expect_last_value().expect_boolean() {
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
        if self.expect_last_value().expect_boolean() {
            self.push_frame();
            self.visit_expression(ast, if_statement.then_branch);
            self.pop_frame();
        } else if let Some(else_branch) = &if_statement.else_branch {
            self.push_frame();
            self.visit_expression(ast, else_branch.expr);
            self.pop_frame();
        }
        self.pop_frame();
    }

    fn visit_let_statement(&mut self, ast: &mut Ast, let_statement: &LetStmt, stmt: &Stmt) {
        self.visit_expression(ast, let_statement.initializer);
        self.frames.insert(let_statement.variable_idx, self.expect_last_value());
    }

    fn visit_call_expression(&mut self, ast: &mut Ast, call_expression: &CallExpr, expr: &Expr) {
        let function_name = call_expression.function_name();
        let function = self.global_scope.lookup_function(function_name).map(|f| self.global_scope.functions.get(f)).expect(format!("Function '{}' not found", call_expression.callee.span.literal).as_str());
        let mut arguments = Vec::new();
        for argument in &call_expression.arguments {
            self.visit_expression(ast, *argument);
            arguments.push(self.last_value.unwrap());
        }
        self.push_frame();
        for (argument, param) in arguments.iter().zip(function.parameters.iter()) {
            self.frames.insert(*param, *argument);
        }

        self.visit_expression(ast, function.body);
        self.pop_frame();
    }

    fn visit_assignment_expression(&mut self, ast: &mut Ast, assign_expr: &AssignExpr, expr: &Expr) {
        self.visit_expression(ast, assign_expr.expression);
        self.frames.update(assign_expr.variable_idx, self.last_value.unwrap());
    }

    fn visit_variable_expression(&mut self, ast: &mut Ast, var_expr: &VarExpr, expr: &Expr) {
        let identifier = &var_expr.identifier.span.literal;
        self.last_value = Some(*self.frames.get(&var_expr.variable_idx).expect(format!("Variable {} '{}' not found", var_expr.variable_idx.as_index(),  identifier).as_str()));
    }


    fn visit_number_expression(&mut self, ast: &mut Ast, number: &NumberExpr, expr: &Expr) {
        self.last_value = Some(Value::Number(number.number));
    }

    fn visit_boolean_expression(&mut self, ast: &mut Ast, boolean: &BoolExpr, expr: &Expr) {
        self.last_value = Some(Value::Boolean(boolean.value));
    }

    fn visit_error(&mut self, ast: &mut Ast, span: &TextSpan) {
        panic!("Cannot evaluate error expression")
    }

    fn visit_unary_expression(&mut self, ast: &mut Ast, unary_expression: &UnaryExpr, expr: &Expr) {
        self.visit_expression(ast, unary_expression.operand);
        let operand = self.expect_last_value().expect_number();
        self.last_value = Some(Value::Number(match unary_expression.operator.kind {
            UnOpKind::Minus => -operand,
            UnOpKind::BitwiseNot => !operand,
        }));
    }

    fn visit_binary_expression(&mut self, ast: &mut Ast, binary_expr: &BinaryExpr, expr: &Expr) {
        self.visit_expression(ast, binary_expr.left);
        let left = self.expect_last_value();
        self.visit_expression(ast, binary_expr.right);
        let right = self.expect_last_value();
        self.last_value = Some(match binary_expr.operator.kind {
            BinOpKind::Plus => Value::Number(left.expect_number() + right.expect_number()),
            BinOpKind::Minus => Value::Number(left.expect_number() - right.expect_number()),
            BinOpKind::Multiply => Value::Number(left.expect_number() * right.expect_number()),
            BinOpKind::Divide => Value::Number(left.expect_number() / right.expect_number()),
            BinOpKind::Power => Value::Number(left.expect_number().pow(right.expect_number() as u32)),
            BinOpKind::BitwiseAnd => Value::Number(left.expect_number() & right.expect_number()),
            BinOpKind::BitwiseOr => Value::Number(left.expect_number() | right.expect_number()),
            BinOpKind::BitwiseXor => Value::Number(left.expect_number() ^ right.expect_number()),
            BinOpKind::Equals => Value::Boolean(left == right),
            BinOpKind::NotEquals => Value::Boolean(left != right),
            BinOpKind::LessThan => Value::Boolean(left.expect_number() < right.expect_number()),
            BinOpKind::LessThanOrEqual => Value::Boolean(left.expect_number() <= right.expect_number()),
            BinOpKind::GreaterThan => Value::Boolean(left.expect_number() > right.expect_number()),
            BinOpKind::GreaterThanOrEqual => Value::Boolean(left.expect_number() >= right.expect_number()),
        });
    }

    fn visit_parenthesized_expression(&mut self, ast: &mut Ast, parenthesized_expression: &ParenthesizedExpr, expr: &Expr) {
        self.visit_expression(ast, parenthesized_expression.expression);
    }
}