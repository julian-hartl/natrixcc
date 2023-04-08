use std::collections::HashMap;
use crate::ast::{ASTBinaryExpression, ASTBinaryOperatorKind, ASTLetStatement, ASTNumberExpression, ASTParenthesizedExpression, ASTVariableExpression, ASTVisitor};
use crate::ast::lexer::TextSpan;

pub struct ASTEvaluator {
    pub last_value: Option<i64>,
    pub variables: HashMap<String, i64>,
}

impl ASTEvaluator {
    pub fn new() -> Self {
        Self { last_value: None, variables: HashMap::new() }
    }
}

impl ASTVisitor for ASTEvaluator {
    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement) {
        self.visit_expression(&let_statement.initializer);
        self.variables.insert(let_statement.identifier.span.literal.clone(), self.last_value.unwrap());
    }

    fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression) {
        self.last_value = Some(*self.variables.get(&variable_expression.identifier.span.literal).unwrap());
    }

    fn visit_number_expression(&mut self, number: &ASTNumberExpression) {
        self.last_value = Some(number.number);
    }

    fn visit_error(&mut self, span: &TextSpan) {
        todo!()
    }

    fn visit_binary_expression(&mut self, expr: &ASTBinaryExpression) {
        self.visit_expression(&expr.left);
        let left = self.last_value.unwrap();
        self.visit_expression(&expr.right);
        let right = self.last_value.unwrap();
        self.last_value = Some(match expr.operator.kind {
            ASTBinaryOperatorKind::Plus => left + right,
            ASTBinaryOperatorKind::Minus => left - right,
            ASTBinaryOperatorKind::Multiply => left * right,
            ASTBinaryOperatorKind::Divide => left / right,
        });
    }

    fn visit_parenthesized_expression(&mut self, parenthesized_expression: &ASTParenthesizedExpression) {
        self.visit_expression(&parenthesized_expression.expression);
    }
}