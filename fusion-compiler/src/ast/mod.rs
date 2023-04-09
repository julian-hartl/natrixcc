use termion::color;
use termion::color::{Fg, Reset};

use crate::ast::lexer::{TextSpan, Token};

pub mod lexer;
pub mod parser;
pub mod evaluator;

pub struct Ast {
    pub statements: Vec<ASTStatement>,
}

impl Ast {
    pub fn new() -> Self {
        Self { statements: Vec::new() }
    }

    pub fn add_statement(&mut self, statement: ASTStatement) {
        self.statements.push(statement);
    }

    pub fn visit(&self, visitor: &mut dyn ASTVisitor) {
        for statement in &self.statements {
            visitor.visit_statement(statement);
        }
    }

    pub fn visualize(&self) -> () {
        let mut printer = ASTPrinter::new();
        self.visit(&mut printer);
        println!("{}", printer.result);
    }
}

pub trait ASTVisitor {
    fn do_visit_statement(&mut self, statement: &ASTStatement) {
        match &statement.kind {
            ASTStatementKind::Expression(expr) => {
                self.visit_expression(expr);
            }
            ASTStatementKind::LetStatement(expr) => {
                self.visit_let_statement(expr);
            }
        }
    }
    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement);
    fn visit_statement(&mut self, statement: &ASTStatement) {
        self.do_visit_statement(statement);
    }
    fn do_visit_expression(&mut self, expression: &ASTExpression) {
        match &expression.kind {
            ASTExpressionKind::Number(number) => {
                self.visit_number_expression(number);
            }
            ASTExpressionKind::Binary(expr) => {
                self.visit_binary_expression(expr);
            }
            ASTExpressionKind::Parenthesized(expr) => {
                self.visit_parenthesized_expression(expr);
            }
            ASTExpressionKind::Error(span) => {
                self.visit_error(span);
            }
            ASTExpressionKind::Variable(expr) => {
                self.visit_variable_expression(expr);
            }
            ASTExpressionKind::Unary(expr) => {
                self.visit_unary_expression(expr);
            }
        }
    }
    fn visit_expression(&mut self, expression: &ASTExpression) {
        self.do_visit_expression(expression);
    }

    fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression);

    fn visit_number_expression(&mut self, number: &ASTNumberExpression);

    fn visit_error(&mut self, span: &TextSpan);

    fn visit_unary_expression(&mut self, unary_expression: &ASTUnaryExpression);

    fn visit_binary_expression(&mut self, binary_expression: &ASTBinaryExpression) {
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
    }
    fn visit_parenthesized_expression(&mut self, parenthesized_expression: &ASTParenthesizedExpression) {
        self.visit_expression(&parenthesized_expression.expression);
    }
}

pub struct ASTPrinter {
    indent: usize,
    result: String,
}

impl ASTPrinter {
    const NUMBER_COLOR: color::Cyan = color::Cyan;
    const TEXT_COLOR: color::LightWhite = color::LightWhite;
    const KEYWORD_COLOR: color::Magenta = color::Magenta;
    const VARIABLE_COLOR: color::Green = color::Green;

    fn add_whitespace(&mut self) {
        self.result.push_str(" ");
    }

    fn add_newline(&mut self) {
        self.result.push_str("
");
    }

    pub fn new() -> Self {
        Self { indent: 0, result: String::new() }
    }
}

impl ASTVisitor for ASTPrinter {
    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement) {
        self.result.push_str(&format!("{}let",
                                      Self::KEYWORD_COLOR.fg_str()));
        self.add_whitespace();
        self.result.push_str(&format!("{}{}",
                                      Self::TEXT_COLOR.fg_str(),
                                      let_statement.identifier.span.literal, ));
        self.add_whitespace();
        self.result.push_str(&format!("{}=",
                                      Self::TEXT_COLOR.fg_str(),
                                      ));
        self.add_whitespace();
        self.visit_expression(&let_statement.initializer);
    }

    fn visit_statement(&mut self, statement: &ASTStatement) {

        Self::do_visit_statement(self, statement);
        self.result.push_str(&format!("{}\n",
                                      Fg(Reset),
        ));
    }

    fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression) {
        self.result.push_str(&format!("{}{}",
                                      Self::VARIABLE_COLOR.fg_str(),
                                      variable_expression.identifier.span.literal, ));
    }

    fn visit_number_expression(&mut self, number: &ASTNumberExpression) {
        self.result.push_str(&format!("{}{}",
                                      Self::NUMBER_COLOR.fg_str(),
                                      number.number, ));
    }

    fn visit_error(&mut self, span: &TextSpan) {
        self.result.push_str(&format!("{}{}",
                                      Self::TEXT_COLOR.fg_str(),
                                      span.literal, ));
    }

    fn visit_unary_expression(&mut self, unary_expression: &ASTUnaryExpression) {
        self.result.push_str(&format!("{}{}",
                                      Self::TEXT_COLOR.fg_str(),
                                      unary_expression.operator.token.span.literal, ));
        self.visit_expression(&unary_expression.operand);
    }

    fn visit_binary_expression(&mut self, binary_expression: &ASTBinaryExpression) {
        self.visit_expression(&binary_expression.left);
        self.add_whitespace();
        self.result.push_str(&format!("{}{}",
                                      Self::TEXT_COLOR.fg_str(),
                                      binary_expression.operator.token.span.literal, ));
        self.add_whitespace();
        self.visit_expression(&binary_expression.right);
    }

    fn visit_parenthesized_expression(&mut self, parenthesized_expression: &ASTParenthesizedExpression) {
        self.result.push_str(&format!("{}{}",
                                      Self::TEXT_COLOR.fg_str(),
                                      "(", ));
        self.visit_expression(&parenthesized_expression.expression);
        self.result.push_str(&format!("{}{}",
                                      Self::TEXT_COLOR.fg_str(),
                                      ")", ));
    }
}


pub enum ASTStatementKind {
    Expression(ASTExpression),
    LetStatement(ASTLetStatement),
}

pub struct ASTLetStatement {
    pub identifier: Token,
    pub initializer: ASTExpression,
}

pub struct ASTStatement {
    kind: ASTStatementKind,
}

impl ASTStatement {
    pub fn new(kind: ASTStatementKind) -> Self {
        ASTStatement { kind }
    }

    pub fn expression(expr: ASTExpression) -> Self {
        ASTStatement::new(ASTStatementKind::Expression(expr))
    }

    pub fn let_statement(identifier: Token, initializer: ASTExpression) -> Self {
        ASTStatement::new(ASTStatementKind::LetStatement(ASTLetStatement { identifier, initializer }))
    }
}


pub enum ASTExpressionKind {
    Number(
        ASTNumberExpression
    ),
    Binary(
        ASTBinaryExpression
    ),
    Unary(
        ASTUnaryExpression
    ),
    Parenthesized(
        ASTParenthesizedExpression
    ),

    Variable(
        ASTVariableExpression
    ),
    Error(
        TextSpan
    ),
}

pub enum ASTUnaryOperatorKind {
    Minus,
    BitwiseNot,
}
pub struct ASTUnaryOperator {
    kind: ASTUnaryOperatorKind,
    token: Token,
}
impl ASTUnaryOperator {
    pub fn new(kind: ASTUnaryOperatorKind, token: Token) -> Self {
        ASTUnaryOperator { kind, token }
    }
}

pub struct ASTUnaryExpression {
    pub operator: ASTUnaryOperator,
    pub operand: Box<ASTExpression>,
}

pub struct ASTVariableExpression {
    pub identifier: Token,
}

impl ASTVariableExpression {
    pub fn identifier(&self) -> &str {
        &self.identifier.span.literal
    }

 }


#[derive(Debug)]
pub enum ASTBinaryOperatorKind {
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

pub struct ASTBinaryOperator {
    kind: ASTBinaryOperatorKind,
    token: Token,
}

impl ASTBinaryOperator {
    pub fn new(kind: ASTBinaryOperatorKind, token: Token) -> Self {
        ASTBinaryOperator { kind, token }
    }

    pub fn precedence(&self) -> u8 {
        match self.kind {
            ASTBinaryOperatorKind::Power => 20,
            ASTBinaryOperatorKind::Multiply => 19,
            ASTBinaryOperatorKind::Divide => 19,
            ASTBinaryOperatorKind::Plus => 18,
            ASTBinaryOperatorKind::Minus => 18,
            ASTBinaryOperatorKind::BitwiseAnd => 17,
            ASTBinaryOperatorKind::BitwiseXor => 16,
            ASTBinaryOperatorKind::BitwiseOr => 15,
        }
    }
}

pub struct ASTBinaryExpression {
    left: Box<ASTExpression>,
    operator: ASTBinaryOperator,
    right: Box<ASTExpression>,
}

pub struct ASTNumberExpression {
    number: i64,
}

pub struct ASTParenthesizedExpression {
    expression: Box<ASTExpression>,
}

pub struct ASTExpression {
    kind: ASTExpressionKind,
}

impl ASTExpression {
    pub fn new(kind: ASTExpressionKind) -> Self {
        ASTExpression { kind }
    }

    pub fn number(number: i64) -> Self {
        ASTExpression::new(ASTExpressionKind::Number(ASTNumberExpression { number }))
    }

    pub fn binary(operator: ASTBinaryOperator, left: ASTExpression, right: ASTExpression) -> Self {
        ASTExpression::new(ASTExpressionKind::Binary(ASTBinaryExpression { left: Box::new(left), operator, right: Box::new(right) }))
    }

    pub fn parenthesized(expression: ASTExpression) -> Self {
        ASTExpression::new(ASTExpressionKind::Parenthesized(ASTParenthesizedExpression { expression: Box::new(expression) }))
    }

    pub fn identifier(identifier: Token) -> Self {
        ASTExpression::new(ASTExpressionKind::Variable(ASTVariableExpression { identifier }))
    }

    pub fn unary(operator: ASTUnaryOperator, operand: ASTExpression) -> Self {
        ASTExpression::new(ASTExpressionKind::Unary(ASTUnaryExpression { operator, operand: Box::new(operand) }))
    }

    pub fn error(span: TextSpan) -> Self {
        ASTExpression::new(ASTExpressionKind::Error(span))
    }
}

#[cfg(test)]
mod test {
    use crate::ast::{Ast, ASTBinaryExpression, ASTLetStatement, ASTNumberExpression, ASTParenthesizedExpression, ASTUnaryExpression, ASTVariableExpression, ASTVisitor};
    use crate::ast::lexer::TextSpan;
    use crate::compilation_unit::CompilationUnit;

    #[derive(Debug, PartialEq, Eq)]
    enum TestASTNode {
        Number(i64),
        Binary,
        Unary,
        Parenthesized,
        LetStmt,
        Variable(String),

    }

    struct ASTVerifier {
        expected: Vec<TestASTNode>,
        actual: Vec<TestASTNode>,
    }

    impl ASTVerifier {
        pub fn new(input: &str, expected: Vec<TestASTNode>) -> Self {
            let compilation_unit = CompilationUnit::compile(input);
            assert_eq!(compilation_unit.diagnostics_bag.borrow().diagnostics.len(), 0, "Expected no diagnostics, but got {:?}", compilation_unit.diagnostics_bag.borrow().diagnostics);
            let mut verifier = ASTVerifier { expected, actual: Vec::new() };
            verifier.flatten_ast(&compilation_unit.ast);
            verifier
        }

        fn flatten_ast(&mut self, ast: &Ast)  {
            self.actual.clear();
            ast.visit(&mut *self);
        }

        pub fn verify(&self) {

            assert_eq!(self.expected.len(), self.actual.len(), "Expected {} nodes, but got {}. Actual nodes: {:?}", self.expected.len(), self.actual.len(), self.actual);

            for (index, (expected, actual)) in self.expected.iter().zip(
                self.actual.iter()
            ).enumerate() {

                assert_eq!(expected, actual, "Expected {:?} at index {}, but got {:?}", expected, index, actual);

            }

        }
    }

    impl ASTVisitor for ASTVerifier {
        fn visit_let_statement(&mut self, let_statement: &ASTLetStatement) {
            self.actual.push(TestASTNode::LetStmt);
            self.visit_expression(&let_statement.initializer);
        }

        fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression) {
            self.actual.push(TestASTNode::Variable(
                variable_expression.identifier().to_string()
            ));
        }

        fn visit_number_expression(&mut self, number: &ASTNumberExpression) {
            self.actual.push(TestASTNode::Number(number.number));
        }

        fn visit_error(&mut self, span: &TextSpan) {
            // do nothing
        }

        fn visit_unary_expression(&mut self, unary_expression: &ASTUnaryExpression) {
            self.actual.push(TestASTNode::Unary);
            self.visit_expression(&unary_expression.operand);
        }

        fn visit_binary_expression(&mut self, binary_expression: &ASTBinaryExpression) {
            self.actual.push(TestASTNode::Binary);
            self.visit_expression(&binary_expression.left);
            self.visit_expression(&binary_expression.right);
        }

        fn visit_parenthesized_expression(&mut self, parenthesized_expression: &ASTParenthesizedExpression) {
            self.actual.push(TestASTNode::Parenthesized);
            self.visit_expression(&parenthesized_expression.expression);
        }
    }


    fn assert_tree(input: &str, expected: Vec<TestASTNode>) {
        let verifier = ASTVerifier::new(input, expected);
        verifier.verify();
    }

    #[test]
    pub fn should_parse_basic_binary_expression() {
        let input = "let a = 1 + 2";
        let expected = vec![
            TestASTNode::LetStmt,
            TestASTNode::Binary,
            TestASTNode::Number(1),
            TestASTNode::Number(2),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_parenthesized_binary_expression() {
        let input = "let a = (1 + 2) * 3";
        let expected = vec![
            TestASTNode::LetStmt,
            TestASTNode::Binary,
            TestASTNode::Parenthesized,
            TestASTNode::Binary,
            TestASTNode::Number(1),
            TestASTNode::Number(2),
            TestASTNode::Number(3),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_parenthesized_binary_expression_with_variable() {
        let input = "\
        let b = 1
        let a = (1 + 2) * b";
        let expected = vec![
            TestASTNode::LetStmt,
            TestASTNode::Number(1),
            TestASTNode::LetStmt,
            TestASTNode::Binary,
            TestASTNode::Parenthesized,
            TestASTNode::Binary,
            TestASTNode::Number(1),
            TestASTNode::Number(2),
            TestASTNode::Variable("b".to_string()),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_parenthesized_binary_expression_with_variable_and_number() {
        let input = "\
        let b = 1
        let a = (1 + 2) * b + 3";
        let expected = vec![
            TestASTNode::LetStmt,
            TestASTNode::Number(1),
            TestASTNode::LetStmt,
            TestASTNode::Binary,
            TestASTNode::Binary,
            TestASTNode::Parenthesized,
            TestASTNode::Binary,
            TestASTNode::Number(1),
            TestASTNode::Number(2),
            TestASTNode::Variable("b".to_string()),
            TestASTNode::Number(3),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_bitwise_and() {
        let input = "let a = 1 & 2";
        let expected = vec![
            TestASTNode::LetStmt,
            TestASTNode::Binary,
            TestASTNode::Number(1),
            TestASTNode::Number(2),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_bitwise_or() {
        let input = "let a = 1 | 2";
        let expected = vec![
            TestASTNode::LetStmt,
            TestASTNode::Binary,
            TestASTNode::Number(1),
            TestASTNode::Number(2),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_bitwise_xor() {
        let input = "let a = 1 ^ 2";
        let expected = vec![
            TestASTNode::LetStmt,
            TestASTNode::Binary,
            TestASTNode::Number(1),
            TestASTNode::Number(2),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_bitwise_not() {
        let input = "let a = ~1";
        let expected = vec![
            TestASTNode::LetStmt,
            TestASTNode::Unary,
            TestASTNode::Number(1),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_negation() {
        let input = "let a = -1";
        let expected = vec![
            TestASTNode::LetStmt,
            TestASTNode::Unary,
            TestASTNode::Number(1),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_power() {
        let input = "let a = 1 ** 2";
        let expected = vec![
            TestASTNode::LetStmt,
            TestASTNode::Binary,
            TestASTNode::Number(1),
            TestASTNode::Number(2),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_hilarious_amount_of_unary_operators() {
        let input = "let a = -1 + -2 * -3 ** ------4";
        let expected = vec![
            TestASTNode::LetStmt,
            TestASTNode::Binary,
            TestASTNode::Unary,
            TestASTNode::Number(1),
            TestASTNode::Binary,
            TestASTNode::Unary,
            TestASTNode::Number(2),
            TestASTNode::Binary,
            TestASTNode::Unary,
            TestASTNode::Number(3),
            TestASTNode::Unary,
            TestASTNode::Unary,
            TestASTNode::Unary,
            TestASTNode::Unary,
            TestASTNode::Unary,
            TestASTNode::Unary,
            TestASTNode::Number(4),
        ];

        assert_tree(input, expected);
    }
}
