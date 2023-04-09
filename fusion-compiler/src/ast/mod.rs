use termion::color;
use termion::color::{Fg, Reset};
use printer::ASTPrinter;
use visitor::ASTVisitor;

use crate::ast::lexer::{TextSpan, Token};

pub mod lexer;
pub mod parser;
pub mod evaluator;
pub mod visitor;
pub mod printer;

pub struct Ast {
    pub statements: Vec<ASTStatement>,
}

impl  Ast {
    pub fn new() -> Self {
        Self { statements: Vec::new()}
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

#[derive(Debug, Clone)]
pub enum ASTStatementKind {
    Expression(ASTExpression),
    Let(ASTLetStatement),
    If(ASTIfStatement),
    Block(ASTBlockStatement),
    While(ASTWhileStatement),
    FuncDecl(ASTFuncDeclStatement),
    Return(ASTReturnStatement),
}
#[derive(Debug, Clone)]
pub struct ASTReturnStatement {
    pub return_keyword: Token,
    pub return_value: Option<ASTExpression>,
}
#[derive(Debug, Clone)]
pub struct FuncDeclParameter {
    pub identifier: Token,
}
#[derive(Debug, Clone)]
pub struct ASTFuncDeclStatement {
    pub identifier: Token,
    pub parameters: Vec<FuncDeclParameter>,
    pub body: Box<ASTStatement>,
}
#[derive(Debug, Clone)]
pub struct ASTWhileStatement {
    pub while_keyword: Token,
    pub condition: ASTExpression,
    pub body: Box<ASTStatement>,
}
#[derive(Debug, Clone)]
pub struct ASTBlockStatement {
    pub statements: Vec<ASTStatement>,
}
#[derive(Debug, Clone)]
pub struct ASTElseStatement {
    pub else_keyword: Token,
    pub else_statement: Box<ASTStatement>,
}

impl ASTElseStatement {
    pub fn new(else_keyword: Token, else_statement: ASTStatement) -> Self {
        ASTElseStatement { else_keyword, else_statement: Box::new(else_statement) }
    }
}
#[derive(Debug, Clone)]
pub struct ASTIfStatement {
    pub if_keyword: Token,
    pub condition: ASTExpression,
    pub then_branch: Box<ASTStatement>,
    pub else_branch: Option<ASTElseStatement>,
}
#[derive(Debug, Clone)]
pub struct ASTLetStatement {
    pub identifier: Token,
    pub initializer: ASTExpression,
}

#[derive(Debug, Clone)]
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
        ASTStatement::new(ASTStatementKind::Let(ASTLetStatement { identifier, initializer }))
    }

    pub fn if_statement(if_keyword: Token, condition: ASTExpression, then: ASTStatement, else_statement: Option<ASTElseStatement>) -> Self {
        ASTStatement::new(ASTStatementKind::If(ASTIfStatement { if_keyword, condition, then_branch: Box::new(then), else_branch: else_statement }))
    }

    pub fn block_statement(statements: Vec<ASTStatement>) -> Self {
        ASTStatement::new(ASTStatementKind::Block(ASTBlockStatement { statements }))
    }

    pub fn while_statement(while_keyword: Token, condition: ASTExpression, body: ASTStatement) -> Self {
        ASTStatement::new(ASTStatementKind::While(ASTWhileStatement { while_keyword, condition, body: Box::new(body) }))
    }

    pub fn return_statement(return_keyword: Token, return_value: Option<ASTExpression>) -> Self {
        ASTStatement::new(ASTStatementKind::Return(ASTReturnStatement { return_keyword, return_value }))
    }

    pub fn func_decl_statement(identifier: Token, parameters: Vec<FuncDeclParameter>, body: ASTStatement) -> Self {
        ASTStatement::new(ASTStatementKind::FuncDecl(ASTFuncDeclStatement { identifier, parameters, body: Box::new(body) }))
    }

}

#[derive(Debug, Clone)]
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
    Assignment(
        ASTAssignmentExpression
    ),
    Boolean(
        ASTBooleanExpression
    ),
    Call(
        ASTCallExpression
    ),
    Error(
        TextSpan
    ),
}
#[derive(Debug, Clone)]
pub struct ASTCallExpression {
    pub identifier: Token,
    pub arguments: Vec<ASTExpression>,
}
#[derive(Debug, Clone)]
pub struct ASTBooleanExpression {
    pub value: bool,
    pub token: Token,
}
#[derive(Debug, Clone)]
pub struct ASTAssignmentExpression {
    pub identifier: Token,
    pub expression: Box<ASTExpression>,
}
#[derive(Debug, Clone)]
pub enum ASTUnaryOperatorKind {
    Minus,
    BitwiseNot,
}
#[derive(Debug, Clone)]
pub struct ASTUnaryOperator {
    kind: ASTUnaryOperatorKind,
    token: Token,
}

impl ASTUnaryOperator {
    pub fn new(kind: ASTUnaryOperatorKind, token: Token) -> Self {
        ASTUnaryOperator { kind, token }
    }
}
#[derive(Debug, Clone)]
pub struct ASTUnaryExpression {
    pub operator: ASTUnaryOperator,
    pub operand: Box<ASTExpression>,
}
#[derive(Debug, Clone)]
pub struct ASTVariableExpression {
    pub identifier: Token,
}

impl ASTVariableExpression {
    pub fn identifier(&self) -> &str {
        &self.identifier.span.literal
    }
}


#[derive(Debug, Clone)]
pub enum ASTBinaryOperatorKind {
    // Arithmetic
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
    // Bitwise
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    // Relational
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}
#[derive(Debug, Clone)]
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
            ASTBinaryOperatorKind::Equals => 30,
            ASTBinaryOperatorKind::NotEquals => 30,
            ASTBinaryOperatorKind::LessThan => 29,
            ASTBinaryOperatorKind::LessThanOrEqual => 29,
            ASTBinaryOperatorKind::GreaterThan => 29,
            ASTBinaryOperatorKind::GreaterThanOrEqual => 29,
        }
    }
}
#[derive(Debug, Clone)]
pub struct ASTBinaryExpression {
    left: Box<ASTExpression>,
    operator: ASTBinaryOperator,
    right: Box<ASTExpression>,
}
#[derive(Debug, Clone)]
pub struct ASTNumberExpression {
    number: i64,
}
#[derive(Debug, Clone)]
pub struct ASTParenthesizedExpression {
    expression: Box<ASTExpression>,
}
#[derive(Debug, Clone)]
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

    pub fn assignment(identifier: Token, expression: ASTExpression) -> Self {
        ASTExpression::new(ASTExpressionKind::Assignment(ASTAssignmentExpression { identifier, expression: Box::new(expression) }))
    }

    pub fn boolean(token: Token, value: bool) -> Self {
        ASTExpression::new(ASTExpressionKind::Boolean(ASTBooleanExpression { token, value }))
    }

    pub fn call(identifier: Token, arguments: Vec<ASTExpression>) -> Self {
        ASTExpression::new(ASTExpressionKind::Call(ASTCallExpression { identifier, arguments }))
    }

    pub fn error(span: TextSpan) -> Self {
        ASTExpression::new(ASTExpressionKind::Error(span))
    }
}

#[cfg(test)]
mod test {
    use super::visitor::ASTVisitor;
    use crate::ast::{Ast, ASTAssignmentExpression, ASTBinaryExpression, ASTBlockStatement, ASTBooleanExpression, ASTCallExpression, ASTFuncDeclStatement, ASTIfStatement, ASTLetStatement, ASTNumberExpression, ASTParenthesizedExpression, ASTReturnStatement, ASTUnaryExpression, ASTVariableExpression, ASTWhileStatement};
    use crate::ast::lexer::TextSpan;
    use crate::compilation_unit::CompilationUnit;

    #[derive(Debug, PartialEq, Eq)]
    enum TestASTNode {
        Number(i64),
        Boolean(bool),
        Binary,
        Unary,
        Parenthesized,
        Let,
        Assignment,
        Block,
        Variable(String),
        If,
        Else,
        Func,
        While,
        Return,
        Call,
    }

    struct ASTVerifier {
        expected: Vec<TestASTNode>,
        actual: Vec<TestASTNode>,
    }

    impl ASTVerifier {
        pub fn new(input: &str, expected: Vec<TestASTNode>) -> Self {
            let compilation_unit = CompilationUnit::compile(input).expect("Failed to compile");
            let mut verifier = ASTVerifier { expected, actual: Vec::new() };
            verifier.flatten_ast(&compilation_unit.ast);
            verifier
        }

        fn flatten_ast(&mut self, ast: &Ast) {
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

    impl ASTVisitor<'_> for ASTVerifier {
        fn visit_func_decl_statement(&mut self, func_decl_statement: &ASTFuncDeclStatement) {
            self.actual.push(TestASTNode::Func);
            self.visit_statement(&func_decl_statement.body);
        }

        fn visit_return_statement(&mut self, return_statement: &ASTReturnStatement) {
            self.actual.push(TestASTNode::Return);
            if let Some(expression) = &return_statement.return_value {
                self.visit_expression(expression);
            }
        }

        fn visit_while_statement(&mut self, while_statement: &ASTWhileStatement) {
            self.actual.push(TestASTNode::While);
            self.visit_expression(&while_statement.condition);
            self.visit_statement(&while_statement.body);
        }

        fn visit_block_statement(&mut self, block_statement: &ASTBlockStatement) {
            self.actual.push(TestASTNode::Block);
            for statement in &block_statement.statements {
                self.visit_statement(statement);
            }
        }

        fn visit_if_statement(&mut self, if_statement: &ASTIfStatement) {
            self.actual.push(TestASTNode::If);
            self.visit_expression(&if_statement.condition);
            self.visit_statement(&if_statement.then_branch);
            if let Some(else_branch) = &if_statement.else_branch {
                self.actual.push(TestASTNode::Else);

                self.visit_statement(&else_branch.else_statement);
            }
        }

        fn visit_let_statement(&mut self, let_statement: &ASTLetStatement) {
            self.actual.push(TestASTNode::Let);
            self.visit_expression(&let_statement.initializer);
        }

        fn visit_assignment_expression(&mut self, assignment_expression: &ASTAssignmentExpression) {
            self.actual.push(TestASTNode::Assignment);
            self.visit_expression(&assignment_expression.expression);
        }

        fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression) {
            self.actual.push(TestASTNode::Variable(
                variable_expression.identifier().to_string()
            ));
        }

        fn visit_number_expression(&mut self, number: &ASTNumberExpression) {
            self.actual.push(TestASTNode::Number(number.number));
        }

        fn visit_boolean_expression(&mut self, boolean: &ASTBooleanExpression) {
            self.actual.push(TestASTNode::Boolean(boolean.value));
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

        fn visit_call_expression(&mut self, call_expression: &ASTCallExpression) {
            self.actual.push(TestASTNode::Call);
            for argument in &call_expression.arguments {
                self.visit_expression(argument);
            }
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
            TestASTNode::Let,
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
            TestASTNode::Let,
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
            TestASTNode::Let,
            TestASTNode::Number(1),
            TestASTNode::Let,
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
            TestASTNode::Let,
            TestASTNode::Number(1),
            TestASTNode::Let,
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
            TestASTNode::Let,
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
            TestASTNode::Let,
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
            TestASTNode::Let,
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
            TestASTNode::Let,
            TestASTNode::Unary,
            TestASTNode::Number(1),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_negation() {
        let input = "let a = -1";
        let expected = vec![
            TestASTNode::Let,
            TestASTNode::Unary,
            TestASTNode::Number(1),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_power() {
        let input = "let a = 1 ** 2";
        let expected = vec![
            TestASTNode::Let,
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
            TestASTNode::Let,
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

    #[test]
    pub fn should_parse_if_statement() {
        let input = "\
        let a = 1
        if a > 0 {
            a = 20
        }
        ";
        let expected = vec![
            TestASTNode::Let,
            TestASTNode::Number(1),
            TestASTNode::If,
            TestASTNode::Binary,
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Number(0),
            TestASTNode::Block,
            TestASTNode::Assignment,
            TestASTNode::Number(20),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_if_statement_with_else() {
        let input = "\
        let a = 1
        if a > 0 {
            a = 20
        } else {
            a = 30
        }
        ";
        let expected = vec![
            TestASTNode::Let,
            TestASTNode::Number(1),
            TestASTNode::If,
            TestASTNode::Binary,
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Number(0),
            TestASTNode::Block,
            TestASTNode::Assignment,
            TestASTNode::Number(20),
            TestASTNode::Else,
            TestASTNode::Block,
            TestASTNode::Assignment,
            TestASTNode::Number(30),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_while_statement() {
        let input = "\
        let a = 1
        while a < 10 {
            a = a + 1
        }
        ";
        let expected = vec![
            TestASTNode::Let,
            TestASTNode::Number(1),
            TestASTNode::While,
            TestASTNode::Binary,
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Number(10),
            TestASTNode::Block,
            TestASTNode::Assignment,
            TestASTNode::Binary,
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Number(1),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_function_declaration() {
        let input = "\
        func add(a, b) {
            return a + b
        }
        ";
        let expected = vec![
            TestASTNode::Func,
            TestASTNode::Block,
            TestASTNode::Return,
            TestASTNode::Binary,
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Variable("b".to_string()),
        ];

        assert_tree(input, expected);
    }

    #[test]
    pub fn should_parse_call_expression() {
        let input = "\
        func add(a, b) {
            return a + b
        }
        add(2 * 3, 4 + 5)";
        let expected = vec![
            TestASTNode::Func,
            TestASTNode::Block,
            TestASTNode::Return,
            TestASTNode::Binary,
            TestASTNode::Variable("a".to_string()),
            TestASTNode::Variable("b".to_string()),
            TestASTNode::Call,
            TestASTNode::Binary,
            TestASTNode::Number(2),
            TestASTNode::Number(3),
            TestASTNode::Binary,
            TestASTNode::Number(4),
            TestASTNode::Number(5),
        ];

        assert_tree(input, expected);
    }
}
