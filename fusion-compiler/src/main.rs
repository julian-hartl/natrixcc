use crate::ast::Ast;
use crate::ast::evaluator::ASTEvaluator;
use crate::ast::lexer::Lexer;
use crate::ast::parser::Parser;

mod ast;

fn main() {
    let input = "7 - (30 + 7) * 8 / 2";

    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next_token() {
        tokens.push(token);
    }
    println!("{:?}", tokens);
    let mut ast: Ast = Ast::new();
    let mut parser = Parser::new(
        tokens
    );
    while let Some(stmt) = parser.next_statement() {
        ast.add_statement(stmt);
    }
    ast.visualize();
    let mut eval = ASTEvaluator::new();
    ast.visit(&mut eval);
    println!("Result: {:?}", eval.last_value);
}
