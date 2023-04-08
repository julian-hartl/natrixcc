use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;
use crate::ast::{Ast, ASTBinaryExpression, ASTLetStatement, ASTNumberExpression, ASTParenthesizedExpression, ASTVariableExpression, ASTVisitor};
use crate::ast::evaluator::ASTEvaluator;
use crate::ast::lexer::{Lexer, TextSpan};
use crate::ast::parser::Parser;
use crate::diagnostics::DiagnosticsBagCell;
use crate::diagnostics::printer::DiagnosticsPrinter;
use crate::text::SourceText;

mod ast;
mod diagnostics;
mod text;

struct SymbolChecker {
    symbols: HashMap<String, ()>,
    diagnostics: DiagnosticsBagCell,
}

impl SymbolChecker {
    fn new(diagnostics: DiagnosticsBagCell) -> Self {
        SymbolChecker {
            symbols: HashMap::new(),
            diagnostics,
        }
    }
}

impl ASTVisitor for SymbolChecker {
    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement) {
        let identifier = let_statement.identifier.span.literal.clone();
        self.visit_expression(&let_statement.initializer);
        self.symbols.insert(identifier, ());
    }

    fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression) {
        if self.symbols.get(&variable_expression.identifier.span.literal).is_none() {

            let mut diagnostics_binding = self.diagnostics.borrow_mut();
            diagnostics_binding.report_undeclared_variable(
                &variable_expression.identifier,
            );
        }
    }

    fn visit_number_expression(&mut self, number: &ASTNumberExpression) {

    }

    fn visit_error(&mut self, span: &TextSpan) {

    }
}

fn main() -> Result<(),()> {
    let input = "
        let a =10+30
        let b = 20
        let d = 10 + e
        let c = (a + b) * d
    ";
    let text = text::SourceText::new(input.to_string());
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next_token() {
        tokens.push(token);
    }
    let diagnostics_bag: DiagnosticsBagCell = Rc::new(RefCell::new(diagnostics::DiagnosticsBag::new()));
    let mut ast: Ast = Ast::new();
    let mut parser = Parser::new(
        tokens,
        Rc::clone(&diagnostics_bag)
    );
    while let Some(stmt) = parser.next_statement() {
        ast.add_statement(stmt);
    }
    ast.visualize();
    check_diagnostics(&text, &diagnostics_bag)?;
    let mut symbol_checker = SymbolChecker::new(Rc::clone(&diagnostics_bag));
    ast.visit(&mut symbol_checker);
    check_diagnostics(&text, &diagnostics_bag)?;
    let mut eval = ASTEvaluator::new();
    ast.visit(&mut eval);
    println!("Result: {:?}", eval.last_value);
    Ok(())
}

fn check_diagnostics(text: &SourceText, diagnostics_bag: &DiagnosticsBagCell) -> Result<(),()> {
    let diagnostics_binding = diagnostics_bag.borrow();
    if diagnostics_binding.diagnostics.len() > 0 {
        let diagnostics_printer = DiagnosticsPrinter::new(
            &text,
            &diagnostics_binding.diagnostics
        );
        diagnostics_printer.print();
        return Err(());
    }
    Ok(())
}
