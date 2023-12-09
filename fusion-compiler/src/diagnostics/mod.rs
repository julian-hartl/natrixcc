use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::lexer::{Token, TokenKind};
use crate::text::span::TextSpan;
use crate::typings::Type;

pub mod printer;

#[derive(Clone, Copy, Debug)]
pub enum DiagnosticKind {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub message: String,
    pub span: TextSpan,
    pub kind: DiagnosticKind,
}

impl Diagnostic {
    pub fn new(message: String, span: TextSpan, kind: DiagnosticKind) -> Self {
        Diagnostic {
            message,
            span,
            kind,
        }
    }
}

pub type DiagnosticsBagCell = Rc<RefCell<DiagnosticsBag>>;

#[derive(Debug)]
pub struct DiagnosticsBag {
    pub diagnostics: Vec<Diagnostic>,
}

impl DiagnosticsBag {
    pub fn new() -> Self {
        DiagnosticsBag {
            diagnostics: vec![],
        }
    }

    pub fn report_error(&mut self, message: String, span: TextSpan) {
        let error = Diagnostic::new(message, span, DiagnosticKind::Error);
        self.diagnostics.push(error);
    }

    pub fn report_warning(&mut self, message: String, span: TextSpan) {
        let warning = Diagnostic::new(message, span, DiagnosticKind::Warning);
        self.diagnostics.push(warning);
    }

    pub fn report_unexpected_token(&mut self, expected: &TokenKind, token: &Token) {
        self.report_error(
            format!("Expected <{}>, found <{}>", expected, token.kind),
            token.span.clone(),
        );
    }
    pub fn report_expected_expression(&mut self, token: &Token) {
        self.report_error(
            format!("Expected expression, found <{}>", token.kind),
            token.span.clone(),
        );
    }

    pub fn report_undeclared_variable(&mut self, token: &Token) {
        self.report_error(
            format!("Undeclared variable '{}'", token.span.literal),
            token.span.clone(),
        );
    }

    pub fn report_undeclared_function(&mut self, token: &Token) {
        self.report_error(
            format!("Undeclared function '{}'", token.span.literal),
            token.span.clone(),
        );
    }

    pub fn report_cannot_call_no_callable_expression(
        &mut self,
        callee_span: &TextSpan,
        callee_type: &Type,
    ) {
        self.report_error(
            format!(
                "Cannot call non-callable expression of type '{}'",
                callee_type
            ),
            callee_span.clone(),
        );
    }

    pub fn report_invalid_argument_count(
        &mut self,
        callee_span: &TextSpan,
        expected: usize,
        actual: usize,
    ) {
        self.report_error(
            format!(
                "Function '{}' expects {} {}, but was given {}",
                callee_span.literal,
                expected,
                if expected == 1 {
                    "argument"
                } else {
                    "arguments"
                },
                actual
            ),
            callee_span.clone(),
        );
    }

    pub fn report_function_already_declared(&mut self, token: &Token) {
        self.report_error(
            format!("Function '{}' already declared", token.span.literal),
            token.span.clone(),
        );
    }

    pub fn report_type_mismatch(&mut self, span: &TextSpan, expected: &Type, actual: &Type) {
        self.report_error(
            format!("Expected type '{}', found '{}'", expected, actual),
            span.clone(),
        );
    }

    pub fn report_undeclared_type(&mut self, token: &Token) {
        self.report_error(
            format!("Undeclared type '{}'", token.span.literal),
            token.span.clone(),
        );
    }

    pub fn report_cannot_return_outside_function(&mut self, token: &Token) {
        self.report_error(
            format!("Cannot use 'return' outside of function"),
            token.span.clone(),
        );
    }

    pub fn report_cannot_use_rec_outside_of_function(&mut self, token: &Token) {
        self.report_error(
            format!("Cannot use 'rec' outside of function"),
            token.span.clone(),
        );
    }
}

#[cfg(test)]
mod test {
    use crate::compilation_unit::CompilationUnit;
    use crate::diagnostics::{Diagnostic, DiagnosticKind};
    use crate::text::span::TextSpan;

    struct DiagnosticsVerifier {
        actual: Vec<Diagnostic>,
        expected: Vec<Diagnostic>,
    }

    impl DiagnosticsVerifier {
        pub fn new(input: &str, messages: Vec<&str>) -> Self {
            let messages_len = messages.len();
            let expected = Self::parse_input(input, messages);
            assert_eq!(expected.len(), messages_len);
            let actual = Self::compile(input);
            Self { expected, actual }
        }

        fn compile(input: &str) -> Vec<Diagnostic> {
            let raw = Self::get_raw_text(input);
            let compilation_unit = CompilationUnit::compile(&raw);
            match compilation_unit {
                Ok(_) => vec![],
                Err(e) => e.borrow().diagnostics.clone(),
            }
        }

        fn get_raw_text(input: &str) -> String {
            input.replace("«", "").replace("»", "")
        }

        fn parse_input(input: &str, messages: Vec<&str>) -> Vec<Diagnostic> {
            let raw_text = Self::get_raw_text(input);
            let mut start_index_stack = vec![];

            let mut current_position: usize = 0;
            let mut diagnostics = vec![];
            for c in input.chars() {
                match c {
                    '«' => {
                        start_index_stack.push(current_position);
                    }
                    '»' => {
                        let start_index = start_index_stack.pop().unwrap();
                        let end_index = current_position;
                        let literal = &raw_text[start_index..end_index];
                        let span = TextSpan::new(start_index, end_index, literal.to_string());
                        let message = messages[diagnostics.len()].to_string();
                        let diagnostic = Diagnostic::new(message, span, DiagnosticKind::Error);
                        diagnostics.push(diagnostic);
                    }
                    _ => {
                        current_position += 1;
                    }
                };
            }

            diagnostics
        }

        fn verify(&self) {
            assert_eq!(
                self.actual.len(),
                self.expected.len(),
                "Expected {} diagnostics, found {}",
                self.expected.len(),
                self.actual.len()
            );

            for (actual, expected) in self.actual.iter().zip(self.expected.iter()) {
                assert_eq!(
                    actual.message, expected.message,
                    "Expected message '{}', found '{}'",
                    expected.message, actual.message
                );
                assert_eq!(
                    actual.span.start, expected.span.start,
                    "Expected start index {}, found {}",
                    expected.span.start, actual.span.start
                );
                assert_eq!(
                    actual.span.end, expected.span.end,
                    "Expected end index {}, found {}",
                    expected.span.end, actual.span.end
                );
                assert_eq!(
                    actual.span.literal, expected.span.literal,
                    "Expected literal '{}', found '{}'",
                    expected.span.literal, actual.span.literal
                );
            }
        }
    }

    fn assert_diagnostics(input: &str, expected: Vec<&str>) {
        let verifier = DiagnosticsVerifier::new(input, expected);
        verifier.verify();
    }

    #[test]
    fn should_report_undeclared_variable() {
        let input = "let a = «b»";
        let expected = vec!["Undeclared variable 'b'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    fn should_report_expected_expression() {
        let input = "let a = «+»";
        let expected = vec!["Expected expression, found <+>"];

        assert_diagnostics(input, expected);
    }

    #[test]
    fn should_report_bad_token() {
        let input = "let a = 8 «@» 2";
        let expected = vec!["Expected expression, found <Bad>"];

        assert_diagnostics(input, expected);
    }

    #[test]
    fn should_report_undeclared_variable_when_variable_was_declared_in_another_scope() {
        let input = "\
        let a = 0
        let b = -1
        if b > a {
            a = 10
           b = 2
            let c = 10
        }
         else
            a = 5
        a
b
«c»
    ";
        let expected = vec!["Undeclared variable 'c'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    fn should_not_report_any_errors_when_shadowing_variable() {
        let input = "\
        let a = 0
        {
            let a = 10
        }
    ";
        let expected = vec![];

        assert_diagnostics(input, expected);
    }

    #[test]
    fn should_report_undeclared_variable_when_variable_was_declared_in_if_without_block() {
        let input = "\
        let b = -1
        if b > 10 {
            let a = 10
        }
        «a»
    ";
        let expected = vec!["Undeclared variable 'a'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    fn should_report_function_already_declared() {
        let input = "\
        func a {}
        func «a» {}
    ";

        let expected = vec!["Function 'a' already declared"];

        assert_diagnostics(input, expected);
    }

    #[test]
    fn should_report_error_when_calling_undeclared_function() {
        let input = "\
        «a»()
    ";

        let expected = vec!["Undeclared function 'a'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_error_when_function_is_called_with_wrong_number_of_arguments() {
        let input = "\
        func a(a: int, b: int) {}
        «a»(1)
    ";

        let expected = vec!["Function 'a' expects 2 arguments, but was given 1"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_type_mismatch_when_int_is_used_in_if_condition() {
        let input = "\
        if «1» {
            let a = 10
        }
    ";

        let expected = vec!["Expected type 'bool', found 'int'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_type_mismatch_when_variable_of_type_int_is_used_in_if_condition() {
        let input = "\
        let a = 1
        if «a» {
            let a = 10
        }
    ";

        let expected = vec!["Expected type 'bool', found 'int'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_type_mismatch_when_binary_expression_of_type_int_is_used_in_if_condition()
    {
        let input = "\
        let a = 1
        if «a + 1» {
            let a = 10
        }
    ";

        let expected = vec!["Expected type 'bool', found 'int'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_type_mismatch_when_adding_int_with_bool() {
        let input = "\
        let a = 1
        a + «true»
    ";

        let expected = vec!["Expected type 'int', found 'bool'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_type_mismatch_when_using_minus_unary_operator_on_bool() {
        let input = "\
        let a = true;
        -«a»
    ";

        let expected = vec!["Expected type 'int', found 'bool'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_tm_when_assigning_function_call_result_to_variable_of_another_type() {
        let input = "\
        let b = false
        b = «a()»
        func a -> int {
            return 1
        }
        ";

        let expected = vec!["Expected type 'bool', found 'int'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_tm_when_using_binop_on_incompatible_types_in_function_params() {
        let input = "\
        func a(a: int, b: bool) {
            a + «b»
        }
        ";

        let expected = vec!["Expected type 'int', found 'bool'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_wrong_return_type_when_function_returns_incompatible_type() {
        let input = "\
        func a -> int {
            return «true»
        }
        ";

        let expected = vec!["Expected type 'int', found 'bool'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_tm_when_assigning_wrong_type_to_variable_with_static_type() {
        let input = "\
        let a: int = «true»
        ";

        let expected = vec!["Expected type 'int', found 'bool'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_cannot_use_return_outside_of_function() {
        let input = "\
        «return» 2
        ";

        let expected = vec!["Cannot use 'return' outside of function"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_not_allow_addition_with_void_type() {
        let input = "\
        let a = 1
        a + «a()»
        func a {}
        ";

        let expected = vec!["Expected type 'int', found 'void'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_undeclared_type_in_let_assignment() {
        let input = "\
        let a: «b» = 1
        ";

        let expected = vec!["Undeclared type 'b'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_undeclared_type_in_function_return_type() {
        let input = "\
        func a -> «b» {}
        ";

        let expected = vec!["Undeclared type 'b'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_undeclared_type_in_function_param_type() {
        let input = "\
        func a(a: «b») {}
        ";

        let expected = vec!["Undeclared type 'b'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_tm_in_arguments_in_function_call() {
        let input = "\
        func a(a: int) {}
        a(«true»)
        ";

        let expected = vec!["Expected type 'int', found 'bool'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_error_when_returning_value_from_void_function() {
        let input = "\
        func a() {
            return «1»
        }
        ";

        let expected = vec!["Expected type 'void', found 'int'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_error_when_using_return_without_expression_in_non_void_function() {
        let input = "\
        func a() -> int {
            «return»
        }
        ";

        let expected = vec!["Expected type 'int', found 'void'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_report_error_when_assigning_to_undeclared_variable() {
        let input = "\
        «a» = 1
        ";

        let expected = vec!["Undeclared variable 'a'"];

        assert_diagnostics(input, expected);
    }

    #[test]
    pub fn should_not_allow_non_bool_types_in_while_condition() {
        let input = "\
        let a = add(1, 2)
        func add(a: int, b: int) -> int {
            return a + b
        }
        while «a + 1» {
            a = a + 1
        }

    ";

        let expected = vec!["Expected type 'bool', found 'int'"];

        assert_diagnostics(input, expected);
    }
}
