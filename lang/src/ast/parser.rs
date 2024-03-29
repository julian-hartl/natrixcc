use std::cell::Cell;

use crate::{
    ast::{
        lexer::{
            Token,
            TokenKind,
        },
        Ast,
        BinOpAssociativity,
        BinOpKind,
        BinOperator,
        Body,
        ElseBranch,
        Expr,
        ExprId,
        FuncDeclParameter,
        FunctionReturnTypeSyntax,
        Item,
        ItemKind,
        StaticTypeAnnotation,
        Stmt,
        StmtId,
        UnOpKind,
        UnOperator,
    },
    compilation_unit::{
        resolve_type_from_string,
        GlobalScope,
    },
    diagnostics::DiagnosticsBagCell,
    typings::Type,
};

#[derive(Debug, Clone)]
pub struct Counter {
    value: Cell<usize>,
}

impl Counter {
    pub fn new() -> Self {
        Self {
            value: Cell::new(0),
        }
    }

    pub fn increment(&self) {
        let current_value = self.value.get();
        self.value.set(current_value + 1);
    }

    pub fn get_value(&self) -> usize {
        self.value.get()
    }
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: Counter,
    diagnostics_bag: DiagnosticsBagCell,
    ast: &'a mut Ast,
    global_scope: &'a mut GlobalScope,
}

impl<'a> Parser<'a> {
    pub fn new(
        tokens: Vec<Token>,
        diagnostics_bag: DiagnosticsBagCell,
        ast: &'a mut Ast,
        global_scope: &'a mut GlobalScope,
    ) -> Self {
        Self {
            tokens: tokens
                .iter()
                .filter(|token| token.kind != TokenKind::Whitespace)
                .map(|token| token.clone())
                .collect(),
            current: Counter::new(),
            diagnostics_bag,
            ast,
            global_scope,
        }
    }

    pub fn parse(&mut self) {
        while let Some(_) = self.next_item().map(|stmt| stmt.id) {}
    }

    fn next_item(&mut self) -> Option<&Item> {
        if self.is_at_end() {
            return None;
        }
        Some(self.parse_item())
    }

    fn is_at_end(&self) -> bool {
        self.current().kind == TokenKind::Eof
    }

    fn parse_item(&mut self) -> &Item {
        return match &self.current().kind {
            TokenKind::Func => self.parse_func_item(),
            _ => {
                let id = self.parse_statement();
                self.ast.item_from_kind(ItemKind::Stmt(id))
            }
        };
    }

    fn parse_func_item(&mut self) -> &Item {
        let func_keyword = self.consume_and_check(TokenKind::Func).clone();
        let identifier = self.consume_and_check(TokenKind::Identifier).clone();
        let parameters = self.parse_optional_parameter_list();
        let return_type = self.parse_optional_return_type();

        let opening_brace = self.consume_and_check(TokenKind::OpenBrace).clone();
        let mut body = Vec::new();
        while self.current().kind != TokenKind::CloseBrace && !self.is_at_end() {
            body.push(self.parse_statement());
        }
        let closing_brace = self.consume_and_check(TokenKind::CloseBrace).clone();

        let declared_parameters = parameters
            .iter()
            .map(|parameter| {
                let ty = resolve_type_from_string(
                    &self.diagnostics_bag,
                    &parameter.type_annotation.type_name,
                );
                self.global_scope.declare_variable(
                    &parameter.identifier.span.literal,
                    ty,
                    false,
                    false,
                )
            })
            .collect();

        let body = Body::new(opening_brace, body.clone(), closing_brace);
        let created_function_idx_result = self.global_scope.create_function(
            identifier.span.literal.clone(),
            body.clone(),
            declared_parameters,
            return_type
                .clone()
                .map(|return_type| {
                    resolve_type_from_string(&self.diagnostics_bag, &return_type.type_name)
                })
                .unwrap_or(Type::Void),
        );
        let function_idx = match created_function_idx_result {
            Ok(created_function_idx) => created_function_idx,
            Err(already_existing_function_idx) => {
                self.diagnostics_bag
                    .borrow_mut()
                    .report_function_already_declared(&identifier);
                already_existing_function_idx
            }
        };
        return self.ast.func_item(
            func_keyword,
            identifier,
            parameters,
            body,
            return_type,
            function_idx,
        );
    }

    fn parse_statement(&mut self) -> StmtId {
        let stmt = match self.current().kind {
            TokenKind::Let => self.parse_let_statement().id,
            TokenKind::While => self.parse_while_statement().id,
            TokenKind::Return => self.parse_return_statement().id,
            _ => self.parse_expression_statement().id,
        };
        self.consume_if(TokenKind::SemiColon);
        stmt
    }

    fn parse_optional_return_type(&mut self) -> Option<FunctionReturnTypeSyntax> {
        if self.current().kind == TokenKind::Arrow {
            let arrow = self.consume_and_check(TokenKind::Arrow).clone();
            let type_name = self.consume_and_check(TokenKind::Identifier).clone();
            return Some(FunctionReturnTypeSyntax::new(arrow, type_name));
        }
        return None;
    }

    fn parse_optional_parameter_list(&mut self) -> Vec<FuncDeclParameter> {
        if self.current().kind != TokenKind::LeftParen {
            return Vec::new();
        }
        self.consume_and_check(TokenKind::LeftParen);
        let mut parameters = Vec::new();
        while self.current().kind != TokenKind::RightParen && !self.is_at_end() {
            parameters.push(FuncDeclParameter {
                identifier: self.consume_and_check(TokenKind::Identifier).clone(),
                type_annotation: self.parse_type_annotation(),
            });
            if self.current().kind == TokenKind::Comma {
                self.consume_and_check(TokenKind::Comma);
            }
        }
        self.consume_and_check(TokenKind::RightParen);
        parameters
    }

    fn parse_return_statement(&mut self) -> &Stmt {
        let return_keyword = self.consume_and_check(TokenKind::Return).clone();
        // todo: allow empty return statements
        let expression = self.parse_expr();
        self.ast.return_statement(return_keyword, Some(expression))
    }

    fn parse_while_statement(&mut self) -> &Stmt {
        let while_keyword = self.consume_and_check(TokenKind::While).clone();
        let condition_expr = self.parse_expr();
        let body = self.parse_body();
        self.ast
            .while_statement(while_keyword, condition_expr, body)
    }

    fn parse_block_expression(&mut self, left_brace: Token) -> &Expr {
        let mut statements = Vec::new();
        while self.current().kind != TokenKind::CloseBrace && !self.is_at_end() {
            statements.push(self.parse_statement());
        }
        let right_brace = self.consume_and_check(TokenKind::CloseBrace).clone();
        self.ast
            .block_expression(left_brace, statements, right_brace)
    }

    fn parse_if_expression(&mut self, if_keyword: Token) -> &Expr {
        let condition_expr = self.parse_expr();
        let then = self.parse_body();
        let else_statement = self.parse_optional_else_statement();
        self.ast
            .if_expr(if_keyword, condition_expr, then, else_statement)
    }

    fn parse_body(&mut self) -> Body {
        let opening_brace = self.consume_and_check(TokenKind::OpenBrace).clone();
        let mut body = Vec::new();
        while self.current().kind != TokenKind::CloseBrace && !self.is_at_end() {
            body.push(self.parse_statement());
        }
        let closing_brace = self.consume_and_check(TokenKind::CloseBrace).clone();
        Body::new(opening_brace, body, closing_brace)
    }

    fn parse_optional_else_statement(&mut self) -> Option<ElseBranch> {
        if self.current().kind == TokenKind::Else {
            let else_keyword = self.consume_and_check(TokenKind::Else).clone();
            let else_expr = self.parse_body();
            return Some(ElseBranch::new(else_keyword, else_expr));
        }
        return None;
    }

    fn parse_let_statement(&mut self) -> &Stmt {
        self.consume_and_check(TokenKind::Let);
        let identifier = self.consume_and_check(TokenKind::Identifier).clone();
        let optional_type_annotation = self.parse_optional_type_annotation();
        self.consume_and_check(TokenKind::Equals);
        let expr = self.parse_expr();

        self.ast
            .let_statement(identifier, expr, optional_type_annotation)
    }

    fn parse_optional_type_annotation(&mut self) -> Option<StaticTypeAnnotation> {
        if self.current().kind == TokenKind::Colon {
            return Some(self.parse_type_annotation());
        }
        return None;
    }

    fn parse_type_annotation(&mut self) -> StaticTypeAnnotation {
        let colon = self.consume_and_check(TokenKind::Colon).clone();
        let type_name = self.consume_and_check(TokenKind::Identifier).clone();
        return StaticTypeAnnotation::new(colon, type_name);
    }

    fn parse_expression_statement(&mut self) -> &Stmt {
        let expr = self.parse_expr();
        self.ast.expression_statement(expr)
    }

    fn parse_expr(&mut self) -> ExprId {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> ExprId {
        if self.current().kind == TokenKind::Identifier {
            if self.peek(1).kind == TokenKind::Equals {
                let identifier = self.consume_and_check(TokenKind::Identifier).clone();
                let equals = self.consume_and_check(TokenKind::Equals).clone();
                let expr = self.parse_expr();
                return self.ast.assignment_expression(identifier, equals, expr).id;
            }
        }
        return self.parse_binary_expression();
    }

    fn parse_binary_expression(&mut self) -> ExprId {
        let left = self.parse_unary_expression();
        self.parse_binary_expression_recurse(left, 0)
    }

    fn parse_binary_expression_recurse(&mut self, mut left: ExprId, precedence: u8) -> ExprId {
        while let Some(operator) = self.parse_binary_operator() {
            let operator_precedence = operator.precedence();
            if operator_precedence < precedence {
                break;
            }
            self.consume();
            let mut right = self.parse_unary_expression();

            while let Some(inner_operator) = self.parse_binary_operator() {
                let greater_precedence = inner_operator.precedence() > operator.precedence();
                let equal_precedence = inner_operator.precedence() == operator.precedence();
                if !greater_precedence
                    && !(equal_precedence
                        && inner_operator.associativity() == BinOpAssociativity::Right)
                {
                    break;
                }

                right = self.parse_binary_expression_recurse(
                    right,
                    std::cmp::max(operator.precedence(), inner_operator.precedence()),
                );
            }
            left = self.ast.binary_expression(operator, left, right).id;
        }
        left
    }

    fn parse_unary_expression(&mut self) -> ExprId {
        if let Some(operator) = self.parse_unary_operator() {
            self.consume();
            let operand = self.parse_unary_expression();
            return self.ast.unary_expression(operator, operand).id;
        }
        return self.parse_primary_expression();
    }

    fn parse_unary_operator(&mut self) -> Option<UnOperator> {
        let token = self.current();
        let kind = match token.kind {
            TokenKind::Minus => Some(UnOpKind::Minus),
            TokenKind::Tilde => Some(UnOpKind::BitwiseNot),
            _ => None,
        };
        return kind.map(|kind| UnOperator::new(kind, token.clone()));
    }

    fn parse_binary_operator(&mut self) -> Option<BinOperator> {
        let token = self.current();
        let kind = match token.kind {
            TokenKind::Plus => Some(BinOpKind::Plus),
            TokenKind::Minus => Some(BinOpKind::Minus),
            TokenKind::Asterisk => Some(BinOpKind::Multiply),
            TokenKind::Slash => Some(BinOpKind::Divide),
            TokenKind::Ampersand => Some(BinOpKind::BitwiseAnd),
            TokenKind::Pipe => Some(BinOpKind::BitwiseOr),
            TokenKind::Caret => Some(BinOpKind::BitwiseXor),
            TokenKind::DoubleAsterisk => Some(BinOpKind::Power),
            TokenKind::EqualsEquals => Some(BinOpKind::Equals),
            TokenKind::BangEquals => Some(BinOpKind::NotEquals),
            TokenKind::LessThan => Some(BinOpKind::LessThan),
            TokenKind::LessThanEquals => Some(BinOpKind::LessThanOrEqual),
            TokenKind::GreaterThan => Some(BinOpKind::GreaterThan),
            TokenKind::GreaterThanEquals => Some(BinOpKind::GreaterThanOrEqual),
            TokenKind::Percent => Some(BinOpKind::Modulo),
            _ => None,
        };
        return kind.map(|kind| BinOperator::new(kind, token.clone()));
    }

    fn parse_primary_expression(&mut self) -> ExprId {
        let token = self.consume().clone();
        return match token.kind {
            TokenKind::OpenBrace => self.parse_block_expression(token),
            TokenKind::If => self.parse_if_expression(token),
            TokenKind::Number(number) => self.ast.number_expression(token, number),
            TokenKind::LeftParen => {
                let expr = self.parse_expr();
                let left_paren = token;
                let right_paren = self.consume_and_check(TokenKind::RightParen).clone();
                self.ast
                    .parenthesized_expression(left_paren, expr, right_paren)
            }
            TokenKind::Identifier => {
                if matches!(self.current().kind, TokenKind::LeftParen) {
                    return self.parse_call_expression(token);
                }
                self.ast.variable_expression(token)
            }
            TokenKind::True | TokenKind::False => {
                let value = token.kind == TokenKind::True;
                self.ast.boolean_expression(token, value)
            }
            _ => {
                self.diagnostics_bag
                    .borrow_mut()
                    .report_expected_expression(&token);
                self.ast.error_expression(token.span)
            }
        }
        .id;
    }

    fn parse_call_expression(&mut self, identifier: Token) -> ExprId {
        let left_paren = self.consume_and_check(TokenKind::LeftParen).clone();
        let mut arguments = Vec::new();
        while self.current().kind != TokenKind::RightParen && !self.is_at_end() {
            arguments.push(self.parse_expr());
            if self.current().kind != TokenKind::RightParen {
                self.consume_and_check(TokenKind::Comma);
            }
        }
        let right_paren = self.consume_and_check(TokenKind::RightParen).clone();
        return self
            .ast
            .call_expression(identifier, left_paren, arguments, right_paren)
            .id;
    }

    fn peek(&self, offset: isize) -> &Token {
        let mut index = (self.current.get_value() as isize + offset) as usize;
        if index >= self.tokens.len() {
            index = self.tokens.len() - 1;
        }
        self.tokens.get(index).unwrap()
    }

    fn current(&self) -> &Token {
        self.peek(0)
    }

    fn consume(&self) -> &Token {
        self.current.increment();
        self.peek(-1)
    }

    fn consume_if(&self, kind: TokenKind) -> Option<&Token> {
        if self.current().kind == kind {
            Some(self.consume())
        } else {
            None
        }
    }

    fn consume_and_check(&self, kind: TokenKind) -> &Token {
        let token = self.consume();
        if token.kind != kind {
            self.diagnostics_bag
                .borrow_mut()
                .report_unexpected_token(&kind, token);
        }
        token
    }
}
