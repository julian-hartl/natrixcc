use crate::ast::{AssignExpr, Ast, BinaryExpr, BinOperator, BinOpKind, BlockExpr, BoolExpr, Expr, ExprId, ExprKind, FunctionDeclaration, ItemId, ItemKind, LetStmt, NumberExpr, ParenthesizedExpr, Stmt, StmtId, UnaryExpr, UnOperator, UnOpKind, VarExpr};
use crate::ast::visitor::ASTVisitor;
use crate::compilation_unit::{GlobalScope, VariableIdx};
use crate::text::span::TextSpan;
use crate::typings::Type;

pub struct CTranspiler<'a> {
    pub result: String,
    pub indent: usize,
    pub global_scope: &'a GlobalScope,
    pub l_value_stack: Vec<(VariableIdx, ExprId)>,
}

impl <'a> CTranspiler<'a> {
    pub fn new(global_scope: &'a GlobalScope) -> Self {
        Self {
            result: String::new(),
            indent: 0,
            global_scope,
            l_value_stack: Vec::new(),
        }
    }

    pub fn transpile(mut self, ast: &mut Ast) -> String {
        let items = ast.items.clone();

        for item in items.iter() {
            match &item.kind {
                ItemKind::Stmt(stmt) => {
                }
                ItemKind::Function(function_decl) => {
                    self.visit_func_decl(ast, function_decl, item.id);
                }
            }
        }
        self.result.push_str("int main() {\n");
        self.indent += 1;
        for item in items.iter() {
            match &item.kind {
                ItemKind::Stmt(stmt) => {
                    self.visit_statement(ast, *stmt);
                }
                ItemKind::Function(_) => {}
            }
        }
        self.write_ident();
        self.result.push_str("return 0;\n");
        self.result.push_str("}\n");
        return self.result;
    }

    fn transpile_type(ty: &Type) -> String {
        return match ty {
            Type::Int => "int".to_string(),
            Type::Bool => "int".to_string(),
            Type::Void => "void".to_string(),
            Type::Unresolved => panic!("Unresolved type"),
            Type::Error => panic!("Error type"),
        };
    }

    fn transpile_unary_operator(&self, operator: &UnOperator) -> &'static str {
        return match &operator.kind {
            UnOpKind::Minus => "-",
            UnOpKind::BitwiseNot => "~",
        };
    }

    fn transpile_binary_operator(&self, operator: &BinOperator) -> &'static str {
        return match &operator.kind {
            BinOpKind::Plus => "+",
            BinOpKind::Minus => "-",
            BinOpKind::Multiply => "*",
            BinOpKind::Divide => "/",
            BinOpKind::Equals => "==",
            BinOpKind::NotEquals => "!=",
            BinOpKind::LessThan => "<",
            BinOpKind::GreaterThan => ">",
            BinOpKind::BitwiseAnd => "&",
            BinOpKind::BitwiseOr => "|",
            BinOpKind::BitwiseXor => "^",
            BinOpKind::Power => panic!("Power operator not supported"),
            BinOpKind::LessThanOrEqual => "<=",
            BinOpKind::GreaterThanOrEqual => ">=",
        };
    }

    fn is_valid_r_value(&self,ast: &Ast, expr: ExprId) -> bool {
        let expr = ast.query_expr(expr);
        return match &expr.kind {
            ExprKind::Number(_) => true,
            ExprKind::Binary(binary_expr) => {
                let left = self.is_valid_r_value(ast,binary_expr.left);
                let right = self.is_valid_r_value(ast, binary_expr.right);
                left && right
            }
            ExprKind::Unary(_) => self.is_valid_r_value(ast, expr.id),
            ExprKind::Parenthesized(parenthesized_expr) => self.is_valid_r_value(ast, parenthesized_expr.expression),
            ExprKind::Variable(_) => true,
            ExprKind::Assignment(assign_expr) => self.is_valid_r_value(ast, assign_expr.expression),
            ExprKind::Boolean(_) => true,
            ExprKind::Call(call_expr) => {
                for argument in call_expr.arguments.iter() {
                    if !self.is_valid_r_value(ast, *argument) {
                        return false;
                    }
                }
                true
            }
            ExprKind::If(_) => false,
            ExprKind::Block(_) => false,
            ExprKind::Error(_) => panic!("Error expression"),
        };
    }

    fn write_newline(&mut self) {
        self.result.push('\n');
    }

    fn write_ident(&mut self) {
        for _ in 0..self.indent {
            self.result.push_str("  ");
        }
    }

    fn write_type(&mut self, ty: &Type) {
        self.result.push_str(&CTranspiler::transpile_type(ty));
    }

    fn write_whitespace(&mut self) {
        self.result.push_str(" ");
    }
}

impl ASTVisitor for CTranspiler<'_> {
    fn visit_func_decl(&mut self, ast: &mut Ast, func_decl: &FunctionDeclaration, item_id: ItemId) {
        let function = self.global_scope.functions.get(func_decl.idx);
        self.write_type(&function.return_type);
        self.write_whitespace();
        self.result.push_str(&function.name);
        self.result.push_str("(");
        for (i, parameter) in function.parameters.iter().enumerate() {
            let parameter = self.global_scope.variables.get(*parameter);
            self.write_type(&parameter.ty);
            self.write_whitespace();
            self.result.push_str(&parameter.name);
            if i != function.parameters.len() - 1 {
                self.result.push_str(", ");
            }
        }
        self.result.push_str(") {\n");
        self.indent += 1;
        self.visit_expression(ast, func_decl.body);
        self.indent -= 1;
        self.result.push_str("}\n");
    }

    fn visit_statement(&mut self, ast: &mut Ast, statement: StmtId) {
        self.write_ident();
        self.do_visit_statement(ast, statement);
        self.result.push(';');
        self.write_newline();
    }

    fn visit_let_statement(&mut self, ast: &mut Ast, let_statement: &LetStmt, stmt: &Stmt) {
        let variable = self.global_scope.variables.get(let_statement.variable_idx);
        self.write_type(&variable.ty);
        self.write_whitespace();
        self.result.push_str(&variable.name);
        self.result.push_str(" = ");
        self.visit_expression(ast, let_statement.initializer);
    }

    fn visit_variable_expression(&mut self, ast: &mut Ast, variable_expression: &VarExpr, expr: &Expr) {
        let variable = self.global_scope.variables.get(variable_expression.variable_idx);
        self.result.push_str(&variable.name);
    }

    fn visit_number_expression(&mut self, ast: &mut Ast, number: &NumberExpr, expr: &Expr) {
        self.result.push_str(&number.number.to_string());
    }

    fn visit_boolean_expression(&mut self, ast: &mut Ast, boolean: &BoolExpr, expr: &Expr) {
        self.result.push_str(if boolean.value { "1" } else { "0" });
    }

    fn visit_error(&mut self, ast: &mut Ast, span: &TextSpan) {
        self.result.push_str("/* error */");
    }

    fn visit_unary_expression(&mut self, ast: &mut Ast, unary_expression: &UnaryExpr, expr: &Expr) {
        self.result.push_str(self.transpile_unary_operator(&unary_expression.operator));
        self.visit_expression(ast, unary_expression.operand);
    }

    fn visit_assignment_expression(&mut self, ast: &mut Ast, assignment_expression: &AssignExpr, expr: &Expr) {
        self.result.push_str(&self.global_scope.variables.get(assignment_expression.variable_idx).name);
        self.result.push_str(" = ");
        self.visit_expression(ast, assignment_expression.expression);
    }

    fn visit_binary_expression(&mut self, ast: &mut Ast, binary_expression: &BinaryExpr, expr: &Expr) {
        self.visit_expression(ast, binary_expression.left);
        self.write_whitespace();
        self.result.push_str(self.transpile_binary_operator(&binary_expression.operator));
        self.write_whitespace();
        self.visit_expression(ast, binary_expression.right);
    }

    fn visit_parenthesized_expression(&mut self, ast: &mut Ast, parenthesized_expression: &ParenthesizedExpr, expr: &Expr) {
        self.result.push('(');
        self.visit_expression(ast, parenthesized_expression.expression);
        self.result.push(')');
    }

    fn visit_block_expr(&mut self, ast: &mut Ast, block_expr: &BlockExpr, expr: &Expr) {
        for statement in block_expr.stmts.iter().take(block_expr.stmts.len() - 1) {
            self.visit_statement(ast, *statement);
        }
        if let Some((assign_to, r_value_id)) = self.l_value_stack.last() {
            if *r_value_id == expr.id {
                self.result.push_str(&self.global_scope.variables.get(*assign_to).name);
                self.result.push_str(" = ");
            }
        }
        if let Some(last_stmt) = block_expr.stmts.last() {
            self.visit_statement(ast, *last_stmt);
        }

    }
}