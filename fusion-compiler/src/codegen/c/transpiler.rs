use std::collections::HashMap;

use crate::ast::{Ast, BinaryExpr, BlockExpr, CallExpr, Expr, ExprId, ExprKind, FunctionDeclaration, IfExpr, Item, ItemKind, StmtId, StmtKind, UnaryExpr};
use crate::codegen::c::ast::{CAssignExpr, CAst, CBinaryExpr, CBinOperator, CBlock, CBool, CCallExpr, CExpr, CFunctionDecl, CFunctionImpl, CIfStmt, CItem, CNumber, CParameter, CReturn, CStmt, CType, CUnaryExpr, CUnOperator, CVarDecl, CVarExpr, CWhile};
use crate::compilation_unit::{GlobalScope, VariableIdx};
use crate::typings::Type;

pub struct CTranspiler<'a> {
    global_scope: &'a GlobalScope,
    temp_variable_counter: usize,
    shadowing_variables: HashMap<String, Vec<VariableIdx>>,
}

impl<'a> CTranspiler<'a> {
    pub fn new(global_scope: &'a GlobalScope) -> Self {
        Self {
            global_scope,
            temp_variable_counter: 0,
            shadowing_variables: HashMap::new(),
        }
    }

    pub fn transpile(mut self, ast: &Ast) -> CAst {
        let mut items = vec![
            CItem::Macro("true".to_string(), "1".to_string()),
            CItem::Macro("false".to_string(), "0".to_string()),
        ];
        items.extend(ast.items.iter().filter(
            |item| matches!(item.kind, ItemKind::Function(_))
        ).map(|item| {
            match &item.kind {
                ItemKind::Stmt(_) => {
                    unreachable!()
                }
                ItemKind::Function(function) => {
                    self.transpile_function_decl(ast, function)
                }
            }
        }).collect::<Vec<_>>());
        items.extend(ast
            .items
            .iter()
            .map(|item| self.transpile_item(ast, item))
            .collect::<Vec<_>>());
        CAst::new(items)
    }

    fn transpile_item(&mut self, ast: &Ast, item: &Item) -> CItem {
        match &item.kind {
            ItemKind::Stmt(_) => panic!("Statement in global scope not supported"),
            ItemKind::Function(function) => {
                self.transpile_function(ast, function)
            }
        }
    }

    fn transpile_function_decl(&mut self, ast: &Ast, function: &FunctionDeclaration) -> CItem {
        let function = self.global_scope.functions.get(function.idx);

        CItem::FunctionDecl(CFunctionDecl {
            name: function.name.clone(),
            return_type: CType::try_from(&function.return_type)
                .expect("Unsupported return type"),
            parameters: function
                .parameters
                .iter()
                .map(|parameter_idx| {
                    let parameter = self.global_scope.variables.get(*parameter_idx);
                    CParameter {
                        name: self.get_variable_name(*parameter_idx),
                        ty: CType::try_from(&parameter.ty)
                            .expect("Unsupported parameter type"),
                    }
                })
                .collect(),
        })
    }


    fn transpile_function(&mut self, ast: &Ast, function: &FunctionDeclaration) -> CItem {
        let function_decl = match self.transpile_function_decl(ast, function) {
            CItem::FunctionDecl(function_decl) => function_decl,
            _ => unreachable!()
        };
        let mut body_stmts = function.body.iter().map(|stmt| self.transpile_stmt(ast, *stmt)).flatten().collect::<Vec<_>>();

        CItem::FunctionImpl(CFunctionImpl {
            name: function_decl.name,
            return_type: function_decl.return_type,
            parameters: function_decl.parameters,
            body: body_stmts,
        })
    }

    fn transpile_expr(&mut self, ast: &Ast, expr: ExprId) -> (Option<Vec<CStmt>>, CExpr) {
        let expr = ast.query_expr(expr);
        match &expr.kind {
            ExprKind::Number(number) => (
                None,
                CExpr::Number(CNumber {
                    value: number.number,
                }),
            ),
            ExprKind::Binary(binary_expr) => {
                self.transpile_binary_expr(ast, &binary_expr)
            }
            ExprKind::Unary(unary_expr) => {
                self.transpile_unary_expr(ast, &unary_expr)
            }
            ExprKind::Parenthesized(paren_expr) => {
                let (expr_stmts, expr) = self.transpile_expr(ast, paren_expr.inner);
                (expr_stmts, CExpr::Parenthesized(Box::new(expr)))
            }
            ExprKind::Variable(var_expr) => {
                (
                    None,
                    CExpr::Var(CVarExpr {
                        name: self.get_variable_name(var_expr.variable_idx)
                    }),
                )
            }
            ExprKind::Assignment(assignment_expr) => {
                let (assign_expr_stmts, assign_expr) =
                    self.transpile_expr(ast, assignment_expr.expression);
                (
                    assign_expr_stmts,
                    CExpr::Assign(CAssignExpr {
                        name: self.get_variable_name(assignment_expr.variable_idx),
                        expr: Box::new(assign_expr),
                    }),
                )
            }
            ExprKind::Boolean(bool_expr) => (
                None,
                CExpr::Bool(CBool {
                    value: bool_expr.value,
                }),
            ),
            ExprKind::Call(call_expr) => {
                self.transpile_call_expr(ast, call_expr)
            }
            ExprKind::If(if_expr) => {
                self.transpile_if_expr(ast, &expr, if_expr)
            }
            ExprKind::Block(block_expr) => {
                self.transpile_block_expr(ast, &expr, block_expr)
            }
            ExprKind::Error(_) => panic!("Error expression"),
        }
    }

    fn transpile_if_expr(&mut self, ast: &Ast, expr: &&Expr, if_expr: &IfExpr) -> (Option<Vec<CStmt>>, CExpr) {
        let (cond_stmts, condition) = self.transpile_expr(ast, if_expr.condition);
        let (then_expr_stmts, then_expr) =
            // self.transpile_expr(ast, if_expr.then_branch);
        unreachable!();
        match (if_expr.else_branch.as_ref(), &expr.ty) {
            (None, _) | (Some(_), Type::Void) => {
                let mut stmts = Vec::new();
                if let Some(cond_stmts) = cond_stmts {
                    stmts.extend(cond_stmts);
                }
                match then_expr_stmts {
                    None => {
                        stmts.push(CStmt::If(CIfStmt {
                            condition,
                            then_block: CBlock {
                                statements: vec![CStmt::Expr(then_expr)],
                            },
                            else_block: None,
                        }));
                    }
                    Some(expr_stmts) => {
                        stmts.push(CStmt::If(CIfStmt {
                            condition,
                            then_block: CBlock {
                                statements: expr_stmts,
                            },
                            else_block: None,
                        }));
                        stmts.push(CStmt::Expr(then_expr));
                    }
                };

                (Some(stmts), CExpr::Bool(CBool { value: false }))
            }
            (Some(else_branch), _) => {
                let temp_var_decl = self.next_temp_var_decl(&expr.ty);
                let temp_var_name = temp_var_decl.name.clone();
                let temp_var = CExpr::Var(CVarExpr {
                    name: temp_var_name.clone(),
                });
                let (else_expr_stmts, else_expr): (Option<()>, CExpr) =
                    // self.transpile_expr(ast, else_branch.body);
                unreachable!();
                let mut stmts = Vec::new();
                if let Some(cond_stmts) = cond_stmts {
                    stmts.extend(cond_stmts);
                }
                stmts.push(CStmt::VarDecl(temp_var_decl));
                let mut then_block = Vec::new();
                if let Some(then_expr_stmts) = then_expr_stmts {
                    then_block.extend(then_expr_stmts);
                }
                then_block.push(CStmt::Expr(CExpr::Assign(CAssignExpr {
                    name: temp_var_name.clone(),
                    expr: Box::new(then_expr),
                })));
                let mut else_block = Vec::new();
                // if let Some(else_expr_stmts) = else_expr_stmts {
                //     // else_block.extend(else_expr_stmts);
                // }
                else_block.push(CStmt::Expr(CExpr::Assign(CAssignExpr {
                    name: temp_var_name.clone(),
                    expr: Box::new(else_expr),
                })));
                stmts.push(CStmt::If(CIfStmt {
                    condition,
                    then_block: CBlock {
                        statements: then_block,
                    },
                    else_block: Some(CBlock {
                        statements: else_block,
                    }),
                }));
                (Some(stmts), temp_var)
            }
        }
    }

    fn transpile_block_expr(&mut self, ast: &Ast, expr: &&Expr, block_expr: &BlockExpr) -> (Option<Vec<CStmt>>, CExpr) {
        let mut stmts = Vec::new();
        let assign_returning_expr_to_temp_var = !matches!(&expr.ty, Type::Void);
        let (temp_var, temp_var_name) = if assign_returning_expr_to_temp_var {
            let temp_var_decl = self.next_temp_var_decl(&expr.ty);
            let temp_var_name = temp_var_decl.name.clone();
            stmts.push(CStmt::VarDecl(temp_var_decl));
            let temp_var = CExpr::Var(CVarExpr {
                name: temp_var_name.clone(),
            });
            (Some(temp_var), Some(temp_var_name))
        } else {
            (None, None)
        };

        let returning_expression = block_expr
            .returning_expression(ast)
            .map(|expr| self.transpile_expr(ast, expr));
        for stmt in block_expr.stmts.iter().take(match returning_expression {
            None => block_expr.stmts.len(),
            Some(_) => block_expr.stmts.len() - 1,
        }) {
            let t_stmts = self.transpile_stmt(ast, *stmt);
            stmts.extend(t_stmts);
        }
        if let Some((expr_stmts, expr)) = returning_expression {
            if let Some(expr_stmts) = expr_stmts {
                stmts.extend(expr_stmts);
            }
            if assign_returning_expr_to_temp_var {
                stmts.push(CStmt::Expr(CExpr::Assign(CAssignExpr {
                    name: temp_var_name.unwrap(),
                    expr: Box::new(expr),
                })));
            }
        }
        (
            Some(stmts),
            temp_var.unwrap_or(CExpr::Bool(CBool { value: false })),
        )
    }

    fn transpile_call_expr(&mut self, ast: &Ast, call_expr: &CallExpr) -> (Option<Vec<CStmt>>, CExpr) {
        let function = self.global_scope.functions.get(call_expr.function_idx);
        let mut stmts = Vec::new();
        let arguments = call_expr
            .arguments
            .iter()
            .map(|argument| {
                let (arg_stmts, arg_expr) = self.transpile_expr(ast, *argument);
                if let Some(arg_stmts) = arg_stmts {
                    stmts.extend(arg_stmts);
                }
                arg_expr
            })
            .collect();
        (
            Some(stmts),
            CExpr::Call(CCallExpr {
                name: function.name.clone(),
                arguments: arguments,
            }),
        )
    }

    fn transpile_unary_expr(&mut self, ast: &Ast, unary_expr: &&UnaryExpr) -> (Option<Vec<CStmt>>, CExpr) {
        let (expr_stmts, expr) = self.transpile_expr(ast, unary_expr.operand);
        let op = CUnOperator::try_from(&unary_expr.operator)
            .expect("Unsupported unary operator");
        (
            expr_stmts,
            CExpr::Unary(CUnaryExpr {
                expr: Box::new(expr),
                operator: op,
            }),
        )
    }

    fn transpile_binary_expr(&mut self, ast: &Ast, binary_expr: &&BinaryExpr) -> (Option<Vec<CStmt>>, CExpr) {
        let (left_stmts, left) = self.transpile_expr(ast, binary_expr.left);
        let (right_stmts, right) = self.transpile_expr(ast, binary_expr.right);
        let op = CBinOperator::try_from(&binary_expr.operator)
            .expect("Unsupported binary operator");
        let mut stmts = Vec::new();
        if let Some(left_stmts) = left_stmts {
            stmts.extend(left_stmts);
        }
        if let Some(right_stmts) = right_stmts {
            stmts.extend(right_stmts);
        }
        (
            Some(stmts),
            CExpr::Binary(CBinaryExpr {
                left: Box::new(left),
                right: Box::new(right),
                operator: op,
            }),
        )
    }

    fn transpile_stmt(&mut self, ast: &Ast, stmt: StmtId) -> Vec<CStmt> {
        let mut stmts = vec![];
        let stmt = ast.query_stmt(stmt);
        let c_stmt = match &stmt.kind {
            StmtKind::Expr(expr) => {
                let (expr_stmts, expr) = self.transpile_expr(ast, *expr);
                if let Some(expr_stmts) = expr_stmts {
                    stmts.extend(expr_stmts);
                }
                CStmt::Expr(expr)
            }
            StmtKind::Let(let_stmt) => {
                let var = self.global_scope.variables.get(let_stmt.variable_idx);
                let variable_name = self.get_variable_name(let_stmt.variable_idx);
                let (initializer_stmts, initializer) =
                    self.transpile_expr(ast, let_stmt.initializer);
                if let Some(initializer_stmts) = initializer_stmts {
                    stmts.extend(initializer_stmts);
                }
                CStmt::VarDecl(CVarDecl {
                    name: variable_name,
                    ty: CType::try_from(&var.ty).expect("Unsupported type"),
                    initializer: Some(initializer),
                })
            }
            StmtKind::While(while_stmt) => {
                let (condition_stmts, condition) = self.transpile_expr(ast, while_stmt.condition);
                let (body_stmts, body) =
                    // self.transpile_expr(ast, while_stmt.body);
                unreachable!();
                let mut while_body_stmts = Vec::new();
                if let Some(condition_stmts) = condition_stmts {
                    while_body_stmts.extend(condition_stmts.clone());
                }
                let mut then_block_stmts = match body_stmts {
                    None => vec![],
                    Some(body_stmts) => body_stmts,
                };
                then_block_stmts.push(CStmt::Expr(body));
                while_body_stmts.push(CStmt::If(CIfStmt {
                    condition,
                    then_block: CBlock {
                        statements: then_block_stmts,
                    },
                    else_block: Some(CBlock {
                        statements: vec![
                            CStmt::Break,
                        ]
                    }),
                }));


                CStmt::While(CWhile {
                    condition: CExpr::Bool(CBool {
                        value: true,
                    }),
                    body: CBlock {
                        statements: while_body_stmts,
                    },
                })
            }
            StmtKind::Return(return_stmt) => {
                let return_val = return_stmt
                    .return_value
                    .map(|expr| self.transpile_expr(ast, expr));
                return match return_val {
                    None => vec![CStmt::Return(CReturn { expr: None })],
                    Some((return_val_stmts, expr)) => {
                        if let Some(return_val_stmts) = return_val_stmts {
                            stmts.extend(return_val_stmts);
                        }
                        stmts.push(CStmt::Return(CReturn { expr: Some(expr) }));
                        stmts
                    }
                };
            }
        };
        stmts.push(c_stmt);
        return stmts;
    }
    fn next_temp_var_decl(&mut self, ty: &Type) -> CVarDecl {
        let name = self.next_temp_var_name();
        return CVarDecl {
            name,
            ty: CType::try_from(ty).expect("Unresolved type"),
            initializer: None,
        };
    }

    fn next_temp_var_name(&mut self) -> String {
        let name = format!("_{}", self.temp_variable_counter);
        self.temp_variable_counter += 1;
        return name;
    }
}

impl CTranspiler<'_> {
    fn get_variable_name(&mut self, variable_idx: VariableIdx) -> String {
        let variable = self.global_scope.variables.get(variable_idx);
        let shadowing_variables = self.shadowing_variables.get_mut(&variable.name);
        let shadowing_variables = match shadowing_variables {
            None => {
                self.shadowing_variables.insert(variable.name.clone(), vec![variable_idx]);
                self.shadowing_variables.get_mut(&variable.name).unwrap()
            }
            Some(shadowing_variables) => shadowing_variables,
        };
        let format = |index: usize| format!("{}_{}", variable.name, index);
        for (index, var) in shadowing_variables.iter().rev().enumerate() {
            if var == &variable_idx {
                return format(index);
            }
        }
        shadowing_variables.push(variable_idx);
        return format(shadowing_variables.len() - 1);
    }
}

#[cfg(test)]
mod test {
    use serial_test::serial;

    use crate::codegen::c::CProgram;
    use crate::compilation_unit::CompilationUnit;

    fn expect_return_value(input: &str, expected_return_value: i32, include_main: bool) {
        let input = if include_main {
            let mut input = String::from(input);
            input.push_str("func main() -> int { return test() }");
            input
        } else {
            String::from(input)
        };
        let compilation_unit = CompilationUnit::compile(&input).expect("Compilation failed");
        let program = CProgram::from_compilation_unit(&compilation_unit);
        let c_return_value = program.run().expect("C program failed");
        return assert_eq!(c_return_value, expected_return_value, "Expected return value {} but got {}", expected_return_value, c_return_value);
    }

    #[test]
    #[serial]
    fn simple_if_expression_evaluation() {
        let input = r#"
            func test() -> int {
                let a = if (true) {
                    1
                } else {
                    2
                }
                return a
            }
        "#;
        let expected_value = 1;
        expect_return_value(input, expected_value, true)
    }

    #[test]
    #[serial]
    fn variable_shadowing() {
        let input = r#"
            func test() -> int {
                let a = 10
                let a = 20
                return a
            }
        "#;
        expect_return_value(input, 20, true)
    }

    #[test]
    #[serial]
    fn gdc() {
        let input = r#"
            func test() -> int {
                let a = 50
                let b = 10
                while b != 0 {
                    let t = b
                    b = a % b
                    a = t
                }
                return a
            }
        "#;
        expect_return_value(input, 10, true)
    }

    #[test]
    #[serial]
    fn nested_scopes() {
        let input = r#"
            func test() -> int {
                return {{{{ 10 }}}}
            }
        "#;
        expect_return_value(input, 10, true)
    }

    #[test]
    #[serial]
    fn simple_while_loop() {
        let input = r#"
            func test() -> int {
                let sum = 0
                let n = 10
                let i = 0
                while i < n {
                    sum = sum + i
                    i = i + 1
                }
                return sum
            }
        "#;
        expect_return_value(input, 45, true)
    }

    #[test]
    #[serial]
    fn function_call_without_args() {
        let input = r#"

            func to_call() -> int {
                let a = 10
                return a
            }

            func test() -> int {
                return to_call()
            }
        "#;
        expect_return_value(input, 10, true)
    }

    #[test]
    #[serial]
    fn function_call_with_args() {
        let input = r#"

            func to_call(a: int, b: int) -> int {
                return a + b
            }

            func test() -> int {
                return to_call(10, 20)
            }
        "#;
        expect_return_value(input, 30, true)
    }

    #[test]
    #[serial]
    fn function_reordering() {
        let input = r#"

            func test() -> int {
                return to_call(10, 20)
            }

            func to_call(a: int, b: int) -> int {
                return a + b
            }
        "#;
        expect_return_value(input, 30, true)
    }

    #[test]
    #[serial]
    fn function_without_return_value() {
        let input = r#"

            func to_call() {
                let a = 10
            }

            func test() -> int{
                to_call()
                return 1
            }

        "#;
        expect_return_value(input, 1, true)
    }


    #[test]
    #[serial]
    fn while_loop_condition_expression_evaluation() {
        let input = r#"
            func test() -> int {
                let a = 1
                let b = 0
                while (a = {a + 1}) < 10 {
                    b = b + 1
                }
                return b
            }
        "#;
        let expected_value = 8;
        expect_return_value(input, expected_value, true)
    }

    #[test]
    #[serial]
    fn true_false() {
        let input = r#"
            func test() -> int {
                let condition = (true && false) || (true && true)
                if condition {
                    return 1
                } else {
                    return 0
                }
            }
        "#;
        expect_return_value(input, 1, true)
    }
}


