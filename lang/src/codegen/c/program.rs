use std::{
    fs::File,
    io::Write,
    process::Command,
};

use anyhow::anyhow;

use crate::{
    codegen::c::{
        ast::{
            CAst,
            CBlock,
            CExpr,
            CItem,
            CParameter,
            CStmt,
        },
        CTranspiler,
    },
    compilation_unit::CompilationUnit,
};

impl From<CAst> for CProgram {
    fn from(value: CAst) -> Self {
        Self { ast: value }
    }
}

pub struct CProgram {
    pub ast: CAst,
}

impl CProgram {
    const C_COMPILER: &'static str = "clang";
    const OUTPUT_FILE: &'static str = "out";

    const C_INPUT_FILE: &'static str = "out.c";

    pub fn from_compilation_unit(compilation_unit: &CompilationUnit) -> Self {
        compilation_unit.into()
    }

    pub fn source_code(&self) -> String {
        let mut coder = CCoder::new();
        for item in &self.ast.items {
            coder.transpile_item(item);
        }
        coder.code
    }

    pub fn compile(&self) -> anyhow::Result<File> {
        let mut file = File::create(Self::C_INPUT_FILE)?;
        let transpiled_code = self.source_code();
        file.write_all(transpiled_code.as_bytes())?;
        Command::new(Self::C_COMPILER)
            .arg(Self::C_INPUT_FILE)
            .arg("-o")
            .arg(Self::OUTPUT_FILE)
            .status()?
            .exit_ok()?;
        Ok(file)
    }

    pub fn run(&self) -> anyhow::Result<i32> {
        let _ = self.compile()?;
        let run_result = Command::new(format!("./{}", Self::OUTPUT_FILE)).status()?;
        Ok(run_result
            .code()
            .ok_or(anyhow!("Program exited without returning a value"))?)
    }
}

impl Into<CProgram> for &CompilationUnit {
    fn into(self) -> CProgram {
        let ast = CTranspiler::new(&self.global_scope).transpile(&self.ast);
        ast.into()
    }
}

struct CCoder {
    indent: usize,
    code: String,
}

impl CCoder {
    fn new() -> Self {
        Self {
            indent: 0,
            code: String::new(),
        }
    }

    fn write_whitespace(&mut self) {
        self.code.push(' ');
    }

    fn write(&mut self, text: &str) {
        self.code.push_str(text);
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.write("  ");
        }
    }

    fn transpile_item(&mut self, item: &CItem) {
        match item {
            CItem::FunctionImpl(function) => {
                self.write(format!("{} {}(", function.return_type, function.name).as_str());
                self.transpile_parameters(&function.parameters);
                self.write(") {\n");
                self.indent += 1;
                for statement in &function.body {
                    self.transpile_statement(statement);
                }
                self.indent -= 1;
                self.write("}\n");
            }
            CItem::VarDecl(_) => {
                unimplemented!()
            }
            CItem::FunctionDecl(function) => {
                self.write(format!("{} {}(", function.return_type, function.name).as_str());
                self.transpile_parameters(&function.parameters);
                self.write(");\n");
            }
            CItem::Macro(name, value) => {
                self.write(format!("#define {} {}\n", name, value).as_str());
            }
        }
    }

    fn transpile_parameters(&mut self, parameters: &Vec<CParameter>) {
        for (index, parameter) in parameters.iter().enumerate() {
            if index != 0 {
                self.write(", ");
            }
            self.write(format!("{} {}", parameter.ty, parameter.name).as_str());
        }
    }

    fn transpile_statement(&mut self, stmt: &CStmt) {
        self.write_indent();
        match stmt {
            CStmt::VarDecl(var_decl) => {
                self.write(format!("{} {}", var_decl.ty, var_decl.name).as_str());
                if let Some(initializer) = &var_decl.initializer {
                    self.write(" = ");
                    self.transpile_expr(initializer);
                }
                self.write(";\n");
            }
            CStmt::Return(return_stmt) => {
                self.write("return");
                if let Some(expr) = &return_stmt.expr {
                    self.write(" ");
                    self.transpile_expr(expr);
                }
                self.write(";\n");
            }
            CStmt::If(if_stmt) => {
                self.write("if (");
                self.transpile_expr(&if_stmt.condition);
                self.write(")");
                self.transpile_block(&if_stmt.then_block);
                if let Some(else_block) = &if_stmt.else_block {
                    self.write_indent();
                    self.write("else");
                    self.transpile_block(else_block);
                }
            }
            CStmt::While(while_stmt) => {
                self.write("while (");
                self.transpile_expr(&while_stmt.condition);
                self.write(")");
                self.transpile_block(&while_stmt.body);
            }
            CStmt::Expr(expr) => {
                self.transpile_expr(expr);
                self.write(";\n");
            }
            CStmt::Block(block) => {
                self.transpile_block(block);
            }
            CStmt::Break => {
                self.write("break;\n");
            }
        };
    }

    fn transpile_expr(&mut self, expr: &CExpr) {
        match expr {
            CExpr::Assign(assign) => {
                self.write(format!("{} = ", assign.name).as_str());
                self.transpile_expr(&assign.expr);
            }
            CExpr::Number(number) => {
                self.write(format!("{}", number.value).as_str());
            }
            CExpr::Bool(bool) => {
                self.write(format!("{}", bool.value).as_str());
            }
            CExpr::Var(var) => {
                self.write(var.name.as_str());
            }
            CExpr::Unary(unary) => {
                self.write(format!("{}", unary.operator).as_str());
                self.transpile_expr(&unary.expr);
            }
            CExpr::Binary(binary) => {
                self.transpile_expr(&binary.left);
                self.write(format!(" {} ", binary.operator).as_str());
                self.transpile_expr(&binary.right);
            }
            CExpr::Call(call) => {
                self.write(call.name.as_str());
                self.write("(");
                let mut first = true;
                for argument in &call.arguments {
                    if first {
                        first = false;
                    } else {
                        self.write(", ");
                    }
                    self.transpile_expr(argument);
                }
                self.write(")");
            }
            CExpr::Parenthesized(expr) => {
                self.write("(");
                self.transpile_expr(expr);
                self.write(")");
            }
        }
    }

    fn transpile_block(&mut self, block: &CBlock) {
        self.write(" {\n");
        self.indent += 1;
        for statement in &block.statements {
            self.transpile_statement(statement);
        }
        self.indent -= 1;
        self.write_indent();
        self.write("}\n");
    }
}
