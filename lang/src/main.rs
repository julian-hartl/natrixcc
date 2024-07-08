#![feature(exit_status_error)]

use std::{fs::File, io::Write};

use anyhow::{anyhow, Result};

use crate::{
    compilation_unit::CompilationUnit,
    hir::{HIRBuilder, HIRWriter},
    mir::{optimizations::Optimizer, MIRBuilder, MIRWriter},
};

mod ast;
mod codegen;
mod compilation_unit;
mod diagnostics;
mod hir;
mod lir;
mod mir;
mod text;
mod typings;

fn main() -> Result<()> {
    std::env::set_var("RUST_BACKTRACE", "1");
    std::env::set_var("RUST_LOG", "debug");
    tracing_subscriber::fmt::init();
    // todo: investigate why a phi node with a missing operand is generated
    let input = "

        func main() -> int {
            let a = 1;
            let b = 2;
            return if a < b {
                if b > a {
                    let i = 0;
                    while i < 3 {
                        i = i + 1;
                    }
                    i
                } else {
                    1
                }
            } else {
                2
            }
        }
    ";
    let mut compilation_unit =
        CompilationUnit::compile(input).map_err(|err| anyhow!("Compilation failed"))?;
    compilation_unit.run();
    // let program = CProgram::from_compilation_unit(&compilation_unit);
    // let c_return_value = program.run()?;
    // println!("C program returned {}", c_return_value);
    let hir_builder = HIRBuilder::new();
    let hir = hir_builder.build(&compilation_unit.ast, &mut compilation_unit.global_scope);
    let mut hir_txt = String::new();
    HIRWriter::write(&mut hir_txt, &hir, &compilation_unit.global_scope)?;
    println!("{}", hir_txt);
    let ir_builder = MIRBuilder::new();
    let mut ir = ir_builder.build(&hir, &compilation_unit.global_scope);
    let mut ir_text = String::new();
    MIRWriter::write_text_representation(&mut ir_text, &ir)?;
    let mut mir_graphviz = String::new();
    MIRWriter::write_graphviz_representation(&mut mir_graphviz, &ir)?;
    File::create("mir.dot")?.write_all(mir_graphviz.as_bytes())?;
    println!("{}", ir_text);
    let mut optimizer = Optimizer::new();
    optimizer.run(&mut ir);
    let mut mir_graphviz = String::new();
    MIRWriter::write_graphviz_representation(&mut mir_graphviz, &ir)?;
    File::create("mir-optimized.dot")?.write_all(mir_graphviz.as_bytes())?;
    let lir_builder = lir::builder::LIRBuilder::new(&ir, &compilation_unit.global_scope);
    let lir = lir_builder.build();
    dbg!(&lir);
    let mut gen = codegen::x86_64::X86_64Codegen::new();
    gen.gen(&lir)?;
    let asm_output = gen.get_asm_output()?;
    println!("{}", asm_output);
    Ok(())
}
