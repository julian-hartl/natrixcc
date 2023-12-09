#![feature(exit_status_error)]

use std::fs::File;
use std::io::Write;

use anyhow::{anyhow, Result};

use crate::compilation_unit::CompilationUnit;
use crate::hir::{HIRBuilder, HIRWriter};
use crate::mir::{MIRBuilder, MIRWriter};
use crate::mir::optimizations::Optimizer;

mod ast;
mod codegen;
mod compilation_unit;
mod diagnostics;
mod text;
mod typings;
mod mir;
mod hir;


fn main() -> Result<()> {
    std::env::set_var("RUST_BACKTRACE", "1");
    std::env::set_var("RUST_LOG", "debug");
    tracing_subscriber::fmt::init();
    let input = "


        func main() -> int {
            let d = 2;
            while d < 10 {
                d = d + if d < 5 {
                    1
                } else {
                    2
                };
            }
            return d;
        }
    ";
    let mut compilation_unit = CompilationUnit::compile(input).map_err(
        |err| anyhow!("Compilation failed")
    )?;
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
    Ok(())
}
