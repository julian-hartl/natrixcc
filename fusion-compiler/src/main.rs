#![allow(clippy::needless_return)]

use std::fs::File;
use std::io::Write;
use std::process::Command;
use crate::codegen::CTranspiler;
use crate::compilation_unit::CompilationUnit;
mod ast;
mod diagnostics;
mod text;
mod compilation_unit;
mod typings;
mod codegen;

fn main() -> Result<(), ()> {
    let input = "\
        1 - 1 + 1
        let a = 10
        let b = 20
        let b = b + 20

        let z = mul(a, a)
        func mul (a: int, b: int) -> int {
            let sum = 0
            while b > 0 {
                sum = sum + a
                b = b - 1
            }
            return sum
        }

        let c = mul(a, b)

        let d = if a == b {
            {
                {
                {
                10
            }
            }
            }
        } else {
            20
        }
        let e = c + d
        z
    ";
    let mut compilation_unit = CompilationUnit::compile(input).map_err(|_| ())?;
    compilation_unit.run();
    let mut c_transpiler = CTranspiler::new(&compilation_unit.global_scope);
    let transpiled_code = c_transpiler.transpile(&mut compilation_unit.ast);
    println!("{}", transpiled_code);
    let mut c_file = File::create("out.c").unwrap();
    c_file.write_all(transpiled_code.as_bytes()).unwrap();
    // compile with clang using rust
    Command::new("clang")
        .arg("out.c")
        .arg("-o")
        .arg("out")
        .status()
        .unwrap();
    // run the compiled binary
    Command::new("./out")
        .status()
        .unwrap();

    Ok(())
}
