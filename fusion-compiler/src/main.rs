
use crate::compilation_unit::CompilationUnit;




mod ast;
mod diagnostics;
mod text;
mod compilation_unit;
mod typings;

fn main() -> Result<(),()>{
    let input = "\
        func add(b: int) -> int {
            return a + b
        }
        let a: int = add(2)
        while a < 1 {
            a = a + 1
        }
        a

    ";
    let compilation_unit = CompilationUnit::compile(input).map_err(|_|())?;
    compilation_unit.run();
    Ok(())
}
