
use crate::compilation_unit::CompilationUnit;




mod ast;
mod diagnostics;
mod text;
mod compilation_unit;


fn main() -> Result<(),()>{
    let input = "\
        func doSomething {
        }
        func main {
        }
        let a = add(1, 2)
        func add(a, b) {
            return a + b
        }
        while a < 10 {
            a = a + 1
        }
        if a >= 10 {
            a = 25
        }
        else
        {
            a = 20
            let a = 10
            a = 15
        }
        let b = 10
        if true {
            b = 20
        }
        a
        b
    ";
    let compilation_unit = CompilationUnit::compile(input).map_err(|_|())?;
    compilation_unit.run();
    Ok(())
}
