use crate::compilation_unit::CompilationUnit;

mod ast;
mod diagnostics;
mod text;
mod compilation_unit;
mod typings;

fn main() -> Result<(), ()> {
    let input = "\
        1 - 1 + 1
        let a = 10
        let b = 20
        let b = b + 20
        let mul = func (a: int, b: int) -> int {
            let add = func (a: int, b: int) -> int {
                let hadd = func(a: int, runs: int) -> int {
                    if runs == 0 {
                        return a
                    }
                    return rec(a + 1, runs - 1)
                }
                return hadd(a, b)
            }
            let sum = 0
            while b > 0 {
                sum = add(sum, b)
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
        e
    ";
    let mut compilation_unit = CompilationUnit::compile(input).map_err(|_| ())?;
    compilation_unit.run();
    Ok(())
}
