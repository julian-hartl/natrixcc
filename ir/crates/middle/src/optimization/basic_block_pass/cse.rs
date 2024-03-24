use rustc_hash::FxHashMap;

use crate::{FunctionId, VReg};
use crate::cfg::BasicBlockId;
use crate::instruction::{InstrIdentifyingKey, InstrKind, Op, OpInstr};
use crate::module::Module;
use crate::optimization::basic_block_pass::BasicBlockPass;
use crate::optimization::Pass;

/// # Common Subexpression Elimination
///
/// This pass eliminates common subexpressions by caching the produced values of instructions
/// and replacing them with the cached value if the same expression is produced again.
pub struct CSEPass;

impl Pass for CSEPass {
    fn name(&self) -> &'static str {
        "cse"
    }
}

impl BasicBlockPass for CSEPass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: FunctionId, basic_block: BasicBlockId) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let bb = cfg.basic_block_mut(basic_block);
        let mut changes = 0;
        let mut expr_cache: FxHashMap<InstrIdentifyingKey, VReg> = FxHashMap::default();
        for instr in bb.instructions_mut() {
            let key = instr.identifying_key();
            if let Some(key) = key {
                let produced_value = instr.defined_vreg();
                if let Some(produced_value) = produced_value {
                    let cached_value = expr_cache.get(&key).copied();
                    if let Some(cached_value) = cached_value {
                        instr.kind = InstrKind::Op(OpInstr {
                            value: produced_value,
                            op: Op::Vreg(cached_value),
                        });
                        changes += 1;
                    } else {
                        expr_cache.insert(key, produced_value);
                    }
                }
            }
        }
        changes
    }
}

#[cfg(test)]
mod tests {
    use crate::{cfg, optimization};
    use crate::cfg::{RetTerm, TerminatorKind};
    use crate::instruction::{Const, Op};
    use crate::optimization::{Pipeline, PipelineConfig};
    use crate::test::{create_test_module, create_test_module_from_source};

    #[test]
    fn should_replace_duplicate_subtraction() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test(i32) {
                bb0(i32 v0):
                    v1 = sub i32 v0, 3;
                    v2 = sub i32 v0, 3;
                    v3 = sub i32 v2, v1;
                    ret i32 v3;
                }
            "
        );
        module.optimize(PipelineConfig::cse_only());
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!("fun i32 @test(i32) {
bb0(i32 v0):
    v1 = sub i32 v0, 3;
    v2 = i32 v1;
    v3 = sub i32 v2, v1;
    ret i32 v3;
}
", function.to_string());
    }
}
