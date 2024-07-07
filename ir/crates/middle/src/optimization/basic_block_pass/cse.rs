use rustc_hash::FxHashMap;

use crate::{
    cfg::BasicBlockRef,
    instruction::{
        InstrIdentifyingKey,
        InstrKind,
        Op,
        OpInstr,
    },
    module::Module,
    optimization::{
        basic_block_pass::BasicBlockPass,
        Pass,
    },
    FunctionRef,
    Value,
};

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
    fn run_on_basic_block(
        &mut self,
        module: &mut Module,
        function: FunctionRef,
        basic_block: BasicBlockRef,
    ) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let bb = &mut cfg.basic_blocks[basic_block];
        let mut changes = 0;
        let mut expr_cache: FxHashMap<InstrIdentifyingKey, Value> = FxHashMap::default();
        for instr_id in bb.instructions() {
            let instr = &mut cfg.instructions[instr_id];
            let key = instr.identifying_key();
            if let Some(key) = key {
                let cached_value = expr_cache.get(&key).copied();
                if let Some(cached_value) = cached_value {
                    instr.kind = InstrKind::Op(OpInstr {
                        op: Op::Value(cached_value),
                    });
                    changes += 1;
                } else {
                    expr_cache.insert(key, instr.value());
                }
            }
        }
        changes
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        optimization::PipelineConfig,
        test::create_test_module_from_source,
    };

    #[test]
    fn should_replace_duplicate_subtraction() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test(i32) {
                bb0(i32 %0):
                    i32 %1 = sub %0, 3i32;
                    i32 %2 = sub %0, 3i32;
                    i32 %3 = sub %2, %1;
                    ret %3;
                }
            ",
        );
        module.optimize(PipelineConfig::cse_only());
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!(
            "fun i32 @test(i32) {
bb0(i32 %0):
    i32 %1 = sub %0, 3i32;
    i32 %2 = %1;
    i32 %3 = sub %2, %1;
    ret %3;
}
",
            function.to_string()
        );
    }
}
