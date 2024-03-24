use cranelift_entity::SecondaryMap;
use tracing::debug;

use crate::analysis::dataflow;
use crate::analysis::dataflow::use_def::InstrUid;
use crate::FunctionId;
use crate::module::Module;
use crate::optimization::{FunctionPass, Pass};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct DeadCodeEliminationPass {}

impl Pass for DeadCodeEliminationPass {
    fn name(&self) -> &'static str {
        "dead_code_elim"
    }
}

impl FunctionPass for DeadCodeEliminationPass {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionId) -> usize {
        let runner = dataflow::use_def::AnalysisRunner::new(
            &mut module.functions[function]
        );
        let state = runner.collect();
        let mut changes = 0;
        let mut removed_instr_count = SecondaryMap::new();
        for vreg in state.unused_regs() {
            debug!("Removing unused def {vreg}");
            let InstrUid(bb_id, instr_id) = state.get_def(vreg).unwrap();
            let bb = &mut module.functions[function].cfg.basic_block_mut(bb_id);
            let removed_instrs = removed_instr_count.get(bb_id).copied().unwrap_or(0);
            let instr_id = instr_id - removed_instrs;
            bb.remove_instruction(instr_id);
            removed_instr_count[bb_id] = removed_instrs + 1;
            changes += 1;
        }

        changes
    }
}

#[cfg(test)]
mod tests {
    use crate::optimization::PipelineConfig;
    use crate::test::{assert_module_is_equal_to_src, create_test_module_from_source};

    #[test]
    fn should_eliminate_unused_values() {
        let mut module = create_test_module_from_source("
            fun i32 @test() {
            bb0:
                v0 = i32 20;
                v1 = add i32 v0, 8;
                v2 = sub i32 v0, 9;
                br bb1;
            bb1:
                ret i32 v0;
            }
        ");
        module.optimize(PipelineConfig::dead_code_elimination_only());
        assert_module_is_equal_to_src(
            &module,
            "
            fun i32 @test() {
            bb0:
                v0 = i32 20;
                br bb1;
            bb1:
                ret i32 v0;
            }
        "
        );
    }
}
