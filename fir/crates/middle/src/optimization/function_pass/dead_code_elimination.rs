use crate::analysis::dataflow;
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
        let mut runner = dataflow::dead_code::AnalysisRunner::new(
            &mut module.functions[function]
        );
        let mut changes = 0;
        while let Some((bb_id, instr_walker)) = runner.next_bb() {
            instr_walker.drain();
            let bb = runner.function.cfg.basic_block_mut(bb_id);
            let state = runner.state.get(bb_id);
            bb.remove_instructions_by_pred(
                |instr| {
                    let produced_value = instr.produced_value();
                    if let Some(produced_value) = produced_value {
                        if !state.contains(&produced_value) {
                            changes += 1;
                            return false;
                        }
                    }
                    true
                }
            );
        };

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
