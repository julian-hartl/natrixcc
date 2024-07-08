use tracing::debug;

use crate::{
    analysis::dataflow,
    module::Module,
    optimization::{FunctionPass, Pass},
    FunctionRef, Value,
};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct DeadCodeEliminationPass {}

impl Pass for DeadCodeEliminationPass {
    fn name(&self) -> &'static str {
        "dead_code_elim"
    }
}

impl FunctionPass for DeadCodeEliminationPass {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionRef) -> usize {
        let function = &mut module.functions[function];
        let runner = dataflow::use_def::AnalysisRunner::new(function);
        let state = runner.collect();
        let mut changes = 0;
        let mut instructions_to_remove = Vec::new();
        for value in state.unused_values(function.cfg.values()) {
            match value {
                Value::Instr(instr_id) => {
                    let instr = &function.cfg.instructions[instr_id];
                    debug!("Removing unused def {instr}");
                    changes += 1;
                    instructions_to_remove.push(instr_id);
                }
                Value::BBArg(_) => {}
            }
        }

        for instr_id in instructions_to_remove {
            let defined_in = function
                .cfg
                .instructions
                .remove(instr_id)
                .expect("Tried to remove an instruction that does not exist")
                .defined_in;
            function.cfg.basic_blocks[defined_in]
                .instructions
                .shift_remove(&instr_id);
        }

        changes
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        optimization::PipelineConfig,
        test::{assert_module_is_equal_to_src, create_test_module_from_source},
    };

    #[test]
    fn should_eliminate_unused_values() {
        let mut module = create_test_module_from_source(
            "
            fun i32 @test() {
            bb0:
                i32 %0 = 20i32;
                i32 %1 = add %0, 8i32;
                i32 %2 = sub %0, 9i32;
                br bb1;
            bb1:
                ret %0;
            }
        ",
        );
        module.optimize(PipelineConfig::dead_code_elimination_only());
        assert_module_is_equal_to_src(
            &module,
            "
            fun i32 @test() {
            bb0:
                i32 %0 = 20i32;
                br bb1;
            bb1:
                ret %0;
            }
        ",
        );
    }
}
