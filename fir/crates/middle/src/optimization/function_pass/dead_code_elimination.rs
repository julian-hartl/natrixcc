use crate::analysis::dataflow;
use crate::FunctionId;
use crate::module::Module;
use crate::optimization::FunctionPass;

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct DeadCodeEliminationPass {}

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
    use crate::cfg;
    use crate::cfg::{BranchTerm, JumpTarget, RetTerm, TerminatorKind};
    use crate::instruction::{Const, Op};
    use crate::optimization::Pipeline;
    use crate::optimization::PipelineConfig;
    use crate::test::create_test_module;

    #[test]
    fn should_eliminate_unused_values() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = cfg::Builder::new(function_data);
        let _bb0 = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let value = cfg_builder.op(None, Op::Const(Const::i32(42)));
        let _ = cfg_builder.op(None, Op::Const(Const::i32(90)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(bb1, vec![]))));
        cfg_builder.set_bb(bb1);
        cfg_builder.op(None, Op::Value(value));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(bb2, vec![]))));
        cfg_builder.set_bb(bb2);
        let return_value = cfg_builder.op(None, Op::Value(value));
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::new(Op::Value(return_value))));
        drop(cfg_builder);
        let mut optimization_pipeline = Pipeline::new(&mut module, PipelineConfig::dead_code_elimination_only());
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let mut out = String::new();
        cfg.write_to(&mut out, function_data).unwrap();
        assert_eq!(out, "bb0:
    %0 = i32 42
    br bb1
bb1:
    ; preds = bb0
    br bb2
bb2:
    ; preds = bb1
    %3 = i32 %0
    ret i32 %3
");
    }
}
