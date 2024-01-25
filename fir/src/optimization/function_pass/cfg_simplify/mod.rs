use crate::function::Function;
use crate::module::Module;
use crate::optimization::function_pass::FunctionPass;

mod jump_threading;
mod bb_merge;
mod unreachable_bb_elim;

#[derive(Default)]
pub struct Pass {
    jump_threading: jump_threading::Pass,
    bb_merge: bb_merge::Pass,
    unreachable_bb_elim: unreachable_bb_elim::Pass,
}

impl FunctionPass for Pass {
    fn run_on_function(&mut self, module: &mut Module, function: Function) -> usize {
        let mut changed = 0;
        changed += self.jump_threading.run_on_function(module, function);
        changed += self.unreachable_bb_elim.run_on_function(module, function);
        changed += self.bb_merge.run_on_function(module, function);
        changed
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::{BranchTerm, CondBrTerm, RetTerm, TerminatorKind, UnCondBrTerm, ValueId};
    use crate::cfg_builder::CFGBuilder;
    use crate::instruction::{Const, ICmpCond, Op};
    use crate::optimization::{Pipeline, PipelineConfig};
    use crate::test_utils::create_test_module;

    #[test]
    fn should_merge_jump_chain() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function_data);
        let bb = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let bb3 = cfg_builder.create_bb();
        cfg_builder.op(None, Op::Const(Const::i32(10))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::Cond(CondBrTerm::new(
            Op::Const(Const::i1(true)),
            bb1,
            bb2,
        ))));
        cfg_builder.set_bb(bb1);
        let (return_value, _)  = cfg_builder.op(None, Op::Const(Const::i32(5))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb3))));
        cfg_builder.set_bb(bb2);
        cfg_builder.op(None, Op::Const(Const::i32(2))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb3))));
        cfg_builder.set_bb(bb3);
        cfg_builder.op(None, Op::Const(Const::i32(1))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::new(Op::Value(return_value))));
        drop(cfg_builder);
        let mut optimization_pipeline = Pipeline::new(&mut module, PipelineConfig {
            cfg_simplify: true,
            ..PipelineConfig::all_disabled()
        });
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let mut out = String::new();
        cfg.write_to(&mut out, function_data).unwrap();
        assert_eq!(out, "bb0:
    %0 = i32 10
    %1 = i32 5
    %3 = i32 1
    ret i32 %1
");
        println!("{}", out);
    }
}
