use tracing::{
    debug,
    debug_span,
};

use crate::{
    analysis::dataflow::{
        concrete_value,
        concrete_value::ConcreteValues,
        DFValueState,
        InstrWalker,
    },
    cfg::TerminatorKind,
    instruction::{
        Const,
        InstrKind,
        Op,
    },
    module::Module,
    optimization::{
        FunctionPass,
        Pass,
    },
    FunctionId,
};

#[derive(Default)]
pub struct ConstantPropagation;

impl Pass for ConstantPropagation {
    fn name(&self) -> &'static str {
        "constant_propagation"
    }
}

impl FunctionPass for ConstantPropagation {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionId) -> usize {
        let mut changes = 0;
        let mut analysis_runner =
            concrete_value::AnalysisRunner::new(&mut module.functions[function]);
        while let Some((bb_id, mut instr_walker)) = analysis_runner.next_bb() {
            // let bb = instr_walker.function.cfg.basic_block(bb_id);
            // let bb_args = bb.arguments();
            // if !bb_args.is_empty() {
            // let preds = bb.predecessors(&analysis_runner.function.cfg);
            // // Collect all values that are the same constants for each predecessor
            // let mut const_values = FxHashMap::default();
            // enum State {
            //     Value(Const),
            //     NotCombinable,
            // }
            // let mut process_target = |target: &JumpTarget| {
            //     for (arg, param) in target.arguments.iter().zip(
            //         bb_args.iter().copied()
            //     ) {
            //         if let Op::Const(val) = arg {
            //             let entry = const_values.entry(param).or_insert_with(|| State::Value(val.clone()));
            //             if let State::Value(entry_val) = entry {
            //                 if entry_val.can_be_substituted_by(val) {
            //                     *entry = State::NotCombinable;
            //                 }
            //             }
            //         }
            //     }
            // };
            // for pred in preds.iter().copied().map(|pred| analysis_runner.function.cfg.basic_block(pred)) {
            //     match &pred.terminator().kind {
            //         TerminatorKind::Branch(br) => {
            //             process_target(&br.target);
            //         }
            //         TerminatorKind::CondBranch(br) => {
            //             for target in br.targets() {
            //                 process_target(target);
            //             }
            //         }
            //         TerminatorKind::Ret(_) => unreachable!("Pred terminator does not point to this bb"),
            //     }
            // }
            // // Remove all parameters and arguments, whose value is the same constant in all predecessors,
            // // and insert their value for substitution
            // for (index, (param, const_value)) in const_values.into_iter().enumerate() {
            //     let State::Value(const_value) = const_value else { continue; };
            //     let bb = analysis_runner.function.cfg.basic_block_mut(bb_id);
            //     bb.remove_argument(index);
            //     for pred in preds.iter().copied() {
            //         let pred = analysis_runner.function.cfg.basic_block_mut(pred);
            //         pred.update_terminator(|term| {
            //            match term {
            //                TerminatorKind::Branch(br) => {
            //                    changes += 1;
            //                    br.target.arguments.remove(index);
            //                }
            //                TerminatorKind::CondBranch(br) => {
            //                    for target in br.targets_mut() {
            //                        if target.id == bb_id {
            //                            changes += 1;
            //                            target.arguments.remove(index);
            //                        }
            //                    }
            //                }
            //                TerminatorKind::Ret(_) => unreachable!(),
            //            }
            //         });
            //     }
            // }
            // }

            instr_walker.walk(|instr, state| match &mut instr.kind {
                InstrKind::Op(op) => {
                    if Self::maybe_replace_op(&mut op.op, state) {
                        changes += 1;
                    }
                }
                InstrKind::Sub(instr) => {
                    if Self::maybe_replace_op(&mut instr.lhs, state) {
                        changes += 1;
                    }
                    if Self::maybe_replace_op(&mut instr.rhs, state) {
                        changes += 1;
                    }
                }
                InstrKind::Add(instr) => {
                    if Self::maybe_replace_op(&mut instr.lhs, state) {
                        changes += 1;
                    }
                    if Self::maybe_replace_op(&mut instr.rhs, state) {
                        changes += 1;
                    }
                }
                InstrKind::Cmp(icmp) => {
                    if Self::maybe_replace_op(&mut icmp.lhs, state) {
                        changes += 1;
                    }
                    if Self::maybe_replace_op(&mut icmp.rhs, state) {
                        changes += 1;
                    }
                }
                InstrKind::Alloca(_) | InstrKind::Load(_) | InstrKind::Store(_) => {}
            });

            analysis_runner
                .function
                .cfg
                .basic_block_mut(bb_id)
                .update_terminator(|terminator| {
                    let state = analysis_runner.state.get(bb_id);
                    match &mut terminator.kind {
                        TerminatorKind::Ret(ret) => {
                            if let Some(op) = &mut ret.value {
                                if Self::maybe_replace_op(op, state) {
                                    changes += 1;
                                }
                            }
                        }
                        TerminatorKind::CondBranch(branch) => {
                            if Self::maybe_replace_op(&mut branch.cond, state) {
                                changes += 1;
                            }
                            for target in branch.targets_mut() {
                                for arg in &mut target.arguments {
                                    if Self::maybe_replace_op(arg, state) {
                                        changes += 1;
                                    }
                                }
                            }
                        }
                        TerminatorKind::Branch(branch) => {
                            for arg in &mut branch.target.arguments {
                                if Self::maybe_replace_op(arg, state) {
                                    changes += 1;
                                }
                            }
                        }
                    }
                });
        }
        changes
    }
}

impl ConstantPropagation {
    fn maybe_replace_op(op: &mut Op, state: &DFValueState<ConcreteValues>) -> bool {
        match op {
            Op::Const(_) => false,
            Op::Vreg(value) => match state.get(value).and_then(|s| s.as_single_value()) {
                None => false,
                Some(const_value) => {
                    debug!("Replaced {value} with {const_value}");
                    *op = Op::Const(const_value.clone());
                    true
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::{
        optimization::PipelineConfig,
        test::{
            assert_module_is_equal_to_src,
            create_test_module_from_source,
        },
    };

    #[test]
    #[traced_test]
    fn should_replace_const_variable() {
        let mut module = create_test_module_from_source(
            "
fun i32 @test() {
bb0:
    v0 = i32 7;
    br bb1;
bb1:
    v1 = add i32 v0, 8;
    br bb2;
bb2:
    v2 = i32 9;
    v3 = add i32 v2, v0;
    ret i32 v3;
}
            ",
        );
        module.optimize(PipelineConfig::global_constant_propagation_only());
        assert_module_is_equal_to_src(
            &module,
            "
        fun i32 @test() {
bb0:
    v0 = i32 7;
    br bb1;
bb1:
    v1 = i32 15;
    br bb2;
bb2:
    v2 = i32 9;
    v3 = i32 16;
    ret i32 16;
}
 ",
        );
    }
}
