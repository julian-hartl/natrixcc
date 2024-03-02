use crate::analysis::dataflow::{concrete_value, DFValueState};
use crate::analysis::dataflow::concrete_value::ConcreteValues;
use crate::cfg::TerminatorKind;
use crate::FunctionId;
use crate::instruction::{InstrKind, Op};
use crate::module::Module;
use crate::optimization::FunctionPass;

#[derive(Default)]
pub struct ConstantPropagation;

impl FunctionPass for ConstantPropagation {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionId) -> usize {
        let mut changes = 0;
        let mut analysis_runner = concrete_value::AnalysisRunner::new(&mut module.functions[function]);
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

            instr_walker.walk(|instr, state| {
                match &mut instr.kind {
                    InstrKind::Op(op) => {
                        if let Op::Value(value) = &mut op.op {
                            if let Some(const_value) = state.get(value).unwrap().as_single_value() {
                                op.op = Op::Const(const_value.clone());
                                changes += 1;
                            }
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
                    InstrKind::ICmp(icmp) => {
                        if Self::maybe_replace_op(&mut icmp.op1, state) {
                            changes += 1;
                        }
                        if Self::maybe_replace_op(&mut icmp.op2, state) {
                            changes += 1;
                        }
                    }
                    InstrKind::Alloca(_) |
                    InstrKind::Load(_) |
                    InstrKind::Store(_) => {}
                }
            });

            analysis_runner.function.cfg.basic_block_mut(bb_id).update_terminator(
                |terminator| {
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
                }
            );
        };
        changes
    }
}

impl ConstantPropagation {
    fn maybe_replace_op(op: &mut Op, state: &DFValueState<ConcreteValues>) -> bool {
        match op {
            Op::Const(_) =>
                false,
            Op::Value(value) => {
                state.get(value).unwrap().as_single_value().map_or(false, |const_value| {
                    *op = Op::Const(const_value.clone());
                    true
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg;
    use crate::cfg::{BranchTerm, CondBranchTerm, JumpTarget, RetTerm, TerminatorKind};
    use crate::instruction::{Const, ICmpCond, Op};
    use crate::optimization::Pipeline;
    use crate::optimization::PipelineConfig;
    use crate::test::create_test_module;

    #[test]
    fn should_replace_const_variable() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = cfg::Builder::new(function_data);
        let _bb = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let bb3 = cfg_builder.create_bb();
        let x_value = cfg_builder.op(None, Op::Const(Const::i32(10)));
        let cond_value = cfg_builder.icmp(None, ICmpCond::Eq, Op::Value(x_value), Op::Const(Const::i32(10)));
        cfg_builder.end_bb(TerminatorKind::CondBranch(CondBranchTerm::new(
            Op::Value(cond_value),
            JumpTarget::new(bb1, vec![]),
            JumpTarget::new(bb2, vec![]),
        )));
        cfg_builder.set_bb(bb1);
        let new_val = cfg_builder.sub(None, Op::Value(x_value), Op::Const(Const::i32(5)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(bb3, vec![
            Op::Value(new_val),
        ]))));
        cfg_builder.set_bb(bb2);
        let new_val = cfg_builder.sub(None, Op::Value(x_value), Op::Const(Const::i32(5)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(
            bb3, vec![Op::Value(new_val)],
        ))));
        cfg_builder.set_bb(bb3);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::new(Op::Const(Const::i32(5)))));
        drop(cfg_builder);
        let mut optimization_pipeline = Pipeline::new(&mut module, PipelineConfig::global_constant_propagation_only());
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let mut out = String::new();
        cfg.write_to(&mut out, function_data).unwrap();
        assert_eq!(out, "bb0:
    %0 = i32 10
    %1 = i1 1
    br i1 1, bb1, bb2
bb1:
    ; preds = bb0
    %2 = i32 5
    br label bb3
bb2:
    ; preds = bb0
    %3 = i32 5
    br label bb3
bb3:
    ; preds = bb1, bb2
    ret i32 5
");
        println!("{}", out);
    }
}

