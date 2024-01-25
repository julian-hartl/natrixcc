use rustc_hash::FxHashMap;

use crate::cfg::{BasicBlock, BranchTerm, CondBrTerm, RetTerm, TerminatorKind};
use crate::function::Function;
use crate::instruction::{Const, ICmpCond, InstrKind, Op, OpInstr, Value};
use crate::module::Module;
use crate::optimization::basic_block_pass::BasicBlockPass;

pub struct ConstantFoldPass {}

impl BasicBlockPass for ConstantFoldPass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: Function, basic_block: BasicBlock) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let bb = cfg.basic_blocks[basic_block].as_mut().unwrap();
        let mut changes = 0;
        let mut constant_values: FxHashMap<Value, Const> = FxHashMap::default();
        for instr in bb.instructions.iter().copied() {
            let instr_data = &mut cfg.instructions[instr];
            match &mut instr_data.kind {
                InstrKind::Alloca(_) => {}
                InstrKind::Sub(sub_instr) => {
                    let left_const = match &sub_instr.lhs {
                        Op::Const(constant) => Some(constant.clone()),
                        Op::Value(place) => constant_values.get(place).cloned(),
                    };
                    let right_const = match &sub_instr.rhs {
                        Op::Const(constant) => Some(constant.clone()),
                        Op::Value(place) => constant_values.get(place).cloned(),
                    };
                    if let (Some(left_const), Some(right_const)) = (left_const, right_const) {
                        instr_data.kind = InstrKind::Op(OpInstr { value: sub_instr.value, op: Op::Const(left_const - right_const) });
                        changes += 1;
                    }
                }
                InstrKind::Op(op_instr) => {
                    let updated_op = match &op_instr.op {
                        Op::Const(constant) => {
                            constant_values.insert(op_instr.value, constant.clone());
                            None
                        }
                        Op::Value(place) => {
                            constant_values.get(place).cloned().map_or(None, |constant_value| {
                                constant_values.insert(*place, constant_value.clone());
                                changes += 1;
                                Some(Op::Const(constant_value))
                            })
                        }
                    };
                    if let Some(updated_op) = updated_op {
                        op_instr.op = updated_op;
                    }
                }
                InstrKind::Store(_) => {}
                InstrKind::Load(_) => {}
                InstrKind::Phi(_) => {}
                InstrKind::ICmp(instr) => {
                    let left_const = match &instr.op1 {
                        Op::Const(constant) => Some(constant.clone()),
                        Op::Value(place) => constant_values.get(place).cloned(),
                    };
                    let right_const = match &instr.op2 {
                        Op::Const(constant) => Some(constant.clone()),
                        Op::Value(place) => constant_values.get(place).cloned(),
                    };
                    if let (Some(left_const), Some(right_const)) = (left_const, right_const) {
                        instr_data.kind = InstrKind::Op(OpInstr { value: instr.value, op: Op::Const(Const::i1(
                            match instr.condition {
                                ICmpCond::Eq => left_const == right_const,
                            }
                        )) });
                        changes += 1;
                    }
                }
            }
        }
        match &mut bb.terminator.kind {
            TerminatorKind::Ret(ret_term) => {
                if let Some(ret_value) = &mut ret_term.value {
                    match ret_value {
                        Op::Const(_) => {}
                        Op::Value(value) => {
                            if let Some(constant_value) = constant_values.get(value) {
                                changes += 1;
                                *ret_value = Op::Const(constant_value.clone());
                            }

                        }
                    }
                }
            }
            TerminatorKind::Branch(branch) => {
                match branch {
                    BranchTerm::UnCond(_) => {}
                    BranchTerm::Cond(cond_branch) => {
                        match &cond_branch.cond {
                            Op::Const(_) => {}
                            Op::Value(value) => {
                                if let Some(constant_value) = constant_values.get(value) {
                                    changes += 1;
                                    cond_branch.cond = Op::Const(constant_value.clone());
                                }
                            }
                        }
                    }
                }
            }
        }
        changes
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::{RetTerm, TerminatorKind};
    use crate::cfg_builder::CFGBuilder;
    use crate::instruction::{Const, InstrKind, Op, OpInstr};
    use crate::optimization::{Pipeline, PipelineConfig};
    use crate::test_utils::create_test_module;

    #[test]
    fn should_compute_subtraction() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function_data);
        let bb = cfg_builder.start_bb();
        cfg_builder.sub(None, Op::Const(Const::i32(10)), Op::Const(Const::i32(5))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut optimization_pipeline = Pipeline::new(&mut module, PipelineConfig::all_disabled());
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let bb = cfg.basic_blocks[bb].as_ref().unwrap();
        assert_eq!(bb.instructions.len(), 1);
        let instr = &cfg.instructions[bb.instructions[0]];
        assert_eq!(instr.kind, InstrKind::Op(OpInstr { value: instr.produced_value().unwrap(), op: Op::Const(Const::i32(5)) }));
    }

    #[test]
    fn should_replace_const_variable() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function_data);
        let bb = cfg_builder.start_bb();
        let (var_instr_value, _) = cfg_builder.op(None, Op::Const(Const::i32(10))).unwrap();
        let (sub_value, _) = cfg_builder.sub(None, Op::Value(var_instr_value), Op::Const(Const::i32(5))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut optimization_pipeline = Pipeline::new(&mut module, PipelineConfig::all_disabled());
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let bb = cfg.basic_blocks[bb].as_ref().unwrap();
        // Unused variables are removed in another pass
        assert_eq!(bb.instructions.len(), 2);
        let instr = &cfg.instructions[bb.instructions[1]];

        assert_eq!(instr.kind, InstrKind::Op(OpInstr { value: sub_value, op: Op::Const(Const::i32(5)) }));
    }
}
