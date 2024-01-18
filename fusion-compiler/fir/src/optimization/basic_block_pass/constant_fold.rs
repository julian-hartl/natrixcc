use rustc_hash::FxHashMap;

use crate::cfg::BasicBlock;
use crate::function::Function;
use crate::instruction::{Const, InstrKind, Op, OpInstr, Place};
use crate::module::Module;
use crate::optimization::basic_block_pass::BasicBlockPass;

pub struct ConstantFoldPass {}

impl BasicBlockPass for ConstantFoldPass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: Function, basic_block: BasicBlock) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let bb = cfg.basic_blocks[basic_block].as_ref().unwrap();
        let mut changes = 0;
        let mut constant_values: FxHashMap<Place, Const> = FxHashMap::default();
        for instr in bb.instructions.iter().copied() {
            let instr_data = &mut cfg.instructions[instr];
            match &mut instr_data.kind {
                InstrKind::Alloca(_) => {}
                InstrKind::Sub(sub_instr) => {
                    let left_const = match &sub_instr.lhs {
                        Op::Const(constant) => Some(constant.clone()),
                        Op::Place(place) => constant_values.get(place).cloned(),
                    };
                    let right_const = match &sub_instr.rhs {
                        Op::Const(constant) => Some(constant.clone()),
                        Op::Place(place) => constant_values.get(place).cloned(),
                    };
                    if let (Some(left_const), Some(right_const)) = (left_const, right_const) {
                        instr_data.kind = InstrKind::Op(OpInstr { place: sub_instr.place.clone(), op: Op::Const(left_const - right_const) });
                        changes += 1;
                    }
                }
                InstrKind::Op(op_instr) => {
                    let updated_op = match &op_instr.op {
                        Op::Const(constant) => {
                            constant_values.insert(op_instr.place, constant.clone());
                            None
                        }
                        Op::Place(place) => {
                            if let Some(constant_value) = constant_values.get(place).cloned() {
                                constant_values.insert(*place, constant_value.clone());
                                changes += 1;
                                Some(Op::Const(constant_value))
                            } else {
                                None
                            }
                        }
                    };
                    if let Some(updated_op) = updated_op {
                        op_instr.op = updated_op;
                    }
                }
                InstrKind::Store(_) => {}
                InstrKind::Load(_) => {}
                InstrKind::Phi(_) => {}
            }
        }
        changes
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::{RetTerm, TerminatorKind};
    use crate::cfg_builder::CFGBuilder;
    use crate::create_test_module;
    use crate::instruction::{Const, InstrKind, Op, OpInstr};
    use crate::optimization::OptimizationPipeline;

    #[test]
    fn should_compute_subtraction() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function_data);
        let bb = cfg_builder.start_bb();
        cfg_builder.sub(None, Op::Const(Const::I32(10)), Op::Const(Const::I32(5))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut optimization_pipeline = OptimizationPipeline::new(&mut module);
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let bb = cfg.basic_blocks[bb].as_ref().unwrap();
        assert_eq!(bb.instructions.len(), 1);
        let instr = &cfg.instructions[bb.instructions[0]];
        assert_eq!(instr.kind, InstrKind::Op(OpInstr { place: instr.target_place().unwrap().clone(), op: Op::Const(Const::I32(5)) }));
    }

    #[test]
    fn should_replace_const_variable() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function_data);
        let bb = cfg_builder.start_bb();
        let var_instr = cfg_builder.op(None, Op::Const(Const::I32(10))).unwrap();
        let var_instr = &cfg_builder.func.cfg.instructions[var_instr].kind.as_op_instr().unwrap();
        let sub_instr = cfg_builder.sub(None, Op::Place(var_instr.place), Op::Const(Const::I32(5))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let sub_instr_target_id = function_data.cfg.instructions[sub_instr].kind.as_sub_instr().unwrap().place;
        let mut optimization_pipeline = OptimizationPipeline::new(&mut module);
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let bb = cfg.basic_blocks[bb].as_ref().unwrap();
        // Unused variables are removed in another pass
        assert_eq!(bb.instructions.len(), 2);
        let instr = &cfg.instructions[bb.instructions[1]];

        assert_eq!(instr.kind, InstrKind::Op(OpInstr { place: sub_instr_target_id, op: Op::Const(Const::I32(5)) }));
    }
}
