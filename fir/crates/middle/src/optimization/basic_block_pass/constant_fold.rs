use rustc_hash::FxHashMap;

use crate::{FunctionId, Value};
use crate::cfg::{BasicBlockId, TerminatorKind};
use crate::instruction::{Const, ICmpCond, InstrKind, Op, OpInstr};
use crate::module::Module;
use crate::optimization::basic_block_pass::BasicBlockPass;

pub struct ConstantFoldPass {}

impl BasicBlockPass for ConstantFoldPass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: FunctionId, basic_block_id: BasicBlockId) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let bb = cfg.basic_block_mut(basic_block_id);
        let mut changes = 0;
        let mut constant_values: FxHashMap<Value, Const> = FxHashMap::default();
        for instr in bb.instructions_mut() {
            match &mut instr.kind {
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
                        instr.kind = InstrKind::Op(OpInstr { value: sub_instr.value, op: Op::Const(left_const - right_const) });
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
                            constant_values.get(place).cloned().and_then(|constant_value| {
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
                InstrKind::ICmp(icmp_instr) => {
                    let left_const = match &icmp_instr.op1 {
                        Op::Const(constant) => Some(constant.clone()),
                        Op::Value(place) => constant_values.get(place).cloned(),
                    };
                    let right_const = match &icmp_instr.op2 {
                        Op::Const(constant) => Some(constant.clone()),
                        Op::Value(place) => constant_values.get(place).cloned(),
                    };
                    if let (Some(left_const), Some(right_const)) = (left_const, right_const) {
                        instr.kind = InstrKind::Op(OpInstr {
                            value: icmp_instr.value,
                            op: Op::Const(Const::bool(
                                match icmp_instr.condition {
                                    ICmpCond::Eq => left_const == right_const,
                                }
                            )),
                        });
                        changes += 1;
                    }
                }
            }
        }
        bb.update_terminator(|terminator| {
            match &mut terminator.kind {
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
                TerminatorKind::CondBranch(cond_branch) => {
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
                TerminatorKind::Branch(_) => {}
            }
        });
        changes
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg;
    use crate::optimization::PipelineConfig;
    use crate::test::create_test_module_from_source;

    #[test]
    fn should_compute_subtraction() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test() {
                bb0():
                    %0 = i32 8
                    %1 = sub i32 %0, 7
                    ret i32 %1                       
                }
            "
        );
        module.optimize(PipelineConfig::all_disabled());
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!(
            function.write_to_string().unwrap(),
            "fun i32 @test() {
             bb():
                %0 = i32 8
                %1 = sub i32 %0, 7
                ret i32 1
             }
             "
        )
    }

    #[test]
    fn should_compute_addition() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test() {
                bb0():
                    %0 = i32 8
                    %1 = add i32 %0, 7
                    ret i32 %1                       
                }
            "
        );
        module.optimize(PipelineConfig::all_disabled());
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!(
            function.write_to_string().unwrap(),
            "fun i32 @test() {
             bb():
                %0 = i32 8
                %1 = add i32 %0, 7
                ret i32 15
             }
             "
        )
    }

    #[test]
    fn should_compute_multiplication() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test() {
                bb0():
                    %0 = i32 8
                    %1 = mul i32 %0, 7
                    ret i32 %1                      
                }
            "
        );
        module.optimize(PipelineConfig::all_disabled());
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!(
            function.write_to_string().unwrap(),
            "fun i32 @test() {
             bb():
                %0 = i32 8
                %1 = mul i32 %0, 7
                ret i32 56
             }
             "
        )
    }

    #[test]
    fn should_replace_const_variable() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test(i32) {
                bb0(i32 %0):
                    %1 = i32 8
                    %2 = add i32 %0, %1
                    ret i32 %2                       
                }
            "
        );
        module.optimize(PipelineConfig::all_disabled());
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!(
            function.write_to_string().unwrap(),
            "fun i32 @test(i32) {
             bb(i32 %0):
                %1 = i32 8
                %2 = add i32 %0, 8
                ret i32 %2
             }
             "
        )
    }
}
