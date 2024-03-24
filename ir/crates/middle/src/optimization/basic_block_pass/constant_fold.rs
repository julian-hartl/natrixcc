use rustc_hash::FxHashMap;
use tracing::debug;

use crate::{FunctionId, VReg};
use crate::cfg::{BasicBlockId, TerminatorKind};
use crate::instruction::{Const, CmpOp, InstrKind, Op, OpInstr};
use crate::module::Module;
use crate::optimization::basic_block_pass::BasicBlockPass;
use crate::optimization::Pass;

pub struct ConstantFoldPass {}

impl Pass for ConstantFoldPass {
    fn name(&self) -> &'static str {
        "constant_fold"
    }
}

impl BasicBlockPass for ConstantFoldPass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: FunctionId, basic_block_id: BasicBlockId) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let bb = cfg.basic_block_mut(basic_block_id);
        let mut changes = 0;
        let mut constant_values: FxHashMap<VReg, Const> = FxHashMap::default();
        for instr in bb.instructions_mut() {
            match &mut instr.kind {
                InstrKind::Alloca(_) => {}
                InstrKind::Sub(sub_instr) => {
                    if Self::try_replace_op(&constant_values, &mut sub_instr.lhs) {
                        changes += 1;
                    }
                    if Self::try_replace_op(&constant_values, &mut sub_instr.rhs) {
                        changes += 1;
                    }
                    if let Some(const_val) = Self::eval_binary_instr(&constant_values, &sub_instr.lhs, &sub_instr.rhs, |lhs, rhs| {
                        lhs.sub(rhs).unwrap()
                    }) {
                        instr.kind = InstrKind::Op(OpInstr {
                            value: sub_instr.value,
                            op: Op::Const(const_val),
                        });
                        changes += 1;
                    }
                }
                InstrKind::Add(add_instr) => {
                    if Self::try_replace_op(&constant_values, &mut add_instr.lhs) {
                        changes += 1;
                    }
                    if Self::try_replace_op(&constant_values, &mut add_instr.rhs) {
                        changes += 1;
                    }
                    if let Some(const_val) = Self::eval_binary_instr(&constant_values, &add_instr.lhs, &add_instr.rhs, |lhs, rhs| {
                        lhs.add(rhs).unwrap()
                    }) {
                        instr.kind = InstrKind::Op(OpInstr {
                            value: add_instr.value,
                            op: Op::Const(const_val),
                        });
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
                InstrKind::Store(store_instr) => {
                    if Self::try_replace_op(&constant_values, &mut store_instr.value) {
                        changes += 1;
                    }
                }
                InstrKind::Load(load_instr) => {
                    if Self::try_replace_op(&constant_values, &mut load_instr.source) {
                        changes += 1;
                    }
                }
                InstrKind::Cmp(icmp_instr) => {
                    if Self::try_replace_op(&constant_values, &mut icmp_instr.lhs) {
                        changes += 1;
                    }
                    if Self::try_replace_op(&constant_values, &mut icmp_instr.rhs) {
                        changes += 1;
                    }
                    if let Some(const_val) = Self::eval_binary_instr(&constant_values, &icmp_instr.lhs, &icmp_instr.rhs, |lhs, rhs| {
                        lhs.cmp(rhs, CmpOp::from(icmp_instr.op)).unwrap()
                    }) {
                        instr.kind = InstrKind::Op(OpInstr {
                            value: icmp_instr.value,
                            op: Op::Const(const_val),
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
                    if Self::try_replace_op(&constant_values, &mut cond_branch.cond) {
                        changes += 1;
                    }
                }
                TerminatorKind::Branch(_) => {}
            }
        });
        changes
    }
}

impl ConstantFoldPass {
    fn op_to_const(constant_values: &FxHashMap<VReg, Const>, op: &Op) -> Option<Const> {
        match op {
            Op::Const(constant) => Some(constant.clone()),
            Op::Value(place) => constant_values.get(place).cloned(),
        }
    }

    fn try_replace_op(constant_values: &FxHashMap<VReg, Const>, op: &mut Op) -> bool {
        if let Op::Value(value) = op {
            if let Some(const_val) = constant_values.get(value) {
                debug!("Replacing {value} with {const_val}");
                *op = Op::Const(const_val.clone());
                return true;
            }
        }
        false
    }

    fn eval_binary_instr<E>(constant_values: &FxHashMap<VReg, Const>, lhs: &Op, rhs: &Op, eval: E) -> Option<Const> where E: FnOnce(Const, Const) -> Const {
        let left_const = Self::op_to_const(constant_values, lhs);
        let right_const = Self::op_to_const(constant_values, rhs);
        if let (Some(left_const), Some(right_const)) = (left_const, right_const) {
            return Some(eval(left_const, right_const));
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg;
    use crate::optimization::PipelineConfig;
    use crate::test::{assert_module_is_equal_to_src, create_test_module_from_source};

    #[test]
    fn should_compute_subtraction() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test() {
                bb0:
                    v0 = i32 8;
                    v1 = sub i32 v0, 7;
                    ret i32 v1;
                }
            "
        );
        module.optimize(PipelineConfig::all_disabled());
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!(
            "fun i32 @test() {
bb0:
    v0 = i32 8;
    v1 = i32 1;
    ret i32 1;
}
",
            function.to_string(),
        )
    }

    #[test]
    fn should_compute_addition() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test() {
                bb0:
                    v0 = i32 8;
                    v1 = add i32 v0, 7;
                    ret i32 v1;
                }
            "
        );
        module.optimize(PipelineConfig::all_disabled());
        assert_module_is_equal_to_src(
            &module,
            "
            fun i32 @test() {
            bb0:
                v0 = i32 8;
                v1 = i32 15;
                ret i32 15;
            }",
        )
    }

    #[test]
    fn should_replace_const_variable() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test(i32) {
                bb0(i32 v0):
                    v1 = i32 8;
                    v2 = add i32 v0, v1;
                    ret i32 v2;
                }
            "
        );
        module.optimize(PipelineConfig::all_disabled());
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!(
            "fun i32 @test(i32) {
bb0(i32 v0):
    v1 = i32 8;
    v2 = add i32 v0, 8;
    ret i32 v2;
}
",
            function.to_string()
        )
    }
}
