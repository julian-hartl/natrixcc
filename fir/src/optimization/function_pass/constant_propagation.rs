use crate::analysis;
use crate::analysis::dataflow::concrete_value::ConcreteValues;
use crate::analysis::dataflow::lattice;
use crate::analysis::dataflow::lattice::Element;
use crate::cfg::ValueContext;
use crate::function::Function;
use crate::instruction::{InstrKind, Op};
use crate::module::Module;
use crate::optimization::function_pass::FunctionPass;

#[derive(Default)]
pub struct ConstantPropagation {
    concrete_value_analysis: analysis::dataflow::concrete_value::Analysis
}

impl FunctionPass for ConstantPropagation {
    fn run_on_function(&mut self, module: &mut Module, function: Function) -> usize {
        let function = &mut module.functions[function];
        self.concrete_value_analysis.run(function, |analysis, function, _bb, instr| {
            let instr = &mut function.cfg.instructions[instr];
            match &mut instr.kind {
                InstrKind::Alloca(_) => false,
                InstrKind::Store(_) => false,
                InstrKind::Load(_) => false,
                InstrKind::Op(op) => {
                    match &mut op.op {
                        Op::Const(const_val) => {
                            let values = ConcreteValues::from_single_value(const_val.clone());
                            let value_data = &function.cfg.values_ctx[op.value];
                            let result = analysis.get_lattice_element(op.value, value_data).join(lattice::Element::Value(values));
                            result
                        }
                        Op::Value(value) => {
                            let value_data = &function.cfg.values_ctx[op.value];
                            match analysis.lookup_lattice_element(*value, value_data).unwrap() {
                                Element::Bottom | Element::Top => false,
                                Element::Value(values) => {
                                    match values.as_single_value() {
                                        None => false,
                                        Some(const_value) => {
                                            op.op = Op::Const(const_value.clone());
                                            true
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                InstrKind::Sub(instr) => {
                    let changed_lhs = Self::maybe_replace_op(&mut instr.lhs, analysis, &function.cfg.values_ctx);
                    let changed_rhs = Self::maybe_replace_op(&mut instr.rhs, analysis, &function.cfg.values_ctx);
                    changed_lhs || changed_rhs
                }
                InstrKind::Phi(phi) => {
                    let mut changed = false;
                    for incoming in &mut phi.incoming {
                        let changed_op = Self::maybe_replace_op(&mut incoming.op, analysis, &function.cfg.values_ctx);
                        changed = changed || changed_op;
                    }
                    changed
                }
                InstrKind::ICmp(icmp) => {
                    let changed_lhs = Self::maybe_replace_op(&mut icmp.op1, analysis, &function.cfg.values_ctx);
                    let changed_rhs = Self::maybe_replace_op(&mut icmp.op2, analysis, &function.cfg.values_ctx);
                    changed_lhs || changed_rhs
                }
            }
        })
    }
}

impl ConstantPropagation {
    fn maybe_replace_op(op: &mut Op, analysis: &analysis::dataflow::concrete_value::Analysis, value_ctx: &ValueContext) -> bool {
        match op {
            Op::Const(_) =>
                false,
            Op::Value(value) => {
                let value_data = &value_ctx[*value];
                analysis.lookup_lattice_element(*value, value_data).map_or(false, |elem| match elem {
                    Element::Bottom | Element::Top => false,
                    Element::Value(values) => {
                        values.as_single_value().map_or(false, |const_value| {
                            *op = Op::Const(const_value.clone());
                            true
                        })
                    }
                })
            }
        }
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
    fn should_replace_const_variable() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function_data);
        let _bb = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let bb3 = cfg_builder.create_bb();
        let x_id = ValueId::Named("x".to_string());
        let (x_value, _) = cfg_builder.op(Some(x_id.clone()), Op::Const(Const::i32(10))).unwrap();
        let (cond_value, _) = cfg_builder.icmp(Some(ValueId::Named("cond".to_string())),ICmpCond::Eq, Op::Value(x_value), Op::Const(Const::i32(10))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::Cond(CondBrTerm::new(
            Op::Value(cond_value),
            bb1,
            bb2,
        ))));
        cfg_builder.set_bb(bb1);
        cfg_builder.sub(Some(x_id.clone()), Op::Value(x_value), Op::Const(Const::i32(5))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb3))));
        cfg_builder.set_bb(bb2);
        cfg_builder.sub(Some(x_id.clone()), Op::Value(x_value), Op::Const(Const::i32(8))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb3))));
        cfg_builder.set_bb(bb3);
        let x_joined_value = cfg_builder.find_or_insert_reaching_value(&x_id).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::new(Op::Value(x_joined_value))));
        drop(cfg_builder);
        let mut optimization_pipeline = Pipeline::new(&mut module, PipelineConfig::global_constant_propagation_only());
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let mut out = String::new();
        cfg.write_to(&mut out, function_data).unwrap();
        assert_eq!(out, "bb0:
    %x.0 = i32 10
    %cond = i1 1
    br i1 1, label bb1, label bb2
bb1:
    ; preds = bb0
    %x.1 = i32 5
    br label bb3
bb2:
    ; preds = bb0
    %x.2 = i32 2
    br label bb3
bb3:
    ; preds = bb1, bb2
    %x.3 = phi i32 [ 5, bb1 ], [ 2, bb2 ]
    ret i32 %x.3
");
        println!("{}", out);
    }
}

