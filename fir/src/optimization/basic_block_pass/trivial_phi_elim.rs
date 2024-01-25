use crate::cfg::BasicBlock;
use crate::function::Function;
use crate::instruction::{InstrKind, Op, OpInstr};
use crate::module::Module;
use crate::optimization::basic_block_pass::BasicBlockPass;


#[derive(Default)]
pub struct Pass {}

impl BasicBlockPass for Pass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: Function, _basic_block: BasicBlock) -> usize {
        let function = &mut module.functions[function];
        let mut optimized_phis = 0;
        for bb in function.cfg.basic_blocks.iter().flatten() {
            for instr in bb.instructions.iter().copied() {
                let instr = &mut function.cfg.instructions[instr];
                if let InstrKind::Phi(phi) = &mut instr.kind {
                    if phi.incoming.len() == 1 {
                        instr.kind = InstrKind::Op(OpInstr {
                            op: phi.incoming[0].op.clone(),
                            value: phi.value
                        });
                        optimized_phis += 1;
                    } else if phi.incoming.len() == 2 {
                        let first_incoming = &phi.incoming[0];
                        let second_incoming = &phi.incoming[1];
                        match (&first_incoming.op, &second_incoming.op) {
                            (Op::Value(value), _) if value == &phi.value => {
                                instr.kind = InstrKind::Op(OpInstr {
                                    op: phi.incoming[1].op.clone(),
                                    value: phi.value
                                });
                                optimized_phis += 1;
                            }
                            (_, Op::Value(value)) if value == &phi.value => {
                                instr.kind = InstrKind::Op(OpInstr {
                                    op: phi.incoming[0].op.clone(),
                                    value: phi.value
                                });
                                optimized_phis += 1;
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
        optimized_phis

    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::{BranchTerm, RetTerm, TerminatorKind, UnCondBrTerm, ValueId};
    use crate::cfg_builder::CFGBuilder;
    use crate::instruction::{Const, Incoming, Op};
    use crate::optimization::{Pipeline, PipelineConfig};
    use crate::test_utils::create_test_module;
    use crate::ty::Type;

    #[test]
    fn should_remove_phi_with_one_operand() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function_data);
        cfg_builder.start_bb();
        let bb2 = cfg_builder.create_bb();
        cfg_builder.phi(None, vec![Incoming {
            source: bb2,
            op: Op::Const(Const::i32(42))
        }], Type::I32).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut optimization_pipeline = Pipeline::new(&mut module, PipelineConfig::copy_propagation_only());
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let mut out = String::new();
        cfg.write_to(&mut out, function_data).unwrap();
        assert_eq!(out, "bb0:
    %0 = i32 42
    ret void
");
    }

    #[test]
    fn should_remove_phi_with_two_operands_where_one_is_same_value_from_same_bb() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function_data);
        let bb0 = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let x_id = ValueId::Named("x".to_string());
        let (x_value, _) = cfg_builder.op(Some(x_id.clone()), Op::Const(Const::i32(10))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb1))));
        cfg_builder.set_bb(bb1);
        let (phi_value, phi_instr) = cfg_builder.phi(Some(x_id), vec![Incoming {
            source: bb0,
            op: Op::Value(x_value)
        }], Type::I32).unwrap();
        let phi_instr = cfg_builder.func.cfg.instructions[phi_instr].kind.try_as_phi_mut().unwrap();
        phi_instr.incoming.push(Incoming {
            source: bb1,
            op: Op::Value(phi_value)
        });
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb1))));
        drop(cfg_builder);
        let mut optimization_pipeline = Pipeline::new(&mut module, PipelineConfig::copy_propagation_only());
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let mut out = String::new();
        cfg.write_to(&mut out, function_data).unwrap();
        assert_eq!(out, "bb0:
    %x.0 = i32 10
    br label bb1
bb1:
    ; preds = bb0, bb1
    %x.1 = i32 %x.0
    br label bb1
");
    }
}
