use rustc_hash::FxHashSet;
use crate::cfg::{BranchTerm, TerminatorKind};

use crate::function::Function;
use crate::instruction::{InstrKind, Op, Value};
use crate::module::Module;
use crate::optimization::basic_block_pass::BasicBlockPass;
use crate::optimization::function_pass::FunctionPass;

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct DeadCodeEliminationPass {}

impl FunctionPass for DeadCodeEliminationPass {
    fn run_on_function(&mut self, module: &mut Module, function: Function) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let mut changes = 0;
        let mut referenced_values: FxHashSet<Value> = FxHashSet::default();
        for basic_block in cfg.basic_blocks.indices() {
            let bb = cfg.basic_blocks[basic_block].as_ref().unwrap();
            for instr in bb.instructions.iter().copied() {
                let instruction = &cfg.instructions[instr];
                match &instruction.kind {
                    InstrKind::Alloca(_) => {
                    }
                    InstrKind::Store(store_instr) => {
                        Self::insert_op(&store_instr.value, &mut referenced_values);
                    }
                    InstrKind::Load(load_instr) => {
                        referenced_values.insert(load_instr.source);
                    }
                    InstrKind::Op(op_instr) => {
                        Self::insert_op(&op_instr.op, &mut referenced_values);
                    }
                    InstrKind::Sub(sub_instr) => {
                        Self::insert_op(&sub_instr.lhs, &mut referenced_values);
                        Self::insert_op(&sub_instr.rhs, &mut referenced_values);
                    }
                    InstrKind::Phi(phi_instr) => {
                        for incoming in &phi_instr.incoming {
                            Self::insert_op(&incoming.op, &mut referenced_values);
                        }
                    }
                    InstrKind::ICmp(icmp_instr) => {
                        Self::insert_op(&icmp_instr.op1, &mut referenced_values);
                        Self::insert_op(&icmp_instr.op2, &mut referenced_values);
                    }
                }
            }

            match &bb.terminator.kind {
                TerminatorKind::Ret(ret_term) => {
                    if let Some(value) = &ret_term.value {
                        Self::insert_op(value, &mut referenced_values);
                    }
                }
                TerminatorKind::Branch(branch_term) => {
                    match branch_term {
                        BranchTerm::UnCond(_) => {

                        }
                        BranchTerm::Cond(cond_br_term) => {
                            Self::insert_op(&cond_br_term.cond, &mut referenced_values);
                        }
                    }
                }
            }

        }

        for basic_block in cfg.basic_blocks.indices() {
            let bb = cfg.basic_blocks[basic_block].as_mut().unwrap();
            bb.instructions.retain(
                |instr| {
                    let instruction = &cfg.instructions[*instr];
                    let produced_value = instruction.produced_value();
                    if let Some(produced_value) = produced_value {
                        if !referenced_values.contains(&produced_value) {
                            changes += 1;
                            return false;
                        }
                    }
                    true
                }
            );
        }


        changes
    }
}

impl DeadCodeEliminationPass {
    fn insert_op(op: &Op, referenced_values: &mut FxHashSet<Value>) {
        match op {
            Op::Const(_) => {}
            Op::Value(value) => {
                referenced_values.insert(*value);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::{BranchTerm, RetTerm, TerminatorKind, UnCondBrTerm};
    use crate::cfg_builder::CFGBuilder;
    use crate::instruction::{Const, Op};
    use crate::optimization::{Pipeline, PipelineConfig};
    use crate::test_utils::create_test_module;

    #[test]
    fn should_eliminate_unused_values() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function_data);
        let bb0 = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let (value, _) = cfg_builder.op(None, Op::Const(Const::i32(42))).unwrap();
        let (unused_value, _) = cfg_builder.op(None, Op::Const(Const::i32(90))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb1))));
        cfg_builder.set_bb(bb1);
        cfg_builder.op(None, Op::Value(value)).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb2))));
        cfg_builder.set_bb(bb2);
        let (return_value, _)=cfg_builder.op(None, Op::Value(value)).unwrap();
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
    br label bb1
bb1:
    ; preds = bb0
    br label bb2
bb2:
    ; preds = bb1
    %3 = i32 %0
    ret i32 %3
");
    }
}
