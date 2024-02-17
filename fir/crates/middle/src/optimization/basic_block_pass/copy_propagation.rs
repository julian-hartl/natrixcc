use crate::optimization::basic_block_pass::BasicBlockPass;
use rustc_hash::FxHashMap;

use crate::cfg::{BasicBlockId, BranchTerm, TerminatorKind};
use crate::instruction::{InstrKind, Op};
use crate::module::Module;
use crate::{FunctionId, Value};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
struct CopyGraph {
    edges: FxHashMap<Value, Value>
}

impl CopyGraph {
    pub fn insert_copy(&mut self, original_value: Value, copy: Value) {
        self.edges.insert(copy, original_value);
    }

    pub fn find_original_value(&self, value: Value) -> Value {
        let mut value = value;
        while let Some(original_value) = self.edges.get(&value).copied() {
            value = original_value;
        }
        value
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct CopyPropagationPass {}

impl BasicBlockPass for CopyPropagationPass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: FunctionId, basic_block: BasicBlockId) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let bb = cfg.basic_blocks[basic_block].as_mut().unwrap();
        let mut changes = 0;
        let mut copy_graph = CopyGraph::default();
        for instr in bb.instructions.iter().copied() {
            let instruction = &mut cfg.instructions[instr];
            match &mut instruction.kind {
                InstrKind::Alloca(_) => {}
                InstrKind::Store(store_instr) => {
                    changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut store_instr.value));
                }
                InstrKind::Load(_) => {}
                InstrKind::Op(op_instr) => {
                   match &op_instr.op {
                       Op::Const(_) => {}
                       Op::Value(value) => {
                           copy_graph.insert_copy(*value, op_instr.value);
                       }
                   }
                }
                InstrKind::Sub(sub_instr) => {
                    changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut sub_instr.lhs));
                    changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut sub_instr.rhs));
                }
                InstrKind::Phi(phi_instr) => {
                    for incoming in &mut phi_instr.incoming {
                        changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut incoming.op));
                    }
                }
                InstrKind::ICmp(instr) => {
                    changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut instr.op1));
                    changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut instr.op2));
                }
            }
        }

        match &mut bb.terminator.kind {
            TerminatorKind::Ret(ret_term) => {
                if let Some(value) = &mut ret_term.value {
                    changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, value));
                }
            }
            TerminatorKind::Branch(branch_term) => {
                match branch_term {
                    BranchTerm::UnCond(_) => {}
                    BranchTerm::Cond(cond_br_term) => {
                        changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut cond_br_term.cond));
                    }
                }
            }
        }
        changes
    }
}

impl CopyPropagationPass {
    fn apply_copy_graph_to_op(copy_graph: &CopyGraph, op: &mut Op) -> bool{
        match op {
            Op::Const(_) => false,
            Op::Value(value) => {
                let original_value = copy_graph.find_original_value(*value);
                if original_value != *value {
                    *value = original_value;
                    true
                } else {
                    false
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg;
    use crate::cfg::{RetTerm, TerminatorKind, ValueId};
    use crate::instruction::{Const, Op};
    use crate::optimization::{Pipeline, PipelineConfig};
    use crate::test_utils::create_test_module;
    use crate::ty::Type;

    #[test]
    fn should_propagate_copies() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = cfg::Builder::new(function_data);
        let _bb = cfg_builder.start_bb();
        let (var_instr_value, _) = cfg_builder.op(None, Op::Const(Const::i32(10))).unwrap();
        let (sub1_value, _) = cfg_builder.sub(None, Op::Value(var_instr_value), Op::Const(Const::i32(5))).unwrap();
        let (sub2_value, _) = cfg_builder.op(None, Op::Value(sub1_value)).unwrap();
        let (return_value, _) = cfg_builder.sub(
            Some(ValueId::Named("return_value".to_string())),
            Op::Value(sub1_value),
            Op::Value(sub2_value),
        ).unwrap();
        let (alloca_value, _) = cfg_builder.alloca(None, Type::I32, None, None).unwrap();
        cfg_builder.store(alloca_value, Op::Value(sub2_value)).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::new(Op::Value(return_value))));
        drop(cfg_builder);
        let mut optimization_pipeline = Pipeline::new(&mut module, PipelineConfig::copy_propagation_only());
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let mut out = String::new();
        cfg.write_to(&mut out, function_data).unwrap();
        assert_eq!(out, "bb0:
    %0 = i32 10
    %1 = sub i32 %0, 5
    %2 = i32 %1
    %return_value = sub i32 %1, %1
    %3 = alloca i32
    store i32 %1, ptr %3
    ret i32 %return_value
");
    }
}
