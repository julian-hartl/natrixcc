use rustc_hash::FxHashMap;

use crate::{FunctionId, Value};
use crate::cfg::{BasicBlockId, TerminatorKind};
use crate::instruction::{InstrKind, Op};
use crate::module::Module;
use crate::optimization::basic_block_pass::BasicBlockPass;

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
        let bb = cfg.basic_block_mut(basic_block);
        let mut changes = 0;
        let mut copy_graph = CopyGraph::default();
        for instr in bb.instructions_mut() {
            match &mut instr.kind {
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
                InstrKind::ICmp(instr) => {
                    changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut instr.op1));
                    changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut instr.op2));
                }
            }
        }

        bb.update_terminator(|terminator| {
           match &mut terminator.kind {
               TerminatorKind::Ret(ret_term) => {
                   if let Some(value) = &mut ret_term.value {
                       changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, value));
                   }
               }
               TerminatorKind::CondBranch(cond_branch) => {
                   changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut cond_branch.cond))
               }
               TerminatorKind::Branch(_) => {

               }
           } 
        });
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
    use crate::optimization::PipelineConfig;
    use crate::test::create_test_module_from_source;

    #[test]
    fn should_propagate_copies() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test(i32) {
                bb0(i32 %0):
                    %1 = i32 %0
                    %2 = add i32 %1, %0
                    %3 = i32 %1
                    %4 = sub i32 %2, %3
                    ret i32 %4    
                }
            "
        );
        module.optimize(PipelineConfig::copy_propagation_only());
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!(
            function.write_to_string().unwrap(),
            "fun i32 @test(i32) {
             bb(i32 %0):
                 %1 = i32 %0
                 %2 = add i32 %0, %0
                 %3 = i32 %1
                 %4 = sub i32 %2, %0 
                 ret i32 %4   
             }
             "
        )
    }
}
