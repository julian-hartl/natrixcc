use rustc_hash::FxHashMap;

use crate::{
    cfg::{
        BasicBlockRef,
        TerminatorKind,
    },
    instruction::{
        InstrKind,
        Op,
    },
    module::Module,
    optimization::{
        basic_block_pass::BasicBlockPass,
        Pass,
    },
    FunctionRef,
    Value,
};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
struct CopyGraph {
    edges: FxHashMap<Value, Value>,
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

impl Pass for CopyPropagationPass {
    fn name(&self) -> &'static str {
        "copy_propagation"
    }
}

impl BasicBlockPass for CopyPropagationPass {
    fn run_on_basic_block(
        &mut self,
        module: &mut Module,
        function: FunctionRef,
        basic_block: BasicBlockRef,
    ) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let bb = &mut cfg.basic_blocks[basic_block];
        let mut changes = 0;
        let mut copy_graph = CopyGraph::default();
        for instr_Id in bb.instructions() {
            let instr = &mut cfg.instructions[instr_Id];
            match &mut instr.kind {
                InstrKind::Alloca(_) => {}
                InstrKind::Store(store_instr) => {
                    changes += usize::from(Self::apply_copy_graph_to_op(
                        &copy_graph,
                        &mut store_instr.value,
                    ));
                }
                InstrKind::Load(_) => {}
                InstrKind::Op(op_instr) => match &op_instr.op {
                    Op::Const(_) => {}
                    Op::Value(value) => {
                        copy_graph.insert_copy(*value, instr.value());
                    }
                },
                InstrKind::Sub(sub_instr) => {
                    changes += usize::from(Self::apply_copy_graph_to_op(
                        &copy_graph,
                        &mut sub_instr.lhs,
                    ));
                    changes += usize::from(Self::apply_copy_graph_to_op(
                        &copy_graph,
                        &mut sub_instr.rhs,
                    ));
                }
                InstrKind::Add(add_instr) => {
                    changes += usize::from(Self::apply_copy_graph_to_op(
                        &copy_graph,
                        &mut add_instr.lhs,
                    ));
                    changes += usize::from(Self::apply_copy_graph_to_op(
                        &copy_graph,
                        &mut add_instr.rhs,
                    ));
                }
                InstrKind::Cmp(instr) => {
                    changes +=
                        usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut instr.lhs));
                    changes +=
                        usize::from(Self::apply_copy_graph_to_op(&copy_graph, &mut instr.rhs));
                }
            }
        }

        bb.update_terminator(|terminator| match &mut terminator.kind {
            TerminatorKind::Ret(ret_term) => {
                if let Some(value) = &mut ret_term.value {
                    changes += usize::from(Self::apply_copy_graph_to_op(&copy_graph, value));
                }
            }
            TerminatorKind::CondBranch(cond_branch) => {
                changes += usize::from(Self::apply_copy_graph_to_op(
                    &copy_graph,
                    &mut cond_branch.cond,
                ))
            }
            TerminatorKind::Branch(_) => {}
        });
        changes
    }
}

impl CopyPropagationPass {
    fn apply_copy_graph_to_op(copy_graph: &CopyGraph, op: &mut Op) -> bool {
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
    use crate::{
        optimization::PipelineConfig,
        test::create_test_module_from_source,
    };

    #[test]
    fn should_propagate_copies() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test(i32) {
                bb0(i32 %0):
                    i32 %1 = %0;
                    i32 %2 = add %1, %0;
                    i32 %3 = %1;
                    i32 %4 = sub %2, %3;
                    ret %4;
                }
            ",
        );
        module.optimize(PipelineConfig::copy_propagation_only());
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!(
            function.to_string(),
            "fun i32 @test(i32) {
bb0(i32 %0):
    i32 %1 = %0;
    i32 %2 = add %0, %0;
    i32 %3 = %1;
    i32 %4 = sub %2, %0;
    ret %4;
}
"
        )
    }
}
