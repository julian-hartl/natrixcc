use rustc_hash::FxHashMap;

use crate::{
    cfg::{
        BasicBlockId,
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
    FunctionId,
    VReg,
};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
struct CopyGraph {
    edges: FxHashMap<VReg, VReg>,
}

impl CopyGraph {
    pub fn insert_copy(&mut self, original_value: VReg, copy: VReg) {
        self.edges.insert(copy, original_value);
    }

    pub fn find_original_value(&self, value: VReg) -> VReg {
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
        function: FunctionId,
        basic_block: BasicBlockId,
    ) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let bb = cfg.basic_block_mut(basic_block);
        let mut changes = 0;
        let mut copy_graph = CopyGraph::default();
        for instr in bb.instructions_mut() {
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
                    Op::Vreg(value) => {
                        copy_graph.insert_copy(*value, op_instr.value);
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
            Op::Vreg(value) => {
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
                bb0(i32 v0):
                    v1 = i32 v0;
                    v2 = add i32 v1, v0;
                    v3 = i32 v1;
                    v4 = sub i32 v2, v3;
                    ret i32 v4;
                }
            ",
        );
        module.optimize(PipelineConfig::copy_propagation_only());
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!(
            function.to_string(),
            "fun i32 @test(i32) {
bb0(i32 v0):
    v1 = i32 v0;
    v2 = add i32 v0, v0;
    v3 = i32 v1;
    v4 = sub i32 v2, v0;
    ret i32 v4;
}
"
        )
    }
}
