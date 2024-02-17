use daggy::{NodeIndex, Walker};
use rustc_hash::FxHashMap;
use tracing::debug;

use codegen::selection_dag;
use selection_dag::SelectionDAG;

use crate::{codegen, middle};
use crate::codegen::machine;
use crate::codegen::machine::VirtualRegister;
use crate::codegen::selection_dag::Op;
use crate::middle::instruction::Const;
use crate::ty::Type;

#[derive(Default)]
pub struct Builder<A: machine::Abi> {
    sel_dag: SelectionDAG<A>,
    value_id_to_vreg: FxHashMap<middle::ValueId, VirtualRegister>,
    value_to_defining_node: FxHashMap<middle::Value, NodeIndex>,
}

impl <A: machine::Abi> Builder<A> {
    pub fn build(mut self, func: &middle::Function) -> SelectionDAG<A> {
        debug!("Building SelectionDAGs for function {}", func.name);
        for (bb_id, bb) in func.cfg.basic_blocks.iter_enumerated() {
            if let Some(bb) = bb {
                debug!("Building SelectionDAG for basic block {}", bb_id);
                for  instr_id in bb.instructions.iter().copied() {
                    let instr = &func.cfg.instructions[instr_id];
                    debug!("Building SelectionDAG for instruction {:?}", instr);

                    match &instr.kind {
                        middle::InstrKind::Alloca(_) => unimplemented!(),
                        middle::InstrKind::Store(_) => unimplemented!(),
                        middle::InstrKind::Load(_) => unimplemented!(),
                        middle::InstrKind::Op(op_instr) => {
                            let ty = func.get_value_type(op_instr.value).clone();
                            let node = self.define_op_node(bb_id, Op::Mov, Some(ty));
                            self.add_op_to_node(&op_instr.op, bb_id, node);
                            self.define_out_val(node, op_instr.value);
                        }
                        middle::InstrKind::Sub(sub_instr) => {
                            let ty = func.get_value_type(sub_instr.value).clone();
                            let node = self.define_op_node(bb_id, Op::Sub, Some(ty));
                            self.add_op_to_node(&sub_instr.lhs, bb_id, node);
                            self.add_op_to_node(&sub_instr.rhs, bb_id, node);
                            self.define_out_val(node, sub_instr.value);
                        }
                        middle::InstrKind::Phi(phi_instr) => {
                            unimplemented!()
                        }
                        middle::InstrKind::ICmp(icmp_instr) => unimplemented!(),
                    };
                }

                match &bb.terminator.kind {
                    middle::cfg::TerminatorKind::Ret(ret_term) => {
                        let node = self.define_term_node(bb_id, Op::Ret);
                        if let Some(return_op) = ret_term.value.as_ref() {
                            self.add_op_to_node(return_op, bb_id, node);
                        }
                    }
                    middle::cfg::TerminatorKind::Branch(branch_term) => {
                        unimplemented!()
                    }
                }
            }
        }
        self.sel_dag
    }


    fn define_op_node(&mut self, bb_id: middle::cfg::BasicBlockId, op: Op, ty: Option<Type>) -> NodeIndex {
        let node = self.sel_dag.get_bb_dag(bb_id).add_node(selection_dag::Node {
            kind: selection_dag::NodeKind::Op(op),
            ty,
        });
        node
    }

    fn define_out_val(&mut self, node: NodeIndex, value: middle::Value) {
        self.value_to_defining_node.insert(value, node);
    }

    fn define_term_node(&mut self, bb_id: middle::cfg::BasicBlockId, op: Op) -> NodeIndex {
        let node = self.define_op_node(bb_id, op, None);
        let dag = self.sel_dag.get_bb_dag(bb_id);
        dag.set_term_node(node);
        node
    }

    fn add_op_to_node(&mut self, op: &middle::instruction::Op, current_bb_id: middle::cfg::BasicBlockId, node: NodeIndex) {
        match op {
            middle::instruction::Op::Const(const_value) => {
                let const_node = self.sel_dag.get_bb_dag(current_bb_id).add_node(
                    selection_dag::Node {
                        ty: Some(const_value.ty()),
                        kind: selection_dag::NodeKind::Const(match const_value {
                            Const::Int { value, .. } => selection_dag::Const::Int(*value),
                        }),
                    }
                );
                self.sel_dag.get_bb_dag(current_bb_id).add_edge(
                    node,
                    const_node,
                    selection_dag::Edge,
                ).unwrap();
            }
            middle::instruction::Op::Value(op_value) => {
                let def_node = self.get_defining_node(*op_value);
                match def_node {
                    Some(def_node) => {
                        self.sel_dag.get_bb_dag(current_bb_id).add_edge(
                            node,
                            def_node,
                            selection_dag::Edge,
                        ).unwrap();
                    }
                    None => unimplemented!("Could not find defining node for value {:?}", op_value),
                }
            }
        }
    }

    fn get_defining_node(&self, op_value: middle::Value) -> Option<NodeIndex> {
        self.value_to_defining_node.get(&op_value).copied()
    }
}

#[cfg(test)]
mod tests {
    use crate::codegen::isa::x86_64;
    use crate::middle::cfg;
    use crate::middle::cfg::{RetTerm, TerminatorKind};
    use crate::middle::instruction::{Const, Op};
    use crate::test_utils::create_test_module;

    #[test]
    fn test() {
        let (mut module, function_id) = create_test_module();
        let function = &mut module.functions[function_id];
        let mut cfg_builder = cfg::Builder::new(function);
        cfg_builder.start_bb();
        let (value, _) = cfg_builder.op(None, Op::Const(Const::i32(42))).unwrap();
        let (value, _) = cfg_builder.op(None, Op::Value(value)).unwrap();
        let (sub_result, _) = cfg_builder.sub(None, Op::Value(value), Op::Const(Const::i32(1))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::new(Op::Value(sub_result))));
        drop(cfg_builder);
        // let mut op = optimization::Pipeline::new(&mut module, optimization::PipelineConfig::o1());
        // op.run();
        let dag_builder = super::Builder::<x86_64::Abi>::default();
        let function = &mut module.functions[function_id];
        let dag = dag_builder.build(function);
        println!("{:?}", dag);
    }
}
