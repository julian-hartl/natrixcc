use std::collections::VecDeque;
use std::num::NonZeroUsize;

use cranelift_entity::{EntityRef, SecondaryMap, SparseMap};
use daggy::{NodeIndex, Walker};
use daggy::petgraph::visit::IntoNodeIdentifiers;
use iter_tools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};
use tracing::debug;

use codegen::selection_dag;
use firc_middle::cfg::{BasicBlockId, BranchTerm, JumpTarget, Terminator, TerminatorKind};
use firc_middle::InstrKind;
use firc_middle::instruction::{Const, OpInstr, VRegData};
use firc_middle::ty::Type;
use selection_dag::SelectionDAG;

use crate::codegen;
use crate::codegen::machine;
use crate::codegen::machine::{Abi, Function, Register, VReg};
use crate::codegen::machine::abi::calling_convention::Slot;
use crate::codegen::machine::abi::CallingConvention;
use crate::codegen::selection_dag::{Byte, Immediate, MachineOp, Op, Operand, PseudoOp, Word};

#[derive(Debug)]
pub struct Builder<'func, A: machine::Abi> {
    function: &'func mut Function<A>,
    sel_dag: SelectionDAG<A>,
    reg_mapping: SecondaryMap<firc_middle::VReg, Option<VReg>>,
    defining_nodes: FxHashMap<(VReg, BasicBlockId), NodeIndex>,
}

impl<'func, A: machine::Abi> Builder<'func, A> {
    pub fn new(function: &'func mut Function<A>) -> Self {
        Self {
            function,
            reg_mapping: SecondaryMap::new(),
            sel_dag: SelectionDAG::default(),
            defining_nodes: FxHashMap::default(),
        }
    }

    pub fn build(mut self, func: &mut firc_middle::Function) -> SelectionDAG<A> {
        debug!("Building SelectionDAGs for function {}", func.name);
        let basic_blocks = func.cfg.basic_block_ids().filter(
            |bb_id| *bb_id != func.cfg.entry_block()
        ).collect::<Vec<_>>();

        for bb_id in basic_blocks {
            // Eliminate basic block arguments
            debug!("Eliminating basic block arguments for {}", bb_id);
            let bb_args = func.cfg.basic_block(bb_id).arguments()
                .map(|arg| (arg, func.cfg.vreg_ty_cloned(arg)))
                .collect_vec();
            let mut temp_regs = FxHashMap::<_, Vec<_>>::default();
            let preds = func.cfg.predecessors(bb_id).collect::<Vec<_>>();
            for pred_id in preds {
                let args = func.cfg.basic_block(pred_id).terminator().branch_args(bb_id).unwrap().cloned().collect::<Vec<_>>();
                if args.is_empty() {
                    continue;
                }
                let succ_len = func.cfg.successors(pred_id).count();
                let copy_instr_bb = if succ_len == 1 {
                    // No need to split the critical edge
                    func.cfg.basic_block_mut(pred_id).update_terminator(|term|
                        {
                            term.clear_args(bb_id);
                        });
                    pred_id
                } else {
                    // Create new basic block for copying the argument ops to temp regs
                    let critical_edge_split_bb = func.cfg.new_basic_block();
                    func.cfg.basic_block_mut(critical_edge_split_bb).set_terminator(
                        Terminator::new(TerminatorKind::Branch(BranchTerm::new(JumpTarget::no_args(
                            bb_id
                        ))))
                    );
                    func.cfg.recompute_successors(critical_edge_split_bb);
                    func.cfg.basic_block_mut(pred_id).update_terminator(|term|
                        {
                            term.clear_args(bb_id);
                            term.update_references_to_bb(bb_id, critical_edge_split_bb);
                        });

                    func.cfg.recompute_successors(pred_id);
                    critical_edge_split_bb
                };

                for (arg, (bb_arg, ty)) in args.into_iter().zip(bb_args.iter()) {
                    // Create a temp reg and copy the jump arg to it
                    let temp_reg = func.cfg.new_vreg(VRegData {
                        defined_in: pred_id,
                        ty: ty.clone(),
                    });
                    temp_regs.entry(*bb_arg).or_default().push(temp_reg);
                    let instr = func.cfg.copy_op_instr(
                        temp_reg,
                        arg,
                        ty.clone(),
                    );
                    func.cfg.add_instruction(copy_instr_bb, instr);
                }
            }
            let _ = func.cfg.basic_block_mut(bb_id).clear_arguments();
            for (arg, _) in bb_args {
                let mapped_arg = self.map_vreg(arg, func);
                let operands = temp_regs.remove(&arg).unwrap().into_iter().map(
                    |reg| {
                        let mapped = self.map_vreg(reg, func);
                        // Ensure that the temp reg and arg reg will be placed in the same location => we can trivially remove the phi instruction later on
                        self.function.tie_vreg(mapped, mapped_arg);
                        Register::Virtual(mapped)
                    }
                ).collect_vec();
                let pseudo_op = PseudoOp::Phi(
                    Register::Virtual(mapped_arg),
                    operands,
                );
                debug!("Adding phi to {bb_id}: {:?}", pseudo_op);
                let phi_op = Op::Pseudo(pseudo_op);
                self.define_node(bb_id, phi_op);
            }
            debug!("Eliminated basic block arguments for {}", bb_id);
        }
        for (bb_id, bb) in func.cfg.basic_blocks() {
            debug!("Building SelectionDAG for basic block {}", bb_id);
            if bb_id == func.cfg.entry_block() {
                let param_slots = A::CallingConvention::parameter_slots(
                    bb.arguments().map(|arg| func.cfg.vreg_ty_cloned(arg))
                );
                for (slot, arg) in param_slots.into_iter().zip(bb.arguments()) {
                    match slot {
                        Slot::Register(reg) => {
                            let phys_reg = Register::Physical(reg);
                            let mapped_reg = self.map_vreg(arg, func);
                            self.define_node(bb_id, Op::Pseudo(PseudoOp::Copy(
                                Register::Virtual(mapped_reg),
                                phys_reg,
                            )));
                        }
                        Slot::Stack => unimplemented!()
                    };
                }
            }
            for instr in bb.instructions() {
                debug!("Add instruction to SelectionDAG: {:?}", instr);

                match &instr.kind {
                    firc_middle::InstrKind::Alloca(_) => unimplemented!(),
                    firc_middle::InstrKind::Store(_) => unimplemented!(),
                    firc_middle::InstrKind::Load(_) => unimplemented!(),
                    firc_middle::InstrKind::Op(op_instr) => {
                        let out_reg = self.map_vreg(op_instr.value, func);
                        let op = match self.map_op(&op_instr.op, func) {
                            Operand::Reg(reg) => Op::Pseudo(PseudoOp::Copy(
                                Register::Virtual(out_reg),
                                reg,
                            )),
                            Operand::Imm(imm) => Op::Machine(MachineOp::Mov(
                                Register::Virtual(out_reg),
                                Operand::Imm(imm),
                            )),
                        };
                        self.define_node(bb_id, op);
                    }
                    firc_middle::InstrKind::Sub(sub_instr) => {
                        let out_reg = self.map_vreg(sub_instr.value, func);
                        let lhs = self.map_op(&sub_instr.lhs, func);
                        let rhs = self.map_op(&sub_instr.rhs, func);
                        self.define_node(bb_id, Op::Machine(MachineOp::Sub(
                            Register::Virtual(out_reg),
                            lhs,
                            rhs,
                        )));
                    }
                    firc_middle::InstrKind::Add(add_instr) => {
                        let out_reg = self.map_vreg(add_instr.value, func);
                        let lhs = self.map_op(&add_instr.lhs, func);
                        let rhs = self.map_op(&add_instr.rhs, func);
                        self.define_node(bb_id, Op::Machine(MachineOp::Add(
                            Register::Virtual(out_reg),
                            lhs,
                            rhs,
                        )));
                    }
                    firc_middle::InstrKind::Cmp(cmp_instr) => {
                        let out_reg = self.map_vreg(cmp_instr.value, func);
                        let lhs = self.map_op(&cmp_instr.lhs, func);
                        let rhs = self.map_op(&cmp_instr.rhs, func);
                        self.define_node(bb_id, Op::Machine(MachineOp::Cmp(
                            Register::Virtual(out_reg),
                            cmp_instr.op,
                            lhs,
                            rhs,
                        )));
                    }
                };
            }

            match &bb.terminator().kind {
                firc_middle::cfg::TerminatorKind::Ret(ret_term) => {
                    let value = ret_term.value.as_ref().map(|value| self.map_op(value,  func));
                    self.define_term_node(bb_id, Op::Pseudo(PseudoOp::Ret(
                        value
                    )));
                }
                firc_middle::cfg::TerminatorKind::Branch(branch_term) => {
                    self.define_term_node(bb_id, Op::Machine(MachineOp::Br(
                        branch_term.target.id
                    )));
                }
                firc_middle::cfg::TerminatorKind::CondBranch(branch_term) => {
                    let op = self.map_op(&branch_term.cond, func);
                    self.define_term_node(bb_id, Op::Machine(MachineOp::CondBr(
                        op,
                        branch_term.true_target.id.into(),
                        branch_term.false_target.id.into(),
                    )));
                }
            }
        }
        self.sel_dag
    }

    fn map_op(&mut self, op: &firc_middle::instruction::Op, func: &firc_middle::Function) -> Operand<A> {
        match op {
            firc_middle::instruction::Op::Value(vreg) => Operand::Reg(Register::Virtual(self.map_vreg(*vreg, func))),
            firc_middle::instruction::Op::Const(constant) => Operand::Imm(match constant {
                Const::Int(ty, value) => {
                    let value = *value;
                    match ty {
                        Type::U8 => Immediate::Byte((value as u8).into()),
                        Type::U16 => Immediate::Word((value as u16).into()),
                        Type::U32 => Immediate::DWord((value as u32).into()),
                        Type::U64 => Immediate::QWord((value as u64).into()),
                        Type::I8 => Immediate::Byte((value as i8).into()),
                        Type::I16 => Immediate::Word((value as i16).into()),
                        Type::I32 => Immediate::DWord((value as i32).into()),
                        Type::I64 => Immediate::QWord((value as i64).into()),
                        Type::Bool => Immediate::Byte((value as u8).into()),
                        Type::Void => unreachable!("Cannot have a constant of type void"),
                        Type::Ptr(_) => unimplemented!(),
                    }
                }
            })
        }
    }

    fn add_dependency(&mut self, bb_id: BasicBlockId, depending_node: NodeIndex, producing_node: NodeIndex) {
        debug!("{:?} depends on {:?}", depending_node, producing_node);
        self.sel_dag.get_bb_dag(bb_id).add_edge(
            depending_node,
            producing_node,
            selection_dag::Edge,
        ).unwrap();
    }

    fn define_node(&mut self, bb_id: firc_middle::cfg::BasicBlockId, op: Op<A>) -> NodeIndex {
        let used_regs = op.consumed_regs();
        let out_reg = op.out().and_then(|reg| reg.try_as_virtual());
        debug!("Defining op {:?}. Out reg: {:?}, used regs: {:?}", op, out_reg, used_regs);
        let node = self.sel_dag.get_bb_dag(bb_id).add_node(op);
        debug!("Defined op as node {:?}", node);
        for reg in used_regs {
            if let Some(defining_node) = reg.try_as_virtual().and_then(|reg| self.get_defining_node(reg, bb_id)) {
                self.add_dependency(bb_id, node, defining_node);
            }
        }
        if let Some(out_reg) = out_reg {
            self.define_out_val(node, out_reg, bb_id);
        }
        node
    }

    fn define_term_node(&mut self, bb_id: firc_middle::cfg::BasicBlockId, op: Op<A>) -> NodeIndex {
        let term_node = self.define_node(bb_id, op);
        let dag = self.sel_dag.get_bb_dag(bb_id);
        dag.set_term_node(term_node);
        debug!("Adding dependency on term node for every node");
        for node in dag.node_identifiers().filter(
            |node| *node != term_node
        ) {
            self.add_dependency(bb_id, term_node, node);
        }
        term_node
    }


    fn map_vreg(&mut self, vreg: firc_middle::VReg, func: &firc_middle::Function) -> VReg {
        let mapped = self.reg_mapping[vreg];
        match mapped {
            Some(reg) => reg,
            None => {
                let vreg_ty = func.cfg.vreg_ty(vreg);
                let mapped_vreg = self.function.alloc_vreg(vreg_ty.into());
                self.reg_mapping[vreg] = Some(mapped_vreg);
                mapped_vreg
            }
        }
    }

    fn define_out_val(&mut self, node: NodeIndex, reg: VReg, bb_id: BasicBlockId) {
        self.defining_nodes.insert((reg, bb_id), node);
    }

    fn get_defining_node(&self, vreg: VReg, bb_id: BasicBlockId) -> Option<NodeIndex> {
        self.defining_nodes.get(&(vreg, bb_id)).copied()
    }
}

#[cfg(test)]
mod tests {
    use firc_middle::cfg;
    use firc_middle::cfg::{RetTerm, TerminatorKind};
    use firc_middle::instruction::{Const, Op};
    use firc_middle::test::create_test_module;

    use crate::codegen::isa::x86_64;

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
