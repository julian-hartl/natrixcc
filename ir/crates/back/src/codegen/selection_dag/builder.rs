use codegen::selection_dag;
use daggy::{petgraph::visit::IntoNodeIdentifiers, NodeIndex, Walker};
use iter_tools::Itertools;
use natrix_middle::{
    cfg::{BBArgRef, BasicBlockRef, BranchTerm, InstrRef, JumpTarget, Terminator, TerminatorKind},
    instruction::{Const, OpInstr},
    ty::Type,
    InstrKind, Value,
};
use rustc_hash::FxHashMap;
use selection_dag::SelectionDAG;
use slotmap::SecondaryMap;
use tracing::debug;

use crate::{
    codegen,
    codegen::{
        machine::{
            function::Function,
            reg::{Register, VRegRef},
            TargetMachine,
        },
        selection_dag::{Immediate, MachineOp, Op, Operand, PseudoOp},
    },
};

pub struct Builder<'func, TM: TargetMachine> {
    function: &'func mut Function<TM>,
    sel_dag: SelectionDAG<TM>,
    reg_mapping: SecondaryMap<InstrRef, Option<VRegRef>>,
    bb_arg_reg_mapping: SecondaryMap<BBArgRef, Option<VRegRef>>,
    defining_nodes: FxHashMap<(VRegRef, BasicBlockRef), NodeIndex>,
}

impl<'func, TM: TargetMachine> Builder<'func, TM> {
    pub fn new(function: &'func mut Function<TM>) -> Self {
        Self {
            function,
            reg_mapping: SecondaryMap::new(),
            bb_arg_reg_mapping: SecondaryMap::new(),
            sel_dag: SelectionDAG::default(),
            defining_nodes: FxHashMap::default(),
        }
    }

    pub fn build(mut self, func: &mut natrix_middle::Function) -> SelectionDAG<TM> {
        debug!("Building SelectionDAGs for function {}", func.name);
        let basic_blocks = func
            .cfg
            .basic_blocks
            .keys()
            .filter(|bb_id| *bb_id != func.cfg.entry_block_ref())
            .collect_vec();

        for bb_id in basic_blocks {
            let bb = &func.cfg.basic_blocks[bb_id];
            // Eliminate basic block arguments
            debug!("Eliminating basic block arguments for {}", bb);
            let bb_args = bb
                .arguments()
                .map(|arg| (arg, func.cfg.bb_args[arg].ty.clone()))
                .collect_vec();
            let mut temp_regs = FxHashMap::<_, Vec<_>>::default();
            let preds = func.cfg.predecessors(bb_id).collect::<Vec<_>>();
            for pred_id in preds {
                let args = func.cfg.basic_blocks[pred_id]
                    .terminator()
                    .branch_args(bb_id)
                    .unwrap()
                    .cloned()
                    .collect::<Vec<_>>();
                if args.is_empty() {
                    continue;
                }
                let succ_len = func.cfg.successors(pred_id).count();
                let copy_instr_bb = if succ_len == 1 {
                    // No need to split the critical edge
                    func.cfg.basic_blocks[pred_id].update_terminator(|term| {
                        term.clear_args(bb_id);
                    });
                    pred_id
                } else {
                    // Create new basic block for copying the argument ops to temp regs
                    let split_crit_bb_symbol = format!(
                        "$split_crit:{}->{}",
                        bb_id.display(&func.cfg),
                        pred_id.display(&func.cfg)
                    );
                    let critical_edge_split_bb = func.cfg.new_basic_block(split_crit_bb_symbol);
                    func.cfg.basic_blocks[critical_edge_split_bb].set_terminator(Terminator::new(
                        TerminatorKind::Branch(BranchTerm::new(JumpTarget::no_args(bb_id))),
                        critical_edge_split_bb,
                    ));
                    func.cfg.recompute_successors(critical_edge_split_bb);
                    func.cfg.basic_blocks[pred_id].update_terminator(|term| {
                        term.clear_args(bb_id);
                        term.update_references_to_bb(bb_id, critical_edge_split_bb);
                    });

                    func.cfg.recompute_successors(pred_id);
                    critical_edge_split_bb
                };

                for (arg, (bb_arg_ref, ty)) in args.into_iter().zip(bb_args.iter()) {
                    // Create a temp reg and copy the jump arg to it

                    let bb_arg = &func.cfg.bb_args[*bb_arg_ref];
                    let instr_ref = func.cfg.add_instruction(
                        copy_instr_bb,
                        ty.clone(),
                        InstrKind::Op(OpInstr { op: arg }),
                        // Use $ to create a unique name, because $ can't be used to create a symbol
                        format!("${}", bb_arg.symbol),
                    );
                    temp_regs.entry(*bb_arg_ref).or_default().push(instr_ref);
                }
            }
            let _ = func.cfg.basic_blocks[bb_id].clear_arguments();
            for (arg, _) in bb_args {
                let mapped_arg = self.map_value(arg.into(), func);
                let operands = temp_regs
                    .remove(&arg)
                    .unwrap()
                    .into_iter()
                    .map(|instr_ref| {
                        let mapped = self.map_value(instr_ref.into(), func);
                        // Ensure that the temp reg and arg reg will be placed in the same location => we can trivially remove the phi instruction later on
                        self.function.tie_vreg(mapped, mapped_arg);
                        // Using defined_in here is fine, because the reg that we use here is always defined in the immediate pred of bb_id
                        (
                            Register::Virtual(mapped),
                            func.cfg.instructions[instr_ref].defined_in,
                        )
                    })
                    .collect_vec();
                let pseudo_op = PseudoOp::Phi(Register::Virtual(mapped_arg), operands);
                let bb = &func.cfg.basic_blocks[bb_id];
                debug!("Adding phi to {bb}: {:?}", pseudo_op);
                let phi_op = Op::Pseudo(pseudo_op);
                self.define_node(bb_id, phi_op);
            }
            let bb = &func.cfg.basic_blocks[bb_id];
            debug!("Eliminated basic block arguments for {}", bb);
        }
        for (bb_id, bb) in &func.cfg.basic_blocks {
            debug!("Building SelectionDAG for basic block {}", bb);
            if bb_id == func.cfg.entry_block_ref() {
                for arg in bb.arguments() {
                    let mapped_reg = self.map_value(arg.into(), func);
                    self.function.params.push(mapped_reg);
                    self.define_node(bb_id, Op::Pseudo(PseudoOp::Def(mapped_reg)));
                }
            }
            for instr_ref in bb.instructions() {
                let instr = &func.cfg.instructions[instr_ref];
                debug!("Add instruction to SelectionDAG: {:?}", instr);

                match &instr.kind {
                    natrix_middle::InstrKind::Alloca(_) => unimplemented!(),
                    natrix_middle::InstrKind::Store(_) => unimplemented!(),
                    natrix_middle::InstrKind::Load(_) => unimplemented!(),
                    natrix_middle::InstrKind::Op(op_instr) => {
                        let out_reg = self.map_value(instr.id.into(), func);
                        let op = match self.map_op(&op_instr.op, func) {
                            Operand::Reg(reg) => {
                                Op::Pseudo(PseudoOp::Copy(Register::Virtual(out_reg), reg))
                            }
                            Operand::Imm(imm) => Op::Machine(MachineOp::Mov(
                                Register::Virtual(out_reg),
                                Operand::Imm(imm),
                            )),
                        };
                        self.define_node(bb_id, op);
                    }
                    natrix_middle::InstrKind::Sub(sub_instr) => {
                        let out_reg = self.map_value(instr.id.into(), func);
                        let lhs = self.map_op(&sub_instr.lhs, func);
                        let rhs = self.map_op(&sub_instr.rhs, func);
                        self.define_node(
                            bb_id,
                            Op::Machine(MachineOp::Sub(Register::Virtual(out_reg), lhs, rhs)),
                        );
                    }
                    natrix_middle::InstrKind::Add(add_instr) => {
                        let out_reg = self.map_value(instr.id.into(), func);
                        let lhs = self.map_op(&add_instr.lhs, func);
                        let rhs = self.map_op(&add_instr.rhs, func);
                        self.define_node(
                            bb_id,
                            Op::Machine(MachineOp::Add(Register::Virtual(out_reg), lhs, rhs)),
                        );
                    }
                    natrix_middle::InstrKind::Cmp(cmp_instr) => {
                        let out_reg = self.map_value(instr.id.into(), func);
                        let lhs = self.map_op(&cmp_instr.lhs, func);
                        let rhs = self.map_op(&cmp_instr.rhs, func);
                        self.define_node(
                            bb_id,
                            Op::Machine(MachineOp::Cmp(
                                Register::Virtual(out_reg),
                                cmp_instr.op,
                                lhs,
                                rhs,
                            )),
                        );
                    }
                };
            }

            match &bb.terminator().kind {
                natrix_middle::cfg::TerminatorKind::Ret(ret_term) => {
                    let value = ret_term
                        .value
                        .as_ref()
                        .map(|value| self.map_op(value, func));
                    self.define_term_node(bb_id, Op::Pseudo(PseudoOp::Ret(value)));
                }
                natrix_middle::cfg::TerminatorKind::Branch(branch_term) => {
                    self.define_term_node(bb_id, Op::Machine(MachineOp::Br(branch_term.target.id)));
                }
                natrix_middle::cfg::TerminatorKind::CondBranch(branch_term) => {
                    let op = self.map_op(&branch_term.cond, func);
                    self.define_term_node(
                        bb_id,
                        Op::Machine(MachineOp::CondBr(
                            op,
                            branch_term.true_target.id.into(),
                            branch_term.false_target.id.into(),
                        )),
                    );
                }
            }
        }
        self.sel_dag
    }

    fn map_op(
        &mut self,
        op: &natrix_middle::instruction::Op,
        func: &natrix_middle::Function,
    ) -> Operand<TM> {
        match op {
            natrix_middle::instruction::Op::Value(vreg) => {
                Operand::Reg(Register::Virtual(self.map_value(*vreg, func)))
            }
            natrix_middle::instruction::Op::Const(constant) => Operand::Imm(match constant {
                Const::Int(ty, value) => {
                    let value = *value;
                    match ty {
                        Type::U8 => Immediate::from(value as u8),
                        Type::U16 => Immediate::from(value as u16),
                        Type::U32 => Immediate::from(value as u32),
                        Type::U64 => Immediate::from(value as u64),
                        Type::I8 => Immediate::from(value as i8),
                        Type::I16 => Immediate::from(value as i16),
                        Type::I32 => Immediate::from(value as i32),
                        Type::I64 => Immediate::from(value),
                        Type::Bool => Immediate::from(value as u8),
                        Type::Void => unreachable!("Cannot have a constant of type void"),
                        Type::Ptr(_) => unimplemented!(),
                    }
                }
            }),
        }
    }

    fn add_dependency(
        &mut self,
        bb_id: BasicBlockRef,
        depending_node: NodeIndex,
        producing_node: NodeIndex,
    ) {
        debug!("{:?} depends on {:?}", depending_node, producing_node);
        self.sel_dag
            .get_bb_dag(bb_id)
            .add_edge(depending_node, producing_node, selection_dag::Edge)
            .unwrap();
    }

    fn define_node(&mut self, bb_id: natrix_middle::cfg::BasicBlockRef, op: Op<TM>) -> NodeIndex {
        let used_regs = op.consumed_regs();
        let out_reg = op.out().and_then(|reg| reg.try_as_virtual());
        debug!(
            "Defining op {:?}. Out reg: {:?}, used regs: {:?}",
            op, out_reg, used_regs
        );
        let node = self.sel_dag.get_bb_dag(bb_id).add_node(op);
        debug!("Defined op as node {:?}", node);
        for reg in used_regs {
            if let Some(defining_node) = reg
                .try_as_virtual()
                .and_then(|reg| self.get_defining_node(reg, bb_id))
            {
                self.add_dependency(bb_id, node, defining_node);
            }
        }
        if let Some(out_reg) = out_reg {
            self.define_out_val(node, out_reg, bb_id);
        }
        node
    }

    fn define_term_node(
        &mut self,
        bb_id: natrix_middle::cfg::BasicBlockRef,
        op: Op<TM>,
    ) -> NodeIndex {
        let term_node = self.define_node(bb_id, op);
        let dag = self.sel_dag.get_bb_dag(bb_id);
        dag.set_term_node(term_node);
        debug!("Adding dependency on term node for every node");
        for node in dag.node_identifiers().filter(|node| *node != term_node) {
            self.add_dependency(bb_id, term_node, node);
        }
        term_node
    }

    fn map_value(&mut self, value: Value, func: &natrix_middle::Function) -> VRegRef {
        match value {
            Value::Instr(instr_ref) => {
                if let Some(reg) = self.reg_mapping[instr_ref] {
                    return reg;
                }
            }
            Value::BBArg(bb_arg_ref) => {
                if let Some(reg) = self.bb_arg_reg_mapping[bb_arg_ref] {
                    return reg;
                }
            }
        };
        match value {
            Value::Instr(instr_ref) => {
                let instr = &func.cfg.instructions[instr_ref];
                let ty = &instr.ty;
                let mapped_vreg = self.function.alloc_vreg(ty.into(), instr.symbol.clone());
                self.reg_mapping[instr_ref] = Some(mapped_vreg);
                mapped_vreg
            }

            Value::BBArg(bb_arg_ref) => {
                let bb_arg = &func.cfg.bb_args[bb_arg_ref];
                let ty = &bb_arg.ty;
                let mapped_vreg = self.function.alloc_vreg(ty.into(), bb_arg.symbol.clone());
                self.bb_arg_reg_mapping[bb_arg_ref] = Some(mapped_vreg);
                mapped_vreg
            }
        }
    }

    fn define_out_val(&mut self, node: NodeIndex, reg: VRegRef, bb_id: BasicBlockRef) {
        self.defining_nodes.insert((reg, bb_id), node);
    }

    fn get_defining_node(&self, vreg: VRegRef, bb_id: BasicBlockRef) -> Option<NodeIndex> {
        self.defining_nodes.get(&(vreg, bb_id)).copied()
    }
}
