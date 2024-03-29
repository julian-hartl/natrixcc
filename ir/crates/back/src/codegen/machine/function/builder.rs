use daggy::{
    petgraph::prelude::Bfs,
    Walker,
};
use natrix_middle::instruction::CmpOp;
use rustc_hash::FxHashMap;
use tracing::debug;

use crate::codegen::{
    machine::{
        backend::{
            Backend,
            Pattern,
        },
        function::{
            cfg::BasicBlockId,
            Function,
        },
        instr::{
            InstrOperand,
            PseudoInstr,
        },
        Instr,
        MachInstr,
        Register,
        Size,
        TargetMachine,
    },
    selection_dag,
    selection_dag::{
        Immediate,
        MachineOp,
        Op,
        Operand,
        PseudoOp,
    },
};

#[derive(Debug)]
pub struct FunctionBuilder<TM: TargetMachine> {
    function: Function<TM>,
    backend: TM::Backend,
    bb_mapping: FxHashMap<natrix_middle::cfg::BasicBlockId, BasicBlockId>,
}

impl<TM: TargetMachine> FunctionBuilder<TM> {
    pub fn new() -> Self {
        Self {
            function: Function::new(Default::default()),
            backend: Backend::new(),
            bb_mapping: FxHashMap::default(),
        }
    }

    pub fn build(mut self, function: &mut natrix_middle::Function) -> Function<TM> {
        self.function.name = function.name.clone();
        self.function.return_ty_size = Size::from_ty(&function.ret_ty);
        debug!("Building machine function for function {}", function.name);
        let sel_dag_builder = selection_dag::Builder::new(&mut self.function);
        let mut sel_dag = sel_dag_builder.build(function);
        for bb in function.cfg.basic_block_ids_ordered() {
            self.create_bb(bb);
        }
        for mbb_id in self.function.basic_blocks.indices() {
            let bb = *self
                .bb_mapping
                .iter()
                .find(|(_, mbb)| **mbb == mbb_id)
                .unwrap()
                .0;
            debug!("Building machine basic block for basic block {}", bb);
            let dag = sel_dag.get_bb_dag(bb);
            dag.save_graphviz("out").unwrap();
            let mut node_list = Vec::with_capacity(dag.node_count());
            debug!("Determining traversal order for basic block {}", bb);
            let bfs = Bfs::new(dag.graph(), dag.term_node());
            for n in bfs.iter(dag.graph()) {
                node_list.push(n);
            }
            debug!("Traversal order: {:?}", node_list);
            let mut instructions = Vec::new();
            while let Some(node_id) = node_list.pop() {
                let op = &dag[node_id];
                match op {
                    Op::Pseudo(op) => {
                        debug!("Found pseudo op {:?}", op);
                        match op {
                            PseudoOp::Copy(dest, src) => {
                                instructions.push(Instr::Pseudo(PseudoInstr::Copy(
                                    dest.clone(),
                                    src.clone(),
                                )));
                            }
                            PseudoOp::Ret(operand) => {
                                instructions.push(Instr::Pseudo(PseudoInstr::Ret(
                                    operand.as_ref().cloned().map(|operand| match operand {
                                        Operand::Reg(reg) => InstrOperand::Reg(reg),
                                        Operand::Imm(imm) => InstrOperand::Imm(imm),
                                    }),
                                )));
                            }
                            PseudoOp::Phi(dest, operands) => {
                                instructions.push(Instr::Pseudo(PseudoInstr::Phi(
                                    dest.clone(),
                                    operands.clone(),
                                )));
                            }
                            PseudoOp::Def(reg) => {
                                instructions
                                    .push(Instr::Pseudo(PseudoInstr::Def(Register::Virtual(*reg))));
                            }
                        }
                    }
                    Op::Machine(op) => {
                        let dag_node_pattern = match op {
                            MachineOp::Mov(dest, src) => PatternIn::Mov(
                                PatternInOutput::Reg(dest.size(&self.function)),
                                self.operand_to_pattern(src),
                            ),
                            MachineOp::Sub(dest, lhs, rhs) => PatternIn::Sub(
                                PatternInOutput::Reg(dest.size(&self.function)),
                                self.operand_to_pattern(lhs),
                                self.operand_to_pattern(rhs),
                            ),
                            MachineOp::Add(dest, lhs, rhs) => PatternIn::Add(
                                PatternInOutput::Reg(dest.size(&self.function)),
                                self.operand_to_pattern(lhs),
                                self.operand_to_pattern(rhs),
                            ),
                            MachineOp::Br(bb_id) => PatternIn::Br,
                            MachineOp::Cmp(dest, cmp_op, lhs, rhs) => PatternIn::Cmp(
                                PatternInOutput::Reg(dest.size(&self.function)),
                                *cmp_op,
                                self.operand_to_pattern(lhs),
                                self.operand_to_pattern(rhs),
                            ),
                            MachineOp::CondBr(cond, true_target, false_target) => {
                                PatternIn::CondBr(self.operand_to_pattern(cond))
                            }
                        };
                        let mut matching_pattern = None;
                        debug!("Matching patterns for node {:?}", op);

                        for pattern in TM::Backend::patterns() {
                            let pattern_in = pattern.in_();
                            debug!("Checking {:?}", pattern_in);
                            debug!("Matching with {:?}", dag_node_pattern);
                            if pattern_in != dag_node_pattern {
                                debug!("Pattern does not match");
                                continue;
                            }
                            debug!("Pattern matches");
                            matching_pattern = Some(pattern.clone());
                            break;
                        }
                        match matching_pattern {
                            None => {
                                panic!("No pattern matched for node {:?}", op);
                            }
                            Some(pattern) => {
                                let matched = match op {
                                    MachineOp::Mov(dest, src) => {
                                        MatchedPattern::Mov(MatchedMovPattern {
                                            dest: MatchedPatternOutput::Reg(*dest),
                                            src: self.operand_to_matched_pattern_operand(src),
                                        })
                                    }
                                    MachineOp::Sub(dest, lhs, rhs) => {
                                        MatchedPattern::Sub(MatchedSubPattern {
                                            dest: MatchedPatternOutput::Reg(*dest),
                                            lhs: self.operand_to_matched_pattern_operand(lhs),
                                            rhs: self.operand_to_matched_pattern_operand(rhs),
                                        })
                                    }
                                    MachineOp::Add(dest, lhs, rhs) => {
                                        MatchedPattern::Add(MatchedAddPattern {
                                            dest: MatchedPatternOutput::Reg(*dest),
                                            lhs: self.operand_to_matched_pattern_operand(lhs),
                                            rhs: self.operand_to_matched_pattern_operand(rhs),
                                        })
                                    }
                                    MachineOp::Br(target) => MatchedPattern::Br(MatchedBrPattern {
                                        target: self.bb_mapping[target],
                                    }),
                                    MachineOp::Cmp(dest, cmp_op, lhs, rhs) => {
                                        MatchedPattern::Cmp(MatchedCmpPattern {
                                            dest: MatchedPatternOutput::Reg(*dest),
                                            cmp_op: *cmp_op,
                                            lhs: self.operand_to_matched_pattern_operand(lhs),
                                            rhs: self.operand_to_matched_pattern_operand(rhs),
                                        })
                                    }
                                    MachineOp::CondBr(cond, true_target, false_target) => {
                                        MatchedPattern::CondBr(MatchedCondBrPattern {
                                            cond: self.operand_to_matched_pattern_operand(cond),
                                            true_target: self.bb_mapping[true_target],
                                            false_target: self.bb_mapping[false_target],
                                        })
                                    }
                                };
                                let generated_instructions =
                                    pattern.into_instr(&mut self.function, matched);
                                debug!("Generated instructions {:?}", generated_instructions);
                                instructions.extend(generated_instructions.into_iter());
                            }
                        }
                    }
                }
            }
            for instr in instructions {
                self.function.basic_blocks[mbb_id].instructions.push(instr);
            }
        }
        debug!(
            "Finished building machine function for function {}",
            function.name
        );
        debug!("{}", self.function);
        self.function
    }

    fn create_bb(&mut self, bb: natrix_middle::cfg::BasicBlockId) -> BasicBlockId {
        let mbb = self.function.create_bb();
        self.bb_mapping.insert(bb, mbb);
        mbb
    }

    fn operand_to_matched_pattern_operand(&self, src: &Operand<TM>) -> MatchedPatternOperand<TM> {
        match src {
            Operand::Reg(reg) => MatchedPatternOperand::Reg(*reg),
            Operand::Imm(imm) => MatchedPatternOperand::Imm(imm.clone()),
        }
    }

    fn operand_to_pattern(&self, src: &Operand<TM>) -> PatternInOperand {
        match src {
            Operand::Reg(reg) => PatternInOperand::Reg(reg.size(&self.function)),
            Operand::Imm(imm) => PatternInOperand::Imm(imm.size),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PatternIn {
    Mov(PatternInOutput, PatternInOperand),
    Sub(PatternInOutput, PatternInOperand, PatternInOperand),
    Add(PatternInOutput, PatternInOperand, PatternInOperand),
    Cmp(PatternInOutput, CmpOp, PatternInOperand, PatternInOperand),
    Br,
    CondBr(PatternInOperand),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PatternInOutput {
    Reg(Size),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PatternInOperand {
    Reg(Size),
    Imm(Size),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedMovPattern<TM: TargetMachine> {
    pub dest: MatchedPatternOutput<TM>,
    pub src: MatchedPatternOperand<TM>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedSubPattern<TM: TargetMachine> {
    pub dest: MatchedPatternOutput<TM>,
    pub lhs: MatchedPatternOperand<TM>,
    pub rhs: MatchedPatternOperand<TM>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedAddPattern<TM: TargetMachine> {
    pub dest: MatchedPatternOutput<TM>,
    pub lhs: MatchedPatternOperand<TM>,
    pub rhs: MatchedPatternOperand<TM>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedBrPattern {
    pub target: BasicBlockId,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedCmpPattern<TM: TargetMachine> {
    pub dest: MatchedPatternOutput<TM>,
    pub cmp_op: CmpOp,
    pub lhs: MatchedPatternOperand<TM>,
    pub rhs: MatchedPatternOperand<TM>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedCondBrPattern<TM: TargetMachine> {
    pub cond: MatchedPatternOperand<TM>,
    pub true_target: BasicBlockId,
    pub false_target: BasicBlockId,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MatchedPattern<TM: TargetMachine> {
    Mov(MatchedMovPattern<TM>),
    Sub(MatchedSubPattern<TM>),
    Add(MatchedAddPattern<TM>),
    Cmp(MatchedCmpPattern<TM>),
    CondBr(MatchedCondBrPattern<TM>),
    Br(MatchedBrPattern),
}

impl<TM: TargetMachine> MatchedPattern<TM> {
    pub fn try_as_mov(&self) -> Option<&MatchedMovPattern<TM>> {
        match self {
            MatchedPattern::Mov(mov) => Some(mov),
            _ => None,
        }
    }

    pub fn try_as_sub(&self) -> Option<&MatchedSubPattern<TM>> {
        match self {
            MatchedPattern::Sub(sub) => Some(sub),
            _ => None,
        }
    }

    pub fn try_as_add(&self) -> Option<&MatchedAddPattern<TM>> {
        match self {
            MatchedPattern::Add(add) => Some(add),
            _ => None,
        }
    }

    pub fn try_as_br(&self) -> Option<&MatchedBrPattern> {
        match self {
            MatchedPattern::Br(br) => Some(br),
            _ => None,
        }
    }

    pub fn try_as_cmp(&self) -> Option<&MatchedCmpPattern<TM>> {
        match self {
            MatchedPattern::Cmp(cmp) => Some(cmp),
            _ => None,
        }
    }

    pub fn try_as_cond_br(&self) -> Option<&MatchedCondBrPattern<TM>> {
        match self {
            MatchedPattern::CondBr(cond_br) => Some(cond_br),
            _ => None,
        }
    }
}

impl<TM: TargetMachine> MatchedPatternOutput<TM> {
    pub fn try_as_reg(&self) -> Option<&Register<TM>> {
        match self {
            MatchedPatternOutput::Reg(reg) => Some(reg),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MatchedPatternOperand<TM: TargetMachine> {
    Reg(Register<TM>),
    Imm(Immediate),
}

impl<TM: TargetMachine> MatchedPatternOperand<TM> {
    pub fn try_as_reg(&self) -> Option<&Register<TM>> {
        match self {
            MatchedPatternOperand::Reg(reg) => Some(reg),
            _ => None,
        }
    }

    pub fn try_as_imm(&self) -> Option<&Immediate> {
        match self {
            MatchedPatternOperand::Imm(imm) => Some(imm),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MatchedPatternOutput<TM: TargetMachine> {
    Reg(Register<TM>),
}
