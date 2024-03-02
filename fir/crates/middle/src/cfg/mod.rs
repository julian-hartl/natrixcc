#![doc = include_str!("cfg.md")]

use std::fmt::{Display, Formatter};
use std::ops::Not;

use petgraph::prelude::*;
use slotmap::{HopSlotMap, Key};
use smallvec::SmallVec;

#[allow(unused_imports)]
pub use builder::Builder;
pub use domtree::DomTree;

use crate::function::Function;
use crate::instruction::{Instr, Op};
use crate::Value;

mod builder;
mod domtree;

slotmap::new_key_type! {
    pub struct BasicBlockId;
}

pub type Successors = SmallVec<[BasicBlockId; 2]>;

pub type Predecessors = SmallVec<[BasicBlockId; 4]>;

pub type Graph = StableGraph<BasicBlockId, ()>;

#[derive(Debug, Clone)]
pub struct Cfg {
    graph: Graph,
    pub(crate) basic_blocks: HopSlotMap<BasicBlockId, BasicBlock>,
    entry_block: BasicBlockId,
}

impl Cfg {
    pub fn new() -> Self {
        Self {
            graph: StableGraph::new(),
            basic_blocks: HopSlotMap::with_key(),
            entry_block: BasicBlockId::null(),
        }
    }

    pub fn new_basic_block(&mut self) -> BasicBlockId {
        let id = self.basic_blocks.insert_with_key(|id| {
            let idx = self.graph.add_node(id);
            BasicBlock::new(id, idx)
        });
        if self.entry_block.is_null() {
            self.entry_block = id;
        }
        id
    }

    pub fn remove_basic_block(&mut self, bb_id: BasicBlockId) -> BasicBlock {
        let bb = self.basic_blocks.remove(bb_id).unwrap();
        self.graph.remove_node(bb.node_idx);
        bb
    }

    /// Returns an iterator visiting all basics blocks in arbitrary order.
    ///
    /// # Examples
    ///
    /// ```
    /// # use firc_middle::cfg::Cfg;
    /// let mut cfg = Cfg::new();
    /// let bb_id = cfg.new_basic_block();
    /// let bb = cfg.basic_block(bb_id);
    /// assert_eq!(vec![bb].iter(), cfg.basic_blocks());
    /// ```
    pub fn basic_blocks(&self) -> impl Iterator<Item=&BasicBlock> {
        self.basic_blocks.values()
    }

    /// Returns an iterator visiting all basics block ids in arbitrary order.
    ///
    /// # Examples
    ///
    /// ```
    /// # use firc_middle::cfg::Cfg;
    /// let mut cfg = Cfg::new();
    /// let bb_id = cfg.new_basic_block();
    /// assert_eq!(vec![bb_id].iter(), cfg.basic_block_ids());
    /// ```
    pub fn basic_block_ids(&self) -> impl Iterator<Item=BasicBlockId> + '_ {
        self.basic_blocks.keys()
    }

    /// The basic block associated with `id`.
    pub fn basic_block(&self, id: BasicBlockId) -> &BasicBlock {
        &self.basic_blocks[id]
    }

    pub fn basic_block_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        &mut self.basic_blocks[id]
    }

    fn node_idx(&self, id: BasicBlockId) -> NodeIndex {
        self.basic_block(id).node_idx
    }

    fn root_idx(&self) -> NodeIndex {
        self.node_idx(self.entry_block)
    }

    pub fn entry_block(&self) -> BasicBlockId {
        self.entry_block
    }

    fn bb_id_from_node(&self, node_idx: NodeIndex) -> BasicBlockId {
        self.graph[node_idx]
    }

    pub fn add_bb_argument(&mut self, id: BasicBlockId, arg: Value) {
        self.basic_blocks[id].arguments.push(arg)
    }

    pub fn add_instruction(&mut self, id: BasicBlockId, instr: Instr) {
        self.basic_blocks[id].instructions.push(instr)
    }

    pub fn set_terminator(&mut self, id: BasicBlockId, terminator: Terminator) {
        self.set_edges_from_terminator(id, &terminator);
        self.basic_blocks[id].terminator = Some(terminator);
    }

    fn set_edges_from_terminator(&mut self, id: BasicBlockId, terminator: &Terminator) {
        let basic_block = &self.basic_blocks[id];
        let source_node_idx = basic_block.node_idx;
        self.graph.retain_edges(
            |graph, edge| {
                graph.edge_endpoints(edge).unwrap().0 != source_node_idx
            }
        );
        match &terminator.kind {
            TerminatorKind::Branch(BranchTerm {
                                       target,
                                   }) => {
                self.add_edge(id, target.id);
            }
            TerminatorKind::CondBranch(CondBranchTerm {
                                           true_target,
                                           false_target,
                                           ..
                                       }) => {
                self.add_edge(id, true_target.id);
                self.add_edge(id, false_target.id);
            }
            TerminatorKind::Ret(_) => {
                // ignore
            }
        }
    }

    fn add_edge(&mut self, source: BasicBlockId, target: BasicBlockId) {
        self.graph.add_edge(self.basic_block(source).node_idx, self.basic_block(target).node_idx, ());
    }

    pub fn predecessors(&self, basic_block: BasicBlockId) -> Predecessors {
        let neighbors = self.graph.neighbors_directed(self.basic_block(basic_block).node_idx, Incoming);
        neighbors.map(|node_idx| self.graph[node_idx]).collect()
    }

    pub fn successors(&self, basic_block: BasicBlockId) -> Successors {
        let neighbors = self.graph.neighbors(self.basic_block(basic_block).node_idx);
        neighbors.map(|node_idx| self.graph[node_idx]).collect()
    }

    /// Recomputes the [BasicBlock]'s successors.
    pub fn recompute_successors(&mut self, basic_block: BasicBlockId) {
        let terminator = self.basic_block(basic_block).terminator().clone();
        self.set_edges_from_terminator(basic_block, &terminator);
    }

    pub fn dom_tree(&self) -> DomTree {
        DomTree::compute(self)
    }

    pub fn write_to<W: std::fmt::Write>(&self, w: &mut W, function: &Function) -> std::fmt::Result {
        let indent = "    ";
        for bb in self.basic_blocks.values() {
            write!(w, "{}", bb)?;
            if !bb.arguments.is_empty() {
                write!(w, "(")?;
                for (index, arg) in bb.arguments.iter().copied().enumerate() {
                    let arg_ty = &function.values_ctx[arg].ty;
                    write!(w, "{arg_ty} {arg}")?;
                    if index < bb.arguments.len() - 1 {
                        write!(w, ", ")?;
                    }
                }
                write!(w, ")")?;
            }
            writeln!(w, ":")?;
            for instr in &bb.instructions {
                write!(w, "{}", indent)?;
                instr.write_to(w, function)?;
            }
            write!(w, "{}", indent)?;
            bb.terminator().write_to(w, function)?;
        }
        Ok(())
    }
}

impl Display for BasicBlockId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{:?}", self)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BasicBlock {
    id: BasicBlockId,
    node_idx: NodeIndex,
    arguments: Vec<Value>,
    pub(crate) instructions: Vec<Instr>,
    pub(crate) terminator: Option<Terminator>,
}

impl BasicBlock {
    pub fn new(idx: BasicBlockId, node_idx: NodeIndex) -> Self {
        Self {
            id: idx,
            node_idx,
            arguments: Vec::new(),
            instructions: Vec::new(),
            terminator: None,
        }
    }

    /// Returns the successors of the [`BasicBlock`].
    ///
    /// See [`Cfg::successors`] for the implementation.
    pub fn successors(&self, cfg: &Cfg) -> Successors {
        cfg.successors(self.id)
    }

    /// Returns the predecessors of the [`BasicBlock`].
    ///
    /// See [`Cfg::predecessors`] for the implementation.
    pub fn predecessors(&self, cfg: &Cfg) -> Predecessors {
        cfg.predecessors(self.id)
    }

    /// Returns the [`Terminator`] of the [`BasicBlock`].
    ///
    /// Panics if the terminators is not set.
    pub fn terminator(&self) -> &Terminator {
        self.terminator.as_ref().expect("Expected basic block to have terminator")
    }

    pub fn replace_terminator(&mut self, new_term: Terminator) -> Terminator {
        std::mem::replace(&mut self.terminator, Some(new_term)).unwrap()
    }

    /// Allows to update the current terminator.
    ///
    /// **IMPORTANT**: This method does not update any edges in the [`Cfg`], meaning
    /// you will have to call [`Cfg::recompute_successors`] yourself, if the update could
    /// mean a change of the [`BasicBlock`]'s successors.
    ///
    /// *Panics*, if the basic block does not have a terminator yet.
    pub fn update_terminator<'a, F, R>(&'a mut self, f: F) -> R where F: FnOnce(&'a mut Terminator) -> R + 'a {
        // todo: additional safety checks
        f(self.terminator.as_mut().unwrap())
    }

    /// Returns whether the [`Terminator`] is set.
    pub fn has_terminator(&self) -> bool {
        self.terminator.is_some()
    }

    pub fn arguments(&self) -> impl Iterator<Item=Value> + '_ {
        self.arguments.iter().copied()
    }

    pub fn clear_arguments(&mut self) -> impl Iterator<Item=Value> + '_ {
        self.arguments.drain(..)
    }

    pub fn add_argument(&mut self, arg: Value) {
        self.arguments.push(arg);
    }

    pub fn remove_argument(&mut self, idx: usize) -> Value {
        self.arguments.remove(idx)
    }

    /// Returns an iterator over the [`BasicBlock`]'s [`Instructions`][`Instr`].
    pub fn instructions(&self) -> impl Iterator<Item=&Instr> {
        self.instructions.iter()
    }

    /// Returns a mutable iterator over the [`BasicBlock`]'s [`Instructions`][`Instr`].
    pub fn instructions_mut(&mut self) -> impl Iterator<Item=&mut Instr> {
        self.instructions.iter_mut()
    }

    pub fn remove_instructions_by_pred<P>(&mut self, p: P) where P: FnMut(&Instr) -> bool {
        self.instructions.retain(p)
    }

    pub fn append_instructions(&mut self, instructions: impl Iterator<Item=Instr>) {
        self.instructions.extend(instructions)
    }

    /// Appends an [instruction][`Instr`] to the end of the basic block
    pub fn append_instruction(&mut self, instr: Instr) {
        self.instructions.push(instr);
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{}", self.node_idx.index())
    }
}

#[cfg(test)]
mod bb_tests {
    use crate::{Instr, InstrKind, Value};
    use crate::instruction::{Const, Op, OpInstr};

    use super::{BranchTerm, Cfg, CondBranchTerm, JumpTarget, RetTerm, Successors, Terminator, TerminatorKind};

    #[test]
    fn should_set_entry_block() {
        let mut cfg = Cfg::new();
        let bb0 = cfg.new_basic_block();
        let bb1 = cfg.new_basic_block();
        assert_eq!(cfg.entry_block, bb0);
    }

    #[test]
    fn should_return_correct_successors() {
        let mut cfg = Cfg::new();
        let bb0 = cfg.new_basic_block();
        let bb1 = cfg.new_basic_block();
        let bb2 = cfg.new_basic_block();
        cfg.set_terminator(bb0, Terminator::new(TerminatorKind::Ret(RetTerm::empty())));
        assert_eq!(cfg.successors(bb0), Successors::new());
        cfg.set_terminator(bb0, Terminator::new(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(
            bb1,
            vec![],
        )))));
        assert_eq!(cfg.successors(bb0), Successors::from(
            vec![bb1]
        ));
        cfg.set_terminator(bb0, Terminator::new(TerminatorKind::CondBranch(CondBranchTerm::new(
            Op::Const(Const::bool(true)),
            JumpTarget::new(
                bb1,
                vec![],
            ),
            JumpTarget::new(
                bb2,
                vec![],
            ),
        ))));
        assert_eq!(cfg.successors(bb0), Successors::from(
            vec![bb2, bb1]
        ));
    }

    #[test]
    fn should_add_instruction() {
        let mut cfg = Cfg::new();
        let bb0 = cfg.new_basic_block();
        let instr = Instr::new(InstrKind::Op(OpInstr { op: Op::Const(Const::i32(3)), value: Value::new(2) }));
        cfg.add_instruction(bb0, instr.clone());
        assert_eq!(cfg.basic_block(bb0).instructions, vec![instr]);
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Terminator {
    pub kind: TerminatorKind,
}

impl Terminator {
    pub const fn new(kind: TerminatorKind) -> Self {
        Self { kind }
    }

    pub fn clear_args<'a>(&'a mut self, target: BasicBlockId) -> Option<impl Iterator<Item=Op> + 'a> {
        match &mut self.kind {
            TerminatorKind::Ret(_) => None,
            TerminatorKind::Branch(branch_term) => {
                if branch_term.target.id != target {
                    return None;
                }
                Some(branch_term.target.arguments.drain(..))
            }
            TerminatorKind::CondBranch(condbr_term) => {
                for jtarget in condbr_term.targets_mut() {
                    if jtarget.id != target {
                        continue;
                    }
                    return Some(jtarget.arguments.drain(..));
                }
                None
            }
        }
    }

    pub fn branch_args(&self, target: BasicBlockId) -> Option<impl Iterator<Item=&Op>> {
        match &self.kind {
            TerminatorKind::Ret(_) => None,
            TerminatorKind::Branch(branch_term) => {
                if branch_term.target.id != target {
                    return None;
                }
                Some(branch_term.target.arguments.iter())
            }
            TerminatorKind::CondBranch(condbr_term) => {
                for jtarget in condbr_term.targets() {
                    if jtarget.id != target {
                        continue;
                    }
                    return Some(jtarget.arguments.iter());
                }
                None
            }
        }
    }

    pub fn write_to(&self, w: &mut impl std::fmt::Write, function: &Function) -> std::fmt::Result {
        match &self.kind {
            TerminatorKind::Ret(term) => {
                write!(w, "ret")?;
                if let Some(value) = &term.value {
                    write!(w, " {} ", value.ty(function))?;
                    value.write_to(w, function)?;
                } else {
                    write!(w, " void")?;
                }
            }
            TerminatorKind::Branch(branch) => {
                write!(w, "br ")?;
                branch.target.write_to(w, function)?;
            }
            TerminatorKind::CondBranch(branch) => {
                write!(w, "condbr {} ", branch.cond.ty(function))?;
                branch.cond.write_to(w, function)?;
                write!(w, ", ")?;
                branch.true_target.write_to(w, function)?;
                write!(w, ", ")?;
                branch.false_target.write_to(w, function)?;
            }
        }
        writeln!(w)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TerminatorKind {
    Ret(RetTerm),
    Branch(BranchTerm),
    CondBranch(CondBranchTerm),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RetTerm {
    pub value: Option<Op>,
}

impl RetTerm {
    pub const fn new(value: Op) -> Self {
        Self { value: Some(value) }
    }
    pub const fn empty() -> Self {
        Self { value: None }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct JumpTarget {
    pub id: BasicBlockId,
    pub arguments: Vec<Op>,
}

impl JumpTarget {
    pub fn new(id: BasicBlockId, arguments: Vec<Op>) -> Self {
        Self {
            id,
            arguments,
        }
    }

    pub fn no_args(id: BasicBlockId) -> Self {
        Self::new(id, vec![])
    }

    pub fn write_to(&self, w: &mut impl std::fmt::Write, func: &Function) -> std::fmt::Result {
        write!(w, "{}", func.cfg.basic_block(self.id))?;
        if !self.arguments.is_empty() {
            write!(w, "(")?;
            for (index, arg) in self.arguments.iter().enumerate() {
                arg.write_to(w, func)?;
                if index != self.arguments.len() - 1 {
                    write!(w, ", ")?;
                }
            }
            write!(w, ")")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BranchTerm {
    pub target: JumpTarget,
}

impl BranchTerm {
    pub const fn new(target: JumpTarget) -> Self {
        Self { target }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CondBranchTerm {
    pub cond: Op,
    pub true_target: JumpTarget,
    pub false_target: JumpTarget,
}

impl CondBranchTerm {
    pub const fn new(cond: Op, true_target: JumpTarget, false_target: JumpTarget) -> Self {
        Self {
            cond,
            true_target,
            false_target,
        }
    }

    pub fn targets(&self) -> [&JumpTarget; 2] {
        [&self.true_target, &self.false_target]
    }

    pub fn targets_mut(&mut self) -> [&mut JumpTarget; 2] {
        [&mut self.true_target, &mut self.false_target]
    }
}
