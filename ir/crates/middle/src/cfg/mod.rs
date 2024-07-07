#![doc = include_str!("cfg.md")]

use std::fmt::{
    Debug,
    Display,
    Formatter,
};

pub use builder::Builder;
pub use domtree::DomTree;
use indexmap::IndexSet;
#[allow(unused_imports)]
pub use petgraph::{
    prelude::*,
    visit::Walker,
};
use slotmap::{
    new_key_type,
    SlotMap,
};
use smallvec::SmallVec;

use crate::{
    instruction::{
        Instr,
        Op,
    },
    InstrKind,
    Type,
    Value,
};

mod builder;
mod domtree;

new_key_type! { pub struct BasicBlockRef; }
#[derive(Debug, Clone, Eq, PartialEq)]
struct CFGNode {
    bb_ref: BasicBlockRef,
}
pub type Graph = StableGraph<CFGNode, (), Directed>;

#[derive(Debug, Default, Clone)]
pub struct Cfg {
    graph: Graph,
    pub basic_blocks: SlotMap<BasicBlockRef, BasicBlock>,
    pub instructions: SlotMap<InstrRef, Instr>,
    pub basic_block_args: SlotMap<BBArgRef, BBArg>,
    entry_block: Option<BasicBlockRef>,
}

impl Cfg {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_basic_block(&mut self, symbol: String) -> BasicBlockRef {
        let bb = {
            self.basic_blocks.insert_with_key(|id| {
                let node_idx = self.graph.add_node(CFGNode { bb_ref: id });
                BasicBlock::new(id, node_idx, symbol)
            })
        };
        if self.entry_block.is_none() {
            self.entry_block = Some(bb);
        }
        bb
    }

    pub fn remove_basic_block(&mut self, bb_id: BasicBlockRef) -> Option<BasicBlock> {
        let bb = self.basic_blocks.remove(bb_id)?;
        self.graph.remove_node(bb.node_index);
        Some(bb)
    }

    pub fn basic_block_ids_ordered(&self) -> impl Iterator<Item = BasicBlockRef> + '_ {
        Bfs::new(
            &self.graph,
            self.basic_blocks[self.entry_block_ref()].node_index,
        )
        .iter(&self.graph)
        .map(|node| self.graph[node].bb_ref)
    }

    pub fn entry_block_ref(&self) -> BasicBlockRef {
        self.entry_block.expect("Entry block has not been created")
    }

    pub fn add_instruction(
        &mut self,
        defined_in: BasicBlockRef,
        ty: Type,
        instr: InstrKind,
        symbol: String,
    ) -> InstrRef {
        let instr_ref = self.instructions.insert_with_key(|id| Instr {
            ty,
            defined_in,
            kind: instr,
            id,
            symbol,
        });
        self.basic_blocks[defined_in].instructions.insert(instr_ref);
        instr_ref
    }

    pub fn set_terminator(&mut self, id: BasicBlockRef, terminator: TerminatorKind) {
        self.set_edges_from_terminator(id, &terminator);
        let terminator = Terminator::new(terminator, id);
        self.basic_blocks[id].terminator = Some(terminator);
    }

    fn set_edges_from_terminator(&mut self, id: BasicBlockRef, terminator: &TerminatorKind) {
        self.graph.retain_edges(|graph, edge| {
            graph.edge_endpoints(edge).unwrap().0 != self.basic_blocks[id].node_index
        });
        match &terminator {
            TerminatorKind::Branch(BranchTerm { target }) => {
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

    fn add_edge(&mut self, source: BasicBlockRef, target: BasicBlockRef) {
        self.graph.add_edge(
            self.basic_blocks[source].node_index,
            self.basic_blocks[target].node_index,
            (),
        );
    }

    pub fn predecessors(&self, bb_ref: BasicBlockRef) -> impl Iterator<Item = BasicBlockRef> + '_ {
        self.graph
            .neighbors_directed(self.basic_blocks[bb_ref].node_index, Incoming)
            .map(|n| self.graph[n].bb_ref)
    }

    pub fn successors(&self, bb_ref: BasicBlockRef) -> impl Iterator<Item = BasicBlockRef> + '_ {
        self.graph
            .neighbors(self.basic_blocks[bb_ref].node_index)
            .map(|n| self.graph[n].bb_ref)
    }

    /// Recomputes the [BasicBlock]'s successors.
    pub fn recompute_successors(&mut self, bb_ref: BasicBlockRef) {
        let terminator = self.basic_blocks[bb_ref].terminator().kind.clone();
        self.set_edges_from_terminator(bb_ref, &terminator);
    }

    pub fn dom_tree(&self) -> DomTree {
        DomTree::compute(self)
    }

    pub fn dfs_postorder(&self) -> impl Iterator<Item = BasicBlockRef> + '_ {
        DfsPostOrder::new(
            &self.graph,
            self.basic_blocks[self.entry_block_ref()].node_index,
        )
        .iter(&self.graph)
        .map(|node| self.graph[node].bb_ref)
    }

    pub fn add_bb_argument(&mut self, bb_id: BasicBlockRef, ty: Type, symbol: String) -> BBArgRef {
        let arg_ref = self
            .basic_block_args
            .insert_with_key(|id| BBArg { id, ty, symbol });
        self.basic_blocks[bb_id].arguments.insert(arg_ref);
        arg_ref
    }

    pub fn values(&self) -> impl Iterator<Item = Value> + '_ {
        self.instructions
            .keys()
            .map(move |instr_id| Value::Instr(instr_id))
            .chain(
                self.basic_block_args
                    .keys()
                    .map(move |arg_id| Value::BBArg(arg_id)),
            )
    }
}

impl Display for Cfg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent = "    ";
        for (bb_id, bb) in &self.basic_blocks {
            write!(f, "{}", bb)?;
            if !bb.arguments.is_empty() {
                write!(f, "(")?;
                for (index, arg_ref) in bb.arguments.iter().copied().enumerate() {
                    let arg = &self.basic_block_args[arg_ref];
                    let arg_ty = &self.basic_block_args[arg_ref].ty;
                    write!(f, "{arg_ty} {arg}")?;
                    if index < bb.arguments.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;
            }
            writeln!(f, ":")?;
            for instr in bb.instructions() {
                let instr = &self.instructions[instr];
                writeln!(f, "{}{};", indent, instr.display(self))?;
            }
            writeln!(f, "{}{};", indent, bb.terminator().display(self))?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod cfg_tests {
    use itertools::Itertools;

    use super::*;

    #[test]
    fn should_not_return_removed_basic_block() {
        let mut cfg = Cfg::new();
        let bb0 = cfg.new_basic_block("bb0".into());
        cfg.set_terminator(bb0, TerminatorKind::Ret(RetTerm::empty()));
        let bb1 = cfg.new_basic_block("bb1".into());
        cfg.set_terminator(bb1, TerminatorKind::Ret(RetTerm::empty()));
        cfg.remove_basic_block(bb0);
        assert_eq!(cfg.basic_blocks.keys().collect_vec(), vec![bb1],);
    }
}

new_key_type! {
pub struct InstrRef; }

new_key_type! { pub struct BBArgRef; }

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BBArg {
    pub(crate) id: BBArgRef,
    pub(crate) ty: Type,
    pub symbol: String,
}

impl Display for BBArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.symbol)
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BasicBlockRef,
    pub arguments: IndexSet<BBArgRef>,
    pub instructions: IndexSet<InstrRef>,
    pub terminator: Option<Terminator>,
    node_index: NodeIndex,
    pub symbol: String,
}

impl BasicBlock {
    pub fn new(id: BasicBlockRef, graph_index: NodeIndex, symbol: String) -> Self {
        Self {
            id,
            arguments: IndexSet::new(),
            instructions: IndexSet::new(),
            symbol,
            node_index: graph_index,
            terminator: None,
        }
    }

    /// Returns the [`Terminator`] of the [`BasicBlock`].
    ///
    /// Panics if the terminators is not set.
    pub fn terminator(&self) -> &Terminator {
        self.terminator
            .as_ref()
            .expect("Basic blocks must have a terminator")
    }

    pub fn set_terminator(&mut self, term: Terminator) {
        self.terminator = Some(term);
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
    pub fn update_terminator<'a, F, R>(&'a mut self, f: F) -> R
    where
        F: FnOnce(&'a mut Terminator) -> R + 'a,
    {
        // todo: additional safety checks
        f(self.terminator.as_mut().unwrap())
    }

    /// Returns whether the [`Terminator`] is set.
    pub fn has_terminator(&self) -> bool {
        self.terminator.is_some()
    }

    pub fn arguments(&self) -> impl Iterator<Item = BBArgRef> + '_ {
        self.arguments.iter().copied()
    }

    pub fn clear_arguments(&mut self) -> impl Iterator<Item = BBArgRef> + '_ {
        self.arguments.drain(..)
    }

    /// Returns an iterator over the [`BasicBlock`]'s [`Instructions`][`InstrId`].
    pub fn instructions(&self) -> impl DoubleEndedIterator<Item = InstrRef> + '_ {
        self.instructions.iter().copied()
    }

    pub fn remove_instruction(&mut self, id: InstrRef) {
        self.instructions.retain(|instr_id| *instr_id != id)
    }

    pub fn append_instructions(&mut self, e_instructions: impl Iterator<Item = InstrRef>) {
        self.instructions.extend(e_instructions);
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.symbol)
    }
}

#[cfg(test)]
mod bb_tests {
    use super::{
        BranchTerm,
        Cfg,
        CondBranchTerm,
        JumpTarget,
        RetTerm,
        TerminatorKind,
    };
    use crate::{
        instruction::{
            Const,
            Op,
        },
        Type,
    };

    #[test]
    fn should_set_entry_block() {
        let mut cfg = Cfg::new();
        let bb0 = cfg.new_basic_block("bb0".into());
        let bb1 = cfg.new_basic_block("bb1".into());
        assert_eq!(cfg.entry_block, Some(bb0));
    }

    #[test]
    fn should_return_correct_successors() {
        let mut cfg = Cfg::new();
        let bb0 = cfg.new_basic_block("bb0".into());
        let bb1 = cfg.new_basic_block("bb1".into());
        let bb2 = cfg.new_basic_block("bb2".into());
        cfg.set_terminator(bb0, TerminatorKind::Ret(RetTerm::empty()));
        assert!(cfg.successors(bb0).eq(vec![].into_iter()));
        cfg.set_terminator(
            bb0,
            TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(bb1, vec![]))),
        );
        assert!(cfg.successors(bb0).eq(vec![bb1].into_iter()));
        cfg.set_terminator(
            bb0,
            TerminatorKind::CondBranch(CondBranchTerm::new(
                Op::Const(Const::Int(Type::I32, 1)),
                JumpTarget::new(bb1, vec![]),
                JumpTarget::new(bb2, vec![]),
            )),
        );
        assert!(cfg.successors(bb0).eq(vec![bb2, bb1].into_iter()));
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Terminator {
    pub bb: BasicBlockRef,
    pub kind: TerminatorKind,
}

impl Terminator {
    pub const fn new(kind: TerminatorKind, bb: BasicBlockRef) -> Self {
        Self { kind, bb }
    }

    pub fn clear_args<'a>(
        &'a mut self,
        target: BasicBlockRef,
    ) -> Option<impl Iterator<Item = Op> + 'a> {
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

    pub fn branch_args(&self, target: BasicBlockRef) -> Option<impl Iterator<Item = &Op>> {
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

    pub fn update_references_to_bb(&mut self, old: BasicBlockRef, new: BasicBlockRef) {
        match &mut self.kind {
            TerminatorKind::Ret(_) => {}
            TerminatorKind::Branch(br) => {
                if old == br.target.id {
                    br.target.id = new;
                }
            }
            TerminatorKind::CondBranch(br) => {
                for target in br.targets_mut() {
                    if old == target.id {
                        target.id = new;
                    }
                }
            }
        }
    }

    pub fn used(&self) -> SmallVec<[&Op; 2]> {
        match &self.kind {
            TerminatorKind::Ret(ret_term) => ret_term.value.as_ref().into_iter().collect(),
            TerminatorKind::Branch(branch_term) => branch_term.target.arguments.iter().collect(),
            TerminatorKind::CondBranch(condbr_term) => [&condbr_term.cond]
                .into_iter()
                .chain(
                    condbr_term
                        .true_target
                        .arguments
                        .iter()
                        .chain(condbr_term.false_target.arguments.iter()),
                )
                .collect(),
        }
    }

    pub fn display<'cfg>(&self, cfg: &'cfg Cfg) -> TerminatorDisplay<'cfg, '_> {
        TerminatorDisplay {
            cfg,
            terminator: self,
        }
    }
}

struct TerminatorDisplay<'cfg, 'term> {
    cfg: &'cfg Cfg,
    terminator: &'term Terminator,
}

impl Display for TerminatorDisplay<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.terminator.kind {
            TerminatorKind::Ret(term) => {
                write!(f, "ret")?;
                if let Some(value) = &term.value {
                    write!(f, " {}", value.display(self.cfg))?;
                }
            }
            TerminatorKind::Branch(branch) => {
                write!(f, "br {}", branch.target.display(self.cfg))?;
            }
            TerminatorKind::CondBranch(branch) => {
                write!(
                    f,
                    "condbr {}, {}, {}",
                    branch.cond.display(self.cfg),
                    branch.true_target.display(self.cfg),
                    branch.false_target.display(self.cfg)
                )?;
            }
        }
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
    pub id: BasicBlockRef,
    pub arguments: Vec<Op>,
}

impl JumpTarget {
    pub fn new(id: BasicBlockRef, arguments: Vec<Op>) -> Self {
        Self { id, arguments }
    }

    pub fn no_args(id: BasicBlockRef) -> Self {
        Self::new(id, vec![])
    }

    pub fn display<'cfg>(&self, cfg: &'cfg Cfg) -> JumpTargetDisplay<'cfg, '_> {
        JumpTargetDisplay { target: self, cfg }
    }
}

struct JumpTargetDisplay<'cfg, 'target> {
    target: &'target JumpTarget,
    cfg: &'cfg Cfg,
}

impl Display for JumpTargetDisplay<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.cfg.basic_blocks[self.target.id])?;
        if !self.target.arguments.is_empty() {
            write!(f, "(")?;
            for (index, arg) in self.target.arguments.iter().enumerate() {
                write!(f, "{}", arg.display(self.cfg))?;
                if index != self.target.arguments.len() - 1 {
                    write!(f, ", ")?;
                }
            }
            write!(f, ")")?;
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
