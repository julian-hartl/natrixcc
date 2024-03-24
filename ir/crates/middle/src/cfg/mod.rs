#![doc = include_str!("cfg.md")]

use std::fmt::{Debug, Display, Formatter};

use cranelift_entity::{entity_impl, EntityRef, PrimaryMap};
use petgraph::prelude::*;
use petgraph::visit::Walker;

#[allow(unused_imports)]
pub use builder::Builder;
pub use domtree::DomTree;

use crate::{InstrKind, Type, VReg};
use crate::instruction::{Instr, Op, OpInstr, VRegData};

mod builder;
mod domtree;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct BasicBlockId(u32);
entity_impl!(BasicBlockId, "bb");

impl From<BasicBlockId> for NodeIndex<u32> {
    fn from(value: BasicBlockId) -> Self {
        Self::new(value.0 as usize)
    }
}

impl From<NodeIndex<u32>> for BasicBlockId {
    fn from(value: NodeIndex<u32>) -> Self {
        Self::new(value.index())
    }
}

pub type Graph = StableGraph<(), (), Directed>;

#[derive(Debug, Clone, Default)]
pub struct Cfg {
    graph: Graph,
    basic_blocks: PrimaryMap<BasicBlockId, BasicBlock>,
    entry_block: Option<BasicBlockId>,
    vregs: PrimaryMap<VReg, VRegData>,
}

impl Cfg {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn new_basic_block(&mut self) -> BasicBlockId {
        let bb = {
            self.graph.add_node(());
            self.basic_blocks.push(BasicBlock::new())
        };
        if self.entry_block.is_none() {
            self.entry_block = Some(bb);
        }
        bb
    }

    pub fn remove_basic_block(&mut self, bb_id: BasicBlockId) -> (Option<Vec<Instr>>, Terminator) {
        self.graph.remove_node(bb_id.into());
        let instructions = self.basic_block_mut(bb_id).instructions.take();
        let terminator = self.basic_block_mut(bb_id).terminator.take().unwrap();
        (instructions, terminator)
    }

    /// Returns an iterator visiting all basics blocks in arbitrary order.
    pub fn basic_blocks(&self) -> impl Iterator<Item=(BasicBlockId, &BasicBlock)> {
        self.basic_blocks.iter().filter(|(id, bb)| bb.has_terminator())
    }

    /// Returns an iterator visiting all basics block ids in arbitrary order.
    pub fn basic_block_ids(&self) -> impl Iterator<Item=BasicBlockId> + '_ {
        self.basic_blocks().map(|(id, _)| id)
    }

    pub fn basic_block_ids_ordered(&self) -> impl Iterator<Item=BasicBlockId> + '_ {
        Bfs::new(&self.graph, self.entry_block().into()).iter(&self.graph)
            .map(|node| node.into())
    }

    /// The basic block associated with `id`.
    pub fn basic_block(&self, id: BasicBlockId) -> &BasicBlock {
        &self.basic_blocks[id]
    }

    pub fn basic_block_mut(&mut self, id: BasicBlockId) -> &mut BasicBlock {
        &mut self.basic_blocks[id]
    }


    pub fn entry_block(&self) -> BasicBlockId {
        self.entry_block.expect("Entry block has not been created")
    }

    pub fn add_bb_argument(&mut self, id: BasicBlockId, arg: VReg) {
        self.basic_block_mut(id).arguments.push(arg)
    }

    pub fn add_instruction(&mut self, id: BasicBlockId, instr: Instr) {
        self.basic_block_mut(id).append_instruction(instr)
    }
    pub fn copy_reg_instr(&self, dest: VReg, reg: VReg) -> Instr {
        self.copy_op_instr(
            dest,
            Op::Value(reg),
            self.vreg_ty_cloned(reg),
        )
    }

    pub fn copy_op_to_temp_instr(&mut self, id: BasicBlockId, src_op: Op, src_ty: Type) -> (Instr, VReg) {
        let dest = self.new_vreg(VRegData {
            defined_in: id,
            ty: src_ty.clone(),
        });
        let instr = self.copy_op_instr(dest, src_op, src_ty);
        (instr, dest)
    }


    pub fn copy_op_instr(&self, dest: VReg, src_op: Op, src_ty: Type) -> Instr {
        Instr::new(src_ty, InstrKind::Op(
            OpInstr {
                op: src_op,
                value: dest,
            }
        ))
    }

    pub fn set_terminator(&mut self, id: BasicBlockId, terminator: Terminator) {
        self.set_edges_from_terminator(id, &terminator);
        self.basic_block_mut(id).terminator = Some(terminator);
    }

    fn set_edges_from_terminator(&mut self, id: BasicBlockId, terminator: &Terminator) {
        self.graph.retain_edges(
            |graph, edge| {
                graph.edge_endpoints(edge).unwrap().0 != id.into()
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
        self.graph.add_edge(source.into(), target.into(), ());
    }

    pub fn predecessors(&self, basic_block: BasicBlockId) -> impl Iterator<Item=BasicBlockId> + '_ {
        self.graph.neighbors_directed(basic_block.into(), Incoming).map(|n| n.into())
    }

    pub fn successors(&self, basic_block: BasicBlockId) -> impl Iterator<Item=BasicBlockId> + '_ {
        self.graph.neighbors(basic_block.into()).map(|n| n.into())
    }

    /// Recomputes the [BasicBlock]'s successors.
    pub fn recompute_successors(&mut self, basic_block: BasicBlockId) {
        let terminator = self.basic_block(basic_block).terminator().clone();
        self.set_edges_from_terminator(basic_block, &terminator);
    }

    pub fn dom_tree(&self) -> DomTree {
        DomTree::compute(self)
    }

    pub fn new_vreg(&mut self, vreg: VRegData) -> VReg {
        self.vregs.push(vreg)
    }

    pub fn vreg_ty(&self, vreg: VReg) -> &Type {
        &self.vreg(vreg).ty
    }

    pub fn vreg_ty_cloned(&self, vreg: VReg) -> Type {
        self.vreg(vreg).ty.clone()
    }

    pub fn vreg(&self, vreg: VReg) -> &VRegData {
        &self.vregs[vreg]
    }
}


impl Display for Cfg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent = "    ";
        for (bb_id, bb) in self.basic_blocks() {
            write!(f, "{}", bb_id)?;
            if !bb.arguments.is_empty() {
                write!(f, "(")?;
                for (index, arg) in bb.arguments.iter().copied().enumerate() {
                    let arg_ty = self.vreg_ty(arg);
                    write!(f, "{arg_ty} {arg}")?;
                    if index < bb.arguments.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;
            }
            writeln!(f, ":")?;
            for instr in bb.instructions() {
                writeln!(f, "{}{};", indent, instr.display(self))?;
            }
            writeln!(f, "{}{};", indent, bb.terminator())?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod cfg_tests {
    use super::*;

    #[test]
    fn should_not_return_removed_basic_block() {
        let mut cfg = Cfg::new();
        let bb0 = cfg.new_basic_block();
        cfg.set_terminator(bb0, Terminator::new(TerminatorKind::Ret(RetTerm::empty())));
        let bb1 = cfg.new_basic_block();
        cfg.set_terminator(bb1, Terminator::new(TerminatorKind::Ret(RetTerm::empty())));
        cfg.remove_basic_block(bb0);
        assert_eq!(
            cfg.basic_block_ids().collect::<Vec<_>>(),
            vec![bb1],
        );
        assert_eq!(
            cfg.basic_blocks().map(|(id, _)| id).collect::<Vec<_>>(),
            vec![bb1],
        )
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct BasicBlock {
    arguments: Vec<VReg>,
    pub(crate) instructions: Option<Vec<Instr>>,
    pub(crate) terminator: Option<Terminator>,
}

impl BasicBlock {
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns the [`Terminator`] of the [`BasicBlock`].
    ///
    /// Panics if the terminators is not set.
    pub fn terminator(&self) -> &Terminator {
        self.terminator.as_ref().expect("Basic blocks must have a terminator")
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
    pub fn update_terminator<'a, F, R>(&'a mut self, f: F) -> R where F: FnOnce(&'a mut Terminator) -> R + 'a {
        // todo: additional safety checks
        f(self.terminator.as_mut().unwrap())
    }

    /// Returns whether the [`Terminator`] is set.
    pub fn has_terminator(&self) -> bool {
        self.terminator.is_some()
    }

    pub fn arguments(&self) -> impl Iterator<Item=VReg> + '_ {
        self.arguments.iter().copied()
    }

    pub fn clear_arguments(&mut self) -> impl Iterator<Item=VReg> + '_ {
        self.arguments.drain(..)
    }

    pub fn add_argument(&mut self, arg: VReg) {
        self.arguments.push(arg);
    }

    pub fn remove_argument(&mut self, idx: usize) -> VReg {
        self.arguments.remove(idx)
    }

    /// Returns an iterator over the [`BasicBlock`]'s [`Instructions`][`Instr`].
    pub fn instructions(&self) -> impl Iterator<Item=&Instr> {
        match &self.instructions {
            Some(instructions) => instructions.iter(),
            None => [].iter()
        }
    }

    /// Returns a mutable iterator over the [`BasicBlock`]'s [`Instructions`][`Instr`].
    pub fn instructions_mut(&mut self) -> impl Iterator<Item=&mut Instr> {
        match &mut self.instructions {
            Some(instructions) => instructions.iter_mut(),
            None => [].iter_mut()
        }
    }

    pub fn remove_instructions_by_pred<P>(&mut self, p: P) where P: FnMut(&Instr) -> bool {
        if let Some(instructions) = &mut self.instructions {
            instructions.retain(p)
        }
    }

    pub fn append_instructions(&mut self, e_instructions: impl Iterator<Item=Instr>) {
        if let Some(instructions) = &mut self.instructions {
            instructions.extend(e_instructions)
        }
    }

    /// Appends an [instruction][`Instr`] to the end of the basic block
    pub fn append_instruction(&mut self, instr: Instr) {
        self.ensure_instructions_exists().push(instr);
    }

    pub fn insert_instruction_at(&mut self, idx: usize, instr: Instr) {
        self.ensure_instructions_exists().insert(idx, instr);
    }

    fn ensure_instructions_exists(&mut self) -> &mut Vec<Instr> {
        if self.instructions.is_none() {
            self.instructions = Some(Vec::new());
        };
        self.instructions.as_mut().unwrap()
    }
}

#[cfg(test)]
mod bb_tests {
    use cranelift_entity::EntityRef;

    use crate::{Instr, InstrKind, Type, VReg};
    use crate::instruction::{Const, Op, OpInstr};

    use super::{BranchTerm, Cfg, CondBranchTerm, JumpTarget, RetTerm, Terminator, TerminatorKind};

    #[test]
    fn should_set_entry_block() {
        let mut cfg = Cfg::new();
        let bb0 = cfg.new_basic_block();
        let bb1 = cfg.new_basic_block();
        assert_eq!(cfg.entry_block, Some(bb0));
    }

    #[test]
    fn should_return_correct_successors() {
        let mut cfg = Cfg::new();
        let bb0 = cfg.new_basic_block();
        let bb1 = cfg.new_basic_block();
        let bb2 = cfg.new_basic_block();
        cfg.set_terminator(bb0, Terminator::new(TerminatorKind::Ret(RetTerm::empty())));
        assert!(cfg.successors(bb0).eq(vec![].into_iter()));
        cfg.set_terminator(bb0, Terminator::new(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(
            bb1,
            vec![],
        )))));
        assert!(cfg.successors(bb0).eq(vec![bb1].into_iter()));
        cfg.set_terminator(bb0, Terminator::new(TerminatorKind::CondBranch(CondBranchTerm::new(
            Op::Const(Const::Int(Type::I32, 1)),
            JumpTarget::new(
                bb1,
                vec![],
            ),
            JumpTarget::new(
                bb2,
                vec![],
            ),
        ))));
        assert!(cfg.successors(bb0).eq(vec![bb2, bb1].into_iter()));
    }

    #[test]
    fn should_add_instruction() {
        let mut cfg = Cfg::new();
        let bb0 = cfg.new_basic_block();
        let instr = Instr::new(Type::I32, InstrKind::Op(OpInstr { op: Op::Const(Const::Int(Type::I32, 3)), value: VReg::new(2) }));
        cfg.add_instruction(bb0, instr.clone());
        assert_eq!(cfg.basic_block(bb0).instructions, Some(vec![instr]));
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

    pub fn update_references_to_bb(&mut self, old: BasicBlockId, new: BasicBlockId) {
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
}

impl Display for Terminator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TerminatorKind::Ret(term) => {
                write!(f, "ret")?;
                write!(f, " {}", term.ty)?;
                if let Some(value) = &term.value {
                    write!(f, " {value}")?;
                }
            }
            TerminatorKind::Branch(branch) => {
                write!(f, "br {}", branch.target)?;
            }
            TerminatorKind::CondBranch(branch) => {
                write!(f, "condbr {}, {}, {}", branch.cond, branch.true_target, branch.false_target)?;
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
    pub ty: Type,
    pub value: Option<Op>,
}

impl RetTerm {
    pub const fn new(ty: Type, value: Op) -> Self {
        Self { value: Some(value), ty }
    }
    pub const fn empty() -> Self {
        Self { value: None, ty: Type::Void }
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
}

impl Display for JumpTarget {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)?;
        if !self.arguments.is_empty() {
            write!(f, "(")?;
            for (index, arg) in self.arguments.iter().enumerate() {
                write!(f, "{arg}")?;
                if index != self.arguments.len() - 1 {
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
