#![doc = include_str!("cfg.md")]

use std::fmt::{Debug, Display, Formatter};

#[allow(unused_imports)]
pub use petgraph::{prelude::*, visit::Walker};
use slotmap::{new_key_type, SlotMap};
use smallvec::SmallVec;

pub use builder::Builder;
pub use domtree::DomTree;
use fxindexmap::FxIndexSet;

use crate::{
    instruction::{Instr, Op},
    InstrKind, Type, Value,
};

mod builder;
mod domtree;
mod generator;

#[derive(Debug, Default, Clone)]
pub struct Cfg {
    /// A typical graph data structure that represents the nodes of the control flow graph.
    ///
    /// Any graph related algorithms are implemented using this graph, such as dominator tree computation,
    /// depth-first search, etc.
    graph: Graph,
    /// All basic blocks in the control flow graph.
    pub basic_blocks: SlotMap<BasicBlockRef, BasicBlock>,
    /// All instructions in the control flow graph. Use [`BasicBlock::instructions`] to get a single basic block's instructions.
    pub instructions: SlotMap<InstrRef, Instr>,
    /// All basic block arguments in the control flow graph. Use [`BasicBlock::arguments`] to get a single basic block's arguments.
    pub bb_args: SlotMap<BBArgRef, BBArg>,
    /// The entry block of the control flow graph.
    ///
    /// This block is always the first block executed when the respective function is called.
    /// It is wrapped
    /// in an [`Option`] because the entry block is only set after the first basic block is created.
    /// When you are sure that the entry block
    /// has been created, you can use [`Cfg::entry_block_ref`], which panics otherwise.
    entry_block: Option<BasicBlockRef>,
}

impl Cfg {
    pub fn new() -> Self {
        Self::default()
    }

    /// Inserts a new basic block in the control flow graph and returns a reference to it.
    ///
    /// Creates a new [`BasicBlock`] with the given `symbol`, inserts it into the arena and adds a node without any edges to the internal graph structure.
    ///
    /// Sets the entry block to the newly created block if it is the first block created.
    ///
    /// ```
    /// # use natrix_middle::Cfg;
    /// let mut cfg = Cfg::new();
    /// let bb0_ref = cfg.new_basic_block("bb0".into());
    /// assert_eq!(cfg.entry_block_ref(), bb0_ref);
    /// let bb0 = &cfg.basic_blocks[bb0_ref];
    /// assert_eq!(bb0.symbol, "bb0");
    /// let bb1_ref = cfg.new_basic_block("bb1".into());
    /// assert_eq!(cfg.entry_block_ref(), bb0_ref);
    /// ```
    pub fn new_basic_block(&mut self, symbol: String) -> BasicBlockRef {
        let bb = self.basic_blocks.insert_with_key(|id| {
            let node_idx = self.graph.add_node(CFGNode { bb_ref: id });
            BasicBlock::new(id, node_idx, symbol)
        });
        if self.entry_block.is_none() {
            self.entry_block = Some(bb);
        }
        bb
    }

    /// Removes a basic block from the control flow graph and returns it.
    ///
    /// Removes the basic block from the arena and the graph structure, which disconnects it from any predecessors/successors.
    /// Returns `None` if the basic block does not exist.
    ///
    /// ```
    /// # use natrix_middle::Cfg;
    /// let mut cfg = Cfg::new();
    /// let bb0_ref = cfg.new_basic_block("bb0".into());
    /// let bb1_ref = cfg.new_basic_block("bb1".into());
    /// let bb0 = cfg.remove_basic_block(bb0_ref);
    /// assert_eq!(bb0.unwrap().symbol, "bb0");
    /// assert!(cfg.remove_basic_block(bb0_ref).is_none());
    /// assert!(cfg.basic_blocks.get(bb0_ref).is_none());
    /// assert!(cfg.basic_blocks.get(bb1_ref).is_some());
    /// ```
    pub fn remove_basic_block(&mut self, bb_id: BasicBlockRef) -> Option<BasicBlock> {
        let bb = self.basic_blocks.remove(bb_id)?;
        self.graph.remove_node(bb.node_index);
        Some(bb)
    }

    /// Returns the reference to the entry block of the control flow graph.
    ///
    /// # Panics
    ///
    /// Panics if the entry block has not been created yet iff. the control flow graph is empty.
    pub fn entry_block_ref(&self) -> BasicBlockRef {
        self.entry_block.expect("Entry block has not been created")
    }

    /// Adds a new instruction to the control flow graph and returns a reference to it.
    ///
    /// Inserts the instruction into the arena and appends it to the tail of respective basic block's instructions.
    /// The instruction will have the given `ty`, `instr` and `symbol`. E.g. `add i32 %0, %1` would have the symbol `add`,
    /// the type [`Type::I32`] and the instruction [`InstrKind::Add`].
    ///
    /// Does *not* check whether the instruction is semantically correct, e.g. passing the wrong type to an instruction.
    ///
    /// ```
    /// # use natrix_middle::{Cfg, InstrKind, Type};
    /// use natrix_middle::instruction::{BinOpInstr, Op, OpInstr};
    /// use natrix_middle::instruction::const_op::Const;
    /// let mut cfg = Cfg::new();
    /// let bb0_ref = cfg.new_basic_block("bb0".into());
    /// let a_ref = cfg.add_instruction(bb0_ref, Type::I32, InstrKind::Op(OpInstr::new(Op::Const(Const::I32(1)))), "a".into());
    /// let b_ref = cfg.add_instruction(bb0_ref, Type::I32, InstrKind::Op(OpInstr::new(Op::Const(Const::I32(1)))), "b".into());
    /// let sum_ref = cfg.add_instruction(bb0_ref, Type::I32, InstrKind::Add(BinOpInstr{lhs: Op::Value(a_ref.into()), rhs: Op::Value(b_ref.into())}), "sum".into());
    /// assert!(cfg.instructions.get(a_ref).is_some());
    /// assert!(cfg.instructions.get(b_ref).is_some());
    /// assert!(cfg.instructions.get(sum_ref).is_some());
    /// let bb0 = &cfg.basic_blocks[bb0_ref];
    /// assert!(bb0.instructions().eq(vec![a_ref, b_ref, sum_ref].into_iter()));
    /// let a = &cfg.instructions[a_ref];
    /// assert_eq!(a.symbol, "a");
    /// assert_eq!(a.ty, Type::I32);
    /// ```
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

    /// Sets the terminator of the basic block with the given `id` to `terminator` and adjusts the control flow graph accordingly.
    ///
    /// When doing batch updating of the control flow graph, it might be more efficient to set all terminators first directly on the basic block and call [`Cfg::recompute_successors`] afterwards.
    ///
    /// ```
    /// # use natrix_middle::{Cfg};
    /// use natrix_middle::cfg::{BranchTerm, BranchTarget, TerminatorKind};
    /// let mut cfg = Cfg::new();
    /// let bb0_ref = cfg.new_basic_block("bb0".into());
    /// let bb1_ref = cfg.new_basic_block("bb1".into());
    /// cfg.set_terminator(bb0_ref, TerminatorKind::Branch(BranchTerm::new(BranchTarget::new(bb1_ref, vec![]))));
    /// assert_eq!(cfg.basic_blocks[bb0_ref].terminator().kind, TerminatorKind::Branch(BranchTerm::new(BranchTarget::new(bb1_ref, vec![]))));
    /// assert!(cfg.successors(bb0_ref).eq(vec![bb1_ref].into_iter()));
    /// assert!(cfg.predecessors(bb1_ref).eq(vec![bb0_ref].into_iter()));
    /// ```
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

    /// Returns an iterator over all direct predecessors of the given basic block
    pub fn predecessors(&self, bb_ref: BasicBlockRef) -> impl Iterator<Item = BasicBlockRef> + '_ {
        self.graph
            .neighbors_directed(self.basic_blocks[bb_ref].node_index, Incoming)
            .map(|n| self.graph[n].bb_ref)
    }

    /// Returns an iterator over all direct successors of the given basic block
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

    /// Returns the computed [`DomTree`]
    pub fn dom_tree(&self) -> DomTree {
        DomTree::compute(self)
    }

    /// Returns a dfs iterator over all reachable nodes from the entry node in post order
    ///
    /// ```
    /// # use natrix_middle::Cfg;
    /// # use natrix_middle::cfg::{BranchTerm, CondBranchTerm, RetTerm, TerminatorKind};
    /// # use natrix_middle::instruction::const_op::Const;
    /// # use natrix_middle::instruction::Op;
    /// let mut cfg = Cfg::new();
    /// let bb0 = cfg.new_basic_block("bb0".to_string());
    /// let bb1 = cfg.new_basic_block("bb1".to_string());
    /// let bb2 = cfg.new_basic_block("bb2".to_string());
    /// let bb3 = cfg.new_basic_block("bb3".to_string());
    ///
    /// cfg.set_terminator(bb0, TerminatorKind::CondBranch(CondBranchTerm::new(Op::Const(Const::Bool(true)), bb1.into(), bb2.into())));
    /// cfg.set_terminator(bb1, TerminatorKind::Branch(BranchTerm::new(bb3.into())));
    /// cfg.set_terminator(bb2, TerminatorKind::Branch(BranchTerm::new(bb0.into())));
    /// cfg.set_terminator(bb3, TerminatorKind::Ret(RetTerm::empty()));
    ///
    /// let mut traversal = cfg.dfs_postorder();
    /// assert_eq!(traversal.next(), Some(bb3));
    /// assert_eq!(traversal.next(), Some(bb1));
    /// assert_eq!(traversal.next(), Some(bb2));
    /// assert_eq!(traversal.next(), Some(bb0));
    /// assert_eq!(traversal.next(), None);
    /// ```
    pub fn dfs_postorder(&self) -> impl Iterator<Item = BasicBlockRef> + '_ {
        DfsPostOrder::new(
            &self.graph,
            self.basic_blocks[self.entry_block_ref()].node_index,
        )
        .iter(&self.graph)
        .map(|node| self.graph[node].bb_ref)
    }

    /// Adds a basic block argument to the given basic block with the given type and symbol.
    ///
    /// Returns a reference to the newly created basic block argument.
    ///
    /// ```
    /// # use natrix_middle::{Cfg, Type};
    /// # use natrix_middle::cfg::BBArg;
    /// let mut cfg = Cfg::new();
    /// let bb0 = cfg.new_basic_block("bb0".to_string());
    /// let a_ref = cfg.add_bb_argument(bb0, Type::I16, "a".to_string());
    ///
    /// let mut args = cfg.basic_blocks[bb0].arguments.iter().copied();
    /// assert_eq!(args.next(), Some(a_ref));
    /// assert_eq!(args.next(), None);
    /// assert_eq!(cfg.bb_args[a_ref], BBArg {symbol: "a".to_string(), id: a_ref, ty: Type::I16});
    pub fn add_bb_argument(&mut self, bb_ref: BasicBlockRef, ty: Type, symbol: String) -> BBArgRef {
        let arg_ref = self.bb_args.insert_with_key(|id| BBArg { id, ty, symbol });
        self.basic_blocks[bb_ref].arguments.insert(arg_ref);
        arg_ref
    }

    /// Returns an iterator over all existing values, that could be referenced by instructions
    ///
    /// ```
    /// # use natrix_middle::{Cfg, InstrKind, Type};
    /// # use natrix_middle::instruction::{BinOpInstr, Op, OpInstr};
    /// # use natrix_middle::instruction::const_op::Const;
    /// let mut cfg = Cfg::new();
    /// let bb0 = cfg.new_basic_block("bb0".to_string());
    /// let bb1 = cfg.new_basic_block("bb1".to_string());
    ///
    /// let a_ref = cfg.add_instruction(bb0, Type::I16, InstrKind::Op(OpInstr::new(Op::Const(Const::I64(1)))), "a".to_string());
    /// let b_ref = cfg.add_bb_argument(bb0, Type::I32, "b".to_string());
    /// let c_ref = cfg.add_instruction(bb1, Type::I32, InstrKind::Add(BinOpInstr{lhs: Op::Value(a_ref.into()), rhs: Op::Value(b_ref.into())}), "c".to_string());
    ///
    /// let mut values = cfg.values();
    /// assert_eq!(values.next(), Some(a_ref.into()));
    /// assert_eq!(values.next(), Some(c_ref.into()));
    /// assert_eq!(values.next(), Some(b_ref.into()));
    /// assert_eq!(values.next(), None);
    /// ```
    pub fn values(&self) -> impl Iterator<Item = Value> + '_ {
        self.instructions
            .keys()
            .map(Value::Instr)
            .chain(self.bb_args.keys().map(Value::BBArg))
    }
}
new_key_type! {
    pub struct BasicBlockRef;
}
impl BasicBlockRef {
    pub fn display(self, cfg: &Cfg) -> &String {
        &cfg.basic_blocks[self].symbol
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct CFGNode {
    bb_ref: BasicBlockRef,
}

pub type Graph = StableGraph<CFGNode, (), Directed>;

impl Display for Cfg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent = "    ";
        for (_, bb) in &self.basic_blocks {
            write!(f, "{}", bb)?;
            if !bb.arguments.is_empty() {
                write!(f, "(")?;
                for (index, arg_ref) in bb.arguments.iter().copied().enumerate() {
                    let arg = &self.bb_args[arg_ref];
                    let arg_ty = &self.bb_args[arg_ref].ty;
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

    mod new_basic_block {
        use super::*;

        #[test]
        fn should_create_basic_block() {
            let name = "bb0".to_string();
            let mut cfg = Cfg::new();
            let bb_ref = cfg.new_basic_block(name.clone());
            let bb = &cfg.basic_blocks[bb_ref];
            assert_eq!(bb.symbol, name);
            assert_eq!(bb.id, bb_ref);
            assert_eq!(bb.instructions.len(), 0);
            assert_eq!(bb.terminator, None);
            assert_eq!(bb.arguments.len(), 0);
            assert_eq!(cfg.graph[bb.node_index].bb_ref, bb_ref);
        }

        #[test]
        fn should_set_first_basic_block_as_entry_block() {
            let mut cfg = Cfg::new();
            let bb0_ref = cfg.new_basic_block("bb0".into());
            let bb1_ref = cfg.new_basic_block("bb1".into());
            assert_eq!(cfg.entry_block, Some(bb0_ref));
            assert_eq!(cfg.entry_block_ref(), bb0_ref);
        }
    }

    mod remove_basic_block {
        use super::*;

        #[test]
        fn should_remove_basic_block_from_arena() {
            let mut cfg = Cfg::new();
            let bb0 = cfg.new_basic_block("bb0".into());
            let bb1 = cfg.new_basic_block("bb1".into());
            cfg.remove_basic_block(bb0);
            assert!(cfg.basic_blocks.get(bb0).is_none());
            assert!(cfg.basic_blocks.get(bb1).is_some());
        }

        #[test]
        fn should_remove_respective_node_in_graph() {
            let mut cfg = Cfg::new();
            let bb0_ref = cfg.new_basic_block("bb0".into());
            let bb0_node_index = cfg.basic_blocks[bb0_ref].node_index;
            let bb1 = cfg.new_basic_block("bb1".into());
            let bb1_node_index = cfg.basic_blocks[bb1].node_index;
            cfg.remove_basic_block(bb0_ref);
            assert!(!cfg.graph.contains_node(bb0_node_index));
            assert!(cfg.graph.contains_node(bb1_node_index));
        }

        #[test]
        fn should_return_correct_bb() {
            let mut cfg = Cfg::new();
            let bb0 = cfg.new_basic_block("bb0".into());
            cfg.new_basic_block("bb1".into());
            let removed_bb = cfg.remove_basic_block(bb0);
            assert_eq!(removed_bb.unwrap().id, bb0);
        }
    }

    mod set_terminator {
        #[test]
        fn test() {
            let mut cfg = super::generator::Generator::default().generate();
            println!("{}", cfg);
        }
    }

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

new_key_type! { pub struct InstrRef; }

new_key_type! { pub struct BBArgRef; }

/// Represents an argument to a basic block.
///
/// Unlike traditional SSA-based compilers, this compiler uses basic block arguments instead of phi functions
/// to manage variable values at control flow merge points.
/// A basic block argument is passed explicitly whenever control flow transfers to the target block,
/// providing several advantages, such as enhanced clarity and simpler SSA maintenance.
///
/// # Advantages
///
/// - **Code Simplicity**: Basic block arguments replace phi nodes with a function-like parameter passing style,
///   improving code readability and unifying the handling of variable values across control flow.
/// - **SSA Compliance**: Arguments naturally conform to SSA form, reducing the need for phi nodes
///   and simplifying SSA transformations.
/// - **Optimization Efficiency**: Basic block arguments streamline value propagation and other
///   optimizations by clearly defining incoming values as parameters.
///
/// # Example
///
/// Instead of something like
///
/// ```text
/// fun i32 @mul(i32 %0, i32 %1) {
///  bb0:
///    br bb1(0i32, %1);
///  bb1:
///    %2 = phi(bb0: 0i32, bb2: %5)
///    %3 = phi(bb0: %1, bb2: %6)
///    bool %4 = cmp gt %3, 0i32;
///    condbr %4 bb2, bb3;
///  bb2:
///    i32 %5 = add %2, %0;
///    i32 %6 = sub %3, 1i32;
///    br bb1;
///  bb3:
///    i32 %7 = add %2, %2;
///    ret %7;
///  }
/// ```
///
/// we'd write
///
/// ```text
/// fun i32 @mul(i32, i32) {
///  bb0(i32 %0, i32 %1):
///    br bb1(0i32, %1);
///  bb1(i32 %2, i32 %3):
///    bool %4 = cmp gt %3, 0i32;
///    condbr %4 bb2, bb3;
///  bb2:
///    i32 %5 = add %2, %0;
///    i32 %6 = sub %3, 1i32;
///    br bb1(%5, %6);
///  bb3:
///    i32 %7 = add %2, %2;
///    ret %7;
///  }
/// ```
///
/// which makes control flow a lot more obvious as you don't have to go looking
/// for where to phi arguments actually come from.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BBArg {
    /// A unique identifier for this basic block argument
    pub id: BBArgRef,
    /// The data type of the argument (e.g., `i16`, `i32`, etc.)
    pub ty: Type,
    /// A debug symbol associated with this argument, used for diagnostics and debugging purposes
    pub symbol: String,
}

impl Display for BBArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.symbol)
    }
}

/// Represents a basic block in a control flow graph (CFG).
///
/// A basic block is a sequence of instructions that executes sequentially without any internal branching
/// or interruptions, such as jumps, branches, or exception handling. This guarantees that
/// once the block is entered, all instructions within it will execute in order until the block's end.
///
/// Basic blocks are foundational to control flow analysis, as they provide a simplified,
/// linear segment of code that is easy to analyze for runtime behavior within the block.
/// By design, each block has a single entry point and a single exit point,
/// ensuring straightforward reasoning about the block's local behavior at runtime.
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// A unique identifier for this basic block
    pub id: BasicBlockRef,
    /// A set of references to the basic block's arguments
    pub arguments: FxIndexSet<BBArgRef>,
    /// A set of references to the basic block's instructions
    pub instructions: FxIndexSet<InstrRef>,
    /// The basic block's terminator. Will be [`Option::None`] during construction
    pub terminator: Option<Terminator>,
    /// The basic block's index into the control flow graph's internal graph data structure
    node_index: NodeIndex,
    /// A debug symbol associated with this basic block, used for diagnostics and debugging purposes
    pub symbol: String,
}

impl BasicBlock {
    pub fn new(id: BasicBlockRef, graph_index: NodeIndex, symbol: String) -> Self {
        Self {
            id,
            arguments: FxIndexSet::default(),
            instructions: FxIndexSet::default(),
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

    /// Returns whether the basic block has a [`Terminator`].
    ///
    /// Should only return `false` during construction.
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

    /// Removes the instruction locally from the basic block.
    ///
    /// Does *not* remove it from the internal arena managed by the (control flow graph)[Cfg]
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
    use super::{BranchTarget, BranchTerm, Cfg, CondBranchTerm, RetTerm, TerminatorKind};
    use crate::instruction::const_op::Const;
    use crate::instruction::Op;

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
            TerminatorKind::Branch(BranchTerm::new(BranchTarget::new(bb1, vec![]))),
        );
        assert!(cfg.successors(bb0).eq(vec![bb1].into_iter()));
        cfg.set_terminator(
            bb0,
            TerminatorKind::CondBranch(CondBranchTerm::new(
                Op::Const(Const::I32(1)),
                BranchTarget::new(bb1, vec![]),
                BranchTarget::new(bb2, vec![]),
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

    /// Removes all passed arguments from the terminator.
    ///
    /// ```
    /// # use itertools::Itertools;
    /// # use natrix_middle::{Cfg, Type};
    /// # use natrix_middle::cfg::{BranchTarget, BranchTerm, CondBranchTerm, RetTerm, TerminatorKind};
    /// # use natrix_middle::instruction::const_op::Const;
    /// # use natrix_middle::instruction::Op;
    /// let mut cfg = Cfg::new();
    /// let bb0 = cfg.new_basic_block("bb0".to_string());
    /// let bb1 = cfg.new_basic_block("bb1".to_string());
    /// let bb2 = cfg.new_basic_block("bb2".to_string());
    ///
    /// let a_ref = cfg.add_bb_argument(bb1, Type::I32, "a".to_string());
    /// let b_ref = cfg.add_bb_argument(bb0, Type::I32, "b".to_string());
    /// let c_ref = cfg.add_bb_argument(bb2, Type::I32, "c".to_string());
    ///
    /// cfg.set_terminator(bb0, TerminatorKind::Branch(BranchTerm::new(BranchTarget::new(bb1, vec![Op::Const(Const::I32(1))]))));
    /// cfg.set_terminator(bb1, TerminatorKind::CondBranch(CondBranchTerm::new(Op::Const(Const::Bool(true)), BranchTarget::new(bb0, vec![Op::Const(Const::I32(1))]), BranchTarget::new(bb2, vec![Op::Const(Const::I32(2))]))));
    /// cfg.set_terminator(bb2, TerminatorKind::Ret(RetTerm::empty()));
    ///
    /// cfg.basic_blocks[bb0].update_terminator(|term| assert!(term.clear_args(bb1).is_some(), "should clear args for existing branch target"));
    /// cfg.basic_blocks[bb0].update_terminator(|term| assert!(term.clear_args(bb2).is_none(), "should not clear args when there is no branch to bb0"));
    /// cfg.basic_blocks[bb1].update_terminator(|term| assert!(term.clear_args(bb2).is_some()));
    ///
    /// assert_eq!(cfg.basic_blocks[bb1].terminator().branch_args(bb0).unwrap().collect_vec(), vec![&Op::Const(Const::I32(1))], "branch from bb1 to bb0 should still have arguments");
    /// assert_eq!(cfg.basic_blocks[bb0].terminator().branch_args(bb1).unwrap().collect_vec().len(), 0,"branch from bb0 to bb1 should not have any arguments");
    /// assert_eq!(cfg.basic_blocks[bb1].terminator().branch_args(bb2).unwrap().collect_vec().len(),0, "branch from bb1 to bb2 should not have any arguments");
    ///
    /// ```
    pub fn clear_args(&mut self, target: BasicBlockRef) -> Option<impl Iterator<Item = Op> + '_> {
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

    pub fn update_refs(&mut self, from: Value, to: Value) -> u32 {
        match &mut self.kind {
            TerminatorKind::Ret(ret_term) => {
                if let Some(value) = &mut ret_term.value {
                    return value.update_refs(from, to);
                }
                0
            }
            TerminatorKind::Branch(branch_term) => {
                let mut changes = 0;
                for arg in &mut branch_term.target.arguments {
                    changes += arg.update_refs(from, to);
                }
                changes
            }
            TerminatorKind::CondBranch(condbr_term) => {
                let mut changes = 0;
                changes += condbr_term.cond.update_refs(from, to);
                for target in condbr_term.targets_mut() {
                    for arg in &mut target.arguments {
                        changes += arg.update_refs(from, to);
                    }
                }
                changes
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
pub struct BranchTarget {
    pub id: BasicBlockRef,
    pub arguments: Vec<Op>,
}

impl BranchTarget {
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

impl From<BasicBlockRef> for BranchTarget {
    fn from(value: BasicBlockRef) -> Self {
        Self::new(value, vec![])
    }
}

pub struct JumpTargetDisplay<'cfg, 'target> {
    target: &'target BranchTarget,
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
    pub target: BranchTarget,
}

impl BranchTerm {
    pub const fn new(target: BranchTarget) -> Self {
        Self { target }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CondBranchTerm {
    pub cond: Op,
    pub true_target: BranchTarget,
    pub false_target: BranchTarget,
}

impl CondBranchTerm {
    pub const fn new(cond: Op, true_target: BranchTarget, false_target: BranchTarget) -> Self {
        Self {
            cond,
            true_target,
            false_target,
        }
    }

    pub fn targets(&self) -> [&BranchTarget; 2] {
        [&self.true_target, &self.false_target]
    }

    pub fn targets_mut(&mut self) -> [&mut BranchTarget; 2] {
        [&mut self.true_target, &mut self.false_target]
    }
}
