use std::fmt::{Display, Formatter};

use fusion_compiler::{bug, idx, Idx};

use crate::mir::{InstructionIdx, Terminator, TerminatorKind};

/// A node in the MIR.
///
/// A basic block is a sequence of instructions that are executed sequentially. There are no branches
/// (e.g. `if`s or `while`s) in a basic block. Instead, branches are represented by the [terminator][Terminator]
/// of a basic block. This makes it easier to perform data flow analysis and therefore optimizations on the MIR.
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// Instructions of the basic block.
    ///
    /// Note that this is just a list of indices into the [`Function.instructions`](super::Function::instructions) vector.
    /// You may wonder why we don't store the instructions directly in the basic block. The reason lies in the
    /// [SSA](https://en.wikipedia.org/wiki/Static_single_assignment_form) form of the MIR. In SSA, each variable is
    /// assigned exactly once. In our form of SSA, instructions are treated as variables. This means that each instruction
    /// is assigned exactly once and because we refer to instructions by their index, we shall only ever have one
    /// instruction with a given index per function.
    pub instructions: Vec<InstructionIdx>,

    /// Terminator of the basic block.
    ///
    /// This should *ONLY* be `None` during construction of the MIR. After construction, each basic block should have
    /// a terminator. If you want to mark the terminator of a basic block as unresolved, use [`TerminatorKind::Unresolved`].
    ///
    /// To access the terminator, you should use [`BasicBlock::terminator`] and [`BasicBlock::terminator_mut`].
    pub terminator: Option<Terminator>,

    /// Index of the basic block.
    pub idx: BasicBlockIdx,
}

impl BasicBlock {
    pub fn new(idx: BasicBlockIdx) -> Self {
        Self {
            instructions: vec![],
            terminator: None,
            idx,
        }
    }

    #[inline]
    pub fn is_terminated(&self) -> bool {
        self.terminator.is_some()
    }

    #[inline]
    pub fn set_terminator(&mut self, kind: TerminatorKind) {
        tracing::debug!("Setting terminator of {:?} to {:?}", self.idx, kind);
        self.terminator = Some(Terminator::new(kind));
    }

    /// Sets the [terminator][Terminator] of the basic block if the basic block is not already terminated.
    pub fn maybe_set_terminator(&mut self, kind: TerminatorKind) {
        if !self.is_terminated() {
            self.set_terminator(kind);
        }
    }

    #[inline]
    pub fn terminator(&self) -> &Terminator {
        self.terminator
            .as_ref()
            .unwrap_or_else(|| bug!("Invalid terminator state in {:?}", self.idx))
    }

    #[inline]
    pub fn terminator_mut(&mut self) -> &mut Terminator {
        self.terminator
            .as_mut()
            .unwrap_or_else(|| bug!("Invalid terminator state in {:?}", self.idx))
    }

    /// Appends `other` to `self`.
    ///
    /// 1. Appends the instructions of `other` to `self`
    /// 2. Replaces `self.terminator` with `other.terminator`
    ///
    /// For example, if `self` is
    /// ```text
    /// bb0:
    ///    %0 = add %1, %2
    ///    %3 = add %0, %4
    ///    jump bb1
    /// ```
    /// and `other` is
    /// ```text
    /// bb1:
    ///   %5 = add %6, %7
    ///   %8 = add %5, %9
    ///   jump bb2
    /// ```
    /// then `self.append(other)` will result in
    /// ```text
    /// bb0:
    ///   %0 = add %1, %2
    ///   %3 = add %0, %4
    ///   %5 = add %6, %7
    ///   %8 = add %5, %9
    ///   jump bb2 ; note that the terminator of bb0 is replaced with the terminator of bb1
    pub fn append(&mut self, other: Self) {
        self.instructions.extend(other.instructions);
        self.terminator = other.terminator;
    }
}

/// Index of a basic block.
///
/// This can be used to refer to a basic block in [`MIR.basic_blocks`](super::MIR::basic_blocks).
idx!(BasicBlockIdx);

impl Display for BasicBlockIdx {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{}", self.as_index())
    }
}
