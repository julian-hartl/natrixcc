use index_vec::IndexVec;
use std::fmt::{Display, Formatter};
use crate::function::Function;
use crate::ty::Type;

mod builder;
#[allow(unused_imports)]
pub use builder::Builder;
use crate::{InstrId, Value};
use crate::instruction::{Instr, Op, ValueData};
index_vec::define_index_type! {
    pub struct BasicBlockId = usize;
}
pub type ValueContext = IndexVec<Value, ValueData>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Cfg {
    pub basic_blocks: IndexVec<BasicBlockId, Option<BasicBlock>>,
    pub instructions: IndexVec<InstrId, Instr>,
    pub values_ctx: ValueContext,
    pub entry_block: BasicBlockId,
}

impl Cfg {

    pub fn new() -> Self {
        Self {
            basic_blocks: IndexVec::new(),
            instructions: IndexVec::new(),
            values_ctx: IndexVec::new(),
            entry_block: BasicBlockId::new(0),
        }
    }

    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &Function) -> std::fmt::Result {
        let indent = "    ";
        for (bb, bb_data) in self.basic_blocks.iter_enumerated() {
            if let Some(bb_data) = bb_data {
                writeln!(writer, "{bb}:")?;
                let predecessors = bb_data.predecessors(self);
                if !predecessors.is_empty() {
                    write!(writer, "{indent}; preds = ")?;
                    for (i, predecessor) in predecessors.iter().copied().enumerate() {
                        if i > 0 {
                            write!(writer, ", ")?;
                        }
                        write!(writer, "{predecessor}")?;
                    }
                    writeln!(writer)?;
                }
                for instr in &bb_data.instructions {
                    write!(writer, "{}", indent)?;
                    self.instructions[*instr].write_to(writer, function)?;
                }
                write!(writer, "{}", indent)?;
                bb_data.terminator.write_to(writer, function)?;
            }
        }
        Ok(())
    }

    pub fn target_place(&self, instr: InstrId) -> Option<Value> {
        let instr = &self.instructions[instr];
        instr.produced_value()
    }

    pub fn target_place_data(&self, instr: InstrId) -> Option<&ValueData> {
        let place = self.target_place(instr)?;
        Some(&self.values_ctx[place])
    }

    pub fn value_id_ty(&self, value: &ValueId) -> Option<Type> {
        for value_data in self.values_ctx.iter() {
            if value_data.id == *value {
                return Some(value_data.ty.clone());
            }
        }
        None

    }
}

impl Display for BasicBlockId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{}", self.index())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BasicBlock {
    pub id: BasicBlockId,
    pub instructions: Vec<InstrId>,
    pub terminator: Terminator,
}

impl BasicBlock {
    pub fn new(idx: BasicBlockId) -> Self {
        Self {
            id: idx,
            instructions: Vec::new(),
            terminator: Terminator::new(TerminatorKind::Ret(RetTerm { value: None })),
        }
    }

    pub fn declarations(&self, cfg: &Cfg) -> Vec<Value> {
        let mut declarations = Vec::new();
        for instr in self.instructions.iter().copied() {
            let instr = &cfg.instructions[instr];
            if let Some(place) = instr.produced_value() {
                declarations.push(place);
            }
        }
        declarations
    }

    pub fn successors(&self) -> Vec<BasicBlockId> {
        match &self.terminator.kind {
            TerminatorKind::Ret(_) => {
                Vec::new()
            }
            TerminatorKind::Branch(jmp) => {
                match jmp {
                    BranchTerm::UnCond(term) => {
                        vec![term.target]
                    }
                    BranchTerm::Cond(term) => {
                        vec![term.true_target, term.false_target]
                    }
                }
            }
        }
    }

    pub fn predecessors(&self, cfg: &Cfg) -> Vec<BasicBlockId> {
        let mut predecessors = Vec::new();
        for (bb, bb_data) in cfg.basic_blocks.iter_enumerated() {
            if let Some(bb_data) = bb_data {
                if bb_data.successors().contains(&self.id) {
                    predecessors.push(bb);
                }
            }
        }
        predecessors
    }
}

#[cfg(test)]
mod bb_tests {
    use crate::cfg;
    use crate::cfg::{BasicBlockId, BranchTerm, RetTerm, TerminatorKind, UnCondBrTerm};
    use crate::test_utils::create_test_function;

    #[test]
    fn should_return_correct_predecessors_and_successors() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        let bb0 = cfg_builder.create_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let bb3 = cfg_builder.create_bb();
        cfg_builder.set_bb(bb0);
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb1))));
        cfg_builder.set_bb(bb1);
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb3))));
        cfg_builder.set_bb(bb2);
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb3))));
        cfg_builder.set_bb(bb3);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let bb0_data = function.cfg.basic_blocks[bb0].as_ref().unwrap();
        let bb0_predecessors = bb0_data.predecessors(&function.cfg);
        let bb0_successors = bb0_data.successors();
        assert_eq!(bb0_predecessors, Vec::<BasicBlockId>::new());
        assert_eq!(bb0_successors, vec![bb1]);
        let bb1_data = function.cfg.basic_blocks[bb1].as_ref().unwrap();
        let bb1_predecessors = bb1_data.predecessors(&function.cfg);
        let bb1_successors = bb1_data.successors();
        assert_eq!(bb1_predecessors, vec![bb0]);
        assert_eq!(bb1_successors, vec![bb3]);
        let bb2_data = function.cfg.basic_blocks[bb2].as_ref().unwrap();
        let bb2_predecessors = bb2_data.predecessors(&function.cfg);
        let bb2_successors = bb2_data.successors();
        assert_eq!(bb2_predecessors, Vec::<BasicBlockId>::new());
        assert_eq!(bb2_successors, vec![bb3]);
        let bb3_data = function.cfg.basic_blocks[bb3].as_ref().unwrap();
        let bb3_predecessors = bb3_data.predecessors(&function.cfg);
        let bb3_successors = bb3_data.successors();
        assert_eq!(bb3_predecessors, vec![bb1, bb2]);
        assert_eq!(bb3_successors, Vec::<BasicBlockId>::new());
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

    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &Function) -> std::fmt::Result {
        match &self.kind {
            TerminatorKind::Ret(term) => {
                write!(writer, "ret")?;

                if let Some(value) = &term.value {
                    write!(writer, " {} ",value.ty(function))?;
                    value.write_to(writer, function)?;
                } else {
                    write!(writer, " void")?;
                }
            }
            TerminatorKind::Branch(term) => {
                match term {
                    BranchTerm::UnCond(term) => {
                        write!(writer, "br label {}", term.target)?;
                    }
                    BranchTerm::Cond(term) => {
                        write!(writer, "br {} ", term.cond.ty(function))?;
                        term.cond.write_to(writer, function)?;
                        write!(writer, ", label {}, label {}", term.true_target, term.false_target)?;
                    }
                }
            }
        }
        writeln!(writer)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TerminatorKind {
    Ret(RetTerm),
    Branch(BranchTerm),
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
pub enum BranchTerm {
    UnCond(UnCondBrTerm),
    Cond(CondBrTerm),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnCondBrTerm {
    pub target: BasicBlockId,
}

impl UnCondBrTerm {
    pub const fn new(target: BasicBlockId) -> Self {
        Self { target }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CondBrTerm {
    pub cond: Op,
    pub true_target: BasicBlockId,
    pub false_target: BasicBlockId,
}

impl CondBrTerm {
    pub const fn new(cond: Op, true_target: BasicBlockId, false_target: BasicBlockId) -> Self {
        Self {
            cond,
            true_target,
            false_target,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum ValueId {
    Unnamed(usize),
    Named(String),
}

impl Display for ValueId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueId::Unnamed(id) => write!(f, "%{}", id),
            ValueId::Named(name) => {
                write!(f, "%{}", name)?;
                Ok(())
            }
        }
    }
}
