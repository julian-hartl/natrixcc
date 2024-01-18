use std::fmt::{Display, Formatter};

use index_vec::IndexVec;

use crate::function::FunctionData;
use crate::instruction::{Instr, InstrData, Op, Place, PlaceData};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct CFG {
    pub basic_blocks: IndexVec<BasicBlock, Option<BasicBlockData>>,
    pub instructions: IndexVec<Instr, InstrData>,
    pub places: IndexVec<Place, PlaceData>,
}

impl CFG {
    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &FunctionData) -> std::fmt::Result {
        let indent = "    ";
        for (bb, bb_data) in self.basic_blocks.iter_enumerated() {
            if let Some(bb_data) = bb_data {
                writeln!(writer, "{}:", bb)?;
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

    pub fn target_place(&self, instr: Instr) -> Option<Place> {
        let instr = &self.instructions[instr];
        instr.target_place()
    }

    pub fn target_place_data(&self, instr: Instr) -> Option<&PlaceData> {
        let place = self.target_place(instr)?;
        Some(&self.places[place])
    }
}

index_vec::define_index_type! {
    pub struct BasicBlock = usize;
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{}", self.index())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BasicBlockData {
    pub idx: BasicBlock,
    pub instructions: Vec<Instr>,
    pub terminator: Terminator,
}

impl BasicBlockData {
    pub fn new(idx: BasicBlock) -> Self {
        Self {
            idx,
            instructions: Vec::new(),
            terminator: Terminator::new(TerminatorKind::Ret(RetTerm { value: None })),
        }
    }

    pub fn declarations(&self, cfg: &CFG) -> Vec<Place> {
        let mut declarations = Vec::new();
        for instr in self.instructions.iter().copied() {
            let instr = &cfg.instructions[instr];
            if let Some(place) = instr.target_place() {
                declarations.push(place);
            }
        }
        declarations
    }

    pub fn successors(&self) -> Vec<BasicBlock> {
        match &self.terminator.kind {
            TerminatorKind::Ret(_) => {
                Vec::new()
            }
            TerminatorKind::Jmp(jmp) => {
                vec![jmp.target]
            }
        }
    }

    pub fn predecessors(&self, cfg: &CFG) -> Vec<BasicBlock> {
        let mut predecessors = Vec::new();
        for (bb, bb_data) in cfg.basic_blocks.iter_enumerated() {
            if let Some(bb_data) = bb_data {
                if bb_data.successors().contains(&self.idx) {
                    predecessors.push(bb);
                }
            }
        }
        predecessors
    }
}

#[cfg(test)]
mod bb_tests {
    use crate::cfg::{BasicBlock, JmpTerm, RetTerm, TerminatorKind};
    use crate::cfg_builder::CFGBuilder;
    use crate::create_test_function;

    #[test]
    fn should_return_correct_predecessors_and_successors() {
        let mut function =create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        let bb0 = cfg_builder.create_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let bb3 = cfg_builder.create_bb();
        cfg_builder.set_bb(bb0);
        cfg_builder.end_bb(TerminatorKind::Jmp(JmpTerm::new(bb1)));
        cfg_builder.set_bb(bb1);
        cfg_builder.end_bb(TerminatorKind::Jmp(JmpTerm::new(bb3)));
        cfg_builder.set_bb(bb2);
        cfg_builder.end_bb(TerminatorKind::Jmp(JmpTerm::new(bb3)));
        cfg_builder.set_bb(bb3);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let bb0_data = function.cfg.basic_blocks[bb0].as_ref().unwrap();
        let bb0_predecessors = bb0_data.predecessors(&function.cfg);
        let bb0_successors = bb0_data.successors();
        assert_eq!(bb0_predecessors, Vec::<BasicBlock>::new());
        assert_eq!(bb0_successors, vec![bb1]);
        let bb1_data = function.cfg.basic_blocks[bb1].as_ref().unwrap();
        let bb1_predecessors = bb1_data.predecessors(&function.cfg);
        let bb1_successors = bb1_data.successors();
        assert_eq!(bb1_predecessors, vec![bb0]);
        assert_eq!(bb1_successors, vec![bb3]);
        let bb2_data = function.cfg.basic_blocks[bb2].as_ref().unwrap();
        let bb2_predecessors = bb2_data.predecessors(&function.cfg);
        let bb2_successors = bb2_data.successors();
        assert_eq!(bb2_predecessors, Vec::<BasicBlock>::new());
        assert_eq!(bb2_successors, vec![bb3]);
        let bb3_data = function.cfg.basic_blocks[bb3].as_ref().unwrap();
        let bb3_predecessors = bb3_data.predecessors(&function.cfg);
        let bb3_successors = bb3_data.successors();
        assert_eq!(bb3_predecessors, vec![bb1, bb2]);
        assert_eq!(bb3_successors, Vec::<BasicBlock>::new());
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Terminator {
    pub kind: TerminatorKind,
}

impl Terminator {
    pub fn new(kind: TerminatorKind) -> Self {
        Self { kind }
    }

    pub fn write_to<W: std::fmt::Write>(&self, writer: &mut W, function: &FunctionData) -> std::fmt::Result {
        match &self.kind {
            TerminatorKind::Ret(term) => {
                write!(writer, "ret")?;
                if let Some(value) = &term.value {
                    write!(writer, " ")?;
                    value.write_to(writer, function)?;
                }
            }
            TerminatorKind::Jmp(term) => {
                write!(writer, "jmp {}", term.target)?;
            }
        }
        writeln!(writer)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TerminatorKind {
    Ret(RetTerm),
    Jmp(JmpTerm),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct RetTerm {
    pub value: Option<Op>,
}

impl RetTerm {
    pub fn empty() -> Self {
        Self { value: None }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct JmpTerm {
    pub target: BasicBlock,
}

impl JmpTerm {
    pub fn new(target: BasicBlock) -> Self {
        Self { target }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VarId {
    Unnamed(usize),
    Named(String)
}

impl Display for VarId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VarId::Unnamed(id) => write!(f, "%{}", id),
            VarId::Named (name) => {
                write!(f, "%{}", name)?;
                Ok(())
            }
        }
    }
}
