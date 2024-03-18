use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};

use daggy::petgraph::dot::{Config, Dot};
use rustc_hash::FxHashMap;
use smallvec::{SmallVec, smallvec};

pub use builder::Builder;
use firc_middle::cfg::BasicBlockId;
use firc_middle::instruction::CmpOp;

use crate::codegen::machine;
use crate::codegen::machine::{Abi,  MachineInstr, Pattern, Register};

pub mod builder;

type Dag<A> = daggy::Dag<Op<A>, Edge>;

#[derive(Debug, Clone)]
pub struct BasicBlockDAG<A: machine::Abi> {
    dag: Dag<A>,
    term_node: Option<daggy::NodeIndex>,
    bb: firc_middle::cfg::BasicBlockId,
}

impl<A: machine::Abi> BasicBlockDAG<A> {
    pub fn new(bb: firc_middle::cfg::BasicBlockId) -> Self {
        Self {
            dag: Dag::new(),
            term_node: None,
            bb,
        }
    }
    pub fn term_node(&self) -> daggy::NodeIndex {
        self.term_node.unwrap()
    }

    pub fn set_term_node(&mut self, node: daggy::NodeIndex) {
        self.term_node = Some(node);
    }

    pub fn graphviz(&self) -> String {
        format!("{:?}", Dot::with_config(&self.dag, &[Config::EdgeNoLabel]))
    }

    pub fn save_graphviz<P: AsRef<std::path::Path>>(&self, path: P) -> std::io::Result<()> {
        std::fs::create_dir_all(&path)?;
        std::fs::write(format!("{}/{}.dot", path.as_ref().display(), self.bb), self.graphviz())
    }
}

impl<A: machine::Abi> Deref for BasicBlockDAG<A> {
    type Target = Dag<A>;

    fn deref(&self) -> &Self::Target {
        &self.dag
    }
}

impl<A: machine::Abi> DerefMut for BasicBlockDAG<A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.dag
    }
}

#[derive(Debug, Default)]
pub struct SelectionDAG<A: machine::Abi> {
    pub basic_blocks: FxHashMap<firc_middle::cfg::BasicBlockId, BasicBlockDAG<A>>,
}

impl<A: machine::Abi> SelectionDAG<A> {
    pub fn get_bb_dag(&mut self, basic_block: firc_middle::cfg::BasicBlockId) -> &mut BasicBlockDAG<A> {
        self.basic_blocks.entry(basic_block).or_insert_with(|| BasicBlockDAG::new(basic_block))
    }
}


#[derive(Debug, Copy, Clone, Ord, PartialOrd, PartialEq, Eq, EnumTryAs)]
pub enum Immediate {
    Byte(Byte),
    Word(Word),
    DWord(DWord),
    QWord(QWord),
}

impl Immediate {
    pub fn size(&self) -> machine::Size {
        match self {
            Self::Byte(_) => machine::Size::Byte,
            Self::Word(_) => machine::Size::Word,
            Self::DWord(_) => machine::Size::DWord,
            Self::QWord(_) => machine::Size::QWord,
        }
    }
}

impl Display for Immediate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Byte(b) => write!(f, "{:x?}", b.0),
            Self::Word(w) => write!(f, "{:x?}", w.0),
            Self::DWord(dw) => write!(f, "{:x?}", dw.0),
            Self::QWord(qw) => write!(f, "{:x?}", qw.0),
        }
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, PartialEq, Eq)]
pub struct Byte(u8);

impl From<u8> for Byte {
    fn from(val: u8) -> Self {
        Self(val)
    }
}

impl From<i8> for Byte {
    fn from(val: i8) -> Self {
        Self(val as u8)
    }
}

impl From<bool> for Byte {
    fn from(val: bool) -> Self {
        Self(val as u8)
    }
}

impl Into<u8> for Byte {
    fn into(self) -> u8 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, PartialEq, Eq)]
pub struct Word([u8; 2]);

impl From<u16> for Word {
    fn from(val: u16) -> Self {
        Self(val.to_le_bytes())
    }
}

impl From<i16> for Word {
    fn from(val: i16) -> Self {
        Self(val.to_le_bytes())
    }
}

impl Into<u16> for Word {
    fn into(self) -> u16 {
        u16::from_le_bytes(self.0)
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, PartialEq, Eq)]
pub struct DWord([u8; 4]);

impl DWord {
    pub fn into_unsigned(self) -> u32 {
        u32::from_le_bytes(self.0)
    }
}

impl From<u32> for DWord {
    fn from(val: u32) -> Self {
        Self(val.to_le_bytes())
    }
}

impl From<i32> for DWord {
    fn from(val: i32) -> Self {
        Self(val.to_le_bytes())
    }
}

#[derive(Debug, Clone, Copy, Ord, PartialOrd, PartialEq, Eq)]
pub struct QWord([u8; 8]);

impl From<u64> for QWord {
    fn from(val: u64) -> Self {
        Self(val.to_le_bytes())
    }
}

impl From<i64> for QWord {
    fn from(val: i64) -> Self {
        Self(val.to_le_bytes())
    }
}


#[derive(Debug, Clone)]
pub struct Edge;

impl Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Op<A: Abi> {
    Machine(MachineOp<A>),
    Pseudo(PseudoOp<A>),
}

impl<A: Abi> Op<A> {
    pub fn out(&self) -> Option<Register<A>> {
        match self {
            Op::Machine(op) => op.out(),
            Op::Pseudo(op) => op.out(),
        }
    }

    pub fn consumed_regs(&self) -> SmallVec<[Register<A>; 2]> {
        match self {
            Op::Machine(op) => op.consumed_regs(),
            Op::Pseudo(op) => op.consumed_regs(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PseudoOp<A: Abi> {
    Copy(Register<A>, Register<A>),
    Ret(Option<Operand<A>>),
    Phi(Register<A>, Vec<Register<A>>)
}

impl<A: Abi> PseudoOp<A> {
    pub fn out(&self) -> Option<Register<A>> {
        match self {
            Self::Copy(dest, _) => Some(*dest),
            Self::Ret(_) => None,
            Self::Phi(dest, _) => Some(*dest)
        }
    }

    pub fn consumed_regs(&self) -> SmallVec<[Register<A>; 2]> {
        match self {
            PseudoOp::Copy(_, dest) => smallvec![*dest],
            PseudoOp::Ret(op) => {
                match op {
                    Some(Operand::Reg(reg)) => smallvec![*reg],
                    _ => smallvec![]
                }
            }
            PseudoOp::Phi(_, regs) => regs.clone().into()
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MachineOp<A: Abi> {
    Mov(Register<A>, Operand<A>),
    Sub(Register<A>, Operand<A>, Operand<A>),
    Add(Register<A>, Operand<A>, Operand<A>),
    Cmp(Register<A>, CmpOp, Operand<A>, Operand<A>),
    Br(BasicBlockId),
    CondBr(Operand<A>, BasicBlockId, BasicBlockId),
}

impl<A: Abi> MachineOp<A> {
    pub fn out(&self) -> Option<Register<A>> {
        match self {
            Self::Mov(dest, _) => Some(*dest),
            Self::Sub(dest, _, _) => Some(*dest),
            Self::Add(dest, _, _) => Some(*dest),
            Self::Cmp(dest, _, _, _) => Some(*dest),
            Self::Br(_) => None,
            Self::CondBr(_, _, _) => None,
        }
    }

    pub fn consumed_regs(&self) -> SmallVec<[Register<A>; 2]> {
        match self {
            MachineOp::Mov(_, src) => {
                match src {
                    Operand::Reg(reg) =>
                        smallvec![*reg],
                    _ =>
                        smallvec![]
                }
            }
            MachineOp::Sub(_, src, dest) => {
                match (src.try_as_register(), dest.try_as_register()) {
                    (Some(src), Some(dest)) => smallvec![src, dest],
                    (Some(src), None) => smallvec![src],
                    (None, Some(dest)) => smallvec![dest],
                    _ => smallvec![]
                }
            }
            MachineOp::Add(_, src, dest) => {
                match (src.try_as_register(), dest.try_as_register()) {
                    (Some(src), Some(dest)) => smallvec![src, dest],
                    (Some(src), None) => smallvec![src],
                    (None, Some(dest)) => smallvec![dest],
                    _ => smallvec![]
                }
            }
            MachineOp::Br(_) => smallvec![],
            MachineOp::Cmp(_, _, lhs, rhs) => {
                match (lhs.try_as_register(), rhs.try_as_register()) {
                    (Some(lhs), Some(rhs)) => smallvec![lhs, rhs],
                    (Some(lhs), None) => smallvec![lhs],
                    (None, Some(rhs)) => smallvec![rhs],
                    _ => smallvec![]
                }
            }
            MachineOp::CondBr(cond, _, _) => {
                match cond.try_as_register() {
                    Some(cond) => smallvec![cond],
                    None => smallvec![]
                }
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Operand<A: Abi> {
    Reg(Register<A>),
    Imm(Immediate),
}

impl<A: Abi> Operand<A> {
    pub fn try_as_register(&self) -> Option<Register<A>> {
        match self {
            Operand::Reg(reg) => Some(*reg),
            Operand::Imm(_) => None,
        }
    }
}

