use std::{
    fmt::Display,
    ops::{
        Deref,
        DerefMut,
    },
};

use daggy::petgraph::dot::{
    Config,
    Dot,
};
use rustc_hash::FxHashMap;
use smallvec::{
    smallvec,
    SmallVec,
};

pub use builder::Builder;
use natrix_middle::{
    cfg::BasicBlockId,
    instruction::CmpOp,
};

use crate::codegen::{
    machine,
    machine::{
        Abi,
        backend::Pattern,
        isa::Instr,
        reg::{
            Register,
            VReg,
        },
        Size,
    },
};

pub mod builder;

type Dag<A> = daggy::Dag<Op<A>, Edge>;

#[derive(Debug, Clone)]
pub struct BasicBlockDAG<A: machine::Abi> {
    dag: Dag<A>,
    term_node: Option<daggy::NodeIndex>,
    bb: natrix_middle::cfg::BasicBlockId,
}

impl<A: machine::Abi> BasicBlockDAG<A> {
    pub fn new(bb: natrix_middle::cfg::BasicBlockId) -> Self {
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
        std::fs::write(
            format!("{}/{}.dot", path.as_ref().display(), self.bb),
            self.graphviz(),
        )
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
    pub basic_blocks: FxHashMap<natrix_middle::cfg::BasicBlockId, BasicBlockDAG<A>>,
}

impl<A: machine::Abi> SelectionDAG<A> {
    pub fn get_bb_dag(
        &mut self,
        basic_block: natrix_middle::cfg::BasicBlockId,
    ) -> &mut BasicBlockDAG<A> {
        self.basic_blocks
            .entry(basic_block)
            .or_insert_with(|| BasicBlockDAG::new(basic_block))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Immediate {
    value: [u8; 8],
    pub signed: bool,
    pub size: Size,
}

impl Immediate {
    pub fn as_encoded_dword(&self) -> Option<i32> {
        let (value, rest) = self.value.split_at(4);
        if rest != [0, 0, 0, 0] {
            return None;
        }
        Some(i32::from_le_bytes(value.try_into().unwrap()))
    }

    pub fn as_u64(&self) -> u64 {
        u64::from_le_bytes(self.value)
    }

    pub fn as_i64(&self) -> i64 {
        i64::from_le_bytes(self.value)
    }
}

impl Display for Immediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.signed {
            write!(f, "{}", self.as_i64())
        } else {
            write!(f, "{}", self.as_u64())
        }
    }
}

impl From<u8> for Immediate {
    fn from(value: u8) -> Self {
        let mut imm = Self::from(value as u64);
        imm.size = Size::Byte;
        imm
    }
}

impl From<i8> for Immediate {
    fn from(value: i8) -> Self {
        let mut imm = Self::from(value as i64);
        imm.size = Size::Byte;
        imm
    }
}

impl From<u16> for Immediate {
    fn from(value: u16) -> Self {
        let mut imm = Self::from(value as u64);
        imm.size = Size::Word;
        imm
    }
}

impl From<i16> for Immediate {
    fn from(value: i16) -> Self {
        let mut imm = Self::from(value as i64);
        imm.size = Size::Word;
        imm
    }
}

impl From<u32> for Immediate {
    fn from(value: u32) -> Self {
        let mut imm = Self::from(value as u64);
        imm.size = Size::DWord;
        imm
    }
}

impl From<i32> for Immediate {
    fn from(value: i32) -> Self {
        let mut imm = Self::from(value as i64);
        imm.size = Size::DWord;
        imm
    }
}

impl From<u64> for Immediate {
    fn from(value: u64) -> Self {
        let bytes = value.to_le_bytes();
        Self {
            value: bytes,
            signed: false,
            size: Size::QWord,
        }
    }
}

impl From<i64> for Immediate {
    fn from(value: i64) -> Self {
        let bytes = value.to_le_bytes();
        Self {
            value: bytes,
            signed: true,
            size: Size::QWord,
        }
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
    Def(VReg),
    Copy(Register<A>, Register<A>),
    Ret(Option<Operand<A>>),
    Phi(Register<A>, Vec<Register<A>>),
}

impl<A: Abi> PseudoOp<A> {
    pub fn out(&self) -> Option<Register<A>> {
        match self {
            Self::Copy(dest, _) => Some(*dest),
            Self::Ret(_) => None,
            Self::Phi(dest, _) => Some(*dest),
            Self::Def(dest) => Some(Register::Virtual(*dest)),
        }
    }

    pub fn consumed_regs(&self) -> SmallVec<[Register<A>; 2]> {
        match self {
            Self::Copy(_, dest) => smallvec![*dest],
            Self::Ret(op) => match op {
                Some(Operand::Reg(reg)) => smallvec![*reg],
                _ => smallvec![],
            },
            Self::Phi(_, regs) => regs.clone().into(),
            Self::Def(_) => smallvec![],
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
            MachineOp::Mov(_, src) => match src {
                Operand::Reg(reg) => smallvec![*reg],
                _ => smallvec![],
            },
            MachineOp::Sub(_, src, dest) => match (src.try_as_register(), dest.try_as_register()) {
                (Some(src), Some(dest)) => smallvec![src, dest],
                (Some(src), None) => smallvec![src],
                (None, Some(dest)) => smallvec![dest],
                _ => smallvec![],
            },
            MachineOp::Add(_, src, dest) => match (src.try_as_register(), dest.try_as_register()) {
                (Some(src), Some(dest)) => smallvec![src, dest],
                (Some(src), None) => smallvec![src],
                (None, Some(dest)) => smallvec![dest],
                _ => smallvec![],
            },
            MachineOp::Br(_) => smallvec![],
            MachineOp::Cmp(_, _, lhs, rhs) => {
                match (lhs.try_as_register(), rhs.try_as_register()) {
                    (Some(lhs), Some(rhs)) => smallvec![lhs, rhs],
                    (Some(lhs), None) => smallvec![lhs],
                    (None, Some(rhs)) => smallvec![rhs],
                    _ => smallvec![],
                }
            }
            MachineOp::CondBr(cond, _, _) => match cond.try_as_register() {
                Some(cond) => smallvec![cond],
                None => smallvec![],
            },
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
