use std::fmt::Display;
use std::fs::OpenOptions;
use std::ops::{Deref, DerefMut};
use daggy::petgraph::dot::{Config, Dot};

use rustc_hash::FxHashMap;

pub use builder::Builder;

use crate::codegen::machine;
use crate::codegen::machine::{Instr, Pattern};
use crate::middle;
use crate::ty::Type;

pub mod builder;

type Dag<A> = daggy::Dag<Node<A>, Edge>;

#[derive( Debug)]
pub struct BasicBlockDAG<A: machine::Abi> {
    dag: Dag<A>,
    term_node: Option<daggy::NodeIndex>,
    bb: middle::cfg::BasicBlockId,
}

impl<A: machine::Abi> BasicBlockDAG<A> {

    pub fn new(bb: middle::cfg::BasicBlockId) -> Self {
        Self {
            dag: Dag::new(),
            term_node: None,
            bb
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
        use std::fs::File;
        std::fs::create_dir_all(&path)?;
        std::fs::write(format!("{}/{}.dot", path.as_ref().display(),self.bb), self.graphviz())
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

#[derive(Default, Debug)]
pub struct SelectionDAG<A: machine::Abi> {
    pub basic_blocks: FxHashMap<middle::cfg::BasicBlockId, BasicBlockDAG<A>>,
}

impl<A: machine::Abi> SelectionDAG<A> {
    pub fn get_bb_dag(&mut self, basic_block: middle::cfg::BasicBlockId) -> &mut BasicBlockDAG<A> {
        self.basic_blocks.entry(basic_block).or_insert_with(|| BasicBlockDAG::new(basic_block))
    }
}

#[derive(Debug, Clone)]
pub struct Node<A: machine::Abi> {
    pub kind: NodeKind<A>,
    pub ty: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum NodeKind<A: machine::Abi> {
    Op(Op),
    Const(Const),
    Reg(machine::Register<A>),
}

#[derive(Debug, Clone, EnumTryAs)]
pub enum Const {
    Int(i64),
}

#[derive(Debug, Clone)]
pub struct Edge;

impl Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Op {
    Mov,
    Sub,
    Ret,
}

