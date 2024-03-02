use petgraph::algo::dominators::Dominators;
use petgraph::prelude::NodeIndex;

use super::{BasicBlockId, Cfg};

pub struct DomTree<'a> {
    dominators: Dominators<NodeIndex>,
    cfg: &'a Cfg
}
impl <'a> DomTree<'a> {
    pub fn compute(cfg: &'a Cfg) -> Self {
        Self {
            dominators: petgraph::algo::dominators::simple_fast(&cfg.graph, cfg.root_idx()),
            cfg
        }
    }

    pub fn idom(&self, basic_block: BasicBlockId) -> Option<BasicBlockId> {
        self.dominators.immediate_dominator(self.cfg.node_idx(basic_block)).map(|node_idx| self.cfg.bb_id_from_node(node_idx))
    }
    
    /// Returns true if `a` dominates `b`.
    /// 
    /// A basic block `a` dominates `b` if every path from the entry block to `b` must go through `a`.
    /// 
    /// **Note** that false is returned if `a` is not reachable from the entry block.
    pub fn dominates(&self, a: BasicBlockId, b: BasicBlockId) -> bool {
        let Some(dominators) = self.dominators.dominators(self.cfg.node_idx(a)) else { return false; };
        dominators.into_iter().find(|node_idx| *node_idx == self.cfg.node_idx(b)).is_some()
    }
}
