use petgraph::{
    algo::dominators::Dominators,
    prelude::NodeIndex,
};

use super::{
    BasicBlockId,
    Cfg,
};

pub struct DomTree<'a> {
    dominators: Dominators<NodeIndex>,
    cfg: &'a Cfg,
}
impl<'a> DomTree<'a> {
    pub fn compute(cfg: &'a Cfg) -> Self {
        Self {
            dominators: petgraph::algo::dominators::simple_fast(
                &cfg.graph,
                cfg.entry_block().into(),
            ),
            cfg,
        }
    }

    pub fn idom(&self, basic_block: BasicBlockId) -> Option<BasicBlockId> {
        self.dominators
            .immediate_dominator(basic_block.into())
            .map(|node_idx| node_idx.into())
    }

    /// Returns true if `a` dominates `b`.
    ///
    /// A basic block `a` dominates `b` if every path from the entry block to `b` must go through `a`.
    ///
    /// **Note** that false is returned if `a` is not reachable from the entry block.
    pub fn dominates(&self, a: BasicBlockId, b: BasicBlockId) -> bool {
        let Some(dominators) = self.dominators.dominators(b.into()) else {
            return false;
        };
        dominators.into_iter().any(|node_idx| node_idx == a.into())
    }
}
