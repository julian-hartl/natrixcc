use petgraph::{
    algo::dominators::Dominators,
    prelude::NodeIndex,
};

use super::{
    BasicBlockRef,
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
                cfg.basic_blocks[cfg.entry_block_ref()].node_index,
            ),
            cfg,
        }
    }

    pub fn idom(&self, basic_block: BasicBlockRef) -> Option<BasicBlockRef> {
        self.dominators
            .immediate_dominator(self.cfg.basic_blocks[basic_block].node_index)
            .map(|node_idx| self.cfg.graph[node_idx].bb_ref)
    }

    /// Returns true if `a` dominates `b`.
    ///
    /// A basic block `a` dominates `b` if every path from the entry block to `b` must go through `a`.
    ///
    /// **Note** that false is returned if `a` is not reachable from the entry block.
    pub fn dominates(&self, a: BasicBlockRef, b: BasicBlockRef) -> bool {
        let Some(dominators) = self
            .dominators
            .dominators(self.cfg.basic_blocks[b].node_index)
        else {
            return false;
        };
        dominators
            .into_iter()
            .any(|node_idx| node_idx == self.cfg.basic_blocks[a].node_index)
    }
}
