use daggy::{
    petgraph::{
        prelude::{
            Bfs,
            DfsPostOrder,
            StableGraph,
        },
        Directed,
        Direction,
    },
    NodeIndex,
    Walker,
};
use index_vec::IndexVec;
use iter_tools::Itertools;
use rustc_hash::FxHashMap;

use crate::codegen::{
    machine::{
        Abi,
        Instr,
        instr::InstrOperand,
        Instr,
        InstrId,
    },
    register_allocator::{
        InstrNumbering,
        InstrUid,
        LivenessRepr,
        ProgPoint,
    },
};
use crate::codegen::machine::isa::Isa;

index_vec::define_index_type! {
    pub struct BasicBlockId = u32;

    DISPLAY_FORMAT = "bb{}";
}

#[derive(Debug, Clone)]
pub struct Cfg {
    entry_block: BasicBlockId,
    graph: StableGraph<(), (), Directed>,
    node_to_block_map: FxHashMap<NodeIndex, BasicBlockId>,
    block_to_node_map: FxHashMap<BasicBlockId, NodeIndex>,
}

impl Cfg {
    pub fn build<I: Isa>(bbs: &IndexVec<BasicBlockId, BasicBlock<I>>) -> Self {
        let mut cfg = Self::new(BasicBlockId::new(0));
        for bb_id in bbs.indices() {
            let node = cfg.graph.add_node(());
            cfg.node_to_block_map.insert(node, bb_id);
            cfg.block_to_node_map.insert(bb_id, node);
        }
        for (bb_id, bb) in bbs.iter_enumerated() {
            for instr in &bb.instructions {
                let ins = instr.operands();
                for operand in ins {
                    if let InstrOperand::Label(successor_id) = operand {
                        cfg.graph.add_edge(
                            *cfg.block_to_node_map
                                .get(&bb_id)
                                .expect("Block not found in block_to_node_map"),
                            *cfg.block_to_node_map
                                .get(&successor_id)
                                .expect("Block not found in block_to_node_map"),
                            (),
                        );
                    }
                }
            }
        }
        cfg
    }

    pub fn new(entry_block: BasicBlockId) -> Self {
        Self {
            entry_block,
            graph: StableGraph::new(),
            node_to_block_map: FxHashMap::default(),
            block_to_node_map: FxHashMap::default(),
        }
    }

    /// Traverses the cfg using a post order depth first traversal
    pub fn dfs_postorder(&self) -> impl Iterator<Item = BasicBlockId> + '_ {
        DfsPostOrder::new(&self.graph, self.entry_node())
            .iter(&self.graph)
            .map(|node| self.node_to_block_map[&node])
    }

    pub fn bfs(&self) -> impl Iterator<Item = BasicBlockId> + '_ {
        Bfs::new(&self.graph, self.entry_node())
            .iter(&self.graph)
            .map(|node| self.node_to_block_map[&node])
    }

    pub fn predecessors(&self, bb: BasicBlockId) -> impl Iterator<Item = BasicBlockId> + '_ {
        self.graph
            .neighbors_directed(self.block_to_node_map[&bb], Direction::Incoming)
            .map(|node| self.node_to_block_map[&node])
    }

    pub fn successors(&self, bb: BasicBlockId) -> impl Iterator<Item = BasicBlockId> + '_ {
        self.graph
            .neighbors(self.block_to_node_map[&bb])
            .map(|node| self.node_to_block_map[&node])
    }

    fn entry_node(&self) -> NodeIndex {
        self.node_to_block_map
            .iter()
            .find_map(|(node, bb)| {
                if *bb == self.entry_block {
                    return Some(*node);
                }
                None
            })
            .expect("Did not find matching entry in node_to_block_map for entry block")
    }

    /// Returns an ordering of basic block with the following guarantees:
    /// 1. All predecessors of a basic block are visited before the basic block itself (except if the bb is a predecessor of itself)
    pub fn ordered(&self) -> Vec<BasicBlockId> {
        // let mut visited = FxHashSet::default();
        let mut order = self.bfs().collect_vec();
        // let mut stack = VecDeque::new();
        // stack.push_back(self.entry_block);
        // while let Some(bb) = stack.pop_front() {
        //     debug!("Visiting basic block {}:{:?}", bb,visited);
        //     if visited.contains(&bb) {
        //         continue;
        //     }
        //     let mut all_preds_visited = true;
        //     for pred in self.predecessors(bb) {
        //         if !(pred == bb || visited.contains(&pred)) {
        //             debug!("Pred {} of {} has not been visited yet", pred, bb);
        //             stack.push_back(pred);
        //             all_preds_visited = false;
        //         }
        //     }
        //     if all_preds_visited {
        //         visited.insert(bb);
        //         order.push(bb);
        //         for succ in self.successors(bb) {
        //             stack.push_back(succ);
        //         }
        //     } else {
        //         stack.push_back(bb);
        //     }
        // }
        order
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock<I: Isa> {
    pub id: BasicBlockId,
    pub instructions: IndexVec<InstrId, Instr<I>>,
}

impl<I: Isa> BasicBlock<I> {
    pub fn new(id: BasicBlockId) -> Self {
        Self {
            id,
            instructions: IndexVec::default(),
        }
    }

    pub fn entry_pp(&self, liveness_repr: &LivenessRepr) -> ProgPoint {
        let instr_nr = liveness_repr
            .instr_numbering
            .get_instr_nr(InstrUid {
                bb: self.id,
                instr: 0.into(),
            })
            .unwrap();
        ProgPoint::Read(instr_nr)
    }

    pub fn exit_pp(&self, instr_numbering: &InstrNumbering) -> ProgPoint {
        let instr_nr = instr_numbering
            .get_instr_nr(InstrUid {
                bb: self.id,
                instr: self.instructions.len_idx() - 1,
            })
            .unwrap();
        ProgPoint::Write(instr_nr)
    }
}
