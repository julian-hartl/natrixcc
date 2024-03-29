use std::collections::VecDeque;

use cranelift_entity::EntitySet;

use crate::{
    analysis::dataflow::{
        Analysis,
        DFState,
        InstrWalker,
    },
    cfg::BasicBlockId,
    Function,
    Instr,
};
pub struct BackwardAnalysisRunner<'a, A: Analysis> {
    pub state: DFState<A::V>,
    visited: EntitySet<BasicBlockId>,
    worklist: VecDeque<BasicBlockId>,
    pub function: &'a mut Function,
    _analysis: std::marker::PhantomData<A>,
}
impl<'a, A: Analysis> BackwardAnalysisRunner<'a, A> {
    pub fn new(function: &'a mut Function) -> Self {
        let worklist = function.cfg.dfs_postorder().collect();
        Self {
            worklist,
            visited: EntitySet::default(),
            state: DFState::new(),
            function,
            _analysis: std::marker::PhantomData,
        }
    }

    pub fn next_bb(&mut self) -> Option<(BasicBlockId, BAInstrWalker<A>)> {
        let bb_id = self.worklist.pop_front()?;
        let bb_state = self
            .state
            .create(bb_id, self.function.cfg.successors(bb_id));
        assert!(self.visited.insert(bb_id), "Block has already been visited");
        for pred in self.function.cfg.predecessors(bb_id) {
            let mut succs = self.function.cfg.successors(pred);
            let all_succs_visited = succs.all(|succ| self.visited.contains(succ));
            assert!(all_succs_visited, "Not all successors have been visited");
            // if !all_succs_visited {
            //     continue;
            // }
            // self.worklist.push_back(pred);
        }
        Some((
            bb_id,
            BAInstrWalker {
                basic_block: bb_id,
                function: self.function,
                bb_state,
            },
        ))
    }

    pub fn collect(mut self) -> A::V {
        while let Some((_, walker)) = self.next_bb() {
            walker.drain();
        }
        self.state.state[self.function.cfg.entry_block()].clone()
    }
}

pub struct BAInstrWalker<'a, 'b, A: Analysis> {
    basic_block: BasicBlockId,
    pub function: &'b mut Function,
    bb_state: &'a mut A::V,
}

impl<'a, 'b, A: Analysis> InstrWalker<A::V> for BAInstrWalker<'a, 'b, A> {
    fn walk<H>(mut self, mut h: H)
    where
        H: FnMut(&mut Instr, &A::V),
    {
        let bb = self.function.cfg.basic_block_mut(self.basic_block);
        A::analyse_term(bb.terminator(), self.bb_state);
        for instr in bb.instructions_mut().rev() {
            h(instr, &*self.bb_state);
            A::analyse_instr(instr, self.bb_state);
        }
    }
}
