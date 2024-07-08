use std::collections::VecDeque;

use indexmap::IndexSet;

use crate::{
    analysis::dataflow::{Analysis, DFState, InstrWalker},
    cfg::BasicBlockRef,
    Function, Instr,
};
pub struct BackwardAnalysisRunner<'a, A: Analysis> {
    pub state: DFState<A::V>,
    visited: IndexSet<BasicBlockRef>,
    worklist: VecDeque<BasicBlockRef>,
    pub function: &'a mut Function,
    _analysis: std::marker::PhantomData<A>,
}
impl<'a, A: Analysis> BackwardAnalysisRunner<'a, A> {
    pub fn new(function: &'a mut Function) -> Self {
        let worklist = function.cfg.dfs_postorder().collect();
        Self {
            worklist,
            visited: IndexSet::default(),
            state: DFState::new(),
            function,
            _analysis: std::marker::PhantomData,
        }
    }

    pub fn next_bb(&mut self) -> Option<(BasicBlockRef, BAInstrWalker<A>)> {
        let bb_id = self.worklist.pop_front()?;
        let bb_state = self
            .state
            .create(bb_id, self.function.cfg.successors(bb_id));
        assert!(self.visited.insert(bb_id), "Block has already been visited");
        for pred in self.function.cfg.predecessors(bb_id) {
            let mut succs = self.function.cfg.successors(pred);
            let all_succs_visited = succs.all(|succ| self.visited.contains(&succ));
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
        self.state.state[self.function.cfg.entry_block_ref()].clone()
    }
}

pub struct BAInstrWalker<'a, 'b, A: Analysis> {
    basic_block: BasicBlockRef,
    pub function: &'b mut Function,
    bb_state: &'a mut A::V,
}

impl<'a, 'b, A: Analysis> InstrWalker<A::V> for BAInstrWalker<'a, 'b, A> {
    fn walk<H>(self, mut h: H)
    where
        H: FnMut(&mut Instr, &A::V),
    {
        let bb = &mut self.function.cfg.basic_blocks[self.basic_block];
        A::analyse_term(bb, bb.terminator(), self.bb_state);
        for instr in bb.instructions.iter().copied().rev() {
            let instr = &mut self.function.cfg.instructions[instr];
            h(instr, &*self.bb_state);
            A::analyse_instr(bb, instr, self.bb_state);
        }
    }
}
