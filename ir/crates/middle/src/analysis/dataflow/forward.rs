use crate::{
    analysis::dataflow::{Analysis, DFState, InstrWalker},
    cfg::BasicBlockRef,
    Function, Instr,
};
use fxindexmap::FxIndexSet;

pub struct ForwardAnalysisRunner<'a, A: Analysis> {
    pub state: DFState<A::V>,
    visited: FxIndexSet<BasicBlockRef>,
    worklist: Vec<BasicBlockRef>,
    pub function: &'a mut Function,
    _analysis: std::marker::PhantomData<A>,
}

impl<'a, A: Analysis> ForwardAnalysisRunner<'a, A> {
    pub fn new(function: &'a mut Function) -> Self {
        Self {
            worklist: vec![function.cfg.entry_block_ref()],
            visited: FxIndexSet::default(),
            state: DFState::new(),
            function,
            _analysis: std::marker::PhantomData,
        }
    }

    pub fn next_bb(&mut self) -> Option<(BasicBlockRef, FAInstrWalker<A>)> {
        let bb_id = self.worklist.pop()?;
        let bb_state = self
            .state
            .create(bb_id, self.function.cfg.predecessors(bb_id));
        assert!(self.visited.insert(bb_id), "Block has already been visited");
        for successor in self.function.cfg.successors(bb_id) {
            let mut predecessors = self.function.cfg.predecessors(successor);
            let all_preds_visited =
                predecessors.all(|predecessor| self.visited.contains(&predecessor));
            if !all_preds_visited {
                continue;
            }
            self.worklist.push(successor);
        }
        Some((
            bb_id,
            FAInstrWalker {
                bb_ref: bb_id,
                function: self.function,
                bb_state,
            },
        ))
    }
}

pub struct FAInstrWalker<'a, 'b, A: Analysis> {
    bb_ref: BasicBlockRef,
    pub function: &'b mut Function,
    bb_state: &'a mut A::V,
}

impl<'a, 'b, A: Analysis> InstrWalker<A::V> for FAInstrWalker<'a, 'b, A> {
    fn walk<H>(self, mut h: H)
    where
        H: FnMut(&mut Instr, &A::V),
    {
        let bb = &self.function.cfg.basic_blocks[self.bb_ref];
        let instructions = bb.instructions.clone();
        for instr_ref in instructions {
            let instr = &mut self.function.cfg.instructions[instr_ref];
            h(instr, &*self.bb_state);
            A::analyse_instr(bb, instr, self.bb_state);
        }
        A::analyse_term(bb, bb.terminator(), self.bb_state);
    }
}
