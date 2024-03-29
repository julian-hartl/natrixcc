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

pub struct ForwardAnalysisRunner<'a, A: Analysis> {
    pub state: DFState<A::V>,
    visited: EntitySet<BasicBlockId>,
    worklist: Vec<BasicBlockId>,
    pub function: &'a mut Function,
    _analysis: std::marker::PhantomData<A>,
}

impl<'a, A: Analysis> ForwardAnalysisRunner<'a, A> {
    pub fn new(function: &'a mut Function) -> Self {
        Self {
            worklist: vec![function.cfg.entry_block()],
            visited: EntitySet::default(),
            state: DFState::new(),
            function,
            _analysis: std::marker::PhantomData,
        }
    }

    pub fn next_bb(&mut self) -> Option<(BasicBlockId, FAInstrWalker<A>)> {
        let bb_id = self.worklist.pop()?;
        let bb_state = self
            .state
            .create(bb_id, self.function.cfg.predecessors(bb_id));
        assert!(self.visited.insert(bb_id), "Block has already been visited");
        for successor in self.function.cfg.successors(bb_id) {
            let mut predecessors = self.function.cfg.predecessors(successor);
            let all_preds_visited =
                predecessors.all(|predecessor| self.visited.contains(predecessor));
            if !all_preds_visited {
                continue;
            }
            self.worklist.push(successor);
        }
        Some((
            bb_id,
            FAInstrWalker {
                basic_block: bb_id,
                function: self.function,
                bb_state,
            },
        ))
    }
}

pub struct FAInstrWalker<'a, 'b, A: Analysis> {
    basic_block: BasicBlockId,
    pub function: &'b mut Function,
    bb_state: &'a mut A::V,
}

impl<'a, 'b, A: Analysis> InstrWalker<A::V> for FAInstrWalker<'a, 'b, A> {
    fn walk<H>(mut self, mut h: H)
    where
        H: FnMut(&mut Instr, &A::V),
    {
        let bb = self.function.cfg.basic_block_mut(self.basic_block);
        for instr in bb.instructions_mut() {
            h(instr, &*self.bb_state);
            A::analyse_instr(instr, &mut self.bb_state);
        }
        A::analyse_term(&bb.terminator(), &mut self.bb_state);
    }
}
