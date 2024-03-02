use std::fmt::Debug;
use std::hash::Hash;

use rustc_hash::{FxHashMap, FxHashSet};

use lattice::Value;

use crate::{Function, Instr};
use crate::cfg::{BasicBlockId, Predecessors, TerminatorKind};

pub mod lattice;
pub mod concrete_value;
pub mod dead_code;

type InstrValue = crate::Value;

#[derive(Default)]
pub struct DFState<V> {
    state: FxHashMap<BasicBlockId, V>,
}

impl<V> DFState<V> where V: Value {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_mut(&mut self, bb: BasicBlockId) -> &mut V {
        self.state.get_mut(&bb).unwrap()
    }

    pub fn get(&self, bb: BasicBlockId) -> &V {
        self.state.get(&bb).unwrap()
    }

    pub fn create(&mut self, bb: BasicBlockId, preds: Predecessors) -> &mut V {
        self.state.entry(bb).or_default();
        for pred in preds {
            let pred_state = self.state[&pred].clone();
            let entry = self.state.get_mut(&bb).unwrap();
            entry.join(pred_state);
        }
        self.state.get_mut(&bb).unwrap()
    }
}

pub struct ForwardAnalysisRunner<'a, A: ForwardAnalysis> {
    pub state: DFState<A::V>,
    visited: FxHashSet<BasicBlockId>,
    worklist: Vec<BasicBlockId>,
    pub function: &'a mut Function,
    _analysis: std::marker::PhantomData<A>,
}


impl<'a, A: ForwardAnalysis> ForwardAnalysisRunner<'a, A> {
    pub fn new(function: &'a mut Function) -> Self {
        Self {
            worklist: vec![function.cfg.entry_block()],
            visited: FxHashSet::default(),
            state: DFState::new(),
            function,
            _analysis: std::marker::PhantomData::default(),
        }
    }

    pub fn next_bb(&mut self) -> Option<(BasicBlockId, InstrWalker<A>)> {
        let bb_id = self.worklist.pop()?;
        let bb_state = self.state.create(bb_id, self.function.cfg.predecessors(bb_id));
        for successor in self.function.cfg.successors(bb_id) {
            let predecessors = self.function.cfg.predecessors(successor);
            let all_preds_visited = predecessors.iter().all(|predecessor| {
                self.visited.contains(predecessor)
            });
            if !all_preds_visited {
                continue;
            }
            self.worklist.push(successor);
        }
        assert!(!self.visited.insert(bb_id), "Block has already been visited");
        Some((bb_id, InstrWalker {
            basic_block: bb_id,
            function: &mut self.function,
            bb_state,
        }))
    }
}

// type InstrWalkerInnerIter<'a> = impl Iterator<Item=&'a mut Instr>;

pub struct InstrWalker<'a, 'b, A: ForwardAnalysis> {
    basic_block: BasicBlockId,
    pub function: &'b mut Function,
    bb_state: &'a mut A::V,
}

impl<'a, 'b, A: ForwardAnalysis> InstrWalker<'a, 'b, A> {
    pub fn walk<H>(self, mut h: H) where H: FnMut(&'b mut Instr, &A::V) {
        let bb = self.function.cfg.basic_block_mut(self.basic_block);
        for instr in &mut bb.instructions {
            if let Some(val) = A::eval_instr(instr) {
                self.bb_state.join(val);
            }
            h(instr, &*self.bb_state);
        }
    }

    pub fn drain(self) {
        self.walk(|_, _| {})
    }
}

pub trait ForwardAnalysis {
    type V: Value;

    fn eval_instr(instr: &Instr) -> Option<Self::V>;

    fn eval_term(term: &TerminatorKind) -> Option<Self::V>;
}

pub type DFValueState<V> = FxHashMap<InstrValue, V>;

impl<K, V> Value for FxHashMap<K, V> where K: Clone + Debug + Eq + Hash, V: Value {
    fn join(&mut self, other: Self) -> bool {
        let mut changed = false;
        for (key, val) in other {
            let entry = self.entry(key).or_default();
            if entry.join(val) {
                changed = true;
            }
        }
        changed
    }
}

impl<T> Value for FxHashSet<T> where T: Clone + Debug + Eq + Hash {
    fn join(&mut self, other: Self) -> bool {
        let len_before = self.len();
        self.extend(other);
        self.len() != len_before
    }
}


