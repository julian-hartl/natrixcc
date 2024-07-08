use std::{fmt::Debug, hash::Hash};

use rustc_hash::{FxHashMap, FxHashSet};
use slotmap::SecondaryMap;

use lattice::Value;

use crate::{
    cfg::{BasicBlock, BasicBlockRef, Terminator},
    Instr,
};

mod backward;
pub mod concrete_value;
mod forward;
pub mod lattice;
pub mod use_def;

type InstrValue = crate::Value;

#[derive(Default)]
pub struct DFState<V>
where
    V: Clone,
{
    state: SecondaryMap<BasicBlockRef, V>,
}

impl<V> DFState<V>
where
    V: Value,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_mut(&mut self, bb: BasicBlockRef) -> &mut V {
        if !self.state.contains_key(bb) {
            self.state.insert(bb, V::default());
        }
        &mut self.state[bb]
    }

    pub fn get(&self, bb: BasicBlockRef) -> &V {
        &self.state[bb]
    }

    pub fn create(
        &mut self,
        bb: BasicBlockRef,
        join_partners: impl IntoIterator<Item = BasicBlockRef>,
    ) -> &mut V {
        for join_partner in join_partners {
            let pred_state = self.get(join_partner).clone();
            let entry = self.get_mut(bb);
            entry.join(pred_state);
        }
        self.get_mut(bb)
    }
}

pub trait InstrWalker<V: Value>: Sized {
    fn walk<H>(self, h: H)
    where
        H: FnMut(&mut Instr, &V);

    fn drain(self) {
        self.walk(|_, _| {});
    }
}

pub trait Analysis {
    type V: Value;

    fn analyse_instr(bb: &BasicBlock, instr: &Instr, v: &mut Self::V);

    fn analyse_term(bb: &BasicBlock, term: &Terminator, v: &mut Self::V);
}

pub type DFValueState<V> = FxHashMap<InstrValue, V>;

impl<K, V> Value for FxHashMap<K, V>
where
    K: Clone + Debug + Eq + Hash,
    V: Value,
{
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

impl<T> Value for FxHashSet<T>
where
    T: Clone + Debug + Eq + Hash,
{
    fn join(&mut self, other: Self) -> bool {
        let len_before = self.len();
        self.extend(other);
        self.len() != len_before
    }
}
