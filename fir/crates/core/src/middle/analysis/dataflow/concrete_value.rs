use rustc_hash::FxHashSet;
use crate::middle::analysis::dataflow::{ForwardAnalysis, lattice};

use crate::middle::instruction::Const;

#[derive(Debug, Default, Clone)]
pub struct ConcreteValues {
    values: FxHashSet<Const>,
}

impl ConcreteValues {
    pub const fn new(values: FxHashSet<Const>) -> Self {
        Self {
            values
        }
    }

    pub fn from_single_value(value: Const) -> Self {
        let mut values = FxHashSet::default();
        values.insert(value);
        Self {
            values
        }
    }

    pub fn as_single_value(&self) -> Option<&Const> {
        if self.values.len() == 1 {
            self.values.iter().next()
        } else {
            None
        }
    }
}

impl lattice::Value for ConcreteValues {
    fn join(&mut self, other: Self) -> bool {
        let len_before = self.values.len();
        self.values.extend(other.values);
        self.values.len() != len_before
    }
}


pub type Analysis = ForwardAnalysis<ConcreteValues>;
