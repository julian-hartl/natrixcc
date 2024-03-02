use rustc_hash::{FxHashMap, FxHashSet};
use crate::analysis::dataflow::{ForwardAnalysis, ForwardAnalysisRunner, lattice};
use crate::{Instr, InstrKind, Value};
use crate::cfg::TerminatorKind;

use crate::instruction::{Const, Op};

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


pub struct Analysis;
pub type AnalysisRunner<'a> = ForwardAnalysisRunner<'a, Analysis>;

impl ForwardAnalysis for Analysis {
    type V = FxHashMap<Value, ConcreteValues>;

    fn eval_instr(instr: &Instr) -> Option<Self::V> {
        if let InstrKind::Op(instr) = &instr.kind {
            if let Op::Const(const_val) = &instr.op {
                let mut map = FxHashMap::default();
                map.insert(instr.value, ConcreteValues::from_single_value(const_val.clone()));
                return Some(map);
            }
        };
        None
    }

    fn eval_term(_: &TerminatorKind) -> Option<Self::V> {
        None
    }
}
