use rustc_hash::{
    FxHashMap,
    FxHashSet,
};

use crate::{
    analysis::dataflow::{
        forward::ForwardAnalysisRunner,
        lattice,
    },
    cfg::{
        BasicBlock,
        Terminator,
    },
    instruction::{
        Const,
        Op,
    },
    Instr,
    InstrKind,
    Value,
};

#[derive(Debug, Default, Clone)]
pub struct ConcreteValues {
    values: FxHashSet<Const>,
}

impl ConcreteValues {
    pub const fn new(values: FxHashSet<Const>) -> Self {
        Self { values }
    }

    pub fn from_single_value(value: Const) -> Self {
        let mut values = FxHashSet::default();
        values.insert(value);
        Self { values }
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

impl super::Analysis for Analysis {
    type V = FxHashMap<Value, ConcreteValues>;

    fn analyse_instr(bb: &BasicBlock, instr: &Instr, values: &mut Self::V) {
        if let InstrKind::Op(op_instr) = &instr.kind {
            if let Op::Const(const_val) = &op_instr.op {
                values.insert(
                    instr.value(),
                    ConcreteValues::from_single_value(const_val.clone()),
                );
            }
        };
    }

    fn analyse_term(bb: &BasicBlock, term: &Terminator, v: &mut Self::V) {
        // todo: add concrete branch args
    }
}
