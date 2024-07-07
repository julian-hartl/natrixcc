use rustc_hash::FxHashMap;

use crate::{
    analysis::dataflow::{
        backward::BackwardAnalysisRunner,
        lattice,
    },
    cfg::{
        BasicBlock,
        BasicBlockRef,
        Terminator,
    },
    instruction::Op,
    Instr,
    Value,
};

#[derive(Debug, Default, Clone)]
pub struct Uses(FxHashMap<Value, Vec<BasicBlockRef>>);

impl Uses {
    pub fn register_use(&mut self, use_: Value, bb_ref: BasicBlockRef) {
        let uses = self.0.entry(use_).or_insert_with(Vec::new);
        uses.push(bb_ref);
    }

    pub fn is_used(&self, value: Value) -> bool {
        self.0.contains_key(&value)
    }
    /// Returns all defined, but unused values
    pub fn unused_values<'v>(
        &'v self,
        values: impl Iterator<Item = Value> + 'v,
    ) -> impl Iterator<Item = Value> + 'v {
        values.filter(|value| !self.is_used(*value))
    }
}

impl lattice::Value for Uses {
    fn join(&mut self, other: Self) -> bool {
        let mut changed = false;
        for (value, uses) in other.0.iter() {
            for use_ in uses.iter().copied() {
                self.register_use(*value, use_);
                changed = true;
            }
        }
        changed
    }
}

pub struct Analysis;

pub type AnalysisRunner<'a> = BackwardAnalysisRunner<'a, Analysis>;

impl super::Analysis for Analysis {
    type V = Uses;

    fn analyse_instr(bb: &BasicBlock, instr: &Instr, use_def: &mut Self::V) {
        for op in instr.used() {
            if let Op::Value(use_) = op {
                use_def.register_use(*use_, bb.id);
            }
        }
    }

    fn analyse_term(bb: &BasicBlock, term: &Terminator, use_def: &mut Self::V) {
        for used_value in term.used().into_iter().flat_map(|op| op.referenced_value()) {
            use_def.register_use(used_value, bb.id);
        }
    }
}
