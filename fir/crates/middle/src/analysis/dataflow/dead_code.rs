use rustc_hash::FxHashSet;

use crate::{Instr, InstrKind, Value};
use crate::analysis::dataflow::{ForwardAnalysis, ForwardAnalysisRunner, lattice};
use crate::cfg::TerminatorKind;
use crate::instruction::Op;

#[derive(Debug, Default, Clone)]
pub struct UsedValues(FxHashSet<Value>);

impl UsedValues {
    pub fn contains(&self, value: &Value) -> bool {
        self.0.contains(value)
    }
}

impl From<&[Value]> for UsedValues {
    fn from(values: &[Value]) -> Self {
        Self(FxHashSet::from_iter(values.iter().copied()))
    }
}

impl From<&[Op]> for UsedValues {
    fn from(ops: &[Op]) -> Self {
        Self(FxHashSet::from_iter(
            ops.iter().flat_map(|op| op.referenced_value())
        ))
    }
}

impl From<&[&Op]> for UsedValues {
    fn from(ops: &[&Op]) -> Self {
        Self(FxHashSet::from_iter(
            ops.iter().flat_map(|op| op.referenced_value())
        ))
    }
}

impl lattice::Value for UsedValues {
    fn join(&mut self, other: Self) -> bool {
        self.0.join(other.0)
    }
}


pub struct Analysis;

pub type AnalysisRunner<'a> = ForwardAnalysisRunner<'a, Analysis>;

impl ForwardAnalysis for Analysis {
    type V = UsedValues;

    fn eval_instr(instr: &Instr) -> Option<Self::V> {
        match &instr.kind {
            InstrKind::Alloca(_) => {}
            InstrKind::Store(store_instr) => {
                return Some(UsedValues::from([&store_instr.value].as_slice()));
            }
            InstrKind::Load(load_instr) => {
                return Some(UsedValues::from([load_instr.source].as_slice()));
            }
            InstrKind::Op(op_instr) => {
                return Some(UsedValues::from([&op_instr.op].as_slice()));
            }
            InstrKind::Sub(sub_instr) => {
                return Some(UsedValues::from([&sub_instr.lhs, &sub_instr.rhs].as_slice()));
            }
            InstrKind::ICmp(icmp_instr) => {
                return Some(UsedValues::from([&icmp_instr.op1, &icmp_instr.op2].as_slice()));
            }
        }
        None
    }

    fn eval_term(term: &TerminatorKind) -> Option<Self::V> {
        match term {
            TerminatorKind::Ret(ret_term) =>
                ret_term.value.as_ref().map(|ret_value| UsedValues::from([ret_value].as_slice())),
            TerminatorKind::Branch(br_term) => {
                Some(UsedValues::from(br_term.target.arguments.as_slice()))
            }
            TerminatorKind::CondBranch(condbr_term) => {
                // todo: maybe create used values directly from iterator
                let args = condbr_term.true_target.arguments.iter().chain(
                    condbr_term.false_target.arguments.iter()
                ).collect::<Vec<_>>();
                Some(UsedValues::from(args.as_slice()))
            }
        }
    }
}
