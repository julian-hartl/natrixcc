use std::fmt::{
    Display,
    Formatter,
};

use cranelift_entity::SecondaryMap;

use crate::{
    analysis::dataflow::{
        backward::BackwardAnalysisRunner,
        lattice,
    },
    cfg::{
        BasicBlockId,
        InstrId,
        Terminator,
    },
    instruction::Op,
    Instr,
    VReg,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct IRLocation(pub BasicBlockId, pub InstrId);

impl Display for IRLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.0, self.1)
    }
}

impl From<(BasicBlockId, InstrId)> for IRLocation {
    fn from((bb, instr): (BasicBlockId, InstrId)) -> Self {
        Self(bb, instr)
    }
}

impl From<&Instr> for IRLocation {
    fn from(instr: &Instr) -> Self {
        Self(instr.bb, instr.id)
    }
}

impl From<&Terminator> for IRLocation {
    fn from(term: &Terminator) -> Self {
        Self(term.bb, InstrId::from_usize_unchecked(InstrId::MAX_INDEX))
    }
}

#[derive(Debug, Default, Clone)]
pub struct UseDef(SecondaryMap<VReg, (Option<IRLocation>, Option<Vec<IRLocation>>)>);

impl UseDef {
    pub fn register_def(&mut self, def: VReg, instr_uid: IRLocation) -> Option<IRLocation> {
        self.0[def].0.replace(instr_uid)
    }

    pub fn register_use(&mut self, use_: VReg, instr_uid: IRLocation) {
        let uses = self.0[use_].1.get_or_insert_with(Vec::new);
        uses.push(instr_uid);
    }

    pub fn is_defined(&self, def: VReg) -> bool {
        self.0[def].0.is_some()
    }

    pub fn get_def(&self, def: VReg) -> Option<IRLocation> {
        self.0[def].0
    }

    /// Returns all defined, but unused registers
    pub fn unused_regs(&self) -> impl Iterator<Item = VReg> + '_ {
        self.0.iter().filter_map(|(vreg, (def, uses))| {
            def.and_then(|_| {
                let is_unused = uses.as_ref().map(|uses| uses.is_empty()).unwrap_or(true);
                if is_unused {
                    Some(vreg)
                } else {
                    None
                }
            })
        })
    }
}

impl lattice::Value for UseDef {
    fn join(&mut self, other: Self) -> bool {
        let mut changed = false;
        for (vreg, (def, uses)) in other.0.iter() {
            if let Some(def) = def {
                let old_def = self.register_def(vreg, *def);
                if old_def.is_none() || old_def != Some(*def) {
                    changed = true;
                }
            }
            for use_ in uses.iter().flatten().copied() {
                self.register_use(vreg, use_);
                changed = true;
            }
        }
        changed
    }
}

pub struct Analysis;

pub type AnalysisRunner<'a> = BackwardAnalysisRunner<'a, Analysis>;

impl super::Analysis for Analysis {
    type V = UseDef;

    fn analyse_instr(instr: &Instr, use_def: &mut Self::V) {
        let def = instr.defined_vreg();
        if let Some(def) = def {
            use_def.register_def(def, instr.into());
        }
        for op in instr.used() {
            if let Op::Vreg(use_) = op {
                use_def.register_use(*use_, instr.into());
            }
        }
    }

    fn analyse_term(term: &Terminator, use_def: &mut Self::V) {
        for used_vreg in term
            .used()
            .into_iter()
            .flat_map(|op| op.try_as_vreg_ref().copied())
        {
            use_def.register_use(used_vreg, term.into());
        }
    }
}
