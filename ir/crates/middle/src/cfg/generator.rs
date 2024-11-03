use itertools::Itertools;
use rand::distributions::WeightedIndex;
use rand::prelude::{Distribution, SliceRandom};
use rand::Rng;
use rustc_hash::FxHashMap;

use crate::cfg::{
    BasicBlockRef, BranchTarget, BranchTerm, Cfg, CondBranchTerm, RetTerm, Terminator,
    TerminatorKind,
};
use crate::instruction::const_op::Const;
use crate::instruction::{BinOpInstr, Op, OpInstr};
use crate::{InstrKind, Type, Value};

/// Generates random, but semantically correct control flow graphs.
#[derive(Debug, Default)]
pub struct Generator {
    cfg: Cfg,
    options: Options,
}

impl Generator {
    pub fn new(options: Options) -> Self {
        Self {
            cfg: Cfg::new(),
            options,
        }
    }
    pub fn generate(mut self) -> Cfg {
        let mut rng = rand::thread_rng();
        let number_of_basic_blocks = rng.gen_range(self.options.number_of_basic_blocks);
        let values_by_type = ValuesByType::default();
        for i in 0..number_of_basic_blocks {
            self.cfg.new_basic_block(format!("bb{}", i));
        }
        for bb_ref in self.cfg.basic_blocks.keys().collect_vec() {
            let number_of_instructions =
                rng.gen_range(self.options.number_of_instructions_per_block.clone());
            for i in 0..number_of_instructions {
                let blueprints = InstrBlueprint::all()
                    .into_iter()
                    .filter(|blueprint| blueprint.is_applicable(&values_by_type))
                    .collect_vec();
                let dist = InstrBlueprint::weighted_index(&blueprints);
                let instr_kind = blueprints[dist.sample(&mut rng)].apply(&values_by_type, &mut rng);
                self.cfg.add_instruction(
                    bb_ref,
                    instr_kind.produced_ty(&self.cfg),
                    instr_kind,
                    format!("{}", i),
                );
            }

            let term_blueprints = TermBlueprint::all()
                .into_iter()
                .filter(|blueprint| {
                    blueprint
                        .is_applicable(&values_by_type, &self.cfg.basic_blocks.keys().collect_vec())
                })
                .collect_vec();
            let term_kind = term_blueprints
                [TermBlueprint::weighted_index(&term_blueprints).sample(&mut rng)]
            .apply(
                &values_by_type,
                &self.cfg.basic_blocks.keys().collect_vec(),
                &mut rng,
            );
            self.cfg.basic_blocks[bb_ref].set_terminator(Terminator::new(term_kind, bb_ref));
        }
        self.cfg
    }
}

#[derive(Debug, Clone)]
pub struct Options {
    pub number_of_basic_blocks: std::ops::Range<u32>,
    pub number_of_instructions_per_block: std::ops::Range<u32>,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            number_of_basic_blocks: 1..15,
            number_of_instructions_per_block: 1..25,
        }
    }
}

type ValuesByType = FxHashMap<Type, Vec<Value>>;

#[derive(Debug, Clone, Eq, PartialEq)]
enum InstrBlueprint {
    // Alloca,
    // Store,
    // Load,
    OpV(Type),
    OpC(Type),
    // Sub(OpBlueprint, OpBlueprint),
    AddCC(Type, Type),
    AddCV(Type, Type),
    AddVC(Type, Type),
    AddVV(Type, Type),
    // Cmp,
}

impl InstrBlueprint {
    fn all() -> Vec<Self> {
        let mut all = Vec::new();

        for ty in &Type::INTEGERS
        // .iter().chain(&[Type::Bool])
        {
            all.push(Self::OpC(ty.clone()));
            all.push(Self::OpV(ty.clone()));
        }

        for ty in &Type::INTEGERS {
            all.push(Self::AddCC(ty.clone(), ty.clone()));
            all.push(Self::AddCV(ty.clone(), ty.clone()));
            all.push(Self::AddVC(ty.clone(), ty.clone()));
            all.push(Self::AddVV(ty.clone(), ty.clone()));
        }

        all
    }

    fn is_applicable(&self, values: &ValuesByType) -> bool {
        match self {
            InstrBlueprint::OpV(ty) => values.contains_key(ty),
            InstrBlueprint::OpC(_) => true,
            InstrBlueprint::AddCC(_, _) => true,
            InstrBlueprint::AddCV(_, ty) | InstrBlueprint::AddVC(ty, _) => values.contains_key(ty),
            InstrBlueprint::AddVV(ty1, ty2) => {
                assert_eq!(ty1, ty2);
                values.get(ty1).map_or(false, |v| v.len() >= 2)
            }
        }
    }

    fn apply<R: Rng>(&self, values: &ValuesByType, rng: &mut R) -> InstrKind {
        match self {
            InstrBlueprint::OpV(ty) => {
                let value = values[ty].choose(rng).copied().unwrap();
                InstrKind::Op(OpInstr {
                    op: Op::Value(value),
                })
            }
            InstrBlueprint::OpC(ty) => InstrKind::Op(OpInstr {
                op: Op::Const(Const::random(ty.clone(), rng).unwrap()),
            }),
            InstrBlueprint::AddCC(ty1, ty2) => InstrKind::Add(BinOpInstr {
                lhs: Op::Const(Const::random(ty1.clone(), rng).unwrap()),
                rhs: Op::Const(Const::random(ty2.clone(), rng).unwrap()),
            }),
            InstrBlueprint::AddCV(ty1, ty2) => {
                let value = values[ty2].choose(rng).copied().unwrap();
                InstrKind::Add(BinOpInstr {
                    lhs: Op::Const(Const::random(ty1.clone(), rng).unwrap()),
                    rhs: Op::Value(value),
                })
            }
            InstrBlueprint::AddVC(ty1, ty2) => {
                let value = values[ty1].choose(rng).copied().unwrap();
                InstrKind::Add(BinOpInstr {
                    lhs: Op::Value(value),
                    rhs: Op::Const(Const::random(ty2.clone(), rng).unwrap()),
                })
            }
            InstrBlueprint::AddVV(ty1, ty2) => {
                let value1 = values[ty1].choose(rng).copied().unwrap();
                let value2 = values[ty2].choose(rng).copied().unwrap();
                InstrKind::Add(BinOpInstr {
                    lhs: Op::Value(value1),
                    rhs: Op::Value(value2),
                })
            }
        }
    }

    fn weighted_index(blueprints: &[Self]) -> WeightedIndex<usize> {
        let weigh_type = |ty: &Type| -> usize {
            match ty {
                Type::U8 => 5,
                Type::U16 => 3,
                Type::U32 => 8,
                Type::U64 => 6,
                Type::I8 => 2,
                Type::I16 => 2,
                Type::I32 => 6,
                Type::I64 => 5,
                Type::Bool => 5,
                Type::Void => 2,
                Type::Ptr(_) => 0,
            }
        };
        let weights = blueprints.iter().map(|blueprint| match blueprint {
            Self::OpV(ty) => weigh_type(ty) + 1,
            Self::OpC(ty) => weigh_type(ty) + 2,
            Self::AddCC(ty1, ty2) => (weigh_type(ty1) + weigh_type(ty2)).div_ceil(2),
            Self::AddCV(ty1, ty2) | Self::AddVC(ty1, ty2) => {
                (weigh_type(ty1) + weigh_type(ty2)).div_ceil(2) + 1
            }
            Self::AddVV(ty1, ty2) => (weigh_type(ty1) + weigh_type(ty2)).div_ceil(2) + 2,
        });
        WeightedIndex::new(weights).unwrap()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum TermBlueprint {
    RetV,
    RetC,
    CondBrV,
    CondBrC,
    Br,
}

impl TermBlueprint {
    fn all() -> Vec<Self> {
        let mut all = Vec::new();

        all.push(Self::RetC);
        all.push(Self::RetV);

        all.push(Self::CondBrV);
        all.push(Self::CondBrC);

        all.push(Self::Br);

        all
    }

    fn is_applicable(&self, values: &ValuesByType, basic_blocks: &[BasicBlockRef]) -> bool {
        match self {
            TermBlueprint::RetV => true,
            TermBlueprint::RetC => true,
            TermBlueprint::CondBrV => basic_blocks.len() >= 2 && values.contains_key(&Type::Bool),
            TermBlueprint::CondBrC => basic_blocks.len() >= 2 && values.contains_key(&Type::Bool),
            TermBlueprint::Br => basic_blocks.len() >= 1,
        }
    }

    fn apply<R: Rng>(
        &self,
        values: &ValuesByType,
        basic_blocks: &[BasicBlockRef],
        rng: &mut R,
    ) -> TerminatorKind {
        match self {
            Self::RetV => TerminatorKind::Ret(RetTerm::empty()),
            Self::RetC => TerminatorKind::Ret(RetTerm::empty()),
            Self::CondBrV => {
                let cond = values[&Type::Bool].choose(rng).copied().unwrap();
                let true_target = basic_blocks.choose(rng).copied().unwrap();
                let false_target = basic_blocks.choose(rng).copied().unwrap();
                TerminatorKind::CondBranch(CondBranchTerm::new(
                    Op::Value(cond),
                    BranchTarget::no_args(true_target),
                    BranchTarget::no_args(false_target),
                ))
            }
            Self::CondBrC => {
                let cond = Op::Const(Const::Bool(rng.gen()));
                let true_target = basic_blocks.choose(rng).copied().unwrap();
                let false_target = basic_blocks.choose(rng).copied().unwrap();
                TerminatorKind::CondBranch(CondBranchTerm::new(
                    cond,
                    BranchTarget::no_args(true_target),
                    BranchTarget::no_args(false_target),
                ))
            }
            Self::Br => {
                let target = basic_blocks.choose(rng).copied().unwrap();
                TerminatorKind::Branch(BranchTerm::new(BranchTarget::no_args(target)))
            }
        }
    }

    fn weighted_index(blueprints: &[Self]) -> WeightedIndex<usize> {
        let weights = blueprints.iter().map(|blueprint| match blueprint {
            Self::RetV => 2,
            Self::RetC => 1,
            Self::CondBrV => 5,
            Self::CondBrC => 3,
            Self::Br => 4,
        });
        WeightedIndex::new(weights).unwrap()
    }
}
