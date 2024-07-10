use rand::prelude::{IteratorRandom, SliceRandom};
use rand::Rng;
use rustc_hash::FxHashMap;

use crate::cfg::{Cfg, RetTerm, Terminator, TerminatorKind};
use crate::instruction::const_op::Const;
use crate::instruction::{BinOpInstr, Op, OpInstr};
use crate::{InstrKind, Type, Value};

#[derive(Debug, Default)]
/// Generates random, but semantically correct, control flow graphs.
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
        let number_of_basic_blocks = rng.gen_range(self.options.number_of_basic_blocks.clone());
        let values_by_type = ValuesByType::default();
        for i in 0..number_of_basic_blocks {
            let number_of_instructions =
                rng.gen_range(self.options.number_of_instructions_per_block.clone());
            let bb_ref = self.cfg.new_basic_block(format!("bb{}", i));
            for i in 0..number_of_instructions {
                let blueprints = InstrBlueprint::all()
                    .into_iter()
                    .filter(|blueprint| blueprint.is_applicable(&values_by_type));
                let instr_kind = blueprints
                    .choose(&mut rng)
                    .unwrap()
                    .apply(&values_by_type, &mut rng);
                self.cfg.add_instruction(
                    bb_ref,
                    instr_kind.produced_ty(&self.cfg),
                    instr_kind,
                    format!("{}", i),
                );
            }

            self.cfg.basic_blocks[bb_ref].set_terminator(Terminator::new(
                TerminatorKind::Ret(RetTerm::empty()),
                bb_ref,
            ));
        }
        self.cfg
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
}

#[derive(Debug, Clone)]
pub struct Options {
    pub number_of_basic_blocks: std::ops::Range<usize>,
    pub number_of_instructions_per_block: std::ops::Range<usize>,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            number_of_basic_blocks: 1..15,
            number_of_instructions_per_block: 1..25,
        }
    }
}
