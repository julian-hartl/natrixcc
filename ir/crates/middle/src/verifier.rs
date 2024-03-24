use std::fmt::{Display, Formatter};

use cranelift_entity::SecondaryMap;

use crate::{Function, Type, VReg};
use crate::analysis::dataflow::use_def::IRLocation;
use crate::cfg::{BasicBlockId, InstrId};
use crate::instruction::Op;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VerifyError {
    DefinedMoreThanOnce(VReg, IRLocation),
    UsedBeforeDefinition(VReg, IRLocation),
    UseNotDominatedByDefinition(VReg, IRLocation),
    MissingTerminator(BasicBlockId),
    TypeMismatch {
        expected: Type,
        actual: Type,
        location: IRLocation,
    },
}

impl Display for VerifyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DefinedMoreThanOnce(vreg, _) => write!(f, "SSA form only allows one definition, but {vreg} has more"),
            Self::UsedBeforeDefinition(vreg, _) => write!(f, "{vreg} used before definition"),
            Self::UseNotDominatedByDefinition(vreg, _) => write!(f, "Definition of {vreg} does not dominate all uses"),
            Self::MissingTerminator(bb_id) => write!(f, "Missing terminator in {bb_id}"),
            Self::TypeMismatch {
                actual,
                expected,
                ..
            } =>
                write!(f, "Expected type {expected}, but got {actual}")
        }
    }
}

#[derive(Debug, Clone)]
pub struct Verifier<'func> {
    function: &'func Function,
}

impl<'func> Verifier<'func> {
    pub fn new(function: &'func Function) -> Self {
        Self {
            function
        }
    }

    pub fn verify(self) -> Vec<VerifyError> {
        let mut errors = vec![];
        let mut definitions = SecondaryMap::new();
        for (bb_id, bb) in self.function.cfg.basic_blocks() {
            if !bb.has_terminator() {
                errors.push(VerifyError::MissingTerminator(bb_id));
            }
            for (instr_id, instr) in bb.instructions_indexed() {
                if let Some(vreg) = instr.defined_vreg() {
                    let def = definitions[vreg];
                    match def {
                        None => {
                            // report error
                            definitions[vreg] = Some((bb_id, instr_id));
                        }
                        Some(_) => {
                            errors.push(VerifyError::DefinedMoreThanOnce(vreg, instr.into()));
                        }
                    }
                }

                // todo: extra case for cmp & load instructions
                let expected = &instr.ty;
                for actual in instr.used().into_iter().flat_map(
                    |op| match op {
                        Op::Const(const_val) => Some(const_val.ty()),
                        Op::Vreg(vreg) => {
                            self.function.cfg.vregs[*vreg].as_ref().map(
                                |vreg| &vreg.ty
                            )
                        }
                    }
                ) {
                    if expected != actual {
                        errors.push(VerifyError::TypeMismatch {
                            actual: actual.clone(),
                            expected: expected.clone(),
                            location: instr.into(),
                        })
                    }
                }
            }
        }
        let domtree = self.function.cfg.dom_tree();
        for (used_in_bb, bb) in self.function.cfg.basic_blocks() {
            for (instr_id, used) in bb.instructions_indexed().map(
                |(instr_id, instr)| (instr_id, instr.used())
            ).chain(
                bb.terminator.as_ref().map(|terminator| std::iter::once((InstrId::TERMINATOR, terminator.used()))).into_iter().flatten()
            ) {
                for used in used {
                    if let Op::Vreg(vreg) = used {
                        let vreg = *vreg;
                        match definitions[vreg] {
                            None => {
                                errors.push(VerifyError::UsedBeforeDefinition(vreg, IRLocation(used_in_bb, instr_id)));
                            }
                            Some((defined_in_bb, defined_at_instr_id)) => {
                                if defined_in_bb == used_in_bb {
                                    let is_used_before_def = instr_id <= defined_at_instr_id;
                                    if is_used_before_def {
                                        errors.push(VerifyError::UsedBeforeDefinition(vreg, IRLocation(used_in_bb, instr_id)));
                                    }
                                } else if !domtree.dominates(defined_in_bb, used_in_bb) {
                                    errors.push(VerifyError::UseNotDominatedByDefinition(vreg, IRLocation(used_in_bb, instr_id)));
                                }
                            }
                        }
                    }
                }
            }
        }
        errors
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::BasicBlockId;
    use crate::test::create_test_module_from_source;

    use super::*;

    #[test]
    fn should_report_use_before_def_in_same_basic_block() {
        let errors = verify_function(
            "
        fun i32 @test() {
        bb0:
            v0 = i32 9;
            v1 = add i32 v0, v1;
            ret i32 v1;
        }
       "
        );

        assert_eq!(errors, vec![
            VerifyError::UsedBeforeDefinition(VReg::from_u32(1), IRLocation(BasicBlockId::from_u32(0), InstrId::from_raw(1))),
        ])
    }

    #[test]
    fn should_report_use_before_def_with_multiple_basic_blocks() {
        let errors = verify_function(
            "
        fun i32 @test() {
        bb0:
            v0 = i32 10;
            br bb1;
        bb1:
            v2 = add i32 v0, v1;
            ret i32 v2;
        }
       "
        );

        assert_eq!(errors, vec![
            VerifyError::UsedBeforeDefinition(VReg::from_u32(1), IRLocation(BasicBlockId::from_u32(1), InstrId::from_raw(0)),
            )
        ])
    }

    #[test]
    fn should_report_multiple_definitions_in_same_basic_block() {
        let errors = verify_function(
            "
        fun i32 @test() {
        bb0:
            v0 = i32 9;
            v0 = add i32 v0, 10;
            ret i32 v0;
        }
       "
        );

        assert_eq!(errors, vec![
            VerifyError::DefinedMoreThanOnce(VReg::from_u32(0),
                                             IRLocation(BasicBlockId::from_u32(0), InstrId::from_raw(1)),
            )
        ])
    }

    #[test]
    fn should_report_multiple_use_before_def_with_multiple_uses() {
        let errors = verify_function(
            "
        fun i32 @test() {
        bb0:
            v0 = i32 9;
            v1 = add i32 v0, v1;
            v2 = add i32 v0, v1;
            ret i32 v2;
        }
       "
        );

        assert_eq!(errors, vec![
            VerifyError::UsedBeforeDefinition(VReg::from_u32(1),
                                              IRLocation(BasicBlockId::from_u32(0), InstrId::from_raw(1)),
            )
        ])
    }

    #[test]
    fn should_report_definition_does_not_dominate_use() {
        let errors = verify_function(
            "
        fun i32 @test() {
        bb0:
            br bb2;
        bb1:
            v0 = i32 20;
            br bb2;
        bb2:
            v1 = i32 v0;
            ret i32 v0;
        }
       "
        );

        assert_eq!(errors, vec![
            VerifyError::UseNotDominatedByDefinition(VReg::from_u32(0),
                                                     IRLocation(BasicBlockId::from_u32(2), InstrId::from_raw(0)),
            ),
            VerifyError::UseNotDominatedByDefinition(VReg::from_u32(0),
                                                     IRLocation(BasicBlockId::from_u32(2), InstrId::TERMINATOR),
            ),
        ])
    }

    #[test]
    fn should_report_missing_terminator() {
        let errors = verify_function(
            "
        fun i32 @test() {
        bb0:
        }
       "
        );

        assert_eq!(errors, vec![
            VerifyError::MissingTerminator(BasicBlockId::from_u32(0))
        ])
    }

    #[test]
    fn should_report_type_mismatch_on_add_instruction() {
        let errors = verify_function(
            "
        fun i32 @test() {
        bb0:
            v0 = i16 0;
            v1 = add i32 v0, 8;
            ret i32 v1;
        }
       "
        );

        assert_eq!(errors, vec![
            VerifyError::TypeMismatch {
                actual: Type::I16,
                expected: Type::I32,
                location: IRLocation(BasicBlockId::from_u32(0), InstrId::from_raw(1)),
            }
        ])
    }

    #[test]
    fn should_report_type_mismatch_on_cmp_instruction() {
        let errors = verify_function(
            "
        fun i32 @test() {
        bb0:
            v0 = icmp eq i32 0, 1;
            ret i32 v1;
        }
       "
        );

        assert_eq!(errors, vec![
            VerifyError::TypeMismatch {
                actual: Type::I16,
                expected: Type::I32,
                location: IRLocation(BasicBlockId::from_u32(0), InstrId::from_raw(1)),
            }
        ])
    }

    /// Returns the errors from verification of a function named "test" declared in the given source
    fn verify_function(src: &str) -> Vec<VerifyError> {
        let module = create_test_module_from_source(src);
        let func = module.find_function_by_name("test").expect("You did not provide a function called 'test' in src");
        let verifier = Verifier::new(func);
        verifier.verify()
    }
}
