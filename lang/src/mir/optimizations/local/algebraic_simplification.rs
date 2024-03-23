use crate::mir::{BasicBlockIdx, Binop, FunctionIdx, InstructionKind, MIR, Type, Value};
use crate::mir::optimizations::local::LocalMIRPass;

pub struct AlgebraicSimplification;

impl AlgebraicSimplification {
    fn simplify_binary_instruction_one_side_known(
        &self,
        operator: Binop,
        op_ty: &Type,
        known_side: i32,
    ) -> Option<i32> {
        let computed = if known_side == 0 {
            match operator {
                Binop::Add | Binop::Sub | Binop::Or | Binop::Shl | Binop::Shr => Some(known_side),
                Binop::Mul | Binop::And => Some(0),
                Binop::Div | Binop::Mod => todo!("report division by zero"),
                Binop::Xor | Binop::Eq | Binop::Neq | Binop::Lt | Binop::Leq | Binop::Gt | Binop::Geq => None,
            }
        } else if known_side == 1 {
            match operator {
                Binop::Add => None,
                Binop::Sub => None,
                Binop::Mul => Some(known_side),
                Binop::Div => Some(known_side),
                Binop::Mod => Some(0),
                Binop::And => if matches!(op_ty,Type::Bool) { Some(known_side) } else { None },
                Binop::Or => if matches!(op_ty,Type::Bool) { Some(1) } else { None },
                Binop::Xor => None,
                Binop::Shl => None,
                Binop::Shr => None,
                Binop::Eq => None,
                Binop::Neq => None,
                Binop::Lt => None,
                Binop::Leq => None,
                Binop::Gt => None,
                Binop::Geq => None,
            }
        } else {
            None
        };
        computed
    }
}

impl LocalMIRPass for AlgebraicSimplification {
    fn run_on_basic_block(&mut self, mir: &mut MIR, function_idx: FunctionIdx, bb_idx: BasicBlockIdx) -> u32 {
        let mut changes = 0;
        let function = mir.functions.get_mut(function_idx);
        let bb = mir.basic_blocks.get_mut_or_panic(bb_idx);
        for instruction_idx in bb.instructions.iter().copied() {
            let instruction = function.instructions.get_mut(instruction_idx);
            match &mut instruction.kind {
                InstructionKind::Binary {
                    lhs,
                    rhs,
                    operator
                } => {
                    let lhs_int = lhs.as_i32();
                    let rhs_int = rhs.as_i32();
                    match (lhs_int, rhs_int) {
                        (None, Some(rhs_int)) => {
                            if let Some(result) = self.simplify_binary_instruction_one_side_known(*operator, &instruction.ty, rhs_int) {
                                instruction.kind = InstructionKind::Value(Value::ConstantInt(result));
                                changes += 1;
                            }
                        }
                        (Some(lhs_int), None) => {
                            if let Some(result) = self.simplify_binary_instruction_one_side_known(*operator, &instruction.ty, lhs_int) {
                                instruction.kind = InstructionKind::Value(Value::ConstantInt(result));
                                changes += 1;
                            }
                        }
                        _ => {}
                    }
                }
                InstructionKind::Unary { .. } => {}
                InstructionKind::Value(_) => {}
                InstructionKind::Call { .. } => {}
                InstructionKind::Phi(_) => {}
            };
        }
        changes
    }
}
