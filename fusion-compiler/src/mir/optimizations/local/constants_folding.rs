use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

use crate::mir::{BasicBlockIdx, Binop, FunctionIdx, Instruction, InstructionIdx, InstructionKind, MIR, TerminatorKind, Unop, Value};
use crate::mir::optimizations::local::LocalMIRPass;

struct ComputedConstantValues(HashMap<InstructionIdx, Value>);

impl ComputedConstantValues {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Returns true if the value is a constant or if it is a reference to an instruction that has a constant value
    fn is_value_constant(&self, value: &Value) -> bool {
        value.is_constant() || {
            let instruction_idx = value.as_instruction_ref();
            match instruction_idx.as_ref() {
                None => false,
                Some(idx) => self.get(idx).is_some()
            }
        }
    }

    fn get_as_constant_integer(&self, value: &Value) -> Option<i64> {
        let as_constant_value = self.get_as_constant_value(value)?;
        as_constant_value.as_i64()
    }

    fn get_as_constant_value(&self, value: &Value) -> Option<Value> {
        match value {
            Value::ConstantInt(value) => Some(Value::ConstantInt(*value)),
            Value::InstructionRef(idx) => {
                match self.get(idx) {
                    None => None,
                    Some(value) => self.get_as_constant_value(value)
                }
            }
            Value::Void => Some(Value::Void),
            Value::ParameterRef(_) => None,
        }
    }
}

impl Deref for ComputedConstantValues {
    type Target = HashMap<InstructionIdx, Value>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ComputedConstantValues {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub struct ConstantFolding;


impl LocalMIRPass for ConstantFolding {
    fn run_on_basic_block(&mut self, mir: &mut MIR, function_idx: FunctionIdx, bb_idx: BasicBlockIdx) -> u32 {
        let mut changes = 0;
        let function = mir.functions.get_mut(function_idx);
        let bb = mir.basic_blocks.get_mut_or_panic(bb_idx);
        let mut constant_values = ComputedConstantValues::new();
        for instruction_idx in bb.instructions.iter().copied() {
            let instruction = function.instructions.get_mut(instruction_idx);
            match &mut instruction.kind {
                InstructionKind::Binary {
                    lhs,
                    rhs,
                    operator
                } => {
                    let lhs_int = constant_values.get_as_constant_integer(lhs);
                    let rhs_int = constant_values.get_as_constant_integer(rhs);
                    if let
                        (Some(lhs_int), Some(rhs_int)) = (lhs_int, rhs_int) {
                        let result = match operator {
                            // todo: replace with safe arithmetic
                            Binop::Add => lhs_int + rhs_int,
                            Binop::Sub => lhs_int - rhs_int,
                            Binop::Mul => lhs_int * rhs_int,
                            Binop::Div => lhs_int / rhs_int,
                            Binop::Mod => lhs_int % rhs_int,
                            Binop::And => lhs_int & rhs_int,
                            Binop::Or => lhs_int | rhs_int,
                            Binop::Xor => lhs_int ^ rhs_int,
                            Binop::Shl => lhs_int << rhs_int,
                            Binop::Shr => lhs_int >> rhs_int,
                            Binop::Eq => (lhs_int == rhs_int) as i64,
                            Binop::Neq => (lhs_int != rhs_int) as i64,
                            Binop::Lt => (lhs_int < rhs_int) as i64,
                            Binop::Leq => (lhs_int <= rhs_int) as i64,
                            Binop::Gt => (lhs_int > rhs_int) as i64,
                            Binop::Geq => (lhs_int >= rhs_int) as i64,
                        };
                        let value = Value::ConstantInt(result);
                        constant_values.insert(instruction_idx, value.clone());
                        changes += 1;
                        *instruction = Instruction::new(InstructionKind::Value(value), instruction.ty);
                    }
                }
                InstructionKind::Unary { operator, operand } => {
                    let operand_int = constant_values.get_as_constant_integer(operand);
                    if let Some(operand_int) = operand_int {
                        let result = match operator {
                            Unop::Neg => -operand_int,
                            Unop::Not => !operand_int,
                        };
                        let value = Value::ConstantInt(result);
                        constant_values.insert(instruction_idx, value.clone());
                        changes += 1;
                        *instruction = Instruction::new(InstructionKind::Value(value), instruction.ty);
                    }
                }
                InstructionKind::Value(value) => {
                    if constant_values.is_value_constant(value) {
                        constant_values.insert(instruction_idx, value.clone());
                    }
                }
                InstructionKind::Call { arguments, .. } => {
                    for argument in arguments.iter_mut() {
                        if let Some(value) = constant_values.get_as_constant_value(argument) {
                            if argument.replace_if_not_equal(value.clone()) {
                                changes += 1;
                            }
                        }
                    }
                }
                InstructionKind::Phi(phi) => {
                    // for phi_value in phi.iter_mut() {
                    //     if let Some(value) = constant_values.get_as_constant_value(phi_value) {
                    //         if phi_value.replace_if_not_equal(value) {
                    //             changes += 1;
                    //         }
                    //     }
                    // }
                }
            };
        }
        if let Some(terminator) = bb.terminator.as_mut() {
            match &mut terminator.kind {
                TerminatorKind::Return { value } => {
                    if let Some(constant_value) = constant_values.get_as_constant_value(value) {
                        if value.replace_if_not_equal(constant_value.clone()) {
                            changes += 1;
                        }
                    }
                }
                TerminatorKind::Jump(_) => {}
                TerminatorKind::SwitchInt {
                    value,
                    ..
                } => {
                    if let Some(constant_value) = constant_values.get_as_constant_value(value) {
                        if value.replace_if_not_equal(constant_value.clone()) {
                            changes += 1;
                        }
                    }
                }
                TerminatorKind::Unresolved  => {}
            }
        }
        changes
    }
}
