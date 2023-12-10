use std::collections::HashMap;

use crate::mir::{BasicBlockIdx, FunctionIdx, InstructionIdx, InstructionKind, MIR, TerminatorKind};
use crate::mir::optimizations::local::LocalMIRPass;

pub struct CopyPropagation;

impl LocalMIRPass for CopyPropagation {
    fn run_on_basic_block(&mut self, mir: &mut MIR, function_idx: FunctionIdx, bb_idx: BasicBlockIdx) -> u32 {
        let mut changes = 0;
        // Marks an instruction as a copy of another instruction
        let mut copies_of: HashMap<InstructionIdx, InstructionIdx> = HashMap::new();
        let function = mir.functions.get_mut(function_idx);
        let bb = mir.basic_blocks.get_mut_or_panic(bb_idx);
        for instruction_idx in bb.instructions.iter().copied() {
            let instruction = &mut function.instructions[instruction_idx];
            match &mut instruction.kind {
                InstructionKind::Binary {
                    lhs,
                    rhs,
                    ..
                } => {
                    if lhs.replace_with_new_reference_from_copies(&copies_of) {
                        changes += 1;
                    }
                    if rhs.replace_with_new_reference_from_copies(&copies_of) {
                        changes += 1;
                    }
                }
                InstructionKind::Unary {
                    operand,
                    ..
                } => {
                    if operand.replace_with_new_reference_from_copies(&copies_of) {
                        changes += 1;
                    }
                }
                InstructionKind::Value(value) => {
                    if let Some(instruction_ref) = value.as_instruction_ref() {
                        copies_of.insert(instruction_idx, instruction_ref);
                    }
                    if value.replace_with_new_reference_from_copies(&copies_of) {
                        changes += 1;
                    }
                }
                InstructionKind::Call {
                    arguments,
                    ..
                } => {
                    for argument in arguments.iter_mut() {
                        if argument.replace_with_new_reference_from_copies(&copies_of) {
                            changes += 1;
                        }
                    }
                }
                InstructionKind::Phi(phi) => {
                    // for (_, value) in phi.iter_mut() {
                    //     if value.replace_with_new_reference_from_copies(&copies_of) {
                    //         changes += 1;
                    //     }
                    // }
                }
            }
        }
        if let Some(terminator) = bb.terminator.as_mut() {
            match &mut terminator.kind {
                TerminatorKind::Return { value } => {
                    if value.replace_with_new_reference_from_copies(&copies_of) {
                        changes += 1;
                    }
                }
                TerminatorKind::Jump(_) => {}
                TerminatorKind::SwitchInt { value, .. } => {
                    if value.replace_with_new_reference_from_copies(&copies_of) {
                        changes += 1;
                    }
                }
                TerminatorKind::Unresolved => {}
            }
        }
        changes
    }
}
