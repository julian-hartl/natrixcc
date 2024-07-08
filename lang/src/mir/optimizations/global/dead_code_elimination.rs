use std::collections::HashSet;

use crate::mir::{
    optimizations::MIRPass, InstructionIdx, InstructionKind, TerminatorKind, Value, MIR,
};

struct ReferencedInstructions(HashSet<InstructionIdx>);

impl ReferencedInstructions {
    pub fn new() -> Self {
        Self(HashSet::new())
    }

    pub fn insert_if_is_instruction_ref(&mut self, value: &Value) {
        if let Some(instruction_ref) = value.as_instruction_ref() {
            self.0.insert(instruction_ref);
        }
    }
}

pub struct DeadCodeElimination;

impl MIRPass for DeadCodeElimination {
    fn run(&mut self, mir: &mut MIR) -> u32 {
        let mut changes = 0;

        for function in mir.functions.iter_mut() {
            let mut referenced_instructions = ReferencedInstructions::new();
            for bb in function.basic_blocks.iter().copied() {
                let bb = mir.basic_blocks.get_mut_or_panic(bb);
                for instruction_idx in bb.instructions.iter().copied() {
                    let instruction = &mut function.instructions[instruction_idx];
                    match &mut instruction.kind {
                        InstructionKind::Binary { lhs, rhs, .. } => {
                            referenced_instructions.insert_if_is_instruction_ref(lhs);
                            referenced_instructions.insert_if_is_instruction_ref(rhs);
                        }
                        InstructionKind::Unary { operand, .. } => {
                            referenced_instructions.insert_if_is_instruction_ref(operand);
                        }
                        InstructionKind::Value(value) => {
                            referenced_instructions.insert_if_is_instruction_ref(value);
                        }
                        InstructionKind::Call { arguments, .. } => {
                            for argument in arguments.iter_mut() {
                                referenced_instructions.insert_if_is_instruction_ref(argument);
                            }
                        }
                        InstructionKind::Phi(phi) => {
                            for (_, instruction_idx) in phi.iter().copied() {
                                referenced_instructions.0.insert(instruction_idx);
                            }
                        }
                    }
                }
                if let Some(terminator) = bb.terminator.as_mut() {
                    match &mut terminator.kind {
                        TerminatorKind::Return { value } => {
                            referenced_instructions.insert_if_is_instruction_ref(value);
                        }
                        TerminatorKind::SwitchInt { value, .. } => {
                            referenced_instructions.insert_if_is_instruction_ref(value);
                        }
                        TerminatorKind::Jump { .. } => {}

                        TerminatorKind::Unresolved => {}
                    }
                }
            }
            for bb in function.basic_blocks.iter().copied() {
                let bb = mir.basic_blocks.get_mut_or_panic(bb);
                bb.instructions.retain(|instruction_idx| {
                    if referenced_instructions.0.contains(instruction_idx)
                        || !function.instructions[*instruction_idx].is_pure()
                    {
                        true
                    } else {
                        changes += 1;
                        false
                    }
                })
            }
        }
        changes
    }
}
