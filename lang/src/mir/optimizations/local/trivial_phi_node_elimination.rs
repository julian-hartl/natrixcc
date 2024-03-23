use crate::mir::{FunctionIdx, InstructionKind, MIR, Value};
use crate::mir::basic_block::BasicBlockIdx;
use crate::mir::optimizations::local::LocalMIRPass;

// todo: we should move this to the mir construction and just remove all unneeded phi nodes at the end of construction
pub struct TrivialPhiNodeElimination;

impl LocalMIRPass for TrivialPhiNodeElimination {
    fn run_on_basic_block(&mut self, mir: &mut MIR, function_idx: FunctionIdx, bb_idx: BasicBlockIdx) -> u32 {
        let mut changes = 0;
        let bb = mir.basic_blocks.get_mut(bb_idx).as_mut().unwrap();
        for instruction_idx in bb.instructions.iter().copied() {
            let instruction = mir.functions[function_idx].instructions.get_mut(instruction_idx);
            if let InstructionKind::Phi(phi) = &instruction.kind {
                assert!(!phi.operands.is_empty());
                let mut referenced_instruction = Some(&phi.operands[0].1);
                for (_, instruction_idx) in &phi.operands[1..] {
                    if referenced_instruction != Some(instruction_idx) {
                        referenced_instruction = None;
                        break;
                    }
                }
                if let Some(referenced_instruction) = referenced_instruction {
                    tracing::info!("Found a trivial phi node in {} with same operands, removing it", bb_idx);
                    changes += 1;
                    instruction.kind = InstructionKind::Value(Value::InstructionRef(*referenced_instruction));
                }
            }
        }
        changes
    }
}
