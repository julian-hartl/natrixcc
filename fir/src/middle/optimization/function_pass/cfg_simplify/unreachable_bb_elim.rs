use crate::middle::FunctionId;
use crate::middle::instruction::InstrKind;
use crate::middle::module::Module;
use crate::middle::optimization::function_pass::FunctionPass;

#[derive(Default)]
pub struct Pass {}

impl FunctionPass for Pass {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionId) -> usize {
        let function = &mut module.functions[function];
        let unreachable_bbs = function.cfg.basic_blocks.iter_enumerated().filter_map(|(bb, bb_data)| {
            bb_data.as_ref().map(|bb_data| {
                if bb_data.id != function.cfg.entry_block && bb_data.predecessors(&function.cfg).is_empty() {
                    Some(bb)
                } else {
                    None
                }
            })
        }).flatten().collect::<Vec<_>>();
        let bb_count = unreachable_bbs.len();
        for bb in unreachable_bbs {
            let bb_data = function.cfg.basic_blocks[bb].take().unwrap();
            let successors = bb_data.successors();
            for successor in successors {
                let successor_data = function.cfg.basic_blocks[successor].as_ref().unwrap();
                for instr in successor_data.instructions.iter().copied() {
                    let instr = &mut function.cfg.instructions[instr];
                    if let InstrKind::Phi(phi) = &mut instr.kind {
                        phi.incoming.retain(|incoming| incoming.source != bb);
                    }
                }
            }
        }
        bb_count
    }
}
