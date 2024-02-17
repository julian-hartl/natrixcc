
use rustc_hash::FxHashMap;
use crate::middle::cfg::{BasicBlock, BasicBlockId};

use crate::middle::function::Function;
use crate::middle::InstrId;
use crate::middle::instruction::ValueData;

pub mod lattice;
pub mod concrete_value;

type InstrValue = crate::middle::Value;

#[derive(Default)]
pub struct ForwardAnalysis< V: lattice::Value> {
    pub(crate) state: FxHashMap<BasicBlockId, FxHashMap<InstrValue, lattice::Element<V>>>,
}

impl< V: lattice::Value> ForwardAnalysis< V> {
    pub fn lookup_lattice_element(&self, value: InstrValue, data: &ValueData) -> Option<&lattice::Element<V>> {
        self.state.get(&data.defined_in)?.get(&value)
    }

    pub fn get_lattice_element(&mut self, value: InstrValue, data: &ValueData) -> &mut lattice::Element<V> {
        self.state.entry(data.defined_in).or_default().entry(value).or_default()
    }

    pub fn run<H: FnMut(&mut Self, &mut Function, BasicBlockId, InstrId) -> bool>(&mut self, function: &mut Function, mut handler: H) -> usize {
        let mut total_changes = 0;
        loop {
            let mut changes = 0;
            let mut worklist = vec![function.cfg.entry_block];
            while let Some(basic_block) = worklist.pop() {
                self.state.entry(basic_block).or_default();
                let basic_block_data = function.cfg.basic_blocks[basic_block].as_ref().unwrap();
                for instr in basic_block_data.instructions.clone() {
                    let did_change = handler(self, function, basic_block, instr);
                    if did_change {
                        changes += 1;
                    }
                }
                let basic_block_data = function.cfg.basic_blocks[basic_block].as_ref().unwrap();
                for successor in basic_block_data.successors() {
                    let predecessors = function.cfg.basic_blocks[successor].as_ref().unwrap().predecessors(&function.cfg);
                    let are_all_processed = predecessors.iter().all(|predecessor| {
                        self.state.get(predecessor).is_some()
                    });
                    if !are_all_processed {
                        continue;
                    }
                    worklist.push(successor);
                }
            }
            if changes == 0 {
                break;
            }
            total_changes += changes;
        }
        total_changes
    }
}


