use crate::mir::{BasicBlockIdx, FunctionIdx, MIR};
use crate::mir::optimizations::MIRPass;

mod constants_folding;
mod algebraic_simplification;
mod copy_propagation;
mod trivial_phi_node_elimination;

pub trait LocalMIRPass {
    /// Returns the number of changes made to the BasicBlock
    fn run_on_basic_block(&mut self, mir: &mut MIR, function_idx: FunctionIdx, bb_idx: BasicBlockIdx) -> u32;
}

pub struct LocalOptimizer {
    passes: Vec<Box<dyn LocalMIRPass>>,
}

impl LocalOptimizer {
    pub fn new() -> Self {
        Self {
            passes: vec![
                Box::new(trivial_phi_node_elimination::TrivialPhiNodeElimination),
                Box::new(constants_folding::ConstantFolding),
                Box::new(copy_propagation::CopyPropagation),
                Box::new(algebraic_simplification::AlgebraicSimplification),
            ]
        }
    }
}

impl MIRPass for LocalOptimizer {
    fn run(&mut self, mir: &mut MIR) -> u32 {
        let mut changes = 0;
        for function_idx in mir.functions.indices() {
            for bb_idx in mir.functions[function_idx].basic_blocks.clone() {
                if mir.basic_blocks[bb_idx].is_none() {
                    continue;
                }
                for pass in self.passes.iter_mut() {
                    changes += pass.run_on_basic_block(mir, function_idx, bb_idx);
                }
            }
        }
        println!("LocalOptimizer: {} changes", changes);
        changes
    }
}
