use crate::mir::MIR;

mod global;
mod local;

pub trait MIRPass {
    /// Returns the number of changes made to the MIR
    fn run(&mut self, mir: &mut MIR) -> u32;
}

pub struct Optimizer {
    passes: Vec<Box<dyn MIRPass>>,
}

impl Optimizer {
    pub fn new() -> Self {
        Self {
            passes: vec![
                Box::new(global::unreachable_code_elimination::UnreachableCodeElimination),
                Box::new(global::branch_elimination::BranchElimination),
                Box::new(global::dead_code_elimination::DeadCodeElimination),
                Box::new(local::LocalOptimizer::new()),
            ],
        }
    }

    pub fn run(&mut self, mir: &mut MIR) {
        let start = std::time::Instant::now();
        loop {
            // todo: compute convergence
            let mut changes = 0;
            for pass in self.passes.iter_mut() {
                changes += pass.run(mir);
            }
            if changes == 0 {
                let end = std::time::Instant::now();
                println!("Optimization took {}s", (end - start).as_secs_f64());
                return;
            }
        }
    }
}
