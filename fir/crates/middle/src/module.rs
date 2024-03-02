use index_vec::IndexVec;

use crate::{Function, FunctionId, optimization};
use crate::optimization::PipelineConfig;

#[derive(Debug, Clone, Default)]
pub struct Module {
    pub functions: IndexVec<FunctionId, Function>,
}

impl Module {
    pub fn find_function_by_name(&self, name: &str) -> Option<&Function> {
        self.functions.iter().find(
            |function| function.name == name
        )
    }

    pub fn optimize(&mut self, config: PipelineConfig) {
        let mut opt_pipeline = optimization::Pipeline::new(self, config);
        opt_pipeline.run();
    }
}
