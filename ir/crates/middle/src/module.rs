use std::{
    fmt::{
        Display,
        Formatter,
    },
    time::Instant,
};

use cranelift_entity::PrimaryMap;
use tracing::{
    debug,
    info,
};

use crate::{
    optimization,
    optimization::PipelineConfig,
    Function,
    FunctionId,
    Instr,
};

#[derive(Debug, Clone, Default)]
pub struct Module {
    pub functions: PrimaryMap<FunctionId, Function>,
}

impl Module {
    pub fn find_function_by_name(&self, name: &str) -> Option<&Function> {
        self.functions
            .values()
            .find(|function| function.name == name)
    }

    pub fn optimize(&mut self, config: PipelineConfig) {
        let start = Instant::now();
        info!("Optimizing module");
        let mut opt_pipeline = optimization::Pipeline::new(self, config);
        debug!("Using config {:?}", opt_pipeline.config());
        opt_pipeline.run();
        let end = Instant::now();
        let time = end.duration_since(start).as_millis();
        info!("Optimized module in {time} ms");
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (_, func) in &self.functions {
            write!(f, "{}", func)?;
        }
        Ok(())
    }
}
