use crate::FunctionId;
use crate::module::Module;
use crate::optimization::basic_block_pass::constant_fold::ConstantFoldPass;
use crate::optimization::basic_block_pass::copy_propagation::CopyPropagationPass;
use crate::optimization::basic_block_pass::cse::CSEPass;
use crate::optimization::function_pass::dead_code_elimination::DeadCodeEliminationPass;
use crate::optimization::function_pass::FunctionPass;

pub mod basic_block_pass;
pub mod function_pass;

#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(clippy::struct_excessive_bools)]
pub struct PipelineConfig {
    pub cse: bool,
    pub copy_propagation: bool,
    pub dead_code_elimination: bool,
    pub global_constant_propagation: bool,
    pub cfg_simplify: CFGSimplifyPipelineConfig,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct CFGSimplifyPipelineConfig {
    pub bb_merge: bool,
    pub unreachable_bb_elim: bool,
    pub jump_threading: bool,
}

impl CFGSimplifyPipelineConfig {
    pub const fn all_disabled() -> Self {
        Self {
            bb_merge: false,
            jump_threading: false,
            unreachable_bb_elim: false,
        }
    }

    pub const fn o1() -> Self {
        Self {
            bb_merge: true,
            jump_threading: true,
            unreachable_bb_elim: true,
        }
    }

    pub const fn o2() -> Self {
        Self::o1()
    }

    pub const fn o3() -> Self {
        Self::o1()
    }
}

#[cfg(test)]
impl CFGSimplifyPipelineConfig {
    pub const fn unreachable_bb_elim_only() -> Self {
        Self {
            unreachable_bb_elim: true,
            ..Self::all_disabled()
        }
    }
}

impl PipelineConfig {
    pub const fn all_disabled() -> Self {
        Self {
            cse: false,
            copy_propagation: false,
            dead_code_elimination: false,
            global_constant_propagation: false,
            cfg_simplify: CFGSimplifyPipelineConfig::all_disabled(),
        }
    }
    pub const fn o0() -> Self {
        Self {
            cse: false,
            copy_propagation: false,
            dead_code_elimination: false,
            global_constant_propagation: false,
            cfg_simplify: CFGSimplifyPipelineConfig::all_disabled(),
        }
    }

    pub const fn o1() -> Self {
        Self {
            cse: true,
            copy_propagation: true,
            dead_code_elimination: true,
            global_constant_propagation: true,
            cfg_simplify: CFGSimplifyPipelineConfig::o1(),
        }
    }

    // pub const fn o2() -> Self {
    //     Self {
    //         constant_fold: true,
    //         cse: true,
    //         copy_propagation: true,
    //     }
    // }
    //
    // pub const fn o3() -> Self {
    //     Self {
    //         constant_fold: true,
    //         cse: true,
    //         copy_propagation: true,
    //     }
    // }
}

#[cfg(test)]
impl PipelineConfig {
    pub const fn cse_only() -> Self {
        Self {
            cse: true,
            ..Self::all_disabled()
        }
    }

    pub const fn copy_propagation_only() -> Self {
        Self {
            copy_propagation: true,
            ..Self::all_disabled()
        }
    }

    pub const fn dead_code_elimination_only() -> Self {
        Self {
            dead_code_elimination: true,
            ..Self::all_disabled()
        }
    }

    pub const fn global_constant_propagation_only() -> Self {
        Self {
            global_constant_propagation: true,
            ..Self::all_disabled()
        }
    }

    pub const fn cfg_simplify_only(config: CFGSimplifyPipelineConfig) -> Self {
        Self {
            cfg_simplify: config,
            ..Self::all_disabled()
        }
    }
}

pub struct Pipeline<'m> {
    module: &'m mut Module,
    config: PipelineConfig,
}

impl<'a> Pipeline<'a> {
    pub fn new(module: &'a mut Module, config: PipelineConfig) -> Self {
        Self {
            module,
            config,
        }
    }

    pub fn run(&mut self) {
        for function in self.module.functions.indices() {
            self.run_on_function(function);
        }
    }

    fn run_on_function(&mut self, function: FunctionId) {
        let mut passes = self.get_passes();
        loop {
            let mut changes = 0;
            for pass in &mut passes {
                changes += pass.run_on_function(self.module, function);
            }
            if changes == 0 {
                break;
            }
        }
    }

    fn get_passes(&mut self) -> Vec<Box<dyn FunctionPass>> {
        let mut passes: Vec<Box<dyn FunctionPass>> = Vec::new();
        passes.push(Box::new(ConstantFoldPass {}));
        if self.config.cse {
            passes.push(Box::new(CSEPass {}));
        }
        if self.config.copy_propagation {
            passes.push(Box::<CopyPropagationPass>::default());
        }
        if self.config.dead_code_elimination {
            passes.push(Box::<DeadCodeEliminationPass>::default());
        }
        if self.config.global_constant_propagation {
            passes.push(Box::<function_pass::constant_propagation::ConstantPropagation>::default());
        }
        passes.push(Box::new(function_pass::cfg_simplify::Pass::new(self.config.cfg_simplify)));
        passes
    }
}

