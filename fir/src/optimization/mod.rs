use crate::function::Function;
use crate::module::Module;
use crate::optimization::basic_block_pass::constant_fold::ConstantFoldPass;
use crate::optimization::basic_block_pass::copy_propagation::CopyPropagationPass;
use crate::optimization::basic_block_pass::cse::CSEPass;
use crate::optimization::function_pass::dead_code_elimination::DeadCodeEliminationPass;
use crate::optimization::function_pass::FunctionPass;

mod basic_block_pass;
mod function_pass;

#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(clippy::struct_excessive_bools)]
pub struct PipelineConfig {
    pub cse: bool,
    pub copy_propagation: bool,
    pub dead_code_elimination: bool,
    pub global_constant_propagation: bool,
    pub cfg_simplify: bool,
}


impl PipelineConfig {
    pub const fn all_disabled() -> Self {
        Self {
            cse: false,
            copy_propagation: false,
            dead_code_elimination: false,
            global_constant_propagation: false,
            cfg_simplify: false,
        }
    }
    pub const fn o0() -> Self {
        Self {
            cse: false,
            copy_propagation: false,
            dead_code_elimination: false,
            global_constant_propagation: false,
            cfg_simplify: false,
        }
    }

    pub const fn o1() -> Self {
        Self {
            cse: true,
            copy_propagation: true,
            dead_code_elimination: true,
            global_constant_propagation: true,
            cfg_simplify: true,
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

    fn run_on_function(&mut self, function: Function) {
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
        passes.push(Box::<basic_block_pass::trivial_phi_elim::Pass>::default());
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
        if self.config.cfg_simplify {
            passes.push(Box::<function_pass::cfg_simplify::Pass>::default());
        }
        passes
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::{BranchTerm, RetTerm, TerminatorKind, UnCondBrTerm};
    use crate::cfg_builder::CFGBuilder;
    use crate::instruction::{Const, Op};
    use crate::optimization::{Pipeline, PipelineConfig};
    use crate::test_utils::create_test_module;

    #[test]
    fn should_do_control_flow_ops() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function_data);
        let bb0 = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let (value, _) = cfg_builder.op(None, Op::Const(Const::i32(42))).unwrap();
        let (unused_value, _) = cfg_builder.op(None, Op::Const(Const::i32(90))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb1))));
        cfg_builder.set_bb(bb1);
        cfg_builder.op(None, Op::Value(value)).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb2))));
        cfg_builder.set_bb(bb2);
        let (return_value, _) = cfg_builder.op(None, Op::Value(value)).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::new(Op::Value(return_value))));
        drop(cfg_builder);
        let mut optimization_pipeline = Pipeline::new(&mut module, PipelineConfig::o1());
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let mut out = String::new();
        cfg.write_to(&mut out, function_data).unwrap();
        assert_eq!(out, "bb0:
    ret i32 42
");
    }
}

