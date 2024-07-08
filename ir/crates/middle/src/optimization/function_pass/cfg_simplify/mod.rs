use crate::{
    module::Module,
    optimization::{CFGSimplifyPipelineConfig, FunctionPass},
    FunctionRef,
};

mod bb_merge;
mod jump_threading;
mod unreachable_bb_elim;

pub struct Pass {
    config: CFGSimplifyPipelineConfig,
    jump_threading: jump_threading::Pass,
    bb_merge: bb_merge::Pass,
    unreachable_bb_elim: unreachable_bb_elim::Pass,
}

impl Pass {
    pub fn new(config: CFGSimplifyPipelineConfig) -> Self {
        Self {
            config,
            jump_threading: Default::default(),
            bb_merge: Default::default(),
            unreachable_bb_elim: Default::default(),
        }
    }
}

impl crate::optimization::Pass for Pass {
    fn name(&self) -> &'static str {
        "cfg_simplify"
    }
}

impl FunctionPass for Pass {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionRef) -> usize {
        let mut changed = 0;
        let config = &self.config;
        if config.jump_threading {
            changed += self.jump_threading.run_on_function(module, function);
        }
        if config.unreachable_bb_elim {
            changed += self.unreachable_bb_elim.run_on_function(module, function);
        }
        if config.bb_merge {
            changed += self.bb_merge.run_on_function(module, function);
        }
        changed
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::{
        optimization::{CFGSimplifyPipelineConfig, PipelineConfig},
        test::{assert_module_is_equal_to_src, create_test_module_from_source},
    };

    #[test]
    #[traced_test]
    fn should_merge_jump_chain() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test(i32) {
                bb0(i32 %0):
                   condbr true bb1, bb2;
                bb1:
                   br bb2;
                bb2:
                   ret %0;
                }
            ",
        );
        module.optimize(PipelineConfig::cfg_simplify_only(
            CFGSimplifyPipelineConfig::o3(),
        ));
        assert_module_is_equal_to_src(
            &module,
            "fun i32 @test(i32) {
             bb0(i32 %0):
                ret %0;
             }
             ",
        )
    }
}
