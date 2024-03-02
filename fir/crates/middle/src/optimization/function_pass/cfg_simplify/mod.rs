use crate::FunctionId;
use crate::module::Module;
use crate::optimization::{CFGSimplifyPipelineConfig, FunctionPass};

mod jump_threading;
mod bb_merge;
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

impl FunctionPass for Pass {
    fn run_on_function(&mut self, module: &mut Module, function: FunctionId) -> usize {
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
    use crate::cfg;
    use crate::optimization::{CFGSimplifyPipelineConfig, PipelineConfig};
    use crate::test::create_test_module_from_source;

    #[test]
    fn should_merge_jump_chain() {
        let mut module = create_test_module_from_source(
            "
                fun i32 @test(i32) {
                bb0(i32 %0):
                   condbr i1 true, bb1, bb2
                bb1:
                   br bb2
                bb2:
                   ret i32 %0          
                }
            "
        );
        module.optimize(PipelineConfig::cfg_simplify_only(CFGSimplifyPipelineConfig::o3()));
        let function = module.find_function_by_name("test").unwrap();
        assert_eq!(
            function.write_to_string().unwrap(),
            "fun i32 @test(i32) {
             bb(i32 %0):
                ret i32 %0
             }
             "
        )
    }
}
