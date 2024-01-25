use rustc_hash::FxHashMap;

use crate::cfg::BasicBlock;
use crate::function::Function;
use crate::instruction::{InstrIdentifyingKey, InstrKind, Op, OpInstr, Value};
use crate::module::Module;
use crate::optimization::basic_block_pass::BasicBlockPass;

pub struct CSEPass {}

impl BasicBlockPass for CSEPass {
    fn run_on_basic_block(&mut self, module: &mut Module, function: Function, basic_block: BasicBlock) -> usize {
        let cfg = &mut module.functions[function].cfg;
        let bb = cfg.basic_blocks[basic_block].as_ref().unwrap();
        let mut changes = 0;
        let mut expr_cache: FxHashMap<InstrIdentifyingKey, Value> = FxHashMap::default();
        for instr in bb.instructions.iter().copied() {
            let instr_data = &mut cfg.instructions[instr];
            let key = instr_data.identifying_key();
            if let Some(key) = key {
                let produced_value = instr_data.produced_value();
                if let Some(produced_value) = produced_value {
                    let cached_value = expr_cache.get(&key).copied();
                    if let Some(cached_value) = cached_value {
                        instr_data.kind = InstrKind::Op(OpInstr {
                            value: produced_value,
                            op: Op::Value(cached_value),
                        });
                        changes += 1;
                    } else {
                        expr_cache.insert(key, produced_value);
                    }
                }
            }
        }
        changes
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::{RetTerm, TerminatorKind, ValueId};
    use crate::cfg_builder::CFGBuilder;
    use crate::instruction::{Const, Op};
    use crate::optimization::{Pipeline, PipelineConfig};
    use crate::test_utils::create_test_module;

    #[test]
    fn should_replace_duplicate_subtraction() {
        let (mut module, function) = create_test_module();
        let function_data = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function_data);
        let bb = cfg_builder.start_bb();
        let val_id = ValueId::Named("val".to_string());
        let (var_instr_value, _) = cfg_builder.op(Some(val_id.clone()), Op::Const(Const::i32(10))).unwrap();
        let (sub1_value, _) = cfg_builder.sub(None, Op::Value(var_instr_value), Op::Const(Const::i32(5))).unwrap();
        let (sub2_value, _) = cfg_builder.sub(None, Op::Value(var_instr_value), Op::Const(Const::i32(5))).unwrap();
        let (return_value, _) = cfg_builder.sub(
            Some(ValueId::Named("return_value".to_string())),
            Op::Value(sub1_value),
            Op::Value(sub2_value),
        ).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::new(Op::Value(return_value))));
        drop(cfg_builder);
        let mut optimization_pipeline = Pipeline::new(&mut module, PipelineConfig::cse_only());
        optimization_pipeline.run();
        let function_data = &module.functions[function];
        let cfg = &function_data.cfg;
        let mut out = String::new();
        cfg.write_to(&mut out, function_data).unwrap();
        assert_eq!(out, "bb0:
    %val = i32 10
    %0 = sub i32 %val, 5
    %1 = i32 %0
    %return_value = sub i32 %0, %1
    ret i32 %return_value
");
    }
}