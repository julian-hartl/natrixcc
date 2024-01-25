use iced_x86::{Code, Instruction, MemoryOperand, Register};
use iced_x86::code_asm::{CodeAssembler, CodeLabel};
use rustc_hash::FxHashMap;

use crate::codegen::x86_64::{BasicBlock, FunctionData, InstrKind, Module, TerminatorKind};

pub struct AsmBuilder<'m> {
    a: CodeAssembler,
    module: &'m Module,
    label_map: FxHashMap<BasicBlock, CodeLabel>,
}


impl From<super::MemoryOperand> for MemoryOperand {
    fn from(value: super::MemoryOperand) -> Self {
        match value.index {
            None => {
                let base = value.base.try_as_physical().unwrap().into();
                if value.displacement == 0 {
                    Self::with_base(base)
                } else {
                    Self::with_base_displ(base, value.displacement)
                }
            }
            Some((index, scale)) => {
                let base = value.base.try_as_physical().unwrap().into();
                let index = index.try_as_physical().unwrap().into();
                if value.displacement == 0 {
                    Self::with_base_index_scale(base, index, scale)
                } else {
                    // todo: think about displacement size
                    Self::with_base_index_scale_displ_size(base, index, scale, value.displacement, 4)
                }
            }
        }
    }
}

impl<'m> AsmBuilder<'m> {
    pub fn new(module: &'m Module) -> Self {
        Self {
            a: CodeAssembler::new(64).unwrap(),
            module,
            label_map: FxHashMap::default(),
        }
    }

    pub fn build(mut self) -> CodeAssembler {
        for function in &self.module.functions {
            self.build_function(function);
        }
        self.a
    }

    fn build_function(&mut self, function: &FunctionData) {
        for (bb_idx, bb) in function.basic_blocks.iter_enumerated() {
            self.label_map.insert(bb_idx, self.a.create_label());
            for instr in &bb.instructions {
                match &instr.kind {
                    InstrKind::Mov32RI {
                        dest,
                        immediate
                    } => {
                        self.a.add_instruction(
                            Instruction::with2::<Register, _>(
                                Code::Mov_rm32_imm32,
                                dest.try_as_physical().unwrap().into(),
                                *immediate,
                            ).unwrap()
                        ).unwrap();
                    }
                    InstrKind::Mov32MI {
                        dest,
                        immediate
                    } => {
                        self.a.add_instruction(
                            Instruction::with2::<MemoryOperand, _>(
                                Code::Mov_rm32_imm32,
                                dest.clone().into(),
                                *immediate,
                            ).unwrap()
                        ).unwrap();
                    }
                    InstrKind::Mov32RM {
                        dest,
                        src,
                    } => {
                        self.a.add_instruction(
                            Instruction::with2::<Register, MemoryOperand>(
                                Code::Mov_rm32_r32,
                                dest.try_as_physical().unwrap().into(),
                                src.clone().into(),
                            ).unwrap()
                        ).unwrap();
                    }
                    InstrKind::Mov32MR {
                        dest,
                        src,
                    } => {
                        self.a.add_instruction(
                            Instruction::with2::<MemoryOperand, Register>(
                                Code::Mov_r32_rm32,
                                dest.clone().into(),
                                src.try_as_physical().unwrap().into(),
                            ).unwrap()
                        ).unwrap();
                    }
                    InstrKind::Mov32RR {
                        dest,
                        src,
                    } => {
                        self.a.add_instruction(
                            Instruction::with2::<Register, Register>(
                                Code::Mov_r32_rm32,
                                dest.try_as_physical().unwrap().into(),
                                src.try_as_physical().unwrap().into(),
                            ).unwrap()
                        ).unwrap();
                    }
                    InstrKind::Mov64RR {
                        dest,
                        src,
                    } => {
                        self.a.add_instruction(
                            Instruction::with2::<Register, Register>(
                                Code::Mov_r64_rm64,
                                dest.try_as_physical().unwrap().into(),
                                src.try_as_physical().unwrap().into(),
                            ).unwrap()
                        ).unwrap();
                    }
                    InstrKind::Sub32RR {
                        dest,
                        src,
                    } => {
                        self.a.add_instruction(
                            Instruction::with2::<Register, Register>(
                                Code::Sub_rm32_r32,
                                dest.try_as_physical().unwrap().into(),
                                src.try_as_physical().unwrap().into(),
                            ).unwrap()
                        ).unwrap();
                    }
                    InstrKind::Sub32RI {
                        dest,
                        immediate,
                    } => {
                        self.a.add_instruction(
                            Instruction::with2::<Register, _>(
                                Code::Sub_rm32_imm32,
                                dest.try_as_physical().unwrap().into(),
                                *immediate,
                            ).unwrap()
                        ).unwrap();
                    }
                    InstrKind::Sub64RI {
                        dest,
                        immediate,
                    } => {
                        self.a.add_instruction(
                            Instruction::with2::<Register, _>(
                                Code::Sub_rm64_imm32,
                                dest.try_as_physical().unwrap().into(),
                                *immediate,
                            ).unwrap()
                        ).unwrap();
                    }
                    InstrKind::Add32RI {
                        dest,
                        immediate,
                    } => {
                        self.a.add_instruction(
                            Instruction::with2::<Register, _>(
                                Code::Add_rm32_imm32,
                                dest.try_as_physical().unwrap().into(),
                                *immediate,
                            ).unwrap()
                        ).unwrap();
                    }
                    InstrKind::Push64R {
                        src,
                    } => {
                        self.a.add_instruction(
                            Instruction::with1::<Register>(
                                Code::Push_r64,
                                src.try_as_physical().unwrap().into(),
                            ).unwrap()
                        ).unwrap();
                    }
                    InstrKind::Neg32R {
                        src,
                    } => {
                        self.a.add_instruction(
                            Instruction::with1::<Register>(
                                Code::Neg_rm32,
                                src.try_as_physical().unwrap().into(),
                            ).unwrap()
                        ).unwrap();
                    }
                }
            }
            match &bb.terminator.kind {
                TerminatorKind::Ret {
                    ..
                } => {
                    self.a.ret().unwrap();
                }
                TerminatorKind::Jmp { target } => {
                    self.a.jmp(self.label_map[target]).unwrap();
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use iced_x86::{Formatter, IntelFormatter, NumberBase};

    use crate::cfg::{BranchTerm, CondBrTerm, RetTerm, TerminatorKind, ValueId};
    use crate::cfg_builder::CFGBuilder;
    use crate::codegen::x86_64::asm::AsmBuilder;
    use crate::codegen::x86_64::module_builder::ModuleBuilder;
    use crate::codegen::x86_64::register_allocator::{FastRegisterAllocator, RegisterAllocator};
    use crate::instruction::{Const, ICmpCond, Op};
    use crate::test_utils::create_test_module;
    use crate::ty::Type;

    #[test]
    fn test() {
        let (mut module, function) = create_test_module();
        let function = &mut module.functions[function];
        let mut cfg_builder = CFGBuilder::new(function);
        cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let (value, _) = cfg_builder.alloca(None, Type::I32, None, None).unwrap();
        cfg_builder.store(value, Op::Const(Const::i32(420))).unwrap();
        let op1_id = cfg_builder.next_unnamed_id();
        cfg_builder.op(Some(op1_id.clone()), Op::Const(Const::i32(69))).unwrap();
        let result_value_id = ValueId::Named("result".to_string());
        cfg_builder.load(Some(result_value_id.clone()), value).unwrap();
        let value = cfg_builder.find_or_insert_reaching_value(&result_value_id).unwrap();
        let op2_value = cfg_builder.find_or_insert_reaching_value(&op1_id).unwrap();
        cfg_builder.sub(Some(ValueId::Named("sub_result".to_string())), Op::Value(value), Op::Value(op2_value)).unwrap();
        let icmp_result_value_id = ValueId::Named("icmp_result".to_string());
        cfg_builder.icmp(Some(icmp_result_value_id.clone()), ICmpCond::Eq, Op::Value(value), Op::Value(op2_value)).unwrap();
        let icmp_result_value = cfg_builder.find_or_insert_reaching_value(&icmp_result_value_id).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::Cond(
            CondBrTerm {
                cond: Op::Value(icmp_result_value),
                true_target: bb1,
                false_target: bb2,
            }
        )));
        cfg_builder.set_bb(bb1);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm {
            value: Some(Op::Value(value)),
        }));
        cfg_builder.set_bb(bb2);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm {
            value: Some(Op::Value(op2_value)),
        }));
        drop(cfg_builder);
        let module_builder = ModuleBuilder::new(&module);
        let mut module = module_builder.build();
        let mut writer = std::io::stdout();
        module.write_to(&mut writer).unwrap();
        for function in &mut module.functions {
            FastRegisterAllocator::new().run(function);
        }
        let asm_builder = AsmBuilder::new(&module);
        let asm = asm_builder.build();
        let mut formatter = IntelFormatter::new();
        formatter.options_mut().set_number_base(NumberBase::Decimal);
        let mut buffer = String::new();
        for instr in asm.instructions() {
            formatter.format(instr, &mut buffer);
            buffer.push('\n');
        }
        println!("{buffer}");
    }
}
