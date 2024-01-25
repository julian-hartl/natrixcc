use std::collections::HashMap;

use index_vec::IndexVec;
use rustc_hash::FxHashMap;

use crate::cfg::ValueId;
use crate::codegen::x86_64::{BasicBlock, BasicBlockData, FunctionData, Instr, InstrKind, MemoryOperand, PhysicalRegister, Register, Size, StackFrame, StackFrameEntry, Terminator, TerminatorKind, VirtualRegisterData};
use crate::instruction::{Const, Op};
use crate::ty::Type;

type MIRInstrKind = crate::instruction::InstrKind;

type MIRFunction = crate::function::FunctionData;
type MIRBasicBlock = crate::cfg::BasicBlock;
type MIRValue = crate::instruction::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Location {
    Register(Register),
    Stack(StackFrameEntry),
    Memory(MemoryOperand),
}

impl Location {
    pub const fn as_register(&self) -> Option<Register> {
        match self {
            Self::Register(reg) => Some(*reg),
            _ => None,
        }
    }

    pub const fn as_stack(&self) -> Option<StackFrameEntry> {
        match self {
            Self::Stack(entry) => Some(*entry),
            _ => None,
        }
    }

    pub const fn as_memory(&self) -> Option<&MemoryOperand> {
        match self {
            Self::Memory(mem_op) => Some(mem_op),
            _ => None,
        }
    }
}

pub struct FunctionBuilder<'mir> {
    pub mir_function: &'mir MIRFunction,
    pub function: FunctionData,
    basic_block_map: FxHashMap<BasicBlock, MIRBasicBlock>,
    // todo: replace value id with index type
    id_to_location: FxHashMap<ValueId, Location>,
}

impl<'mir> FunctionBuilder<'mir> {
    pub fn new(function: &'mir MIRFunction) -> Self {
        Self {
            mir_function: function,
            function: FunctionData {
                name: function.name.clone(),
                basic_blocks: IndexVec::default(),
                stack_frame: StackFrame::default(),
                registers: IndexVec::default(),
            },
            id_to_location: HashMap::default(),
            basic_block_map: HashMap::default(),
        }
    }

    pub fn build(mut self) -> FunctionData {
        for basic_block in self.mir_function.cfg.basic_blocks.iter().map(|bb| bb.as_ref()).flatten() {
            let current_bb = self.new_basic_block(Some(basic_block.idx));
            if self.function.basic_blocks.len() == 1 {
                self.function.basic_blocks[current_bb].instructions.push(Instr::new(InstrKind::Push64R {
                    src: Register::Physical(PhysicalRegister::RBP),
                }
                ));
                self.function.basic_blocks[current_bb].instructions.push(Instr::new(
                    InstrKind::Mov64RR {
                        dest: Register::Physical(PhysicalRegister::RBP),
                        src: Register::Physical(PhysicalRegister::RSP),
                    },
                ));
            }
            for instruction in basic_block.instructions.iter().copied() {
                let instruction = &self.mir_function.cfg.instructions[instruction];
                match &instruction.kind {
                    MIRInstrKind::Alloca(instr) => {
                        let entry = self.function.stack_frame.push(instr.ty.clone(), instr.num_elements);
                        let size = self.function.stack_frame.entries[entry].size();
                        self.add_instruction(current_bb, InstrKind::Sub64RI {
                            dest: Register::Physical(PhysicalRegister::RSP),
                            immediate: size as i32,
                        });
                        self.map_value_to_location(instr.value, Location::Memory(MemoryOperand::new(
                            Register::Physical(PhysicalRegister::RBP),
                            self.function.stack_frame.total_size() as i64,
                            None,
                        )));
                    }
                    MIRInstrKind::Store(instr) => {
                        let mem_op = self.expect_value_as_mem_op(instr.pointer).clone();
                        let instruction = match &instr.value {
                            Op::Const(constant) => match constant {
                                Const::Int {
                                    width,
                                    value,
                                    signed,
                                } => {
                                    match Size::from_bit_width(*width) {
                                        Size::Byte => unimplemented!(),
                                        Size::Word => unimplemented!(),
                                        Size::QWord => unimplemented!(),
                                        Size::DWord => {
                                            InstrKind::Mov32MI {
                                                dest: mem_op,
                                                immediate: *value as i32,
                                            }
                                        }
                                        Size::QWord => unimplemented!(),
                                    }
                                }
                            }
                            Op::Value(value) => {
                                let src_reg = self.expect_value_in_register(*value);
                                match &self.mir_function.cfg.values_ctx[*value].ty {
                                    Type::Int { width, .. } => match Size::from_bit_width(*width) {
                                        Size::Byte => unimplemented!(),
                                        Size::Word => unimplemented!(),
                                        Size::DWord => {
                                            InstrKind::Mov32MR {
                                                dest: mem_op,
                                                src: src_reg,
                                            }
                                        }
                                        Size::QWord => unimplemented!(),
                                    },
                                    _ => unimplemented!()
                                }
                            }
                        };
                        self.add_instruction(current_bb, instruction);
                    }
                    MIRInstrKind::Load(instr) => {
                        let instruction = match &self.mir_function.cfg.values_ctx[instr.value].ty {
                            Type::Int {
                                width,
                                ..
                            } => {
                                match Size::from_bit_width(*width) {
                                    Size::DWord => {
                                        let register = self.expect_value_in_register(instr.value);
                                        self.map_value_to_location(instr.value, Location::Register(register));
                                        let mem_op = self.expect_value_as_mem_op(instr.source).clone();
                                        InstrKind::Mov32RM {
                                            dest: register,
                                            src: mem_op,
                                        }
                                    }
                                    _ => unimplemented!()
                                }
                            }
                            _ => unimplemented!()
                        };
                        self.add_instruction(current_bb, instruction);
                    }
                    MIRInstrKind::Op(instr) => {
                        let instruction = match &instr.op {
                            Op::Const(constant) => match constant {
                                Const::Int {
                                    width,
                                    value,
                                    ..
                                } => match Size::from_bit_width(*width) {
                                    Size::DWord => {
                                        let register = self.expect_value_in_register(instr.value);
                                        self.map_value_to_location(instr.value, Location::Register(register));
                                        InstrKind::Mov32RI {
                                            dest: register,
                                            immediate: *value as i32,
                                        }
                                    }
                                    _ => unimplemented!(),
                                }
                            }
                            Op::Value(value) => {
                                let src_reg = self.expect_value_in_register(*value);
                                match &self.mir_function.cfg.values_ctx[*value].ty {
                                    Type::Int {
                                        width,
                                        ..
                                    } => {
                                        match Size::from_bit_width(*width) {
                                            Size::DWord => {
                                                let dest = self.expect_value_in_register(instr.value);
                                                self.map_value_to_location(instr.value, Location::Register(dest));
                                                InstrKind::Mov32RR {
                                                    dest,
                                                    src: src_reg,
                                                }
                                            }
                                            _ => unimplemented!(),
                                        }
                                    }
                                    _ => unimplemented!()
                                }
                            }
                        };
                        self.add_instruction(current_bb, instruction);
                    }
                    MIRInstrKind::Sub(instr) => {
                        let dest_reg = self.expect_value_in_register(instr.value);
                        match (&instr.lhs, &instr.rhs) {
                            (Op::Const(_constant), Op::Const(_constant2)) => {
                                unreachable!("constant folding should have been done before codegen")
                            }
                            (Op::Const(constant), Op::Value(place)) => {
                                let src_reg = self.expect_value_in_register(*place);
                                if dest_reg != src_reg {
                                    self.add_instruction(current_bb, InstrKind::Mov32RR {
                                        dest: dest_reg,
                                        src: src_reg,
                                    });
                                }
                                match *constant {
                                    Const::Int {
                                        width,
                                        value, ..
                                    } => {
                                        match Size::from_bit_width(width) {
                                            Size::DWord => {
                                                self.add_instruction(current_bb, InstrKind::Neg32R {
                                                    src: dest_reg,
                                                });
                                                self.add_instruction(current_bb, InstrKind::Add32RI {
                                                    dest: dest_reg,
                                                    immediate: value as i32,
                                                });
                                            }
                                            _ => unimplemented!(),
                                        }
                                    }
                                }
                            }
                            (Op::Value(place), Op::Const(constant)) => {
                                let src_reg = self.expect_value_in_register(*place);
                                if dest_reg != src_reg {
                                    self.add_instruction(current_bb, InstrKind::Mov32RR {
                                        dest: dest_reg,
                                        src: src_reg,
                                    });
                                }
                                match *constant {
                                    Const::Int {
                                        width,
                                        value,
                                        ..
                                    } => {
                                        self.add_instruction(current_bb, InstrKind::Sub32RI {
                                            dest: dest_reg,
                                            immediate: value as i32,
                                        });
                                    }
                                }
                            }
                            (Op::Value(place), Op::Value(place2)) => {
                                let src_reg = self.expect_value_in_register(*place);
                                let src_reg2 = self.expect_value_in_register(*place2);
                                if dest_reg != src_reg {
                                    self.add_instruction(current_bb, InstrKind::Mov32RR {
                                        dest: dest_reg,
                                        src: src_reg,
                                    });
                                }
                                self.add_instruction(current_bb, InstrKind::Sub32RR {
                                    dest: dest_reg,
                                    src: src_reg2,
                                });
                            }
                        };
                        self.map_value_to_location(instr.value, Location::Register(dest_reg));
                    }
                    MIRInstrKind::Phi(_) => unimplemented!(),
                    MIRInstrKind::ICmp(_) => unimplemented!(),
                }
            }
        }
        self.function
    }

    fn expect_value_as_mem_op(&self, value: MIRValue) -> &MemoryOperand {
        let location = self.get_location(value).unwrap();
        location.as_memory().expect("expected value to be in memory")
    }

    fn expect_value_in_register(&mut self, value: MIRValue) -> Register {
        let location = self.get_location(value);
        match location {
            None => {
                let value_data = &self.mir_function.cfg.values_ctx[value];
                let register = self.function.registers.push(VirtualRegisterData {
                    ty: value_data.ty.clone(),
                    id: value_data.id.clone(),
                });
                let register = Register::Virtual(register);
                self.map_value_to_location(value, Location::Register(register));
                register
            }
            Some(location) => location.as_register().expect("expected value to be in register")
        }
    }

    fn get_location(&self, value: MIRValue) -> Option<&Location> {
        let value_id = &self.mir_function.cfg.values_ctx[value].id;
        self.id_to_location.get(&value_id)
    }

    fn map_value_to_location(&mut self, value: MIRValue, location: Location) {
        let value_id = &self.mir_function.cfg.values_ctx[value].id;
        self.id_to_location.insert(value_id.clone(), location);
    }


    fn add_instruction(&mut self, basic_block: BasicBlock, instruction: InstrKind) {
        self.function.basic_blocks[basic_block].instructions.push(Instr::new(instruction));
    }

    fn new_basic_block(&mut self, bb: Option<MIRBasicBlock>) -> BasicBlock {
        let basic_block = self.function.basic_blocks.push(BasicBlockData {
            instructions: Vec::new(),
            terminator: Terminator {
                kind: TerminatorKind::Ret {
                    value: None,
                }
            },
        });
        if let Some(bb) = bb {
            self.basic_block_map.insert(basic_block, bb);
        }
        basic_block
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::{RetTerm, TerminatorKind, ValueId};
    use crate::cfg_builder::CFGBuilder;
    use crate::codegen::x86_64::function_builder::FunctionBuilder;
    use crate::instruction::{Const, Op};
    use crate::test_utils::create_test_function;
    use crate::ty::Type;

    #[test]
    fn test() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        let (value, _) = cfg_builder.alloca(None, Type::I32, None, None).unwrap();
        cfg_builder.store(value, Op::Const(Const::i32(420))).unwrap();
        let op1_id = cfg_builder.next_unnamed_id();
        cfg_builder.op(Some(op1_id.clone()), Op::Const(Const::i32(69))).unwrap();
        let result_value_id = ValueId::Named("result".to_string());
        cfg_builder.load(Some(result_value_id.clone()), value).unwrap();
        let value = cfg_builder.find_or_insert_reaching_value(&result_value_id).unwrap();
        let op2_value = cfg_builder.find_or_insert_reaching_value(&op1_id).unwrap();
        cfg_builder.sub(Some(ValueId::Named("sub_result".to_string())), Op::Value(value), Op::Value(op2_value)).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let function_data = &function;
        let function_builder = FunctionBuilder::new(function_data);
        let function = function_builder.build();
        let mut writer = std::io::stdout();
        let mut out = String::new();
        function_data.cfg.write_to(&mut out, function_data).unwrap();
        println!("{}", out);
        function.write_to(&mut writer).unwrap();
    }
}
