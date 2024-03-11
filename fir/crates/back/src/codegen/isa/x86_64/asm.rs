use std::ops::{Deref, DerefMut};
use iced_x86::{Code, Formatter, Instruction, IntelFormatter, NumberBase, Register};
use iced_x86::code_asm::{CodeAssembler, CodeLabel};
use rustc_hash::FxHashMap;

use crate::codegen::isa::x86_64;
use crate::codegen::isa::x86_64::{X86Instr, PhysicalRegister};
use crate::codegen::machine;
use crate::codegen::machine::BasicBlockId;

pub struct Assembler {
    assembler: CodeAssembler,
    bb_to_label: FxHashMap<BasicBlockId, CodeLabel>,
}

impl Assembler {
    pub fn get_or_insert_label(&mut self, bb: BasicBlockId) -> CodeLabel {
        *self.bb_to_label.entry(bb).or_insert_with(|| self.assembler.create_label())
    }
}

impl Deref for Assembler {
    type Target = CodeAssembler;

    fn deref(&self) -> &Self::Target {
        &self.assembler
    }
}

impl DerefMut for Assembler {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.assembler
    }
}

impl From<PhysicalRegister> for Register {
    fn from(value: PhysicalRegister) -> Self {
        match value {
            PhysicalRegister::EAX => Self::EAX,
            PhysicalRegister::ECX => Self::ECX,
            PhysicalRegister::EDX => Self::EDX,
            PhysicalRegister::EBX => Self::EBX,
            PhysicalRegister::ESP => Self::ESP,
            PhysicalRegister::EBP => Self::EBP,
            PhysicalRegister::ESI => Self::ESI,
            PhysicalRegister::EDI => Self::EDI,
            PhysicalRegister::R8D => Self::R8D,
            PhysicalRegister::R9D => Self::R9D,
        }
    }
}

impl machine::asm::Assembler<x86_64::Abi> for  Assembler{
    fn new() -> Self {
        Self {
            bb_to_label: FxHashMap::default(),
            assembler: CodeAssembler::new(64).unwrap()
        }
    }

    fn begin_basic_block(&mut self, bb_id: machine::BasicBlockId) {
        let label = self.bb_to_label.entry(bb_id).or_insert_with(|| self.assembler.create_label());
        self.assembler.set_label(label).unwrap();
    }

    fn assemble(&mut self, instr: &<x86_64::Abi as machine::Abi>::I) {
        match instr {
            X86Instr::SUB32RI { dest, immediate } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Sub_rm32_imm32,
                    dest,
                    immediate.into_unsigned(),
                ).unwrap()).unwrap()
            }
            X86Instr::SUB32RR { src, dest } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let src: Register = src.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Sub_rm32_r32,
                    dest,
                    src,
                ).unwrap()).unwrap()
            }
            X86Instr::ADD32RI { dest, immediate } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Add_rm32_imm32,
                    dest,
                    immediate.into_unsigned(),
                ).unwrap()).unwrap()
            }
            X86Instr::ADD32RR { src, dest } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let src: Register = src.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Add_rm32_r32,
                    dest,
                    src,
                ).unwrap()).unwrap()
            }
            X86Instr::MOV32RR { src, dest } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let src: Register = src.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm32_r32,
                    dest,
                    src,
                ).unwrap()).unwrap()
            }
            X86Instr::MOV32RI {
                dest,
                immediate
            } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm32_imm32,
                    dest,
                    immediate.into_unsigned(),
                ).unwrap()).unwrap()
            }
            X86Instr::RET => {
                self.ret().unwrap()
            }
            X86Instr::JMP { 
                target
            } => {
                let label = self.get_or_insert_label(*target);
                self.jmp(label).unwrap()
            }
        }
    }

    fn finish(mut self) -> Vec<u8> {
        self.assembler.assemble(0x1234_5678).unwrap()
    }

    fn format(&self) -> String {
        let mut formatter = IntelFormatter::new();
        formatter.options_mut().set_use_pseudo_ops(false);
        formatter.options_mut().set_number_base(NumberBase::Decimal);
        let mut output = String::new();
        for instruction in self.instructions() {
            formatter.format(instruction, &mut output);
            output.push('\n');
        }
        output
    }
}
