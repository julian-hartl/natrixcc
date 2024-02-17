use iced_x86::{Code, Formatter, Instruction, IntelFormatter, NumberBase, Register};
use iced_x86::code_asm::CodeAssembler;

use crate::codegen::isa::x86_64;
use crate::codegen::isa::x86_64::{Instr, PhysicalRegister};
use crate::codegen::machine;

impl From<PhysicalRegister> for Register {
    fn from(value: PhysicalRegister) -> Self {
        match value {
            PhysicalRegister::EAX => Register::EAX,
            PhysicalRegister::ECX => Register::ECX,
            PhysicalRegister::EDX => Register::EDX,
            PhysicalRegister::EBX => Register::EBX,
            PhysicalRegister::ESP => Register::ESP,
            PhysicalRegister::EBP => Register::EBP,
            PhysicalRegister::ESI => Register::ESI,
            PhysicalRegister::EDI => Register::EDI,
        }
    }
}

impl machine::asm::Assembler<x86_64::Abi> for CodeAssembler {
    fn new() -> Self {
        Self::new(64).unwrap()
    }

    fn assemble(&mut self, instr: &<x86_64::Abi as machine::Abi>::I) {
        match instr {
            Instr::SUB32RI { dest, immediate } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Sub_rm32_imm32,
                    dest,
                    *immediate,
                ).unwrap()).unwrap()
            }
            Instr::SUB32RR { src, dest } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let src: Register = src.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Sub_rm32_r32,
                    dest,
                    src,
                ).unwrap()).unwrap()
            }
            Instr::MOV32RR { src, dest } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let src: Register = src.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm32_r32,
                    dest,
                    src,
                ).unwrap()).unwrap()
            }
            Instr::MOV32RI {
                dest,
                immediate
            } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm32_imm32,
                    dest,
                    *immediate,
                ).unwrap()).unwrap()
            }
            Instr::RET => {
                self.ret().unwrap()
            }
        }
    }

    fn finish(mut self) -> Vec<u8> {
        self.assemble(0x1234_5678).unwrap()
    }

    fn format(&self) -> String {
        let mut formatter = IntelFormatter::new();
        formatter.options_mut().set_use_pseudo_ops(false);
        formatter.options_mut().set_number_base(NumberBase::Decimal);
        let mut output = String::new();
        for instruction in self.instructions() {
            formatter.format(instruction, &mut output);
        }
        output
    }
}
