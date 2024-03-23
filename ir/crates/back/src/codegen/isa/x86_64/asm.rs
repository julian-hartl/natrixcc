use std::ops::{Deref, DerefMut};

use iced_x86::{Code, Formatter, Instruction, IntelFormatter, NumberBase, Register};
use iced_x86::code_asm::{CodeAssembler, CodeLabel};
use rustc_hash::FxHashMap;

use crate::codegen::isa::x86_64;
use crate::codegen::isa::x86_64::{CC, PhysicalRegister, X86Instr};
use crate::codegen::machine;
use crate::codegen::machine::BasicBlockId;

pub struct Assembler {
    assembler: CodeAssembler,
    bb_to_label: FxHashMap<BasicBlockId, (CodeLabel, usize)>,
    base_addr: u64,
}

impl Assembler {
    pub fn get_or_insert_label(&mut self, bb: BasicBlockId) -> CodeLabel {
        self.bb_to_label.entry(bb).or_insert_with(|| (self.assembler.create_label(), self.assembler.instructions().len())).0
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
            PhysicalRegister::AL => Self::AL,
            PhysicalRegister::AH => Self::AH,
            PhysicalRegister::AX => Self::AX,
            PhysicalRegister::EAX => Self::EAX,
            PhysicalRegister::RAX => Self::RAX,
            PhysicalRegister::BL => Self::BL,
            PhysicalRegister::BH => Self::BH,
            PhysicalRegister::BX => Self::BX,
            PhysicalRegister::EBX => Self::EBX,
            PhysicalRegister::RBX => Self::RBX,
            PhysicalRegister::CL => Self::CL,
            PhysicalRegister::CH => Self::CH,
            PhysicalRegister::CX => Self::CX,
            PhysicalRegister::ECX => Self::ECX,
            PhysicalRegister::RCX => Self::RCX,
            PhysicalRegister::DL => Self::DL,
            PhysicalRegister::DH => Self::DH,
            PhysicalRegister::DX => Self::DX,
            PhysicalRegister::EDX => Self::EDX,
            PhysicalRegister::RDX => Self::RDX,
            PhysicalRegister::SIL => Self::SIL,
            PhysicalRegister::SI => Self::SI,
            PhysicalRegister::ESI => Self::ESI,
            PhysicalRegister::RSI => Self::RSI,
            PhysicalRegister::DIL => Self::DIL,
            PhysicalRegister::DI => Self::DI,
            PhysicalRegister::EDI => Self::EDI,
            PhysicalRegister::RDI => Self::RDI,
            PhysicalRegister::R8L => Self::R8L,
            PhysicalRegister::R8W => Self::R8W,
            PhysicalRegister::R8D => Self::R8D,
            PhysicalRegister::R8 => Self::R8,
            PhysicalRegister::R9L => Self::R9L,
            PhysicalRegister::R9W => Self::R9W,
            PhysicalRegister::R9D => Self::R9D,
            PhysicalRegister::R9 => Self::R9,
            PhysicalRegister::EFLAGS => unreachable!(),
        }
    }
}

impl machine::asm::Assembler<x86_64::Abi> for Assembler {
    fn new(base_addr: u64) -> Self {
        Self {
            bb_to_label: FxHashMap::default(),
            assembler: CodeAssembler::new(64).unwrap(),
            base_addr,
        }
    }

    fn begin_basic_block(&mut self, bb_id: machine::BasicBlockId) {
        let label = &mut self.get_or_insert_label(bb_id);
        self.assembler.set_label(label).unwrap();
    }

    fn assemble(&mut self, instr: &<x86_64::Abi as machine::Abi>::I) {
        match instr {
            X86Instr::SUB32ri { dest, immediate } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Sub_rm32_imm32,
                    dest,
                    immediate.as_encoded_dword().unwrap()
                ).unwrap()).unwrap();
            }
            X86Instr::SUB32rr { src, dest } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let src: Register = src.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Sub_rm32_r32,
                    dest,
                    src,
                ).unwrap()).unwrap();
            }
            X86Instr::ADD32ri { dest, immediate } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Add_rm32_imm32,
                    dest,
                    immediate.as_encoded_dword().unwrap(),
                ).unwrap()).unwrap();
            }
            X86Instr::ADD32rr { src, dest } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let src: Register = src.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Add_rm32_r32,
                    dest,
                    src,
                ).unwrap()).unwrap();
            }
            X86Instr::MOV8ri { dest, immediate } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm8_imm8,
                    dest,
                    immediate.as_encoded_dword().unwrap(),
                ).unwrap()).unwrap();
            }
            X86Instr::MOV8rr { src, dest } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let src: Register = src.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm8_r8,
                    dest,
                    src,
                ).unwrap()).unwrap();
            }
            X86Instr::MOV16ri { dest, immediate } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm16_imm16,
                    dest,
                    immediate.as_encoded_dword().unwrap(),
                ).unwrap()).unwrap();
            }
            X86Instr::MOV16rr { src, dest } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let src: Register = src.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm16_r16,
                    dest,
                    src,
                ).unwrap()).unwrap();
            }
            X86Instr::MOV32ri {
                dest,
                immediate
            } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm32_imm32,
                    dest,
                    immediate.as_encoded_dword().unwrap(),
                ).unwrap()).unwrap();
            }
            X86Instr::MOV32rr { src, dest } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let src: Register = src.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm32_r32,
                    dest,
                    src,
                ).unwrap()).unwrap();
            }
            X86Instr::MOV64ri {
                dest,
                immediate
            } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm64_imm32,
                    dest,
                    immediate.as_encoded_dword().expect("64-bit immediates are too large. Should be stored in memory"),
                ).unwrap()).unwrap();
            }
            X86Instr::MOV64rr { src, dest } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let src: Register = src.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Mov_rm64_r64,
                    dest,
                    src,
                ).unwrap()).unwrap();
            }
            X86Instr::RET => {
                self.ret().unwrap();
            }
            X86Instr::JMP {
                target
            } => {
                let label = self.get_or_insert_label(*target);
                self.jmp(label).unwrap();
            }
            X86Instr::CMP32rr { lhs, rhs } => {
                let lhs: Register = lhs.try_as_physical().unwrap().into();
                let rhs: Register = rhs.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Cmp_rm32_r32,
                    lhs,
                    rhs,
                ).unwrap()).unwrap();
            }
            X86Instr::CMP32ri { lhs, rhs } => {
                let lhs: Register = lhs.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Cmp_rm32_imm32,
                    lhs,
                    rhs.as_encoded_dword().unwrap(),
                ).unwrap()).unwrap();
            }
            X86Instr::CMP8ri { lhs, rhs } => {
                let lhs: Register = lhs.try_as_physical().unwrap().into();
                self.add_instruction(Instruction::with2(
                    Code::Cmp_rm8_imm8,
                    lhs,
                    rhs.as_encoded_dword().unwrap(),
                ).unwrap()).unwrap();
            }
            X86Instr::SETCC { dest, cc } => {
                let dest: Register = dest.try_as_physical().unwrap().into();
                let code: Code = match cc {
                    CC::Eq => Code::Sete_rm8,
                    CC::Gt => Code::Setg_rm8,
                };
                self.add_instruction(Instruction::with1(
                    code,
                    dest,
                ).unwrap()).unwrap();
            }
            X86Instr::JCC { target, cc } => {
                let label = self.get_or_insert_label(*target);
                match cc {
                    CC::Eq => {
                        self.je(label).unwrap();
                    }
                    CC::Gt => {
                        self.jg(label).unwrap();
                    }
                };
            }
        }
    }

    fn finish(mut self) -> Vec<u8> {
        self.assembler.assemble(self.base_addr).unwrap()
    }

    fn format(&self) -> String {
        let mut sym_map: FxHashMap<u64, String> = FxHashMap::default();
        for (bb, (_, instruction_idx)) in &self.bb_to_label {
            sym_map.insert(*instruction_idx as u64, format!("{bb}"));
        }

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

