use std::fmt::{Debug, Display};

use index_vec::IndexVec;

use crate::cfg::ValueId;
use crate::ty::Type;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Module {
    pub functions: IndexVec<Function, FunctionData>,
}

impl Module {
    pub fn write_to<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        for (_, function) in self.functions.iter_enumerated() {
            function.write_to(writer)?;
            writeln!(writer)?;
        }
        Ok(())
    }
}

mod function_builder;
mod module_builder;
mod asm;
mod register_allocator;

index_vec::define_index_type! {
    pub struct BasicBlock = usize;

    DISPLAY_FORMAT = "bb{}";
}

index_vec::define_index_type! {
    pub struct Function = usize;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionData {
    pub name: String,
    pub stack_frame: StackFrame,
    pub basic_blocks: IndexVec<BasicBlock, BasicBlockData>,
    pub registers: IndexVec<VirtualRegister, VirtualRegisterData>,
}

impl FunctionData {
    pub fn write_to<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        writeln!(writer, "function {} {{", self.name)?;
        self.stack_frame.write_to(writer, 4)?;
        for (bb_idx, bb) in self.basic_blocks.iter_enumerated() {
            writeln!(writer, "    bb{} {{", bb_idx.index())?;
            for instr in bb.instructions.iter() {
                write!(writer, "        ")?;
                instr.write_to(writer)?;
            }
            write!(writer, "        ")?;
            bb.terminator.write_to(writer)?;
            writeln!(writer, "    }}")?;
        }
        writeln!(writer, "}}")?;
        Ok(())
    }
}

index_vec::define_index_type! {
    pub struct VirtualRegister = usize;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VirtualRegisterData {
    pub ty: Type,
    pub id: ValueId,
}

pub type PhysicalRegister = iced_x86::Register;

#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumTryAs)]
pub enum Register {
    Virtual(VirtualRegister),
    Physical(PhysicalRegister),
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Virtual(virtual_register) => {
                write!(f, "%{}", virtual_register.index())
            }
            Self::Physical(physical_register) => {
                write!(f, "%")?;
                physical_register.fmt(f)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BasicBlockData {
    pub instructions: Vec<Instr>,
    pub terminator: Terminator,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instr {
    pub kind: InstrKind,
}

impl Instr {
    pub const fn new(kind: InstrKind) -> Self {
        Self {
            kind,
        }
    }

    pub fn write_to<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        match &self.kind {
            InstrKind::Mov32RI { dest, immediate } => {
                writeln!(writer, "MOV32RI {dest}, {immediate}")?;
            }
            InstrKind::Mov32MI { dest, immediate } => {
                writeln!(writer, "MOV32MI {}, {}", dest, immediate)?;
            }
            InstrKind::Mov32RM { dest, src } => {
                writeln!(writer, "MOV32RM {}, {}", dest, src)?;
            }
            InstrKind::Mov32MR { dest, src } => {
                writeln!(writer, "MOV32MR {}, {}", dest, src)?;
            }
            InstrKind::Mov32RR { dest, src } => {
                writeln!(writer, "MOV32RR {}, {}", dest, src)?;
            }
            InstrKind::Mov64RR { dest, src } => {
                writeln!(writer, "MOV64RR {}, {}", dest, src)?;
            }
            InstrKind::Sub32RR { dest, src } => {
                writeln!(writer, "SUB32RR {}, {}", dest, src)?;
            }
            InstrKind::Sub32RI { dest, immediate } => {
                writeln!(writer, "SUB32RI {}, {}", dest, immediate)?;
            }
            InstrKind::Sub64RI { dest, immediate } => {
                writeln!(writer, "SUB64RI {}, {}", dest, immediate)?;
            }
            InstrKind::Add32RI { dest, immediate } => {
                writeln!(writer, "ADD32RI {}, {}", dest, immediate)?;
            }
            InstrKind::Push64R { src } => {
                writeln!(writer, "PUSH64R {}", src)?;
            }
            InstrKind::Neg32R { src } => {
                writeln!(writer, "NEG32R {}", src)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstrKind {
    Mov32RI {
        dest: Register,
        immediate: i32,
    },
    Mov32MI {
        dest: MemoryOperand,
        immediate: i32,
    },
    Mov32RM {
        dest: Register,
        src: MemoryOperand,
    },
    Mov32MR {
        dest: MemoryOperand,
        src: Register,
    },
    Mov32RR {
        dest: Register,
        src: Register,
    },
    Mov64RR {
        dest: Register,
        src: Register,
    },
    Sub32RR {
        dest: Register,
        src: Register,
    },
    Sub32RI {
        dest: Register,
        immediate: i32,
    },
    Sub64RI {
        dest: Register,
        immediate: i32,
    },
    Add32RI {
        dest: Register,
        immediate: i32,
    },
    Push64R {
        src: Register,
    },
    Neg32R {
        src: Register,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryOperand {
    pub base: Register,
    pub displacement: i64,
    pub index: Option<(Register, u32)>,
}

impl Display for MemoryOperand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some((index, scale)) = &self.index {
            if self.displacement == 0 {
                write!(f, "[{}, {} * {}]", self.base, index, scale)
            } else {
                write!(f, "[{}, {} * {} + {}]", self.base, index, scale, self.displacement)
            }
        } else if self.displacement == 0 {
            write!(f, "[{}]", self.base)
        } else {
            write!(f, "[{} + {}]", self.base, self.displacement)
        }
    }
}

impl MemoryOperand {
    pub const fn new(base: Register, displacement: i64, index: Option<(Register, u32)>) -> Self {
        Self {
            base,
            displacement,
            index,
        }
    }

    pub const fn from_stack_frame_entry(entry: &StackFrameEntryData) -> Self {
        Self {
            base: Register::Physical(PhysicalRegister::RBP),
            displacement: entry.offset as i64,
            index: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Terminator {
    pub kind: TerminatorKind,
}

impl Terminator {
    pub fn new(kind: TerminatorKind) -> Self {
        Self { kind }
    }

    pub fn write_to<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        match &self.kind {
            TerminatorKind::Ret { value } => {
                write!(writer, "RET")?;
                if let Some(value) = value {
                    write!(writer, " {}", value)?;
                }
            }
            TerminatorKind::Jmp { target } => {
                write!(writer, "JMP {}", target)?;
            }
        }
        writeln!(writer)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TerminatorKind {
    Ret {
        value: Option<Register>,
    },
    Jmp {
        target: BasicBlock,
    },
}


index_vec::define_index_type! {
    pub struct StackFrameEntry = usize;
}

#[derive(Debug, Clone, Eq, PartialEq,Default)]
pub struct StackFrame {
    pub entries: IndexVec<StackFrameEntry, StackFrameEntryData>,
    total_size: u32,
}

impl StackFrame {
    pub fn push(&mut self, ty: Type, num_elements: u32) -> StackFrameEntry {
        let offset = self.total_size;
        self.total_size += ty.size() * num_elements;
        let entry = StackFrameEntryData {
            offset,
            ty,
            num_elements,
        };
        self.entries.push(entry)
    }

    pub fn total_size(&self) -> u32 {
        self.total_size
    }

    pub fn write_to<W: std::io::Write>(&self, writer: &mut W, indent: usize) -> std::io::Result<()> {
        let indent = " ".repeat(indent);
        writeln!(writer, "{}stack_frame: {{", indent)?;
        writeln!(writer, "{}    total_size: {}", indent, self.total_size)?;
        writeln!(writer, "{}    entries: {{", indent)?;
        for (entry_idx, entry) in self.entries.iter_enumerated() {
            writeln!(writer, "{}{}    {}: size={}, align={}, bp_offset={} ", indent,indent, entry_idx.index(), entry.size(), entry.ty.alignment(), entry.offset)?;
        }
        writeln!(writer, "{}    }}", indent)?;
        write!(writer, "{}", indent)?;
        writeln!(writer, "}}")?;
        Ok(())
    }

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackFrameEntryData {
    pub offset: u32,
    pub ty: Type,
    pub num_elements: u32,
}

impl StackFrameEntryData {
    pub fn size(&self) -> u32 {
        self.ty.size() * self.num_elements
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Size {
    Byte,
    Word,
    DWord,
    QWord,
}

impl Size {
    pub fn from_bit_width(bit_width: u32) -> Self {
        if bit_width <= 8 {
            Self::Byte
        } else if bit_width <= 16 {
            Self::Word
        } else if bit_width <= 32 {
            Self::DWord
        } else if bit_width <= 64 {
            Self::QWord
        } else {
            panic!("Invalid bit width: {}", bit_width)
        }
    }
}
