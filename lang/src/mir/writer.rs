use std::fmt::Write;

use anyhow::Result;

use fusion_compiler::{Idx, IdxVec};

use crate::mir::{BasicBlockIdx, Function, FunctionIdx, Instruction, InstructionIdx, InstructionKind, MIR, Terminator, TerminatorKind, Type, Value};
use crate::mir::basic_block::BasicBlock;

pub struct MIRWriter<W> {
    _phantom: std::marker::PhantomData<W>,
}

impl<W> MIRWriter<W> where W: Write {
    pub fn write_graphviz_representation(writer: &mut W, ir: &MIR) -> Result<()> {
        writeln!(writer, "digraph {{")?;
        for function in ir.functions.iter() {
            writeln!(writer, "    subgraph cluster_{} {{", function.name)?;
            writeln!(writer, "        label = \"{}\";", function.name)?;
            for bb_idx in function.basic_blocks.iter().copied() {
                let bb = ir.basic_blocks.get_or_panic(bb_idx);
                let mut bb_content = String::new();
                MIRWriter::write_basic_block(&mut bb_content, &ir, function, bb_idx, &bb)?;
                writeln!(writer, "        {} [label=\"{}\"];", Self::format_bb_idx(bb_idx), bb_content)?;
                match &bb.terminator.as_ref().unwrap().kind {
                    TerminatorKind::Jump(target) => {
                        writeln!(writer, "        {} -> {};", Self::format_bb_idx(bb_idx), Self::format_bb_idx(*target))?;
                    }
                    TerminatorKind::SwitchInt { value, cases, default } => {
                        writeln!(writer, "        {} -> {};", Self::format_bb_idx(bb_idx), Self::format_bb_idx(*default))?;
                        for (case_value, case_target) in cases.iter() {
                            writeln!(writer, "        {} -> {};", Self::format_bb_idx(bb_idx), Self::format_bb_idx(*case_target))?;
                        }
                    }
                    TerminatorKind::Return { .. } => {}
                    TerminatorKind::Unresolved => {}
                }
            }
            writeln!(writer, "    }}")?;
        }
        writeln!(writer, "}}")?;
        Ok(())
    }
    pub fn write_text_representation(writer: &mut W, ir: &MIR) -> Result<()> {
        for function in ir.functions.iter() {
            writeln!(writer, "func {}:", function.name)?;
            for bb_idx in function.basic_blocks.iter().copied() {
                let bb = ir.basic_blocks.get_or_panic(bb_idx);
                Self::write_basic_block(writer, &ir, function, bb_idx, &bb)?;
            }
        }
        Ok(())
    }

    fn write_basic_block(writer: &mut W, ir: &MIR, function: &Function, bb_idx: BasicBlockIdx, bb: &&BasicBlock) -> Result<()> {
        writeln!(writer, "{}:", Self::format_bb_idx(bb_idx))?;
        let indentation = "    ";
        for instruction_idx in &bb.instructions {
            let instruction = function.instructions.get(*instruction_idx);
            write!(writer, "{}", indentation)?;
            if !matches!(instruction.ty, Type::Void) {
                write!(writer, "{} = ", Self::format_instruction_idx(*instruction_idx))?;
            }
            Self::write_instruction(writer, &ir.functions, instruction)?;
            writeln!(writer)?;
        }
        write!(writer, "{}", indentation)?;
        Self::write_terminator(writer, &ir.functions, bb.terminator.as_ref().unwrap())?;
        writeln!(writer)?;
        Ok(())
    }

    fn write_instruction(
        writer: &mut W,
        functions: &IdxVec<FunctionIdx, Function>,
        instruction: &Instruction,
    ) -> Result<()> {
        match &instruction.kind {
            InstructionKind::Binary { operator, lhs, rhs } => {
                write!(writer, "{}", operator)?;
                write!(writer, " ")?;
                Self::write_value(writer, lhs)?;
                writer.write_str(", ")?;
                Self::write_value(writer, rhs)?;
            }
            InstructionKind::Unary { operator, operand } => {
                write!(writer, "{}", operator)?;
                write!(writer, " ")?;
                Self::write_value(writer, operand)?;
            }
            InstructionKind::Value(value) => {
                Self::write_value(writer, value)?;
            }
            InstructionKind::Call { function_idx, arguments } => {
                let function = functions.get(*function_idx);
                write!(writer, "{}(", function.name)?;
                for (arg_idx, arg) in arguments.iter().enumerate() {
                    Self::write_value(writer, arg)?;
                    if arg_idx != arguments.len() - 1 {
                        write!(writer, ", ")?;
                    }
                }
                write!(writer, ")")?;
            }
            InstructionKind::Phi(phi) => {
                write!(writer, "phi {{ ")?;
                for (idx, (from, instruction_ref)) in phi.iter().enumerate() {
                    write!(writer, "{} -> {}", from, Self::format_instruction_idx(*instruction_ref))?;
                    if idx != phi.len() - 1 {
                        write!(writer, ", ")?;
                    }
                }
                write!(writer, " }}")?;
            }
        }
        Ok(())
    }

    fn write_terminator(
        writer: &mut W,
        functions: &IdxVec<FunctionIdx, Function>,
        terminator: &Terminator,
    ) -> Result<()> {
        match &terminator.kind {
            TerminatorKind::Return { value } => {
                write!(writer, "return ")?;
                Self::write_value(writer, value)?;
            }
            TerminatorKind::Jump(target) => {
                write!(writer, "jump {}", target)?;
            }
            TerminatorKind::SwitchInt { value, cases, default } => {
                write!(writer, "switchInt (")?;
                Self::write_value(writer, value)?;
                writeln!(writer, ") {{")?;
                for (case_value, case_target) in cases.iter() {
                    write!(writer, "    case {}: {}", case_value, case_target)?;
                    writeln!(writer)?;
                }
                write!(writer, "    default: {}", default)?;
                writeln!(writer)?;
                write!(writer, "}}")?;
            }
            TerminatorKind::Unresolved => {
                write!(writer, "unresolved")?;
            }
        }
        Ok(())
    }


    fn write_value(writer: &mut W, value: &Value) -> Result<()> {
        match value {
            Value::InstructionRef(instruction_idx) => {
                write!(writer, "{}", Self::format_instruction_idx(*instruction_idx))?;
            }
            Value::ConstantInt(value) => {
                write!(writer, "{}", value)?;
            }
            Value::Void => {
                write!(writer, "()")?;
            }
            Value::ParameterRef(parameter_idx) => {
                write!(writer, "${}", parameter_idx)?;
            }
        }
        Ok(())
    }


    fn format_instruction_idx(instruction_idx: InstructionIdx) -> String {
        format!("%{}", instruction_idx.as_index())
    }


    fn format_bb_idx(bb_idx: BasicBlockIdx) -> String {
        format!("bb{}", bb_idx.as_index())
    }
}
