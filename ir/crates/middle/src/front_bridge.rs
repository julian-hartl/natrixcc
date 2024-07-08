use itertools::Itertools;
use natrix_front::module::{Identifier, Instruction, Literal, Operand};
use rustc_hash::FxHashMap;

use crate::{
    cfg,
    cfg::{
        BBArgRef, BasicBlockRef, BranchTerm, CondBranchTerm, InstrRef, JumpTarget, RetTerm,
        TerminatorKind,
    },
    instruction::{CmpOp, Const, Op},
    Function, Module, Type, Value,
};

#[derive(Debug, Default)]
pub struct FrontBridge {
    bb_symbol_table: FxHashMap<Identifier, BasicBlockRef>,
    bb_arg_symbol_table: FxHashMap<Identifier, BBArgRef>,
    instr_symbol_table: FxHashMap<Identifier, InstrRef>,
}

impl FrontBridge {
    pub fn new() -> Self {
        Self {
            bb_symbol_table: FxHashMap::default(),
            bb_arg_symbol_table: FxHashMap::default(),
            instr_symbol_table: FxHashMap::default(),
        }
    }

    pub fn bridge(mut self, front_module: natrix_front::Module) -> Module {
        let mut module = Module::default();
        for function in front_module.functions {
            let function = self.bridge_function(function);
            module.functions.insert(function);
        }
        module
    }

    fn bridge_function(&mut self, front_f: natrix_front::module::Function) -> Function {
        let mut function = Function::new(
            front_f.name,
            front_f
                .args
                .into_iter()
                .map(|arg| arg.into())
                .collect::<Vec<_>>(),
            front_f.ret_ty.into(),
        );
        let mut cfg_builder = cfg::Builder::new(&mut function);
        for basic_block in &front_f.basic_blocks {
            assert!(
                self.bb_symbol_table
                    .insert(
                        basic_block.id.clone(),
                        cfg_builder.create_bb(basic_block.id.clone())
                    )
                    .is_none(),
                "Duplicate basic block id {}",
                basic_block.id
            );
        }
        for basic_block in &front_f.basic_blocks {
            let bb_id = self.bb_symbol_table[&basic_block.id];
            cfg_builder.set_bb(bb_id);
            for arg in &basic_block.args {
                let id = cfg_builder.add_argument(arg.ty.clone().into(), arg.id.clone());
                assert!(
                    self.bb_arg_symbol_table
                        .insert(arg.id.clone(), id)
                        .is_none(),
                    "Duplicate basic block argument id {}",
                    arg.id
                );
            }
        }
        for basic_block in front_f.basic_blocks {
            let bb_id = self
                .bb_symbol_table
                .get(&basic_block.id)
                .copied()
                .unwrap_or_else(|| panic!("Basic block id not found: {}", basic_block.id));
            cfg_builder.set_bb(bb_id);

            for instruction in basic_block.instructions {
                match instruction {
                    Instruction::Add(dest, ty, lhs, rhs) => {
                        let lhs = self.operand_to_op(lhs);
                        let rhs = self.operand_to_op(rhs);
                        let id = cfg_builder.add(dest.clone(), ty.into(), lhs, rhs);
                        self.insert_instr_symbol(&dest, id);
                    }
                    Instruction::Sub(dest, ty, lhs, rhs) => {
                        let lhs = self.operand_to_op(lhs);
                        let rhs = self.operand_to_op(rhs);
                        let instr_ref = cfg_builder.sub(dest.clone(), ty.into(), lhs, rhs);
                        self.insert_instr_symbol(&dest, instr_ref);
                    }
                    Instruction::Ret(op) => {
                        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm {
                            value: op.map(|op| self.operand_to_op(op)),
                        }));
                    }
                    Instruction::Op(dest, ty, op) => {
                        let instr_ref =
                            cfg_builder.op(dest.clone(), ty.into(), self.operand_to_op(op));
                        self.insert_instr_symbol(&dest, instr_ref);
                    }
                    Instruction::Condbr(condition, true_target, false_target) => {
                        let cond = self.operand_to_op(condition);
                        let true_target = self.map_target(true_target, &mut cfg_builder);
                        let false_target = self.map_target(false_target, &mut cfg_builder);
                        cfg_builder.end_bb(TerminatorKind::CondBranch(CondBranchTerm::new(
                            cond,
                            true_target,
                            false_target,
                        )));
                    }
                    Instruction::Br(target) => {
                        let target = self.map_target(target, &mut cfg_builder);
                        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(target)));
                    }
                    Instruction::Cmp(dest, op, ty, lhs, rhs) => {
                        let lhs = self.operand_to_op(lhs);
                        let rhs = self.operand_to_op(rhs);
                        let instr_ref = cfg_builder.icmp(dest.clone(), op.into(), lhs, rhs);
                        self.insert_instr_symbol(&dest, instr_ref);
                    }
                }
            }
        }
        function
    }

    fn insert_instr_symbol(&mut self, id: &Identifier, instr_id: InstrRef) {
        assert!(
            self.instr_symbol_table
                .insert(id.clone(), instr_id)
                .is_none(),
            "Duplicate instruction id {}",
            id
        );
    }

    fn map_target(
        &mut self,
        target: natrix_front::module::Target,
        builder: &mut cfg::Builder,
    ) -> JumpTarget {
        let target_bb_ref = self.bb_symbol_table[&target.0];
        JumpTarget::new(
            target_bb_ref,
            target
                .1
                .map(|args| {
                    let bb_args = builder.bb_arguments(target_bb_ref).collect_vec();
                    args.into_iter()
                        .enumerate()
                        .map(|(i, arg_op)| self.operand_to_op(arg_op))
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default(),
        )
    }

    fn operand_to_op(&self, operand: Operand) -> Op {
        match operand {
            Operand::Literal(literal) => Op::Const(match literal {
                Literal::Int(value, ty) => Const::Int(ty.into(), value),
                Literal::Bool(value) =>
                // todo: Migrate to Const::Bool
                {
                    Const::Int(Type::Bool, value as i64)
                }
            }),
            Operand::Value(value) => {
                Op::Value(match self.instr_symbol_table.get(&value).copied() {
                    None => self
                        .bb_arg_symbol_table
                        .get(&value)
                        .copied()
                        .map(Value::BBArg)
                        .unwrap_or_else(|| panic!("Variable not defined: {}", value)),
                    Some(instr_ref) => Value::Instr(instr_ref),
                })
            }
        }
    }
}

impl From<natrix_front::module::Type> for Type {
    fn from(value: natrix_front::module::Type) -> Self {
        match value {
            natrix_front::module::Type::U8 => Self::U8,
            natrix_front::module::Type::U16 => Self::U16,
            natrix_front::module::Type::U32 => Self::U32,
            natrix_front::module::Type::U64 => Self::U64,
            natrix_front::module::Type::I8 => Self::I8,
            natrix_front::module::Type::I16 => Self::I16,
            natrix_front::module::Type::I32 => Self::I32,
            natrix_front::module::Type::I64 => Self::I64,
            natrix_front::module::Type::Void => Self::Void,
            natrix_front::module::Type::Bool => Self::Bool,
            natrix_front::module::Type::Ptr(ty) => Self::Ptr(Box::new((*ty).into())),
        }
    }
}

impl From<natrix_front::Module> for Module {
    fn from(value: natrix_front::Module) -> Self {
        FrontBridge::new().bridge(value)
    }
}

impl From<natrix_front::module::CmpOp> for CmpOp {
    fn from(value: natrix_front::module::CmpOp) -> Self {
        match value {
            natrix_front::module::CmpOp::Eq => Self::Eq,
            natrix_front::module::CmpOp::Gt => Self::Gt,
        }
    }
}
