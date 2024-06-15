use natrix_front::module::{
    Instruction,
    Literal,
    Operand,
    RegId,
};

use crate::{
    cfg,
    cfg::{
        BasicBlockId,
        BranchTerm,
        Builder,
        CondBranchTerm,
        JumpTarget,
        RetTerm,
        TerminatorKind,
    },
    instruction::{
        CmpOp,
        Const,
        Op,
    },
    Function,
    Module,
    Type,
    VReg,
};

pub struct FrontBridge {}

impl FrontBridge {
    pub fn new() -> Self {
        Self {}
    }

    pub fn bridge(mut self, front_module: natrix_front::Module) -> Module {
        let mut module = Module::default();
        for function in front_module.functions {
            let function = self.bridge_function(function);
            module.functions.push(function);
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
            let bb_id = basic_block.id.into();
            Self::ensure_bb_exists(&mut cfg_builder, bb_id);
            cfg_builder.set_bb(bb_id);
            for arg in &basic_block.args {
                cfg_builder.set_next_vreg(arg.id.into());
                cfg_builder.add_argument(arg.ty.into());
            }
        }
        for basic_block in front_f.basic_blocks {
            let bb_id = basic_block.id.into();
            cfg_builder.set_bb(bb_id);

            for instruction in basic_block.instructions {
                match instruction {
                    Instruction::Add(dest, ty, lhs, rhs) => {
                        let lhs = self.operand_to_op(lhs, ty.into());
                        let rhs = self.operand_to_op(rhs, ty.into());
                        cfg_builder.set_next_vreg(dest.into());
                        cfg_builder.add(ty.into(), lhs, rhs);
                    }
                    Instruction::Sub(dest, ty, lhs, rhs) => {
                        let lhs = self.operand_to_op(lhs, ty.into());
                        let rhs = self.operand_to_op(rhs, ty.into());
                        cfg_builder.set_next_vreg(dest.into());
                        cfg_builder.sub(ty.into(), lhs, rhs);
                    }
                    Instruction::Ret(ty, op) => {
                        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm {
                            ty: ty.into(),
                            value: op.map(|op| self.operand_to_op(op, ty.into())),
                        }));
                    }
                    Instruction::Op(dest, ty, op) => {
                        cfg_builder.set_next_vreg(dest.into());
                        cfg_builder.op(ty.into(), self.operand_to_op(op, ty.into()));
                    }
                    Instruction::Condbr(condition, true_target, false_target) => {
                        let cond = self.operand_to_op(condition, Type::Bool);
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
                    Instruction::ICmp(dest, op, ty, lhs, rhs) => {
                        let lhs = self.operand_to_op(lhs, ty.into());
                        let rhs = self.operand_to_op(rhs, ty.into());
                        cfg_builder.set_next_vreg(dest.into());
                        cfg_builder.icmp(op.into(), lhs, rhs);
                    }
                }
            }
        }
        function
    }

    fn map_target(
        &mut self,
        target: natrix_front::module::Target,
        builder: &mut cfg::Builder,
    ) -> JumpTarget {
        let target_bb_id = target.0.into();
        JumpTarget::new(
            target_bb_id,
            target
                .1
                .map(|args| {
                    let bb_args = builder.get_bb_arguments(target_bb_id);
                    args.into_iter()
                        .enumerate()
                        .map(|(i, arg_op)| {
                            self.operand_to_op(arg_op, builder.vreg(bb_args[i]).ty.clone())
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default(),
        )
    }

    fn ensure_bb_exists(builder: &mut Builder, bb_id: BasicBlockId) {
        while builder
            .max_bb_id()
            .map(|max_bb_id| max_bb_id < bb_id)
            .unwrap_or(true)
        {
            builder.create_empty_bb();
        }
        builder.ensure_exists(bb_id);
    }

    fn operand_to_op(&self, operand: Operand, context_ty: Type) -> Op {
        match operand {
            Operand::Literal(literal) => Op::Const(match literal {
                Literal::Int(value) => Const::Int(context_ty, value),
            }),
            Operand::Register(reg) => Op::Vreg(reg.into()),
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
        }
    }
}

impl From<natrix_front::Module> for Module {
    fn from(value: natrix_front::Module) -> Self {
        FrontBridge::new().bridge(value)
    }
}

impl From<natrix_front::module::BasicBlockId> for BasicBlockId {
    fn from(value: natrix_front::module::BasicBlockId) -> Self {
        Self::from_u32(value.0)
    }
}

impl From<RegId> for VReg {
    fn from(value: RegId) -> Self {
        Self::from_u32(value.0)
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
