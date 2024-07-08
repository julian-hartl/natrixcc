use crate::{
    cfg::{BBArgRef, BasicBlockRef, InstrRef, TerminatorKind},
    function::Function,
    instruction::{
        AllocaInstr, BinOpInstr, CmpInstr, CmpOp, InstrKind, LoadInstr, Op, OpInstr, StoreInstr,
    },
    ty::Type,
    Value,
};

#[derive(Debug)]
pub struct Builder<'func> {
    pub func: &'func mut Function,
    current_bb: Option<BasicBlockRef>,
}

impl<'func> Builder<'func> {
    pub fn new(func: &'func mut Function) -> Self {
        Self {
            func,
            current_bb: None,
        }
    }

    pub fn start_bb(&mut self, symbol: String) -> BasicBlockRef {
        let bb = self.create_bb(symbol);
        self.current_bb = Some(bb);
        bb
    }

    pub fn create_bb(&mut self, symbol: String) -> BasicBlockRef {
        self.func.cfg.new_basic_block(symbol)
    }

    pub fn set_bb(&mut self, bb: BasicBlockRef) {
        self.current_bb = Some(bb);
    }

    pub fn end_bb(&mut self, terminator: TerminatorKind) {
        let current_bb = self.current_bb();
        self.func.cfg.set_terminator(current_bb, terminator);
        self.current_bb = None;
    }

    pub fn alloca(&mut self, symbol: String, ty: Type, num_elements: Option<u32>) -> InstrRef {
        let alloca = AllocaInstr::new(ty.clone(), num_elements);
        self.add_instr(Type::Ptr(Box::new(ty)), InstrKind::Alloca(alloca), symbol)
    }

    pub fn add(&mut self, symbol: String, ty: Type, lhs: Op, rhs: Op) -> InstrRef {
        let instr = BinOpInstr { lhs, rhs };
        self.add_instr(ty, InstrKind::Add(instr), symbol)
    }

    pub fn sub(&mut self, symbol: String, ty: Type, lhs: Op, rhs: Op) -> InstrRef {
        let sub = BinOpInstr { lhs, rhs };
        self.add_instr(ty, InstrKind::Sub(sub), symbol)
    }

    pub fn store(&mut self, symbol: String, dest: Value, value: Op) {
        let store = InstrKind::Store(StoreInstr { value, dest });
        self.add_instr(Type::Void, store, symbol);
    }

    pub fn load(&mut self, symbol: String, ty: Type, source: Op) -> InstrRef {
        let load = InstrKind::Load(LoadInstr { source });
        self.add_instr(ty, load, symbol)
    }

    pub fn op(&mut self, symbol: String, ty: Type, op: Op) -> InstrRef {
        let op_instr = OpInstr { op };
        self.add_instr(ty, InstrKind::Op(op_instr), symbol)
    }

    pub fn icmp(&mut self, symbol: String, condition: CmpOp, op1: Op, op2: Op) -> InstrRef {
        self.add_instr(
            Type::Bool,
            InstrKind::Cmp(CmpInstr {
                op: condition,
                lhs: op1,
                rhs: op2,
            }),
            symbol,
        )
    }

    pub fn add_argument(&mut self, ty: Type, symbol: String) -> BBArgRef {
        let current_bb = self.current_bb();
        self.func.cfg.add_bb_argument(current_bb, ty, symbol)
    }

    pub fn bb_arguments(&self, bb_ref: BasicBlockRef) -> impl Iterator<Item = BBArgRef> + '_ {
        self.func.cfg.basic_blocks[bb_ref].arguments.iter().copied()
    }

    pub fn add_instr(&mut self, ty: Type, instr_kind: InstrKind, symbol: String) -> InstrRef {
        self.func
            .cfg
            .add_instruction(self.current_bb(), ty, instr_kind, symbol)
    }

    pub fn current_bb(&self) -> BasicBlockRef {
        self.current_bb.unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        cfg,
        cfg::{BranchTerm, CondBranchTerm, JumpTarget, RetTerm, TerminatorKind},
        instruction::{CmpOp, Const, Op},
        test::create_test_function,
        ty::Type,
    };

    #[test]
    fn should_add_allocas_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb("bb0".into());

        cfg_builder.alloca("v0".into(), Type::I32, None);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        assert_eq!(
            function.cfg.to_string(),
            "bb0:
    &i32 %v0 = alloca i32;
    ret;
"
        );
    }

    #[test]
    fn should_add_sub_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb("bb0".into());
        cfg_builder.sub(
            "v0".into(),
            Type::I32,
            Op::Const(Const::Int(Type::I32, 0)),
            Op::Const(Const::Int(Type::I32, 1)),
        );
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        assert_eq!(
            function.cfg.to_string(),
            "bb0:
    i32 %v0 = sub 0i32, 1i32;
    ret;
"
        );
    }

    #[test]
    fn should_add_op_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb("bb0".into());
        cfg_builder.op("v0".into(), Type::I32, Op::Const(Const::Int(Type::I32, 0)));
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        assert_eq!(
            function.cfg.to_string(),
            "bb0:
    i32 %v0 = 0i32;
    ret;
"
        );
    }

    #[test]
    fn should_add_store_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb("bb0".into());
        let alloca_value = cfg_builder.alloca("v0".into(), Type::I32, None);
        cfg_builder.store(
            "t".into(),
            alloca_value.into(),
            Op::Const(Const::Int(Type::I32, 0)),
        );
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        assert_eq!(
            "bb0:
    &i32 %v0 = alloca i32;
    void %t = store 0i32, %v0;
    ret;
",
            function.cfg.to_string()
        );
    }

    #[test]
    fn should_add_load_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb("bb0".into());
        let alloca_value = cfg_builder.alloca("v0".into(), Type::I32, None);
        cfg_builder.load("v1".into(), Type::I32, Op::Value(alloca_value.into()));
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        assert_eq!(
            "bb0:
    &i32 %v0 = alloca i32;
    i32 %v1 = load %v0;
    ret;
",
            function.cfg.to_string()
        );
    }

    #[test]
    fn should_add_conditional_branch_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        let _bb0 = cfg_builder.start_bb("bb0".into());
        let bb1 = cfg_builder.create_bb("bb1".into());
        let bb2 = cfg_builder.create_bb("bb2".into());
        let cmp_value = cfg_builder.icmp(
            "v0".into(),
            CmpOp::Eq,
            Op::Const(Const::Int(Type::I32, 0)),
            Op::Const(Const::Int(Type::I32, 1)),
        );
        cfg_builder.end_bb(TerminatorKind::CondBranch(CondBranchTerm {
            cond: Op::Value(cmp_value.into()),
            true_target: JumpTarget::no_args(bb1),
            false_target: JumpTarget::no_args(bb2),
        }));
        cfg_builder.set_bb(bb1);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        cfg_builder.set_bb(bb2);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        assert_eq!(
            "bb0:
    bool %v0 = cmp eq 0i32, 1i32;
    condbr %v0, bb1, bb2;
bb1:
    ret;
bb2:
    ret;
",
            function.cfg.to_string()
        );
    }

    #[test]
    fn should_add_basic_block_arguments() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        let _bb0 = cfg_builder.start_bb("bb0".into());
        let bb1 = cfg_builder.create_bb("bb1".into());
        let bb2 = cfg_builder.create_bb("bb2".into());
        let bb3 = cfg_builder.create_bb("bb3".into());
        let bb4 = cfg_builder.create_bb("bb4".into());
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(
            JumpTarget::no_args(bb1),
        )));
        cfg_builder.set_bb(bb1);
        let var_0 = cfg_builder.op("v0".into(), Type::I32, Op::Const(Const::Int(Type::I32, 0)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(
            bb3,
            vec![Op::Value(var_0.into())],
        ))));
        cfg_builder.set_bb(bb2);
        let var_1 = cfg_builder.op("v1".into(), Type::I32, Op::Const(Const::Int(Type::I32, 1)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(
            bb3,
            vec![Op::Value(var_1.into())],
        ))));
        cfg_builder.set_bb(bb3);
        let var_2 = cfg_builder.add_argument(Type::I32, "v2".into());
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::new(Op::Value(var_2.into()))));
        cfg_builder.set_bb(bb4);
        let var_3 = cfg_builder.op("v3".into(), Type::I32, Op::Const(Const::Int(Type::I32, 2)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(
            bb3,
            vec![Op::Value(var_3.into())],
        ))));
        assert_eq!(
            "bb0:
    br bb1;
bb1:
    i32 %v0 = 0i32;
    br bb3(%v0);
bb2:
    i32 %v1 = 1i32;
    br bb3(%v1);
bb3(i32 %v2):
    ret %v2;
bb4:
    i32 %v3 = 2i32;
    br bb3(%v3);
",
            function.cfg.to_string()
        );
    }
}
