use crate::cfg::{BasicBlockId, Terminator, TerminatorKind};
use crate::function::{Function, Symbol};
use crate::instruction::{AllocaInstr, ICmpCond, ICmpInstr, Instr, InstrKind, LoadInstr, Op, OpInstr, StoreInstr, SubInstr, ValueData};
use crate::ty::Type;
use crate::Value;

#[derive(Debug)]
pub struct Builder<'func> {
    func: &'func mut Function,
    current_bb: Option<BasicBlockId>,
}

impl<'func> Builder<'func> {
    pub fn new(func: &'func mut Function) -> Self {
        Self { func, current_bb: None }
    }

    pub fn start_bb(&mut self) -> BasicBlockId {
        let bb = self.create_bb();
        self.current_bb = Some(bb);
        bb
    }

    pub fn create_bb(&mut self) -> BasicBlockId {
        self.func.cfg.new_basic_block()
    }

    pub fn set_bb(&mut self, bb: BasicBlockId) {
        self.current_bb = Some(bb);
    }

    pub fn end_bb(&mut self, terminator: TerminatorKind) {
        let current_bb = self.current_bb();
        self.func.cfg.set_terminator(current_bb, Terminator::new(terminator));
        self.current_bb = None;
    }

    pub fn alloca(&mut self, value_sym: Option<Symbol>, ty: Type, num_elements: Option<u32>, alignment: Option<u32>) -> Value {
        let value = self.push_value_for_next_instr(Type::Ptr(Box::new(ty.clone())));
        let alloca = AllocaInstr::new(
            value,
            ty,
            num_elements,
            alignment,
        );
        self.add_instr(Instr::new(InstrKind::Alloca(alloca)));
        value
    }

    pub fn sub(&mut self, value_sym: Option<Symbol>, lhs: Op, rhs: Op) -> Value {
        let value = self.push_value_for_next_instr(lhs.ty(&self.func));
        let sub = SubInstr {
            value,
            lhs,
            rhs,
        };
        self.add_instr(Instr::new(InstrKind::Sub(sub)));
        value
    }

    pub fn store(&mut self, dest: Value, value: Op) {
        let store = Instr::new(InstrKind::Store(StoreInstr { value, dest }));
        self.add_instr(store);
    }

    pub fn load(&mut self, value_sym: Option<Symbol>, source: Value) -> Value {
        let value = self.push_value_for_next_instr(self.func.values_ctx[source].ty.deref().clone());
        let load = Instr::new(InstrKind::Load(LoadInstr {
            dest: value,
            source,
        }));
        self.add_instr(load);
        value
    }

    pub fn op(&mut self, value_sym: Option<Symbol>, op: Op) -> Value {
        let value = self.push_value_for_next_instr(op.ty(self.func));
        let op_instr = OpInstr {
            value,
            op,
        };
        self.add_instr(Instr::new(InstrKind::Op(op_instr)));
        value
    }

    pub fn icmp(&mut self, value_sym: Option<Symbol>, condition: ICmpCond, op1: Op, op2: Op) -> Value {
        let value = self.push_value_for_next_instr(Type::Bool);
        self.add_instr(Instr::new(InstrKind::ICmp(
            ICmpInstr {
                value,
                condition,
                op1,
                op2,
            }
        )));
        value
    }

    pub fn add_argument(&mut self, ty: Type) -> Value {
        let value = self.func.values_ctx.push(ValueData {
            id: self.func.values_ctx.next_idx(),
            ty,
            defined_in: self.current_bb.unwrap(),
        });
        let current_bb = self.current_bb();
        self.func.cfg.basic_blocks[current_bb].arguments.push(value);
        value
    }

    pub fn add_instr(&mut self, instr: Instr) {
        self.func.cfg.add_instruction(self.current_bb(), instr);
    }

    fn push_value_for_next_instr(&mut self, ty: Type) -> Value {
        self.func.values_ctx.push(ValueData {
            id: self.func.values_ctx.next_idx(),
            ty,
            defined_in: self.current_bb(),
        })
    }

    pub fn current_bb(&self) -> BasicBlockId {
        self.current_bb.unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg;
    use crate::cfg::{BranchTerm, CondBranchTerm, JumpTarget, RetTerm, TerminatorKind};
    use crate::instruction::{Const, ICmpCond, Op};
    use crate::test::create_test_function;
    use crate::ty::Type;

    #[test]
    fn should_add_allocas_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb();

        cfg_builder.alloca(None, Type::I32, None, None);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, "bb0:
    %0 = alloca i32
    ret void
");
    }

    #[test]
    fn should_add_sub_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb();
        cfg_builder.sub(
            None,
            Op::Const(Const::i32(0)),
            Op::Const(Const::i32(1)),
        );
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, "bb0:
    %0 = sub i32 0, 1
    ret void
");
    }

    #[test]
    fn should_add_op_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb();
        cfg_builder.op(None, Op::Const(Const::i32(0)));
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, "bb0:
    %0 = i32 0
    ret void
");
    }

    #[test]
    fn should_add_store_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb();
        let alloca_value = cfg_builder.alloca(None, Type::I32, None, None);
        cfg_builder.store(alloca_value, Op::Const(Const::i32(0)));
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!("bb0:
    %0 = alloca i32
    store i32 0, ptr %0
    ret void
", cfg_out);
    }

    #[test]
    fn should_add_load_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb();
        let alloca_value = cfg_builder.alloca(None, Type::I32, None, None);
        cfg_builder.load(None, alloca_value);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!("bb0:
    %0 = alloca i32
    %1 = load i32 ptr %0
    ret void
", cfg_out);
    }

    #[test]
    fn should_add_conditional_branch_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        let _bb0 = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let cmp_value = cfg_builder.icmp(None, ICmpCond::Eq, Op::Const(Const::i32(0)), Op::Const(Const::i32(1)));
        cfg_builder.end_bb(TerminatorKind::CondBranch(
            CondBranchTerm {
                cond: Op::Value(cmp_value),
                true_target: JumpTarget::no_args(bb1),
                false_target: JumpTarget::no_args(bb2),
            }
        ));
        cfg_builder.set_bb(bb1);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        cfg_builder.set_bb(bb2);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        let mut out = String::new();
        function.cfg.write_to(&mut out, &function).unwrap();
        assert_eq!("bb0:
    %0 = icmp bool eq 0, 1
    condbr bool %0, bb1, bb2
bb1:
    ret void
bb2:
    ret void
", out);
    }
    
    #[test]
    fn should_add_basic_block_arguments() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        let _bb0 = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let bb3 = cfg_builder.create_bb();
        let bb4 = cfg_builder.create_bb();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::no_args(bb1))));
        cfg_builder.set_bb(bb1);
        let var_0 = cfg_builder.op(None, Op::Const(Const::i32(0)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(bb3, vec![var_0.into()]))));
        cfg_builder.set_bb(bb2);
        let var_1 = cfg_builder.op(None, Op::Const(Const::i32(1)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(bb3, vec![var_1.into()]))));
        cfg_builder.set_bb(bb3);
        let var_2 = cfg_builder.add_argument(Type::I32);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::new(var_2.into())));
        cfg_builder.set_bb(bb4);
        let var_3 = cfg_builder.op(None, Op::Const(Const::i32(2)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(bb3, vec![var_3.into()]))));
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!("bb0:
    br bb1
bb1:
    %0 = i32 0
    br bb3(%0)
bb2:
    %1 = i32 1
    br bb3(%0)
bb3(i32 %2):
    ret i32 %2
bb4:
    %3 = i32 2
    br bb3(%3)
", cfg_out);
    }
}
