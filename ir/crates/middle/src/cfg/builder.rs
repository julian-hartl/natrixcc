use cranelift_entity::EntityRef;

use crate::cfg::{BasicBlockId, Terminator, TerminatorKind};
use crate::function::Function;
use crate::instruction::{AllocaInstr, BinOpInstr, CmpInstr, CmpOp, Instr, InstrKind, LoadInstr, Op, OpInstr, StoreInstr, VRegData};
use crate::ty::Type;
use crate::VReg;

#[derive(Debug)]
pub struct Builder<'func> {
    func: &'func mut Function,
    current_bb: Option<BasicBlockId>,
    next_vreg: Option<VReg>,
}

impl<'func> Builder<'func> {
    pub fn new(func: &'func mut Function) -> Self {
        Self { func, current_bb: None, next_vreg: None }
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

    pub fn alloca(&mut self, ty: Type, num_elements: Option<u32>) -> VReg {
        let value = self.next_vreg(Type::Ptr(Box::new(ty.clone())));
        let alloca = AllocaInstr::new(
            value,
            num_elements,
        );
        self.add_instr(Instr::new(ty, InstrKind::Alloca(alloca)));
        value
    }

    pub fn add(&mut self, ty: Type, lhs: Op, rhs: Op) -> VReg {
        let value = self.next_vreg(ty.clone());
        let instr = BinOpInstr {
            value,
            lhs,
            rhs,
        };
        self.add_instr(Instr::new(ty, InstrKind::Add(instr)));
        value
    }

    pub fn sub(&mut self, ty: Type, lhs: Op, rhs: Op) -> VReg {
        let value = self.next_vreg(ty.clone());
        let sub = BinOpInstr {
            value,
            lhs,
            rhs,
        };
        self.add_instr(Instr::new(ty, InstrKind::Sub(sub)));
        value
    }

    pub fn store(&mut self, ty: Type, dest: VReg, value: Op) {
        let store = Instr::new(ty, InstrKind::Store(StoreInstr { value, dest }));
        self.add_instr(store);
    }

    pub fn load(&mut self, ty: Type, source: Op) -> VReg {
        let value = self.next_vreg(ty.clone());
        let load = Instr::new(ty, InstrKind::Load(LoadInstr {
            dest: value,
            source,
        }));
        self.add_instr(load);
        value
    }

    pub fn op(&mut self, ty: Type, op: Op) -> VReg {
        let value = self.next_vreg(ty.clone());
        let op_instr = OpInstr {
            value,
            op,
        };
        self.add_instr(Instr::new(ty, InstrKind::Op(op_instr)));
        value
    }

    pub fn icmp(&mut self, condition: CmpOp, op1: Op, op2: Op) -> VReg {
        let ty = Type::Bool;
        let value = self.next_vreg(ty.clone());
        self.add_instr(Instr::new(ty, InstrKind::Cmp(
            CmpInstr {
                value,
                op: condition,
                lhs: op1,
                rhs: op2,
            }
        )));
        value
    }

    pub fn add_argument(&mut self, ty: Type) -> VReg {
        let value = self.next_vreg(ty);
        let current_bb = self.current_bb();
        self.func.cfg.basic_block_mut(current_bb).add_argument(value);
        value
    }

    pub fn get_bb_arguments(&self, bb: BasicBlockId) -> &[VReg] {
        &self.func.cfg.basic_block(bb).arguments
    }

    pub fn add_instr(&mut self, instr: Instr) {
        self.func.cfg.add_instruction(self.current_bb(), instr);
    }

    /// Tells the builder to use this vreg for the next instruction
    /// instead of continuing serially.
    pub(crate) fn set_next_vreg(&mut self, vreg: VReg) {
        self.next_vreg = Some(vreg);
    }

    pub fn vreg(&self, vreg: VReg) -> &VRegData {
        &self.func.cfg.vregs[vreg]
    }

    fn next_vreg(&mut self, ty: Type) -> VReg {
        match self.next_vreg.take() {
            Some(vreg) => {
                let vreg_idx = vreg.0 as usize;
                if vreg_idx < self.func.cfg.vregs.len() {
                    let current_bb = self.current_bb();
                    let vreg = &mut self.func.cfg.vregs[vreg];
                    vreg.ty = ty;
                    vreg.defined_in = current_bb;
                } else {
                    for _ in self.func.cfg.vregs.len()..vreg_idx {
                        self.func.cfg.new_vreg(VRegData {
                            ty: Type::Void,
                            defined_in: self.current_bb(),
                        });
                    }
                    self.func.cfg.new_vreg(VRegData {
                        ty,
                        defined_in: self.current_bb(),
                    });
                }
                vreg
            }
            None => self.func.cfg.new_vreg(VRegData {
                ty,
                defined_in: self.current_bb(),
            })
        }
    }

    pub(crate) fn max_bb_id(&self) -> Option<BasicBlockId> {
        Some(BasicBlockId::new(self.func.cfg.basic_blocks.len().checked_sub(1)?))
    }

    pub fn current_bb(&self) -> BasicBlockId {
        self.current_bb.unwrap()
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg;
    use crate::cfg::{BranchTerm, CondBranchTerm, JumpTarget, RetTerm, TerminatorKind};
    use crate::instruction::{CmpOp, Const, Op};
    use crate::test::create_test_function;
    use crate::ty::Type;

    #[test]
    fn should_add_allocas_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb();

        cfg_builder.alloca(Type::I32, None);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        assert_eq!(function.cfg.to_string(), "bb0:
    v0 = alloca i32;
    ret void;
");
    }

    #[test]
    fn should_add_sub_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb();
        cfg_builder.sub(
            Type::I32,
            Op::Const(Const::Int(Type::I32, 0)),
            Op::Const(Const::Int(Type::I32, 1)),
        );
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        assert_eq!(function.cfg.to_string(), "bb0:
    v0 = sub i32 0, 1;
    ret void;
");
    }

    #[test]
    fn should_add_op_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb();
        cfg_builder.op(Type::I32, Op::Const(Const::Int(Type::I32, 0)));
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        assert_eq!(function.cfg.to_string(), "bb0:
    v0 = i32 0;
    ret void;
");
    }

    #[test]
    fn should_add_store_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb();
        let alloca_value = cfg_builder.alloca(Type::I32, None);
        cfg_builder.store(Type::I32, alloca_value, Op::Const(Const::Int(Type::I32, 0)));
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        assert_eq!("bb0:
    v0 = alloca i32;
    store i32 0, ptr v0;
    ret void;
", function.cfg.to_string());
    }

    #[test]
    fn should_add_load_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        cfg_builder.start_bb();
        let alloca_value = cfg_builder.alloca(Type::I32, None);
        cfg_builder.load(Type::I32, Op::Value(alloca_value));
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        assert_eq!("bb0:
    v0 = alloca i32;
    v1 = load i32 ptr v0;
    ret void;
", function.cfg.to_string());
    }

    #[test]
    fn should_add_conditional_branch_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = cfg::Builder::new(&mut function);
        let _bb0 = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let cmp_value = cfg_builder.icmp(CmpOp::Eq, Op::Const(Const::Int(Type::I32, 0)), Op::Const(Const::Int(Type::I32, 1)));
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
        assert_eq!("bb0:
    v0 = icmp bool eq 0, 1;
    condbr v0, bb1, bb2;
bb1:
    ret void;
bb2:
    ret void;
", function.cfg.to_string());
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
        let var_0 = cfg_builder.op(Type::I32, Op::Const(Const::Int(Type::I32, 0)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(bb3, vec![var_0.into()]))));
        cfg_builder.set_bb(bb2);
        let var_1 = cfg_builder.op(Type::I32, Op::Const(Const::Int(Type::I32, 1)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(bb3, vec![var_1.into()]))));
        cfg_builder.set_bb(bb3);
        let var_2 = cfg_builder.add_argument(Type::I32);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::new(Type::I32, var_2.into())));
        cfg_builder.set_bb(bb4);
        let var_3 = cfg_builder.op(Type::I32, Op::Const(Const::Int(Type::I32, 2)));
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::new(JumpTarget::new(bb3, vec![var_3.into()]))));
        assert_eq!("bb0:
    br bb1;
bb1:
    v0 = i32 0;
    br bb3(v0);
bb2:
    v1 = i32 1;
    br bb3(v1);
bb3(i32 v2):
    ret i32 v2;
bb4:
    v3 = i32 2;
    br bb3(v3);
", function.cfg.to_string());
    }
}
