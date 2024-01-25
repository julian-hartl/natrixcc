use std::collections::HashSet;
use std::ops::Deref;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::cfg::{BasicBlock, BasicBlockData, RetTerm, Terminator, TerminatorKind};
use crate::cfg::ValueId;
use crate::function::FunctionData;
use crate::instruction::{AllocaInstr, ICmpCond, ICmpInstr, Incoming, Instr, InstrData, InstrKind, LoadInstr, Op, OpInstr, PhiInstr, PlaceKind, StoreInstr, SubInstr, Value, ValueData};
use crate::ty::Type;

#[derive(Debug)]
pub struct CFGBuilder<'func> {
    pub func: &'func mut FunctionData,
    current_bb: Option<BasicBlock>,
    next_unnamed_id: usize,
    current_instr: Option<InstrData>,
    incomplete_phi_nodes: FxHashMap<BasicBlock, Vec<(Instr, ValueId)>>,
    sealed_blocks: FxHashSet<BasicBlock>,
}

impl<'func> CFGBuilder<'func> {
    pub fn new(func: &'func mut FunctionData) -> Self {
        Self { func, current_bb: None, next_unnamed_id: 0, current_instr: None, incomplete_phi_nodes: FxHashMap::default() , sealed_blocks: FxHashSet::default() }
    }

    pub fn start_bb(&mut self) -> BasicBlock {
        let bb = self.create_bb();
        self.current_bb = Some(bb);
        bb
    }

    pub fn create_bb(&mut self) -> BasicBlock {
        self.func.cfg.basic_blocks.push(Some(BasicBlockData {
            idx: self.func.cfg.basic_blocks.next_idx(),
            instructions: Vec::new(),
            terminator: Terminator::new(TerminatorKind::Ret(RetTerm::empty())),
        }))
    }

    pub fn set_bb(&mut self, bb: BasicBlock) {
        self.current_bb = Some(bb);
    }

    pub fn end_bb(&mut self, terminator: TerminatorKind) {
        let bb = self.func.cfg.basic_blocks[self.current_bb.unwrap()].as_mut().unwrap();
        bb.terminator = Terminator::new(terminator);
        self.current_bb = None;
    }

    pub fn alloca(&mut self, place_id: Option<ValueId>, ty: Type, num_elements: Option<u32>, alignment: Option<u32>) -> Result<(Value, Instr), String> {
        let place_id = place_id.unwrap_or_else(|| self.next_unnamed_id());
        let place = self.push_value_for_next_instr(place_id, Type::Ptr(Box::new(ty.clone())));
        let alloca = AllocaInstr::new(
            place,
            ty,
            num_elements,
            alignment,
        );
        self.add_instr(InstrData::new(InstrKind::Alloca(alloca))).map(|instr| (place, instr))
    }

    pub fn sub(&mut self, place_id: Option<ValueId>, lhs: Op, rhs: Op) -> Result<(Value, Instr), String> {
        let place_id = place_id.unwrap_or_else(|| self.next_unnamed_id());
        let value = self.push_value_for_next_instr(place_id, lhs.ty(&self.func));
        let sub = SubInstr {
            value,
            lhs,
            rhs,
        };
        self.add_instr(InstrData::new(InstrKind::Sub(sub))).map(|instr| (value, instr))
    }

    pub fn store(&mut self, place: Value, value: Op) -> Result<Instr, String> {
        let store = InstrData::new(InstrKind::Store(StoreInstr { value, pointer: place }));
        self.add_instr(store)
    }

    pub fn load(&mut self, place_id: Option<ValueId>, source: Value) -> Result<Instr, String> {
        let place_id = place_id.unwrap_or_else(|| self.next_unnamed_id());
        let place = self.push_value_for_next_instr(place_id, self.func.cfg.values_ctx[source].ty.deref().clone());
        let load = InstrData::new(InstrKind::Load(LoadInstr {
            value: place,
            source,
        }));
        self.add_instr(load)
    }

    pub fn op(&mut self, place_id: Option<ValueId>, op: Op) -> Result<(Value, Instr), String> {
        let place_id = place_id.unwrap_or_else(|| self.next_unnamed_id());
        let place = self.push_value_for_next_instr(place_id, op.ty(self.func));
        let op_instr = OpInstr {
            value: place,
            op,
        };
        self.add_instr(InstrData::new(InstrKind::Op(op_instr))).map(|instr| (place, instr))
    }

    pub fn phi(&mut self, place_id: Option<ValueId>, incoming: Vec<Incoming>, ty: Type) -> Result<(Value, Instr), String> {
        let place_id = place_id.unwrap_or_else(|| self.next_unnamed_id());
        let place = self.push_value_for_next_instr(place_id, ty);
        self.add_instr(InstrData::new(InstrKind::Phi(
            PhiInstr {
                value: place,
                incoming,
            }
        ))).map(|instr| (place, instr))
    }

    pub fn icmp(&mut self, place_id: Option<ValueId>, condition: ICmpCond, op1: Op, op2: Op) -> Result<(Value, Instr), String> {
        let place_id = place_id.unwrap_or_else(|| self.next_unnamed_id());
        let place = self.push_value_for_next_instr(place_id, Type::I1);
        self.add_instr(InstrData::new(InstrKind::ICmp(
            ICmpInstr {
                value: place,
                condition,
                op1,
                op2,
            }
        ))).map(|instr| (place, instr))
    }

    fn push_value_for_next_instr(&mut self, place_id: ValueId, ty: Type) -> Value {
        let instr = self.func.cfg.instructions.next_idx();
        let mut current_version = None;
        for place in self.func.cfg.values_ctx.iter_mut().filter(|place| place.id == place_id) {
            match place.version {
                None => {
                    place.version = Some(0);
                    current_version = Some(0);
                }
                Some(version) => {
                    current_version = current_version.map_or(Some(version), |current_version_value| if current_version_value < version {
                        Some(version)
                    } else {
                        current_version
                    });
                }
            }
        }
         self.func.cfg.values_ctx.push(ValueData {
            id: place_id,
            ty,
            defined_in: self.current_bb(),
            version: current_version.map(|version| version + 1),
        })
    }

    pub fn add_instr(&mut self, instr: InstrData) -> Result<Instr, String> {
        assert!(self.current_instr.is_none(), "current_instr should be None");
        assert!(self.current_bb.is_some(), "current_bb should be Some");
        instr.validate(self.func)?;
        let instr = self.func.cfg.instructions.push(instr);
        self.func.cfg.basic_blocks[self.current_bb.unwrap()].as_mut().unwrap().instructions.push(instr);
        Ok(instr)
    }

    pub fn next_unnamed_id(&mut self) -> ValueId {
        let id = self.next_unnamed_id;
        self.next_unnamed_id += 1;
        ValueId::Unnamed(id)
    }

    fn revert_unnamed_id(&mut self) {
        self.next_unnamed_id -= 1;
    }

    pub fn find_or_insert_reaching_value(&mut self, id: &ValueId) -> Option<Value> {
        let current_bb = self.current_bb();
        if self.is_block_sealed(current_bb) {
            let reaching_definitions = self.find_reaching_definitions(id, current_bb);
            match reaching_definitions.len() {
                0 => None,
                1 => Some(reaching_definitions[0].1),
                _ => {
                    let (phi_value, _) = self.phi(Some(id.clone()), reaching_definitions.into_incoming(), self.func.cfg.value_id_ty(id).unwrap()).unwrap();
                    Some(phi_value)
                }
            }
        } else {
            let (phi_value, phi_instr) = self.phi(Some(id.clone()), vec![], self.func.cfg.value_id_ty(id).unwrap()).unwrap();
            self.incomplete_phi_nodes.entry(current_bb).or_default().push((phi_instr, id.clone()));
            Some(phi_value)
        }
    }

    pub fn seal_block(&mut self, bb: BasicBlock) -> bool{
        if self.is_block_sealed(bb) {
            return false;
        }
        self.sealed_blocks.insert(bb);
        let incomplete_phis = self.incomplete_phi_nodes.remove(&bb);
        if let Some(incomplete_phis) = incomplete_phis {
            for (phi_instr, id) in incomplete_phis {
                let reaching_definitions = self.find_reaching_definitions_in_preds(&id, bb);
                let phi_instr = self.func.cfg.instructions[phi_instr].kind.try_as_phi_mut().unwrap();
                phi_instr.incoming = reaching_definitions.into_incoming();
            }
        }
        true
    }

    pub fn is_block_sealed(&self, bb: BasicBlock) -> bool {
        self.sealed_blocks.contains(&bb)
    }

    pub fn find_current_reaching_definitions(&self, id: &ValueId) -> ReachingDefinitions {
        let mut visited = HashSet::new();
        self.find_reaching_definitions_recursively(id, self.current_bb(), &mut visited)
    }


    pub fn find_reaching_definitions(&self, id: &ValueId, bb: BasicBlock) -> ReachingDefinitions {
        let mut visited = HashSet::new();
        self.find_reaching_definitions_recursively(id, bb, &mut visited)
    }

    fn find_reaching_definitions_recursively(&self, id: &ValueId, bb: BasicBlock, visited: &mut HashSet<BasicBlock>) -> ReachingDefinitions {
        let current_bb = self.func.cfg.basic_blocks[bb].as_ref().unwrap();
        for declaration in current_bb.declarations(&self.func.cfg).iter().copied() {
            let value = &self.func.cfg.values_ctx[declaration];
            if &value.id == id {
                return ReachingDefinitions(vec![(bb, declaration)]);
            }
        }
        self.find_reaching_definitions_in_preds_recursive(id, bb, visited)
    }

    pub fn find_reaching_definitions_in_preds(&self, id: &ValueId, bb: BasicBlock) -> ReachingDefinitions {
        let mut visited = HashSet::new();
        self.find_reaching_definitions_in_preds_recursive(id, bb, &mut visited)
    }

    fn find_reaching_definitions_in_preds_recursive(&self, id: &ValueId, bb: BasicBlock, visited: &mut HashSet<BasicBlock>) -> ReachingDefinitions {
        visited.insert(bb);
        let current_bb = self.func.cfg.basic_blocks[bb].as_ref().unwrap();
        let predecessors = current_bb.predecessors(&self.func.cfg);
        let mut reaching_values = Vec::new();
        for predecessor in predecessors {
            if visited.contains(&predecessor) {
                continue;
            }
            let predecessor_dominating_places = self.find_reaching_definitions_recursively(id, predecessor, visited);
            reaching_values.extend(predecessor_dominating_places.0.into_iter());
        }
        ReachingDefinitions(reaching_values)
    }

    pub fn current_bb(&self) -> BasicBlock {
        self.current_bb.unwrap()
    }
}

pub struct ReachingDefinitions(Vec<(BasicBlock, Value)>);

impl ReachingDefinitions {
    pub fn into_incoming(self) -> Vec<Incoming> {
        self.0.into_iter().map(|(bb, place)| Incoming { source: bb, op: Op::Value(place) }).collect()
    }
}

impl Deref for ReachingDefinitions {
    type Target = Vec<(BasicBlock, Value)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Drop for CFGBuilder<'_> {
    fn drop(&mut self) {
        assert!(self.current_bb.is_none(), "current_bb should be None");
        assert!(self.current_instr.is_none(), "current_instr should be None");
        for bb in self.func.cfg.basic_blocks.indices() {
           if !self.is_block_sealed(bb) {
               self.seal_block(bb);
           }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::{BranchTerm, CondBrTerm, RetTerm, TerminatorKind, UnCondBrTerm};
    use crate::cfg::ValueId;
    use crate::cfg_builder::CFGBuilder;
    use crate::instruction::{Const, ICmpCond, Op, Value};
    use crate::test_utils::create_test_function;
    use crate::ty::Type;

    #[test]
    fn should_use_unnamed_id_counter_by_default() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        assert_eq!(cfg_builder.next_unnamed_id, 0);
        let (op_instr_value, _) = cfg_builder.op(None, Op::Const(Const::i32(0))).unwrap();
        assert_eq!(cfg_builder.next_unnamed_id, 1);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let expected_place = Value::new(0);
        assert_eq!(op_instr_value, expected_place);
        let place_data = &function.cfg.values_ctx[expected_place];
        assert_eq!(place_data.id, ValueId::Unnamed(0));
    }

    #[test]
    fn should_add_allocas_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();

        cfg_builder.alloca(None, Type::I32, None, None).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
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
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        cfg_builder.sub(
            Some(ValueId::Named("result".to_string())),
            Op::Const(Const::i32(0)),
            Op::Const(Const::i32(1)),
        ).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, "bb0:
    %result = sub i32 0, 1
    ret void
");
    }

    #[test]
    fn should_add_op_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        cfg_builder.op(Some(ValueId::Named("result".to_string())), Op::Const(Const::i32(0))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, "bb0:
    %result = i32 0
    ret void
");
    }

    #[test]
    fn should_add_store_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        let (alloca_value, _) = cfg_builder.alloca(None, Type::I32, None, None).unwrap();
        cfg_builder.store(alloca_value, Op::Const(Const::i32(0))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, "bb0:
    %0 = alloca i32
    store i32 0, ptr %0
    ret void
");
    }

    #[test]
    fn should_add_load_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        let (alloca_value, _) = cfg_builder.alloca(None, Type::I32, None, None).unwrap();
        cfg_builder.load(Some(ValueId::Named("result".to_string())), alloca_value).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, "bb0:
    %0 = alloca i32
    %result = load i32, ptr %0
    ret void
");
    }

    #[test]
    fn should_add_conditional_branch_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        let bb0 = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let (cmp_value, _) = cfg_builder.icmp(None, ICmpCond::Eq, Op::Const(Const::i32(0)), Op::Const(Const::i32(1))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(
            BranchTerm::Cond(
                CondBrTerm {
                    cond: Op::Value(cmp_value),
                    true_target: bb1,
                    false_target: bb2,
                }
            )));
        cfg_builder.set_bb(bb1);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        cfg_builder.set_bb(bb2);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut out = String::new();
        function.cfg.write_to(&mut out, &function).unwrap();
        assert_eq!(out, "bb0:
    %0 = icmp eq i32 0, 1
    br i1 %0, label bb1, label bb2
bb1:
    ; preds = bb0
    ret void
bb2:
    ; preds = bb0
    ret void
");
    }

    #[test]
    fn should_version_assignments_to_named_variables() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        cfg_builder.op(Some(ValueId::Named("result".to_string())), Op::Const(Const::i32(0))).unwrap();
        cfg_builder.op(Some(ValueId::Named("result".to_string())), Op::Const(Const::i32(1))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, "bb0:
    %result.0 = i32 0
    %result.1 = i32 1
    ret void
");
    }

    #[test]
    fn should_add_phi_node_when_different_usages_dominate_usage() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        let _bb0 = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let bb3 = cfg_builder.create_bb();
        let bb4 = cfg_builder.create_bb();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb1))));
        cfg_builder.set_bb(bb1);
        let result_var_id = ValueId::Named("result".to_string());
        cfg_builder.op(Some(result_var_id.clone()), Op::Const(Const::i32(0))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb3))));
        cfg_builder.set_bb(bb2);
        cfg_builder.op(Some(result_var_id.clone()), Op::Const(Const::i32(1))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb3))));
        cfg_builder.set_bb(bb3);
        let value = cfg_builder.find_or_insert_reaching_value(&result_var_id).unwrap();
        cfg_builder.op(Some(result_var_id.clone()), Op::Value(value)).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        cfg_builder.set_bb(bb4);
        cfg_builder.op(Some(result_var_id.clone()), Op::Const(Const::i32(2))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Branch(BranchTerm::UnCond(UnCondBrTerm::new(bb3))));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, "bb0:
    br label bb1
bb1:
    ; preds = bb0
    %result.0 = i32 0
    br label bb3
bb2:
    %result.1 = i32 1
    br label bb3
bb3:
    ; preds = bb1, bb2, bb4
    %result.2 = phi i32 [ %result.0, bb1 ], [ %result.1, bb2 ], [ %result.4, bb4 ]
    %result.3 = i32 %result.2
    ret void
bb4:
    %result.4 = i32 2
    br label bb3
");
    }
}
