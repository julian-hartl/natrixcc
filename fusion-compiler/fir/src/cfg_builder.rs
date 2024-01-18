use std::collections::HashSet;
use std::ops::Deref;

use crate::cfg::{BasicBlock, BasicBlockData, RetTerm, Terminator, TerminatorKind};
use crate::cfg::VarId;
use crate::function::FunctionData;
use crate::instruction::{AllocaInstr, Incoming, Instr, InstrData, InstrKind, LoadInstr, Op, OpInstr, PhiInstr, Place, PlaceData, PlaceKind, StoreInstr, SubInstr};
use crate::ty::Type;

#[derive(Debug)]
pub struct CFGBuilder<'func> {
    pub func: &'func mut FunctionData,
    current_bb: Option<BasicBlock>,
    next_unnamed_id: usize,
    current_instr: Option<InstrData>,
}

impl<'func> CFGBuilder<'func> {
    pub fn new(func: &'func mut FunctionData) -> Self {
        Self { func, current_bb: None, next_unnamed_id: 0, current_instr: None }
    }

    pub fn start_bb(&mut self) -> BasicBlock {
        let bb= self.create_bb();
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
        let mut bb = self.func.cfg.basic_blocks[self.current_bb.unwrap()].as_mut().unwrap();
        bb.terminator = Terminator::new(terminator);
        self.current_bb = None;
    }

    pub fn alloca(&mut self, place_id: Option<VarId>, ty: Type, num_elements: Option<usize>, alignment: Option<usize>) -> Result<Instr, String> {
        let place_id = place_id.unwrap_or_else(|| self.next_unnamed_id());
        let place = self.push_place_for_next_instr(place_id, Type::Ptr(Box::new(ty.clone())));
        let alloca = AllocaInstr::new(
            place,
            ty,
            num_elements,
            alignment,
        );
        self.add_instr(InstrData::new(InstrKind::Alloca(alloca)))
    }

    pub fn sub(&mut self, place_id: Option<VarId>, lhs: Op, rhs: Op) -> Result<Instr, String> {
        let place_id = place_id.unwrap_or_else(|| self.next_unnamed_id());
        let place = self.push_place_for_next_instr(place_id, lhs.ty(&self.func));
        let sub = SubInstr {
            place,
            lhs,
            rhs,
        };
        self.add_instr(InstrData::new(InstrKind::Sub(sub)))
    }

    pub fn store(&mut self, place: Place, value: Op) -> Result<Instr, String> {
        let store = InstrData::new(InstrKind::Store(StoreInstr { place, value }));
        self.add_instr(store)
    }

    pub fn load(&mut self, place_id: Option<VarId>, source: Place) -> Result<Instr, String> {
        let place_id = place_id.unwrap_or_else(|| self.next_unnamed_id());
        let place = self.push_place_for_next_instr(place_id, self.func.cfg.places[source].ty.deref().clone());
        let load = InstrData::new(InstrKind::Load(LoadInstr {
            place,
            source,
        }));
        self.add_instr(load)
    }

    pub fn op(&mut self, place_id: Option<VarId>, op: Op) -> Result<Instr, String> {
        let place_id = place_id.unwrap_or_else(|| self.next_unnamed_id());
        let place = self.push_place_for_next_instr(place_id, op.ty(&self.func));
        let op_instr = OpInstr {
            place,
            op,
        };
        self.add_instr(InstrData::new(InstrKind::Op(op_instr)))
    }

    pub fn phi(&mut self, place_id: Option<VarId>, incoming: Vec<Incoming>) -> Result<Instr, String> {
        let place_id = place_id.unwrap_or_else(|| self.next_unnamed_id());
        let place = self.push_place_for_next_instr(place_id, incoming[0].op.ty(&self.func));
        self.add_instr(InstrData::new(InstrKind::Phi(
            PhiInstr {
                place,
                incoming,
            }
        )))
    }

    fn push_place_for_next_instr(&mut self, mut place_id: VarId, ty: Type) -> Place {
        let instr = self.func.cfg.instructions.next_idx();
        let mut current_version = None;
        for place in self.func.cfg.places.iter_mut().filter(|place| place.id == place_id) {
            match place.version {
                None => {
                    place.version = Some(0);
                    current_version = Some(0);
                }
                Some(version) => {
                    current_version = match current_version {
                        None => Some(version),
                        Some(current_version_value) => {
                            if current_version_value < version {
                                Some(version)
                            } else {
                                current_version
                            }
                        }
                    };
                }
            }
        }
        let place = self.func.cfg.places.push(PlaceData {
            id: place_id,
            ty,
            kind: PlaceKind::Local(instr),
            version: current_version.map(|version| version + 1),
        });
        place
    }

    pub fn add_instr(&mut self, instr: InstrData) -> Result<Instr, String> {
        assert!(self.current_instr.is_none(), "current_instr should be None");
        assert!(self.current_bb.is_some(), "current_bb should be Some");
        instr.validate(&self.func)?;
        let instr = self.func.cfg.instructions.push(instr);
        self.func.cfg.basic_blocks[self.current_bb.unwrap()].as_mut().unwrap().instructions.push(instr);
        Ok(instr)
    }

    pub fn next_unnamed_id(&mut self) -> VarId {
        let id = self.next_unnamed_id;
        self.next_unnamed_id += 1;
        VarId::Unnamed(id)
    }

    fn revert_unnamed_id(&mut self) {
        self.next_unnamed_id -= 1;
    }

    pub fn find_dominating_places(&self, id: &VarId) -> DominatingPlaces {
        let mut visited = HashSet::new();
        self.find_dominating_places_recursively(id, self.current_bb.unwrap(), &mut visited)
    }

    fn find_dominating_places_recursively(&self, id: &VarId, bb: BasicBlock, visited: &mut HashSet<BasicBlock>) -> DominatingPlaces {
        visited.insert(bb);
        let current_bb = self.func.cfg.basic_blocks[bb].as_ref().unwrap();
        for declaration in current_bb.declarations(&self.func.cfg).iter().copied() {
            let place = &self.func.cfg.places[declaration];
            if &place.id == id {
                return DominatingPlaces(vec![(bb, declaration)]);
            }
        }
        let predecessors = current_bb.predecessors(&self.func.cfg);
        let mut dominating_places = Vec::new();
        for predecessor in predecessors.into_iter() {
            if visited.contains(&predecessor) {
                continue;
            }
            let predecessor_dominating_places = self.find_dominating_places_recursively(id, predecessor, visited);
            dominating_places.extend(predecessor_dominating_places.0.into_iter());
        }
        DominatingPlaces(dominating_places)
    }
}

pub struct DominatingPlaces(Vec<(BasicBlock, Place)>);

impl DominatingPlaces {
    pub fn into_incoming(self) -> Vec<Incoming> {
        self.0.into_iter().map(|(bb, place)| Incoming { source: bb, op: Op::Place(place) }).collect()
    }
}

impl Deref for DominatingPlaces {
    type Target = Vec<(BasicBlock, Place)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Drop for CFGBuilder<'_> {
    fn drop(&mut self) {
        assert!(self.current_bb.is_none(), "current_bb should be None");
        assert!(self.current_instr.is_none(), "current_instr should be None");
    }
}

#[cfg(test)]
mod tests {
    use crate::cfg::{BasicBlock, JmpTerm, RetTerm, TerminatorKind};
    use crate::cfg::VarId;
    use crate::cfg_builder::CFGBuilder;
    use crate::create_test_function;
    use crate::instruction::{Const, Op, Place};
    use crate::ty::Type;

    #[test]
    fn should_use_unnamed_id_counter_by_default() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        assert_eq!(cfg_builder.next_unnamed_id, 0);
        let op_instr = cfg_builder.op(None, Op::Const(Const::I32(0))).unwrap();
        assert_eq!(cfg_builder.next_unnamed_id, 1);
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let expected_place = Place::new(0);
        assert_eq!(function.cfg.instructions[op_instr].target_place().unwrap(), expected_place);
        let place_data = &function.cfg.places[expected_place];
        assert_eq!(place_data.id, VarId::Unnamed(0));
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
        assert_eq!(cfg_out, r#"bb0:
    %0 = alloca i32
    ret
"#);
    }

    #[test]
    fn should_add_sub_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        cfg_builder.sub(
            Some(VarId::Named("result".to_string())),
            Op::Const(Const::I32(0)),
            Op::Const(Const::I32(1)),
        ).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, r#"bb0:
    %result = sub i32 0, 1
    ret
"#);
    }

    #[test]
    fn should_add_op_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        cfg_builder.op(Some(VarId::Named("result".to_string())), Op::Const(Const::I32(0))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, r#"bb0:
    %result = i32 0
    ret
"#);
    }

    #[test]
    fn should_add_store_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        let alloca = cfg_builder.alloca(None, Type::I32, None, None).unwrap();
        let place = cfg_builder.func.cfg.instructions[alloca].target_place().unwrap().clone();
        cfg_builder.store(place, Op::Const(Const::I32(0))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, r#"bb0:
    %0 = alloca i32
    store i32 0, ptr %0
    ret
"#);
    }

    #[test]
    fn should_add_load_to_entry_block() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        let alloca = cfg_builder.alloca(None, Type::I32, None, None).unwrap();
        let place = cfg_builder.func.cfg.instructions[alloca].target_place().unwrap().clone();
        cfg_builder.load(Some(VarId::Named("result".to_string())), place).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, r#"bb0:
    %0 = alloca i32
    %result = load i32, ptr %0
    ret
"#);
    }

    #[test]
    fn should_version_assignments_to_named_variables() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        cfg_builder.start_bb();
        cfg_builder.op(Some(VarId::Named("result".to_string())), Op::Const(Const::I32(0))).unwrap();
        cfg_builder.op(Some(VarId::Named("result".to_string())), Op::Const(Const::I32(1))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, r#"bb0:
    %result.0 = i32 0
    %result.1 = i32 1
    ret
"#);
    }

    #[test]
    fn should_add_phi_node_when_different_usages_dominate_usage() {
        let mut function = create_test_function();
        let mut cfg_builder = CFGBuilder::new(&mut function);
        let bb0 = cfg_builder.start_bb();
        let bb1 = cfg_builder.create_bb();
        let bb2 = cfg_builder.create_bb();
        let bb3 = cfg_builder.create_bb();
        cfg_builder.end_bb(TerminatorKind::Jmp(JmpTerm::new(bb1)));
        cfg_builder.set_bb(bb1);
        let result_var_id = VarId::Named("result".to_string());
        cfg_builder.op(Some(result_var_id.clone()), Op::Const(Const::I32(0))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Jmp(JmpTerm::new(bb3)));
        cfg_builder.set_bb(bb2);
        cfg_builder.op(Some(result_var_id.clone()), Op::Const(Const::I32(1))).unwrap();
        cfg_builder.end_bb(TerminatorKind::Jmp(JmpTerm::new(bb3)));
        cfg_builder.set_bb(bb3);
        let dominating_places = cfg_builder.find_dominating_places(&result_var_id);
        assert_eq!(dominating_places.len(), 2);
        let phi_instr = cfg_builder.phi(Some(result_var_id.clone()), dominating_places.into_incoming()).unwrap();
        let phi_instr = cfg_builder.func.cfg.instructions[phi_instr].kind.as_phi_instr().unwrap();
        cfg_builder.op(Some(result_var_id.clone()), Op::Place(phi_instr.place)).unwrap();
        cfg_builder.end_bb(TerminatorKind::Ret(RetTerm::empty()));
        drop(cfg_builder);
        let mut cfg_out = String::new();
        function.cfg.write_to(&mut cfg_out, &function).unwrap();
        assert_eq!(cfg_out, r#"bb0:
    jmp bb1
bb1:
    %result.0 = i32 0
    jmp bb3
bb2:
    %result.1 = i32 1
    jmp bb3
bb3:
    %result.2 = phi i32 [ %result.0, bb1 ], [ %result.1, bb2 ]
    %result.3 = i32 %result.2
    ret
"#);
    }
}
