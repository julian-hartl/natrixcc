use std::collections::HashMap;

use fusion_compiler::Idx;

use crate::{
    compilation_unit::{
        GlobalScope,
        VariableIdx,
    },
    lir::{
        BasicBlock,
        BasicBlockIdx,
        ConstOp,
        Function,
        Instruction,
        InstructionKind,
        Operand,
        OperandKind,
        Place,
        PlaceIdx,
        Terminator,
        Type,
        LIR,
    },
    mir,
    mir::{
        Binop,
        InstructionIdx,
        Value,
        MIR,
    },
};

pub struct LIRBuilder<'mir> {
    mir: &'mir MIR,
    scope: &'mir GlobalScope,
    lir: LIR,
    current_bb: Option<BasicBlockIdx>,
    var_to_place: HashMap<VariableIdx, PlaceIdx>,
    instruction_to_place: HashMap<InstructionIdx, PlaceIdx>,
    current_mir_function_idx: Option<mir::FunctionIdx>,
}

impl<'mir> LIRBuilder<'mir> {
    pub fn new(mir: &'mir MIR, scope: &'mir GlobalScope) -> Self {
        Self {
            mir,
            lir: LIR::new(),
            scope,
            current_bb: None,
            var_to_place: HashMap::new(),
            current_mir_function_idx: None,
            instruction_to_place: HashMap::new(),
        }
    }

    pub fn build(mut self) -> LIR {
        for (mir_function_idx, mir_function) in self.mir.functions.indexed_iter() {
            self.current_mir_function_idx = Some(mir_function_idx);
            let function_idx = self.lir.functions.push(Function {
                name: mir_function.name.clone(),
                return_type: mir_function.return_type.into(),
                basic_blocks: Vec::new(),
                parameters: Vec::new(),
            });
            for bb_idx in mir_function.basic_blocks.iter().copied() {
                let mir_bb = self.mir.basic_blocks.get_or_panic(bb_idx);
                let _ = self.start_basic_block();
                for instruction_idx in mir_bb.instructions.iter().copied() {
                    let mir_instruction = &mir_function.instructions[instruction_idx];
                    let instruction = match &mir_instruction.kind {
                        mir::InstructionKind::Binary { lhs, rhs, operator } => {
                            let lhs = self.build_operand(lhs);
                            let rhs = self.build_operand(rhs);
                            match operator {
                                Binop::Add => InstructionKind::Add {
                                    target: self.get_referencing_place(instruction_idx),
                                    lhs,
                                    rhs,
                                },
                                Binop::Sub => InstructionKind::Sub {
                                    target: self.get_referencing_place(instruction_idx),
                                    lhs,
                                    rhs,
                                },
                                Binop::Gt => InstructionKind::Gt {
                                    target: self.get_referencing_place(instruction_idx),
                                    lhs,
                                    rhs,
                                },
                                _ => todo!("Unsupported binary operator {:?}", operator),
                            }
                        }
                        mir::InstructionKind::Unary { .. } => todo!(),
                        mir::InstructionKind::Value(value) => InstructionKind::AllocInit {
                            target: self.get_referencing_place(instruction_idx),
                            value: self.build_operand(value),
                        },
                        mir::InstructionKind::Call { .. } => todo!(),
                        mir::InstructionKind::Phi(_) => todo!(),
                    };
                    self.current_basic_block()
                        .instructions
                        .push(Instruction { kind: instruction });
                }
                let terminator = match &mir_bb.terminator().kind {
                    mir::TerminatorKind::Return { value } => {
                        let value = match value {
                            Value::Void => None,
                            _ => Some(self.build_operand(value)),
                        };
                        Terminator::Return { value }
                    }
                    mir::TerminatorKind::Jump(target) => Terminator::Jump {
                        // todo: validate if that is correct
                        target: BasicBlockIdx::new(target.as_index()),
                    },
                    mir::TerminatorKind::SwitchInt { .. } => todo!(),
                    mir::TerminatorKind::Unresolved => todo!(),
                };
                self.current_basic_block().terminator = Some(terminator);
                let function = self.lir.functions.get_mut(function_idx);
                function
                    .basic_blocks
                    .push(self.current_bb.expect("No current basic block"));
            }
        }
        self.lir
    }

    fn build_operand(&mut self, value: &Value) -> Operand {
        match value {
            Value::InstructionRef(instruction_idx) => {
                let place_idx = self.get_referencing_place(*instruction_idx);
                let place = self.lir.places.get(place_idx);
                Operand {
                    ty: place.ty,
                    kind: OperandKind::Deref(place_idx),
                }
            }
            Value::ParameterRef(_) => todo!(),
            Value::ConstantInt(value) => Operand {
                ty: Type::Int32,
                kind: OperandKind::Const(ConstOp::Int32(*value)),
            },
            Value::Void => todo!(),
        }
    }

    fn start_basic_block(&mut self) -> BasicBlockIdx {
        let bb_idx = self.lir.basic_blocks.push(BasicBlock::default());
        self.current_bb = Some(bb_idx);
        bb_idx
    }

    fn current_basic_block(&mut self) -> &mut BasicBlock {
        self.lir
            .basic_blocks
            .get_mut(self.current_bb.expect("No current basic block"))
    }

    fn get_current_function(&self) -> &mir::Function {
        self.mir
            .functions
            .get(self.current_mir_function_idx.expect("No current function"))
    }

    fn get_referencing_place(&mut self, instruction_idx: InstructionIdx) -> PlaceIdx {
        let instruction = self
            .get_current_function()
            .instructions
            .get(instruction_idx);
        let ty = instruction.ty.into();
        let aliased_var = self
            .get_current_function()
            .local_aliases
            .get(&instruction_idx)
            .copied();
        match aliased_var {
            None => match self.instruction_to_place.get(&instruction_idx) {
                None => {
                    let place = self.create_place(ty);
                    self.instruction_to_place.insert(instruction_idx, place);
                    place
                }
                Some(place) => *place,
            },
            Some(aliased_var) => match self.var_to_place.get(&aliased_var) {
                None => {
                    let place = self.create_place(ty);
                    self.var_to_place.insert(aliased_var, place);
                    place
                }
                Some(place) => *place,
            },
        }
    }

    fn create_place(&mut self, ty: Type) -> PlaceIdx {
        self.lir.places.push_with_index(|idx| Place { ty, idx })
    }
}
