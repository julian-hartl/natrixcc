use std::collections::{
    HashMap,
    HashSet,
};

use fusion_compiler::{
    bug,
    Idx,
    IdxVec,
};

use crate::{
    compilation_unit::{
        GlobalScope,
        VariableIdx,
    },
    hir::{
        HIRExpr,
        HIRExprKind,
        HIRStmt,
        HIRStmtKind,
        HIR,
    },
    mir::{
        basic_block::{
            BasicBlock,
            BasicBlockIdx,
        },
        BasicBlocks,
        Function,
        FunctionIdx,
        Instruction,
        InstructionIdx,
        InstructionKind,
        PhiNode,
        TerminatorKind,
        Value,
        MIR,
    },
};

pub struct MIRBuilder {
    mir: MIR,
}

impl MIRBuilder {
    pub fn new() -> Self {
        Self { mir: MIR::new() }
    }

    pub fn build(mut self, hir: &HIR, global_scope: &GlobalScope) -> MIR {
        let mut calls_to_resolve = Vec::new();
        let mut function_map = HashMap::new();
        for (function_idx, function_body) in hir.functions.iter() {
            let function = global_scope.functions.get(*function_idx);
            let function_builder = FunctionBuilder::new(Function {
                name: function.name.clone(),
                return_type: function.return_type.clone().into(),
                parameters: function.parameters.clone(),
                basic_blocks: Vec::new(),
                instructions: IdxVec::new(),
                local_aliases: HashMap::new(),
            });
            let (function, to_be_resolved) =
                function_builder.build(&mut self.mir.basic_blocks, global_scope, function_body);
            let mir_function_idx = self.mir.functions.push(function);
            calls_to_resolve.extend(to_be_resolved.into_iter().map(
                |(instruction_idx, function_idx)| (instruction_idx, function_idx, mir_function_idx),
            ));
            function_map.insert(*function_idx, mir_function_idx);
        }
        for (instruction_idx, function_idx, function_that_called) in calls_to_resolve {
            let mir_function_idx = function_map[&function_idx];
            let instruction = self.mir.functions[function_that_called]
                .instructions
                .get_mut(instruction_idx);
            match &mut instruction.kind {
                InstructionKind::Call {
                    function_idx: call_function_idx,
                    ..
                } => {
                    *call_function_idx = mir_function_idx;
                }
                _ => bug!("Expected call instruction, found {:?}", instruction.kind),
            }
        }
        self.mir
    }
}

type LocalDefinitions = HashMap<VariableIdx, HashMap<BasicBlockIdx, InstructionIdx>>;

struct FunctionBuilder {
    function: Function,
    temp_var_counter: usize,
    loops: Vec<Vec<BasicBlockIdx>>,
    /// Maps a variable to the instruction in each basic block where it is defined.
    /// This is used to determine the last definition of a variable in a basic block.
    ///
    /// For example:
    ///
    /// ```rust
    /// let a = 0;
    /// if true {
    ///     let a = 1;
    ///     a = 2;
    ///     println(a);
    /// }
    /// ```
    /// would be translated to:
    /// ```
    /// bb0:
    ///  %0 = 0
    ///  switchInt (true) {
    ///   case 0: bb1
    ///   default: bb2
    ///  }
    /// bb1:
    ///  %1 = 1
    ///  %2 = 2
    ///  println(%2)
    /// bb2:
    ///  return
    ///
    /// Definitions:
    /// a: {bb0: %0, bb1: %2}
    definitions: LocalDefinitions,
    incomplete_phis: HashMap<BasicBlockIdx, Vec<(InstructionIdx, VariableIdx)>>,
    sealed_blocks: HashSet<BasicBlockIdx>,
    call_references_to_resolve: Vec<(InstructionIdx, crate::compilation_unit::FunctionIdx)>,
}

impl FunctionBuilder {
    pub fn new(function: Function) -> Self {
        Self {
            function,
            temp_var_counter: 0,
            loops: vec![],
            definitions: HashMap::new(),
            incomplete_phis: HashMap::new(),
            sealed_blocks: HashSet::new(),
            call_references_to_resolve: Vec::new(),
        }
    }

    pub fn build(
        mut self,
        basic_blocks: &mut BasicBlocks,
        global_scope: &GlobalScope,
        body: &[HIRStmt],
    ) -> (
        Function,
        Vec<(InstructionIdx, crate::compilation_unit::FunctionIdx)>,
    ) {
        let mut bb_builder = BasicBlockBuilder::new(basic_blocks, &mut self.function);
        for (index, variable_idx) in self.function.parameters.clone().into_iter().enumerate() {
            let param_type = global_scope.variables.get(variable_idx).ty.clone().into();
            let instruction_idx = bb_builder.add_instruction(
                basic_blocks,
                &mut self.function,
                Instruction::new(
                    InstructionKind::Value(Value::ParameterRef(index)),
                    param_type,
                ),
            );
            self.write_variable(variable_idx, bb_builder.current_bb, instruction_idx);
        }
        for stmt in body.iter() {
            self.build_stmt(basic_blocks, &mut bb_builder, global_scope, stmt)
        }
        let predecessors = self.function.predecessors(basic_blocks);
        for block in self.function.basic_blocks.clone().into_iter() {
            if !self.is_sealed(block) {
                self.seal_block(basic_blocks, block, global_scope);
            }
            let immediate_predecessors = predecessors.get_immediate(block);
            for instr_idx in basic_blocks
                .get_or_panic(block)
                .instructions
                .iter()
                .copied()
            {
                let instruction = &self.function.instructions[instr_idx];
                if let InstructionKind::Phi(phi) = &instruction.kind {
                    let predecessors_len = immediate_predecessors
                        .map(|ip| ip.len())
                        .unwrap_or_default();
                    assert_eq!(
                        phi.operands.len(),
                        predecessors_len,
                        "Phi node in {} has {} operand(s), but {} predecessor(s)",
                        block,
                        phi.operands.len(),
                        predecessors_len
                    );
                    for (pred, _) in phi.operands.iter() {
                        if let Some(immediate_predecessors) = immediate_predecessors {
                            assert!(immediate_predecessors.contains(pred), "Phi node {:?} has operand for predecessor {:?}, but that is not an immediate predecessor of {:?}", phi, pred, block);
                        }
                    }
                }
            }
        }
        assert!(self.incomplete_phis.is_empty());
        assert!(self.loops.is_empty());
        (self.function, self.call_references_to_resolve)
    }

    fn build_stmt(
        &mut self,
        basic_blocks: &mut BasicBlocks,
        bb_builder: &mut BasicBlockBuilder,
        global_scope: &GlobalScope,
        stmt: &HIRStmt,
    ) {
        match &stmt.kind {
            HIRStmtKind::Expr { expr } => {
                // Transform the expression into a value and assign it to a new instruction
                // E.g. `1 + 2` becomes `%0 = 1 + 2`
                let value = self.build_expr(basic_blocks, bb_builder, global_scope, expr);
                let ty = expr.ty.clone().into();
                bb_builder.add_instruction(
                    basic_blocks,
                    &mut self.function,
                    Instruction::new(InstructionKind::Value(value), ty),
                );
            }
            HIRStmtKind::Decl {
                initializer,
                variable_idx,
            } => {
                // Transform the initializer into a value and assign it to a new instruction
                // The instruction now represents the variable
                // E.g.:
                // ```text
                // let a = 1 + 2
                // println(a)
                // ```
                // becomes:
                // ```text
                // %0 = 1 + 2
                // println(%0)
                // ```
                let value = initializer.as_ref().map(|initializer| {
                    self.build_expr(basic_blocks, bb_builder, global_scope, initializer)
                });
                let ty = global_scope.variables.get(*variable_idx).ty.clone().into();
                // todo: we should figure out a way to omit the instruction if the variable is not initialized, e.g. by just setting the variable as live (using a StorageLive instruction?)
                let instruction_idx = bb_builder.add_instruction(
                    basic_blocks,
                    &mut self.function,
                    Instruction::new(InstructionKind::Value(value.unwrap_or(Value::Void)), ty),
                );
                self.write_variable(*variable_idx, bb_builder.current_bb, instruction_idx);
            }
            HIRStmtKind::Loop { body } => {
                // High level steps:
                // 1. Create a loop entry block
                // 2. Build the loop body
                // 3. Terminate the last block of the loop body with a jump to the loop entry block.
                //    We only do that, if the last block of the loop body is not already terminated,
                //    because otherwise, we could override `break` and `return` statements.
                // 4. Create a loop exit block
                // 5. Update all blocks that need to know the exit block of the loop
                //    This includes e.g. `break` statements
                //
                // Note, that during construction, phi nodes are automatically inserted, if necessary.
                //
                // Example:
                // ```text
                // let a = 10
                // loop {
                //   if a == 5 { break }
                //   else { a = a - 1 }
                // }
                // ```
                // becomes:
                // ```text
                // bb0:
                //   %0 = 10
                // bb1: <-- loop entry
                //   %1 = phi { bb0 -> %0, bb4 -> %3 }
                //   %2 = %1 == 5
                //   switchInt(%2) {
                //     case 0: bb2
                //     default: bb3
                //   }
                // bb2:
                //   %3 = %1 - 1
                //   jump bb4
                // bb3:
                //   jump bb5 <-- is updated with the exit block of the loop
                // bb4:
                //   jump bb1
                // bb5: <-- loop exit
                //   return
                let pred = bb_builder.terminate_and(
                    basic_blocks,
                    &mut self.function,
                    TerminatorKind::Jump,
                );
                let loop_entry_bb = bb_builder.current_bb;
                self.push_loop(loop_entry_bb);
                for stmt in body.iter() {
                    self.build_stmt(basic_blocks, bb_builder, global_scope, stmt);
                }
                if !basic_blocks
                    .get_or_panic(bb_builder.current_bb)
                    .is_terminated()
                {
                    bb_builder.terminate(basic_blocks, TerminatorKind::Jump(loop_entry_bb));
                    self.seal_block(basic_blocks, bb_builder.current_bb, global_scope);
                }
                // todo: reevaluate this once we add continue
                self.seal_block(basic_blocks, loop_entry_bb, global_scope);
                let exit_block = bb_builder.start_new_bb(basic_blocks, &mut self.function);
                self.pop_loop_and_update(basic_blocks, exit_block);
                self.seal_block(basic_blocks, exit_block, global_scope);
            }
            HIRStmtKind::Return { expr } => {
                let value = self.build_expr(basic_blocks, bb_builder, global_scope, expr);
                if basic_blocks
                    .get_or_panic(bb_builder.current_bb)
                    .is_terminated()
                {
                    bb_builder.start_new_bb(basic_blocks, &mut self.function);
                }
                let bb = bb_builder.terminate(basic_blocks, TerminatorKind::Return { value });
            }
            HIRStmtKind::If {
                condition,
                then_body,
                else_body,
            } => {
                tracing::debug!("Building if statement");
                tracing::debug!("Building condition");
                let condition = self.build_expr(basic_blocks, bb_builder, global_scope, condition);
                let pred = bb_builder.current_bb;
                let then_start_bb = bb_builder.start_new_bb(basic_blocks, &mut self.function);
                let else_start_bb = bb_builder.start_new_bb(basic_blocks, &mut self.function);
                bb_builder.set_bb(pred);
                bb_builder.terminate(
                    basic_blocks,
                    TerminatorKind::SwitchInt {
                        value: condition,
                        cases: vec![(0, else_start_bb)],
                        default: then_start_bb,
                    },
                );
                tracing::debug!("Built condition");
                // todo: is this correct?
                // self.seal_block(basic_blocks, pred, global_scope);
                self.seal_block(basic_blocks, then_start_bb, global_scope);
                self.seal_block(basic_blocks, else_start_bb, global_scope);
                tracing::debug!("Building then body");
                bb_builder.set_bb(then_start_bb);
                for stmt in then_body.iter() {
                    self.build_stmt(basic_blocks, bb_builder, global_scope, stmt);
                }
                tracing::debug!("Built then body");
                let then_exit_bb = bb_builder.current_bb;
                tracing::debug!("Building else body");
                bb_builder.set_bb(else_start_bb);
                for stmt in else_body.iter() {
                    self.build_stmt(basic_blocks, bb_builder, global_scope, stmt);
                }
                tracing::debug!("Built else body");
                let else_exit_bb = bb_builder.current_bb;
                let if_end_bb = bb_builder.start_new_bb(basic_blocks, &mut self.function);
                tracing::debug!("Building if terminator");
                basic_blocks
                    .get_mut_or_panic(then_exit_bb)
                    .maybe_set_terminator(TerminatorKind::Jump(if_end_bb));
                basic_blocks
                    .get_mut_or_panic(else_exit_bb)
                    .maybe_set_terminator(TerminatorKind::Jump(if_end_bb));
                tracing::debug!("Built if terminator");
                // self.seal_block(basic_blocks, if_end_bb, global_scope);
            }
            HIRStmtKind::Block { body } => {
                for stmt in body.iter() {
                    self.build_stmt(basic_blocks, bb_builder, global_scope, stmt);
                }
            }
            HIRStmtKind::Assign { rhs, lhs } => {
                let value = self.build_expr(basic_blocks, bb_builder, global_scope, rhs);
                let instruction_idx = bb_builder.add_instruction(
                    basic_blocks,
                    &mut self.function,
                    Instruction::new(InstructionKind::Value(value), rhs.ty.clone().into()),
                );
                self.write_variable(*lhs, bb_builder.current_bb, instruction_idx);
            }
            HIRStmtKind::Break => {
                let break_block = bb_builder.terminate(basic_blocks, TerminatorKind::Unresolved);
                // self.seal_block(mir, break_block, global_scope);
                self.push_depending_block(break_block);
            }
        }
    }

    fn push_depending_block(&mut self, bb: BasicBlockIdx) {
        self.loops.last_mut().unwrap().push(bb);
    }

    fn pop_loop_and_update(&mut self, basic_blocks: &mut BasicBlocks, exit_block: BasicBlockIdx) {
        tracing::debug!("Exiting loop at {}", exit_block);
        let basic_blocks_to_update = self.pop_loop();
        for bb in basic_blocks_to_update.iter() {
            let bb = basic_blocks.get_mut_or_panic(*bb);
            assert_eq!(bb.terminator.as_ref().map(|terminator| &terminator.kind), Some(&TerminatorKind::Unresolved), "Basic blocks that depend on the exit/start block of a loop must have an unresolved terminator");
            bb.set_terminator(TerminatorKind::Jump(exit_block));
        }
    }

    fn build_expr(
        &mut self,
        basics_blocks: &mut BasicBlocks,
        bb_builder: &mut BasicBlockBuilder,
        global_scope: &GlobalScope,
        expr: &HIRExpr,
    ) -> Value {
        match &expr.kind {
            // todo: support other numbers
            HIRExprKind::Number(value) => Value::ConstantInt(*value as i32),
            HIRExprKind::Bool(value) => Value::ConstantInt(if *value { 1 } else { 0 }),
            HIRExprKind::Binary { lhs, operator, rhs } => {
                let lhs = self.build_expr(basics_blocks, bb_builder, global_scope, lhs);
                let rhs = self.build_expr(basics_blocks, bb_builder, global_scope, rhs);
                let ty = expr.ty.clone().into();
                let instruction_ref = bb_builder.add_instruction(
                    basics_blocks,
                    &mut self.function,
                    Instruction::new(
                        InstructionKind::Binary {
                            operator: (*operator).into(),
                            lhs,
                            rhs,
                        },
                        ty,
                    ),
                );
                Value::InstructionRef(instruction_ref)
            }
            HIRExprKind::Unary { operator, operand } => {
                let operand = self.build_expr(basics_blocks, bb_builder, global_scope, operand);
                let ty = expr.ty.clone().into();
                let instruction_ref = bb_builder.add_instruction(
                    basics_blocks,
                    &mut self.function,
                    Instruction::new(
                        InstructionKind::Unary {
                            operator: (*operator).into(),
                            operand,
                        },
                        ty,
                    ),
                );
                Value::InstructionRef(instruction_ref)
            }
            HIRExprKind::Var(variable_idx) => {
                let instruction_ref = self
                    .read_variable(
                        basics_blocks,
                        *variable_idx,
                        bb_builder.current_bb,
                        global_scope,
                    )
                    .unwrap();
                Value::InstructionRef(instruction_ref)
            }
            HIRExprKind::Call {
                function_idx,
                arguments,
            } => {
                let arguments = arguments
                    .iter()
                    .map(|arg| self.build_expr(basics_blocks, bb_builder, global_scope, arg))
                    .collect();
                let ty = expr.ty.clone().into();
                let instruction_idx = bb_builder.add_instruction(
                    basics_blocks,
                    &mut self.function,
                    Instruction::new(
                        InstructionKind::Call {
                            // todo: fix this
                            function_idx: FunctionIdx::first(),
                            arguments,
                        },
                        ty,
                    ),
                );
                self.call_references_to_resolve
                    .push((instruction_idx, *function_idx));
                Value::InstructionRef(instruction_idx)
            }
            HIRExprKind::Unit => Value::Void,
        }
    }

    #[inline]
    pub fn push_loop(&mut self, entry_bb: BasicBlockIdx) {
        tracing::debug!("Entering loop at {}", entry_bb);
        self.loops.push(vec![]);
    }

    #[inline]
    pub fn pop_loop(&mut self) -> Vec<BasicBlockIdx> {
        self.loops.pop().unwrap()
    }

    /// Records the definition of a variable in the current basic block.
    #[inline]
    pub fn write_variable(
        &mut self,
        variable: VariableIdx,
        bb_idx: BasicBlockIdx,
        instruction: InstructionIdx,
    ) {
        tracing::debug!(
            "Writing variable {:?} in {} as {:?}",
            variable,
            bb_idx,
            instruction
        );
        self.definitions
            .entry(variable)
            .or_default()
            .insert(bb_idx, instruction);
        self.function.local_aliases.insert(instruction, variable);
    }

    /// Returns the latest definition of a variable above and including the given basic block.
    /// If there is no definition, it returns None.
    ///
    /// It first checks if there is a definition in the given basic block (local definition).
    /// todo!()
    pub fn read_variable(
        &mut self,
        basic_blocks: &mut BasicBlocks,
        variable: VariableIdx,
        bb_idx: BasicBlockIdx,
        scope: &GlobalScope,
    ) -> Option<InstructionIdx> {
        let definitions = self.definitions.get(&variable)?;
        match definitions.get(&bb_idx) {
            Some(instruction) => Some(*instruction),
            None => self.read_variable_recursive(basic_blocks, variable, bb_idx, scope),
        }
    }

    pub fn read_variable_recursive(
        &mut self,
        basic_blocks: &mut BasicBlocks,
        variable: VariableIdx,
        bb_idx: BasicBlockIdx,
        scope: &GlobalScope,
    ) -> Option<InstructionIdx> {
        let predecessors = self.function.predecessors(basic_blocks);
        let preceding_bbs = predecessors.get_immediate(bb_idx)?;
        let instruction_ref = if !self.is_sealed(bb_idx) {
            tracing::debug!(
                "Found unsealed block {:?} for variable {:?}. Inserting operandless phi",
                bb_idx,
                variable
            );
            let instruction_ref =
                self.add_operandless_phi_to_bb(basic_blocks, variable, bb_idx, scope);
            self.incomplete_phis
                .entry(bb_idx)
                .or_default()
                .push((instruction_ref, variable));
            instruction_ref
        } else if preceding_bbs.len() == 1 {
            // Optimize the common use case of a single predecessor
            self.read_variable(basic_blocks, variable, preceding_bbs[0], scope)?
        } else {
            // Break potential cycles with operandless phi
            tracing::debug!(
                "Inserting operandless phi for variable {:?} in block {:?}",
                variable,
                bb_idx
            );
            let instruction_ref =
                self.add_operandless_phi_to_bb(basic_blocks, variable, bb_idx, scope);
            self.write_variable(variable, bb_idx, instruction_ref);
            tracing::debug!(
                "Adding phi operands for {:?} in block {:?}",
                variable,
                bb_idx
            );

            self.add_phi_operands(
                basic_blocks,
                instruction_ref,
                variable,
                preceding_bbs,
                scope,
            );
            // todo: remove trivial phi
            // self.try_remove_trivial_phi(phi);
            instruction_ref
        };
        self.write_variable(variable, bb_idx, instruction_ref);
        Some(instruction_ref)
    }

    fn add_operandless_phi_to_bb(
        &mut self,
        basic_blocks: &mut BasicBlocks,
        variable: VariableIdx,
        bb: BasicBlockIdx,
        scope: &GlobalScope,
    ) -> InstructionIdx {
        let instruction_ref = self.function.instructions.push(Instruction::new(
            InstructionKind::Phi(PhiNode::operandless()),
            scope.variables[variable].ty.clone().into(),
        ));
        let instructions = basic_blocks.get_or_panic(bb).instructions.clone();
        let mut instructions_with_phi = vec![instruction_ref];
        instructions_with_phi.extend(instructions);
        basic_blocks.get_mut_or_panic(bb).instructions = instructions_with_phi;
        instruction_ref
    }

    fn try_remove_trivial_phi(&self, phi: &mut PhiNode) {}

    fn seal_block(
        &mut self,
        basic_blocks: &mut BasicBlocks,
        bb_idx: BasicBlockIdx,
        global_scope: &GlobalScope,
    ) {
        if self.is_sealed(bb_idx) {
            bug!("Tried to seal block {} after it had been sealed", bb_idx);
        }
        tracing::debug!("Sealing {:?}", bb_idx);
        // todo: do not clone
        if let Some(incomplete_phis) = self.incomplete_phis.get(&bb_idx).cloned() {
            tracing::debug!("{:?} has incomplete phis {:?}", bb_idx, incomplete_phis);
            let predecessors = self.function.predecessors(basic_blocks);
            for (incomplete_phi, variable_idx) in incomplete_phis.iter().copied() {
                self.add_phi_operands(
                    basic_blocks,
                    incomplete_phi,
                    variable_idx,
                    predecessors.get_immediate(bb_idx).unwrap(),
                    global_scope,
                );
            }
        }
        self.incomplete_phis.remove(&bb_idx);
        self.sealed_blocks.insert(bb_idx);
    }

    fn add_phi_operands(
        &mut self,
        basic_blocks: &mut BasicBlocks,
        phi: InstructionIdx,
        variable: VariableIdx,
        preds: &Vec<BasicBlockIdx>,
        scope: &GlobalScope,
    ) {
        tracing::debug!(
            "Adding phi operands for {:?}  with predecessors {:?}",
            phi,
            preds
        );
        for pred in preds.iter().copied() {
            let variable_ref = self
                .read_variable(basic_blocks, variable, pred, scope)
                .unwrap_or_else(|| {
                    bug!(
                        "No definition for variable {:?} in block {:?}",
                        variable,
                        pred
                    )
                });
            let phi = self.function.instructions[phi].kind.as_phi_mut().unwrap();
            phi.push((pred, variable_ref))
        }
    }
    fn is_sealed(&self, bb_idx: BasicBlockIdx) -> bool {
        self.sealed_blocks.contains(&bb_idx)
    }
}

/// A helper to build basic blocks.
///
/// Can be used to start new basic blocks, add instructions and terminate basic blocks.
struct BasicBlockBuilder {
    current_bb: BasicBlockIdx,
}

impl BasicBlockBuilder {
    pub fn new(basic_blocks: &mut BasicBlocks, function: &mut Function) -> Self {
        let mut builder = Self {
            // Temporary value, will be overwritten in the next step
            current_bb: BasicBlockIdx::first(),
        };
        let new_bb = builder.start_new_bb(basic_blocks, function);
        builder.set_bb(new_bb);
        builder
    }

    pub fn add_instruction(
        &mut self,
        basic_blocks: &mut BasicBlocks,
        function: &mut Function,
        instruction: Instruction,
    ) -> InstructionIdx {
        let current_bb = self.get_current_bb_mut(basic_blocks);
        if let Some(terminator) = current_bb.terminator.as_ref() {
            bug!(
                "{} already has a terminator: {:?}",
                self.current_bb,
                terminator
            )
        }
        let instruction_idx = function.instructions.push(instruction);
        current_bb.instructions.push(instruction_idx);
        instruction_idx
    }

    /// Starts a new basic block in `function` and returns it.
    pub fn start_new_bb(
        &mut self,
        basic_blocks: &mut BasicBlocks,
        function: &mut Function,
    ) -> BasicBlockIdx {
        let new_bb = basic_blocks.push_basic_block();
        function.basic_blocks.push(new_bb);
        tracing::debug!("Starting new basic block {:?}", new_bb);
        self.current_bb = new_bb;
        new_bb
    }

    /// Terminates and returns the current basic block.
    ///
    /// **Panics** if the current basic block is already terminated.
    ///
    /// Note: This does not start a new basic block. Use [`BasicBlockBuilder::terminate_and`] for that.
    pub fn terminate(
        &mut self,
        basic_blocks: &mut BasicBlocks,
        terminator: TerminatorKind,
    ) -> BasicBlockIdx {
        let bb = self.get_current_bb_mut(basic_blocks);
        if let Some(terminator) = bb.terminator.as_ref() {
            bug!(
                "{:?} already has a terminator: {:?}",
                self.current_bb,
                terminator
            )
        }
        // Ensure that we do not jump to the same basic block, which at the moment would lead to an infinite loop, when reading variables.
        match &terminator {
            TerminatorKind::Jump(to) => {
                assert_ne!(
                    *to, self.current_bb,
                    "Jumping to the same basic block is not yet supported"
                );
            }
            TerminatorKind::SwitchInt { default, cases, .. } => {
                assert_ne!(
                    *default, self.current_bb,
                    "Jumping to the same basic block is not yet supported"
                );
                for (_, target) in cases {
                    assert_ne!(
                        *target, self.current_bb,
                        "Jumping to the same basic block is not yet supported"
                    );
                }
            }
            TerminatorKind::Return { .. } | TerminatorKind::Unresolved => {}
        };
        bb.set_terminator(terminator);
        self.current_bb
    }

    /// Terminates the current basic block `b` with the result of `terminator_builder`, starts a new one and returns the previous basic block `b`.
    ///
    /// `terminator_builder` is called with the index of the new basic block.
    ///
    /// See [`BasicBlockBuilder::terminate`] and [`BasicBlockBuilder::start_new_bb`].
    pub fn terminate_and(
        &mut self,
        basic_blocks: &mut BasicBlocks,
        function: &mut Function,
        terminator_builder: impl FnOnce(BasicBlockIdx) -> TerminatorKind,
    ) -> BasicBlockIdx {
        let old_bb = self.current_bb;
        let new_bb = self.start_new_bb(basic_blocks, function);
        self.set_bb(old_bb);
        self.terminate(basic_blocks, terminator_builder(new_bb));
        self.set_bb(new_bb);
        old_bb
    }

    /// Sets the current basic block.
    ///
    /// This means that all methods that do **NOT** explicitly take a basic block index as argument will operate on `bb`.
    #[inline]
    pub fn set_bb(&mut self, bb: BasicBlockIdx) {
        tracing::debug!("Setting current basic block to {:?}", bb);
        self.current_bb = bb;
    }

    #[inline]
    fn get_current_bb_mut<'ctx>(
        &mut self,
        basic_blocks: &'ctx mut BasicBlocks,
    ) -> &'ctx mut BasicBlock {
        basic_blocks.get_mut_or_panic(self.current_bb)
    }

    #[inline]
    fn get_current_bb<'ctx>(&self, basic_blocks: &'ctx BasicBlocks) -> &'ctx BasicBlock {
        basic_blocks.get_or_panic(self.current_bb)
    }
}

#[derive(Debug, Clone)]
/// A map from basic blocks to their immediate dominator.
struct Dominators(HashMap<BasicBlockIdx, BasicBlockIdx>);

impl Dominators {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn from_immediate_dominators(
        immediate_dominators: HashMap<BasicBlockIdx, BasicBlockIdx>,
    ) -> Self {
        Self(immediate_dominators)
    }

    pub fn strictly_dominates(&self, a: BasicBlockIdx, b: BasicBlockIdx) -> bool {
        a != b && self.dominates(a, b)
    }

    pub fn dominates(&self, a: BasicBlockIdx, b: BasicBlockIdx) -> bool {
        self.get_dominated_blocks(b).contains(&a)
    }

    #[inline]
    /// Returns all blocks that are dominated by the given basic block.
    pub fn get_dominated_blocks(&self, bb: BasicBlockIdx) -> HashSet<BasicBlockIdx> {
        let mut worklist = vec![bb];
        let mut visited = HashSet::new();
        while let Some(bb) = worklist.pop() {
            if visited.contains(&bb) {
                continue;
            }
            visited.insert(bb);
            for (block, idom) in self.0.iter() {
                if *idom == bb {
                    worklist.push(*block);
                }
            }
        }
        visited
    }

    pub fn get_immediate_dominator(&self, b: BasicBlockIdx) -> Option<BasicBlockIdx> {
        self.0.get(&b).copied()
    }

    /// Returns all immediate dominators.
    ///
    /// The entry block does not have an immediate dominator.
    pub fn immediate_dominators(&self) -> &HashMap<BasicBlockIdx, BasicBlockIdx> {
        &self.0
    }

    pub fn set_immediate_dominator(&mut self, b: BasicBlockIdx, idom: BasicBlockIdx) {
        self.0.insert(b, idom);
    }
}

#[derive(Debug, Clone)]
pub struct Predecessors(HashMap<BasicBlockIdx, Vec<BasicBlockIdx>>);

impl Predecessors {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Returns all predecessors of the given basic block.
    ///
    /// It resolves all transitive successors.
    pub fn get_all(&self, bb: BasicBlockIdx) -> Vec<BasicBlockIdx> {
        let mut visited = HashSet::new();
        let mut queue = vec![bb];
        while let Some(bb) = queue.pop() {
            if visited.contains(&bb) {
                continue;
            }
            visited.insert(bb);
            if let Some(predecessors) = self.0.get(&bb) {
                queue.extend(predecessors.iter().copied());
            }
        }
        visited.remove(&bb);
        visited.into_iter().collect()
    }

    pub fn get_immediate(&self, bb: BasicBlockIdx) -> Option<&Vec<BasicBlockIdx>> {
        self.0.get(&bb)
    }

    pub fn insert_immediate(&mut self, bb: BasicBlockIdx, successor: BasicBlockIdx) {
        self.0
            .entry(bb)
            .or_insert_with(|| Vec::with_capacity(1))
            .push(successor);
    }
}

#[derive(Debug, Clone)]
/// A map from basic blocks to their immediate successors.
pub struct Successors(HashMap<BasicBlockIdx, HashSet<BasicBlockIdx>>);

impl Successors {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Returns all successors of the given basic block.
    ///
    /// It resolves all transitive successors.
    pub fn get_all(&self, bb: BasicBlockIdx) -> HashSet<BasicBlockIdx> {
        let mut visited = HashSet::new();
        let mut queue = vec![bb];
        while let Some(bb) = queue.pop() {
            if visited.contains(&bb) {
                continue;
            }
            visited.insert(bb);
            if let Some(successors) = self.0.get(&bb) {
                queue.extend(successors.iter().copied());
            }
        }
        visited.remove(&bb);
        visited
    }

    pub fn get_immediate(&self, bb: BasicBlockIdx) -> Option<&HashSet<BasicBlockIdx>> {
        self.0.get(&bb)
    }

    pub fn insert_immediate(&mut self, bb: BasicBlockIdx, successor: BasicBlockIdx) {
        self.0
            .entry(bb)
            .or_insert_with(|| HashSet::with_capacity(1))
            .insert(successor);
    }
}

impl Function {
    pub fn successors(&self, basic_blocks: &BasicBlocks) -> Successors {
        let mut successors = Successors::new();
        for (idx, bb) in basic_blocks.indexed_iter_as_option().flatten() {
            if let Some(terminator) = bb.terminator.as_ref() {
                match &terminator.kind {
                    TerminatorKind::Jump(target) => {
                        successors.insert_immediate(idx, *target);
                    }
                    TerminatorKind::SwitchInt { default, cases, .. } => {
                        successors.insert_immediate(idx, *default);
                        for (_, target) in cases {
                            successors.insert_immediate(idx, *target);
                        }
                    }
                    TerminatorKind::Return { .. } => {}
                    TerminatorKind::Unresolved => {}
                }
            }
        }
        successors
    }

    pub fn predecessors(&self, basic_blocks: &BasicBlocks) -> Predecessors {
        let mut predecessors = Predecessors::new();
        for (idx, bb) in basic_blocks.indexed_iter_as_option().flatten() {
            if let Some(terminator) = bb.terminator.as_ref() {
                match &terminator.kind {
                    TerminatorKind::Jump(target) => {
                        predecessors.insert_immediate(*target, idx);
                    }
                    TerminatorKind::SwitchInt { default, cases, .. } => {
                        predecessors.insert_immediate(*default, idx);
                        for (_, target) in cases {
                            predecessors.insert_immediate(*target, idx);
                        }
                    }
                    TerminatorKind::Return { .. } => {}
                    TerminatorKind::Unresolved => {}
                }
            }
        }
        predecessors
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use fusion_compiler::Idx;

    use crate::mir::{
        BasicBlockIdx,
        FunctionIdx,
        MIR,
    };

    pub fn assert_mir(input: &str, expected_mir: &str) -> MIR {
        let mut compilation_unit =
            crate::compilation_unit::CompilationUnit::compile(input).unwrap();
        let hir = crate::hir::HIRBuilder::new()
            .build(&compilation_unit.ast, &mut compilation_unit.global_scope);
        let mir = crate::mir::MIRBuilder::new().build(&hir, &compilation_unit.global_scope);
        let mut actual_mir = String::new();
        crate::mir::MIRWriter::write_text_representation(&mut actual_mir, &mir).unwrap();
        assert_eq!(actual_mir, expected_mir);
        mir
    }

    #[test]
    fn simple_loop() {
        let input = "
        func main() -> int {
           let b = 0
           while b < 10 {
               b = b + 1
           }
            return b
        }
    ";

        let expected = r#"func main:
bb0():
    %0 = 0
    jump bb1
bb1($0):
    %1 = lt $0, 10
    switchInt (%1) {
    case 0: bb3
    default: bb2
}
bb2():
    %2 = add %0, 1
    %3 = %2
    %4 = %3
    jump bb4
bb3():
    jump bb5
bb4():
    jump bb1
bb5():
    return %0
"#;
        let mir = assert_mir(input, expected);
        let predecessors = mir.functions[FunctionIdx(0)].predecessors();
        assert_eq!(predecessors.get_all(BasicBlockIdx::new(0)), HashSet::new());
        assert_eq!(
            predecessors.get_all(BasicBlockIdx::new(1)),
            HashSet::from([BasicBlockIdx::new(0), BasicBlockIdx::new(4)])
        );
        assert_eq!(
            predecessors.get_all(BasicBlockIdx::new(2)),
            HashSet::from([BasicBlockIdx::new(1)])
        );
        assert_eq!(
            predecessors.get_all(BasicBlockIdx::new(3)),
            HashSet::from([BasicBlockIdx::new(1)])
        );
        assert_eq!(
            predecessors.get_all(BasicBlockIdx::new(4)),
            HashSet::from([BasicBlockIdx::new(2)])
        );
        assert_eq!(
            predecessors.get_all(BasicBlockIdx::new(5)),
            HashSet::from([BasicBlockIdx::new(3)])
        );
    }
}
