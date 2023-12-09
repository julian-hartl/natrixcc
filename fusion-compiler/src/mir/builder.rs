use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};

use fusion_compiler::{bug, Idx, IdxVec};

use crate::compilation_unit::{GlobalScope, VariableIdx};
use crate::hir::{HIR, HIRExpr, HIRExprKind, HIRStmt, HIRStmtKind};
use crate::mir::{BasicBlock, BasicBlockIdx, Function, FunctionIdx, Instruction, InstructionIdx, InstructionKind, MIR, PhiNode, TerminatorKind, Value};

pub struct MIRBuilder {}


impl MIRBuilder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn build(self, hir: &HIR, global_scope: &GlobalScope) -> MIR {
        let mut functions: IdxVec<FunctionIdx, Function> = IdxVec::new();
        for (function_idx, function_body) in hir.functions.iter() {
            let function = global_scope.functions.get(*function_idx);
            let function_builder = FunctionBuilder::new(
                Function {
                    name: function.name.clone(),
                    return_type: function.return_type.clone().into(),
                    parameters: function.parameters.iter().map(
                        |param| global_scope.variables.get(*param).ty.clone().into(),
                    ).collect(),
                    basic_blocks: IdxVec::new(),
                    instructions: IdxVec::new(),
                }
            );
            let function = function_builder.build(
                global_scope,
                function_body,
            );
            functions.push(function);
        }
        MIR::new(functions)
    }
}


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
    ///   let a = 1;
    ///   a = 2;
    ///   println(a);
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
    definitions: HashMap<VariableIdx, HashMap<BasicBlockIdx, InstructionIdx>>,
    incomplete_phis: HashMap<BasicBlockIdx, Vec<(InstructionIdx, VariableIdx)>>,
    sealed_blocks: HashSet<BasicBlockIdx>,
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
        }
    }

    pub fn build(mut self, global_scope: &GlobalScope, body: &[HIRStmt]) -> Function {
        let mut bb_builder = BasicBlockBuilder::new(&mut self.function);
        for stmt in body.iter() {
            self.build_stmt(&mut bb_builder, global_scope, stmt)
        }
        for block in self.function.basic_blocks.indices() {
            assert!(self.is_sealed(block));
        }
        assert!(self.incomplete_phis.is_empty());
        self.function
    }

    fn build_stmt(&mut self, bb_builder: &mut BasicBlockBuilder, global_scope: &GlobalScope, stmt: &HIRStmt) {
        match &stmt.kind {
            HIRStmtKind::Expr {
                expr
            } => {
                let value = self.build_expr(bb_builder, global_scope, expr);
                let ty = expr.ty.clone();
                bb_builder.add_instruction(&mut self.function, Instruction::new(InstructionKind::Value(value), ty.into()));
            }
            HIRStmtKind::Decl {
                initializer,
                variable_idx
            } => {
                let value = initializer.as_ref().map(|initializer| self.build_expr(bb_builder, global_scope, initializer));
                let ty = global_scope.variables.get(*variable_idx).ty.clone().into();
                let instruction_idx = bb_builder.add_instruction(&mut self.function, Instruction::new(
                    InstructionKind::Value(value.unwrap_or(Value::Void)), ty));
                self.write_variable(*variable_idx, bb_builder.current_bb, instruction_idx);
            }
            HIRStmtKind::Loop {
                body
            } => {
                let pred = bb_builder.terminate_and(&mut self.function, |loop_bb| TerminatorKind::Jump(loop_bb));
                self.seal_block(pred, global_scope);
                let body_bb = bb_builder.current_bb;
                tracing::debug!("Entering loop at {:?}", body_bb);
                self.push_loop();
                for stmt in body.iter() {
                    self.build_stmt(bb_builder, global_scope, stmt);
                }
                if !self.function.basic_blocks.get_or_panic(bb_builder.current_bb).is_terminated() {
                    bb_builder.terminate(&mut self.function, TerminatorKind::Jump(body_bb));
                    self.seal_block(bb_builder.current_bb, global_scope);
                }
                // todo: reevaluate this once we add continue
                tracing::debug!("Sealing loop body");
                self.seal_block(body_bb, global_scope);
                let exit_block = bb_builder.start_new_bb(&mut self.function);
                tracing::debug!("Exiting loop at {:?}", exit_block);
                self.pop_loop_and_update(exit_block);
                self.seal_block(exit_block, global_scope);
            }
            HIRStmtKind::Return {
                expr
            } => {
                let value = self.build_expr(bb_builder, global_scope, expr);
                if self.function.basic_blocks.get_or_panic(bb_builder.current_bb).is_terminated() {
                    bb_builder.start_new_bb(&mut self.function);
                }
                let bb = bb_builder.terminate(&mut self.function, TerminatorKind::Return {
                    value,
                });
                self.seal_block(bb, global_scope);
            }
            HIRStmtKind::If {
                condition,
                then_body,
                else_body
            } => {
                tracing::debug!("Building if statement");
                tracing::debug!("Building condition");
                let condition = self.build_expr(bb_builder, global_scope, condition);
                let pred = bb_builder.current_bb;
                let then_start_bb = bb_builder.start_new_bb(&mut self.function);
                let else_start_bb = bb_builder.start_new_bb(&mut self.function);
                bb_builder.set_bb(pred);
                bb_builder.terminate(&mut self.function, TerminatorKind::SwitchInt {
                    value: condition,
                    cases: vec![
                        (0, else_start_bb),
                    ],
                    default: then_start_bb,
                });
                tracing::debug!("Built condition");
                // self.seal_block(pred, global_scope);
                self.seal_block(then_start_bb, global_scope);
                self.seal_block(else_start_bb, global_scope);
                tracing::debug!("Building then body");
                bb_builder.set_bb(then_start_bb);
                for stmt in then_body.iter() {
                    self.build_stmt(bb_builder, global_scope, stmt);
                }
                tracing::debug!("Built then body");
                let then_exit_bb = bb_builder.current_bb;
                tracing::debug!("Building else body");
                bb_builder.set_bb(else_start_bb);
                for stmt in else_body.iter() {
                    self.build_stmt(bb_builder, global_scope, stmt);
                }
                tracing::debug!("Built else body");
                let else_exit_bb = bb_builder.current_bb;
                let if_end_bb = bb_builder.start_new_bb(&mut self.function);
                tracing::debug!("Building if terminator");
                self.function.basic_blocks.get_mut_or_panic(then_exit_bb).maybe_set_terminator(
                    TerminatorKind::Jump(if_end_bb)
                );
                self.function.basic_blocks.get_mut_or_panic(else_exit_bb).maybe_set_terminator(
                    TerminatorKind::Jump(if_end_bb)
                );
                tracing::debug!("Built if terminator");
                self.seal_block(if_end_bb, global_scope);
            }
            HIRStmtKind::Block { body } => {
                for stmt in body.iter() {
                    self.build_stmt(bb_builder, global_scope, stmt);
                }
            }
            HIRStmtKind::Assign {
                rhs,
                lhs
            } => {
                let value = self.build_expr(bb_builder, global_scope, rhs);
                let instruction_idx = bb_builder.add_instruction(&mut self.function, Instruction::new(
                    InstructionKind::Value(value), rhs.ty.clone().into()));
                self.write_variable(*lhs, bb_builder.current_bb, instruction_idx);
            }
            HIRStmtKind::Break => {
                let break_block = bb_builder.terminate(&mut self.function, TerminatorKind::Unreachable);
                // self.seal_block(break_block, global_scope);
                self.push_depending_block(break_block);
            }
        }
    }

    fn push_depending_block(&mut self, bb: BasicBlockIdx) {
        self.loops.last_mut().unwrap().push(bb);
    }

    fn pop_loop_and_update(&mut self, exit_block: BasicBlockIdx) {
        let basic_blocks_to_update = self.pop_loop();
        for bb in basic_blocks_to_update.iter() {
            let bb = self.function.basic_blocks.get_mut_or_panic(*bb);
            assert_eq!(bb.terminator.kind, TerminatorKind::Unreachable);
            bb.set_terminator(TerminatorKind::Jump(exit_block));
        }
    }

    fn build_expr(&mut self, bb_builder: &mut BasicBlockBuilder, global_scope: &GlobalScope, expr: &HIRExpr) -> Value {
        match &expr.kind {
            HIRExprKind::Number(value) => Value::ConstantInt(*value),
            HIRExprKind::Bool(value) => Value::ConstantInt(if *value { 1 } else { 0 }),
            HIRExprKind::Binary {
                lhs,
                operator,
                rhs
            } => {
                let lhs = self.build_expr(bb_builder, global_scope, lhs);
                let rhs = self.build_expr(bb_builder, global_scope, rhs);
                let ty = expr.ty.clone().into();
                let instruction_ref = bb_builder.add_instruction(
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
            HIRExprKind::Unary {
                operator,
                operand
            } => {
                let operand = self.build_expr(bb_builder, global_scope, operand);
                let ty = expr.ty.clone().into();
                let instruction_ref = bb_builder.add_instruction(
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
                let instruction_ref = self.read_variable(*variable_idx, bb_builder.current_bb, global_scope).unwrap();
                Value::InstructionRef(instruction_ref)
            }
            HIRExprKind::Call {
                function_idx,
                arguments
            } => {
                let arguments = arguments.iter().map(
                    |arg| self.build_expr(bb_builder, global_scope, arg)
                ).collect();
                let ty = expr.ty.clone().into();
                let instruction_idx = bb_builder.add_instruction(
                    &mut self.function,
                    Instruction::new(
                        InstructionKind::Call {
                            // todo: fix this
                            function_idx: FunctionIdx::new(function_idx.as_index()),
                            arguments,
                        },
                        ty,
                    ),
                );
                Value::InstructionRef(instruction_idx)
            }
            HIRExprKind::Unit => Value::Void,
        }
    }

    #[inline]
    pub fn push_loop(&mut self) {
        self.loops.push(vec![]);
    }

    #[inline]
    pub fn pop_loop(&mut self) -> Vec<BasicBlockIdx> {
        self.loops.pop().unwrap()
    }

    #[inline]
    /// Records the definition of a variable in the current basic block.
    pub fn write_variable(&mut self, variable: VariableIdx, bb_idx: BasicBlockIdx, instruction: InstructionIdx) {
        self.definitions.entry(variable).or_default().insert(bb_idx, instruction);
    }

    /// Returns the latest definition of a variable above and including the given basic block.
    /// If there is no definition, it returns None.
    ///
    /// It first checks if there is a definition in the given basic block (local definition).
    /// todo!()
    pub fn read_variable(&mut self, variable: VariableIdx, bb_idx: BasicBlockIdx, scope: &GlobalScope) -> Option<InstructionIdx> {
        let definitions = self.definitions.get(&variable)?;
        match definitions.get(&bb_idx) {
            Some(instruction) => Some(*instruction),
            None => {
                self.read_variable_recursive(variable, bb_idx, scope)
            }
        }
    }

    pub fn read_variable_recursive(&mut self, variable: VariableIdx, bb_idx: BasicBlockIdx, scope: &GlobalScope) -> Option<InstructionIdx> {
        let predecessors = self.function.predecessors();
        let preceding_bbs = predecessors.get_immediate(bb_idx)?;
        let instruction_ref = if !self.is_sealed(bb_idx) {
            tracing::debug!("Found unsealed block {:?} for variable {:?}. Inserting operandless phi", bb_idx, variable);
            let instruction_ref = self.add_operandless_phi_to_bb(variable, bb_idx, scope);
            self.incomplete_phis.entry(bb_idx).or_default().push((instruction_ref, variable));
            instruction_ref
        } else if preceding_bbs.len() == 1 {
            // Optimize the common use case of a single predecessor
            self.read_variable(variable, preceding_bbs[0], scope)?
        } else {
            // Break potential cycles with operandless phi
            tracing::debug!("Inserting operandless phi for variable {:?} in block {:?}", variable, bb_idx);
            let instruction_ref = self.add_operandless_phi_to_bb(variable, bb_idx, scope);
            self.write_variable(variable, bb_idx, instruction_ref);

            self.add_phi_operands(instruction_ref, variable, &preceding_bbs, scope);
            // todo: remove trivial phi
            // self.try_remove_trivial_phi(phi);
            instruction_ref
        };
        self.write_variable(variable, bb_idx, instruction_ref);
        Some(instruction_ref)
    }

    fn add_operandless_phi_to_bb(&mut self, variable: VariableIdx, bb: BasicBlockIdx, scope: &GlobalScope) -> InstructionIdx {
        let instruction_ref = self.function.instructions.push(
            Instruction::new(InstructionKind::Phi(PhiNode::operandless()), scope.variables[variable].ty.clone().into(),
            ));
        let instructions = self.function.basic_blocks.get_or_panic(bb).instructions.clone();
        let mut instructions_with_phi = vec![instruction_ref];
        instructions_with_phi.extend(instructions);
        self.function.basic_blocks.get_mut_or_panic(bb).instructions = instructions_with_phi;
        instruction_ref
    }

    fn try_remove_trivial_phi(&self, phi: &mut PhiNode) {}

    fn seal_block(&mut self, bb_idx: BasicBlockIdx, global_scope: &GlobalScope) {
        if self.is_sealed(bb_idx) {
            tracing::debug!("Tried to seal block {:?} after it had been sealed", bb_idx);
            return;
        }
        tracing::debug!("Sealing {:?}", bb_idx);
        // todo: do not clone
        if let Some(incomplete_phis) = self.incomplete_phis.get(&bb_idx).cloned() {
            tracing::debug!("{:?} has incomplete phis {:?}", bb_idx, incomplete_phis);
            let predecessors = self.function.predecessors();
            for (incomplete_phi, variable_idx) in incomplete_phis.iter().copied() {
                self.add_phi_operands(incomplete_phi, variable_idx, &predecessors.get_immediate(bb_idx).unwrap(), global_scope);
            }
        }
        self.incomplete_phis.remove(&bb_idx);
        self.sealed_blocks.insert(bb_idx);
    }

    fn add_phi_operands(&mut self, phi: InstructionIdx, variable: VariableIdx, preds: &Vec<BasicBlockIdx>, scope: &GlobalScope) {
        tracing::debug!("Adding phi operands for {:?}  with predecessors {:?}", phi, preds);
        for pred in preds.iter().copied() {
            let variable_ref = self.read_variable(variable, pred, scope).unwrap_or_else(|| {
                bug!("No definition for variable {:?} in block {:?}", variable, pred)
            });
            let phi = self.function.instructions[phi].kind.as_phi_mut().unwrap();
            phi.push(variable_ref)
        }
    }
    fn is_sealed(&self, bb_idx: BasicBlockIdx) -> bool {
        self.sealed_blocks.contains(&bb_idx)
    }
}


struct BasicBlockBuilder {
    current_bb: BasicBlockIdx,
}

impl BasicBlockBuilder {
    pub fn new(ctx: &mut Function) -> Self {
        Self {
            current_bb: ctx.new_basic_block(),
        }
    }

    pub fn add_instruction(&mut self, ctx: &mut Function, instruction: Instruction) -> InstructionIdx {
        if self.get_current_bb(ctx).is_terminated() {
            bug!("bb already has a terminator: {:?}", self.get_current_bb(ctx).terminator.kind)
        }
        let instruction_idx = ctx.instructions.push(instruction);
        self.get_current_bb_mut(ctx).instructions.push(instruction_idx);
        instruction_idx
    }

    /// Starts a new basic block and returns it.
    pub fn start_new_bb(&mut self, ctx: &mut Function) -> BasicBlockIdx {
        let new_bb = ctx.new_basic_block();
        tracing::debug!("Starting new basic block {:?}", new_bb);
        self.current_bb = new_bb;
        new_bb
    }

    /// Terminates and returns the current basic block.
    pub fn terminate(&mut self, ctx: &mut Function, terminator: TerminatorKind) -> BasicBlockIdx {
        let bb = self.get_current_bb_mut(ctx);
        if bb.is_terminated() {
            bug!("bb already has a terminator: {:?}", bb.terminator.kind)
        }
        bb.set_terminator(terminator);
        self.current_bb
    }

    /// Terminates the current basic block and starts a new one.
    ///
    /// Returns the previous basic block.
    pub fn terminate_and(&mut self, ctx: &mut Function, terminator_builder: impl FnOnce(BasicBlockIdx) -> TerminatorKind) -> BasicBlockIdx {
        let new_bb = ctx.new_basic_block();
        self.get_current_bb_mut(ctx).set_terminator(terminator_builder(new_bb));
        std::mem::replace(&mut self.current_bb, new_bb)
    }

    pub fn set_bb(&mut self, bb: BasicBlockIdx) {
        tracing::debug!("Setting current basic block to {:?}", bb);
        self.current_bb = bb;
    }

    fn get_current_bb_mut<'ctx>(&mut self, ctx: &'ctx mut Function) -> &'ctx mut BasicBlock {
        ctx.basic_blocks.get_mut_or_panic(self.current_bb)
    }

    fn get_current_bb<'ctx>(&self, ctx: &'ctx Function) -> &'ctx BasicBlock {
        ctx.basic_blocks.get_or_panic(self.current_bb)
    }
}


#[derive(Debug, Clone)]
/// A map from basic blocks to their immediate dominator.
struct Dominators(HashMap<BasicBlockIdx, BasicBlockIdx>);

impl Dominators {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn from_immediate_dominators(immediate_dominators: HashMap<BasicBlockIdx, BasicBlockIdx>) -> Self {
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
        self.0.entry(bb).or_insert_with(|| Vec::with_capacity(1)).push(successor);
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
        self.0.entry(bb).or_insert_with(|| HashSet::with_capacity(1)).insert(successor);
    }
}

impl Function {
    fn instruction_to_basic_block(&self) -> HashMap<InstructionIdx, BasicBlockIdx> {
        let mut instruction_to_basic_block = HashMap::new();
        for (bb_idx, bb) in self.basic_blocks.indexed_iter_as_option().flatten() {
            for instruction_idx in bb.instructions.iter().copied() {
                instruction_to_basic_block.insert(instruction_idx, bb_idx);
            }
        }
        instruction_to_basic_block
    }

    fn compute_all_dominators(&self) -> HashMap<BasicBlockIdx, HashSet<BasicBlockIdx>> {
        let mut dominators = HashMap::new();
        let start_node = self.basic_blocks.first_index();
        let all_nodes = self.basic_blocks.indices().collect::<HashSet<_>>();

        // Initialize dominators: start node dominates itself; all others dominate all nodes
        for &node in &all_nodes {
            dominators.insert(node, if node == start_node { HashSet::from([start_node]) } else { all_nodes.clone() });
        }

        let mut changed = true;
        let predecessors = self.predecessors();
        while changed {
            changed = false;

            for &node in &all_nodes {
                if node == start_node {
                    continue;
                }

                let mut new_doms = all_nodes.clone();
                let preds = predecessors.get_immediate(node);
                if let Some(preds) = preds {
                    for &pred in preds {
                        let pred_doms = dominators.get(&pred).unwrap();
                        new_doms = new_doms.intersection(pred_doms).cloned().collect();
                    }
                }
                new_doms.insert(node); // A node always dominates itself

                if new_doms != *dominators.get(&node).unwrap() {
                    dominators.insert(node, new_doms);
                    changed = true;
                }
            }
        }

        dominators
    }

    fn extract_immediate_dominators(&self, all_dominators: HashMap<BasicBlockIdx, HashSet<BasicBlockIdx>>) -> HashMap<BasicBlockIdx, BasicBlockIdx> {
        let mut immediate_dominators = HashMap::new();
        let start_node = self.basic_blocks.first_index();
        dbg!(&all_dominators);
        for (&node, dominators) in all_dominators.iter() {
            if node == start_node {
                continue;
            }

            // Find the immediate dominator based on the provided definition
            let idom = dominators.iter()
                .filter(|&&dom| dom != node) // Strictly dominates N
                .filter(|&&dom|
                    // Every other dominator P that strictly dominates N also dominates M
                    dominators.iter().all(|&p| p == dom || p == node || !all_dominators.get(&p).unwrap().contains(&node) || all_dominators.get(&p).unwrap().contains(&dom)))
                .cloned()
                .next()
                .unwrap(); // There should always be one immediate dominator

            immediate_dominators.insert(node, idom);
        }

        immediate_dominators
    }


    fn dominators(&self) -> Dominators {
        let all_dominators = self.compute_all_dominators();
        let idoms = self.extract_immediate_dominators(all_dominators);
        Dominators::from_immediate_dominators(idoms)
    }


    pub fn successors(&self) -> Successors {
        let mut successors = Successors::new();
        for (idx, bb) in self.basic_blocks.indexed_iter_as_option().flatten() {
            match &bb.terminator.kind {
                TerminatorKind::Jump(target) => {
                    successors.insert_immediate(idx, *target);
                }
                TerminatorKind::SwitchInt {
                    default,
                    cases,
                    ..
                } => {
                    successors.insert_immediate(idx, *default);
                    for (_, target) in cases {
                        successors.insert_immediate(idx, *target);
                    }
                }
                TerminatorKind::Return { .. } => {}
                TerminatorKind::Unresolved | TerminatorKind::Unreachable => {}
            }
        }
        successors
    }

    pub fn predecessors(&self) -> Predecessors {
        let mut predecessors = Predecessors::new();
        for (idx, bb) in self.basic_blocks.indexed_iter_as_option().flatten() {
            match &bb.terminator.kind {
                TerminatorKind::Jump(target) => {
                    predecessors.insert_immediate(*target, idx);
                }
                TerminatorKind::SwitchInt {
                    default,
                    cases,
                    ..
                } => {
                    predecessors.insert_immediate(*default, idx);
                    for (_, target) in cases {
                        predecessors.insert_immediate(*target, idx);
                    }
                }
                TerminatorKind::Return { .. } => {}
                TerminatorKind::Unresolved | TerminatorKind::Unreachable => {}
            }
        }
        predecessors
    }
}

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use fusion_compiler::Idx;

    use crate::mir::{BasicBlockIdx, MIR};

    pub fn assert_mir(input: &str, expected_mir: &str) -> MIR {
        let mut compilation_unit = crate::compilation_unit::CompilationUnit::compile(input).unwrap();
        let hir = crate::hir::HIRBuilder::new().build(&compilation_unit.ast, &mut compilation_unit.global_scope);
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
        let predecessors = mir.functions[mir.functions.first_index()].predecessors();
        assert_eq!(predecessors.get_all(BasicBlockIdx::new(0)), HashSet::new());
        assert_eq!(predecessors.get_all(BasicBlockIdx::new(1)), HashSet::from([BasicBlockIdx::new(0), BasicBlockIdx::new(4)]));
        assert_eq!(predecessors.get_all(BasicBlockIdx::new(2)), HashSet::from([BasicBlockIdx::new(1)]));
        assert_eq!(predecessors.get_all(BasicBlockIdx::new(3)), HashSet::from([BasicBlockIdx::new(1)]));
        assert_eq!(predecessors.get_all(BasicBlockIdx::new(4)), HashSet::from([BasicBlockIdx::new(2)]));
        assert_eq!(predecessors.get_all(BasicBlockIdx::new(5)), HashSet::from([BasicBlockIdx::new(3)]));
    }
}
