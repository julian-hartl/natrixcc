use std::collections::VecDeque;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;

use cranelift_entity::{entity_impl, EntityRef, PrimaryMap};
use daggy::{NodeIndex, Walker};
use daggy::petgraph::visit::{Bfs, IntoEdges};
use iced_x86::Instruction;
use index_vec::IndexVec;
use rustc_hash::FxHashMap;
use slotmap::{HopSlotMap, new_key_type};
use smallvec::{SmallVec, smallvec};
use tracing::debug;

pub use abi::Abi;
use firc_middle;
use firc_middle::instruction::CmpOp;
use firc_middle::ty::Type;
pub use module::Module;

use crate::codegen::{machine, selection_dag};
use crate::codegen::machine::asm::Assembler;
use crate::codegen::selection_dag::{Immediate, MachineOp, Op, Operand, PseudoOp};

pub mod abi;
pub mod asm;
pub mod module;


pub trait PhysicalRegister: Debug + Clone + Copy + PartialEq + Eq + Sized {
    fn name(&self) -> &'static str;

    fn all() -> &'static [Self];

    fn is_gp(&self) -> bool;

    fn size(&self) -> Size;
}


#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct VReg(u32);

entity_impl!(VReg, "v");

impl VReg {
    pub fn size<A>(self, func: &Function<A>) -> Size where A: Abi {
        func.get_vreg(self).size
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Register<A: Abi> {
    Virtual(VReg),
    Physical(A::REG),
}

impl<A: Abi> From<VReg> for Register<A> {
    fn from(vreg: VReg) -> Self {
        Self::Virtual(vreg)
    }
}

impl<A: Abi> Register<A> {
    pub fn try_as_virtual(&self) -> Option<VReg> {
        match self {
            Register::Virtual(virt_reg) => Some(*virt_reg),
            Register::Physical(_) => None,
        }
    }

    pub fn try_as_physical(&self) -> Option<A::REG> {
        match self {
            Register::Virtual(_) => None,
            Register::Physical(phys_reg) => Some(*phys_reg),
        }
    }

    pub fn size(&self, func: &Function<A>) -> Size {
        match self {
            Register::Virtual(vreg) => vreg.size(func),
            Register::Physical(phys_reg) => phys_reg.size(),
        }
    }
}

impl<A: Abi> std::marker::Copy for Register<A> {}

impl<A: Abi> Display for Register<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Virtual(virt_reg) => write!(f, "{}", virt_reg),
            Register::Physical(phys_reg) => write!(f, "${}", phys_reg.name()),
        }
    }
}

new_key_type! {
    pub struct InstrId;
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr<A: Abi> {
    Pseudo(PseudoInstr<A>),
    Machine(A::I),
}

impl<A: Abi> Instr<A> {
    pub fn name(&self) -> &'static str {
        match self {
            Instr::Pseudo(pseudo) => pseudo.name(),
            Instr::Machine(machine) => machine.name(),
        }
    }

    pub fn ins(&self) -> SmallVec<[InstrOperand<A>; 2]> {
        match self {
            Instr::Pseudo(pseudo) => pseudo.ins(),
            Instr::Machine(machine) => machine.ins(),
        }
    }

    pub fn ins_mut(&mut self) -> SmallVec<[InstrInsMut<'_, A>; 2]> {
        match self {
            Instr::Pseudo(instr) => instr.ins_mut(),
            Instr::Machine(instr) => instr.ins_mut(),
        }
    }

    pub fn out(&self) -> Option<Register<A>> {
        match self {
            Instr::Pseudo(pseudo) => Some(pseudo.out()),
            Instr::Machine(machine) => machine.out().map(|reg| reg.into()),
        }
    }

    pub fn out_mut(&mut self) -> Option<&mut Register<A>> {
        match self {
            Instr::Pseudo(pseudo) => Some(pseudo.out_mut()),
            Instr::Machine(machine) => machine.out_mut().map(|reg| reg.into()),
        }
    }

    pub fn try_as_machine(&self) -> Option<&A::I> {
        match self {
            Instr::Pseudo(_) => None,
            Instr::Machine(machine) => Some(machine),
        }
    }
}

pub trait MachineInstr: Debug + PartialEq + Eq + Clone {
    type Abi: Abi;

    fn name(&self) -> &'static str;

    fn out(&self) -> Option<Register<Self::Abi>>;

    fn out_mut(&mut self) -> Option<&mut Register<Self::Abi>>;

    fn ins(&self) -> SmallVec<[InstrOperand<Self::Abi>; 2]>;

    fn ins_mut(&mut self) -> SmallVec<[InstrInsMut<'_, Self::Abi>; 2]>;
}

#[derive(Debug, Clone)]
pub enum InstrOperand<A: Abi> {
    Reg(Register<A>),
    Imm(Immediate),
    Label(BasicBlockId)
}

#[derive(Debug)]
pub enum InstrInsMut<'a, A: Abi> {
    Reg(&'a mut Register<A>),
    // Imm(&'a mut Immediate),
}

impl<A: Abi> Display for InstrOperand<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrOperand::Reg(reg) => write!(f, "{}", reg),
            InstrOperand::Imm(imm) => write!(f, "{}", imm),
            InstrOperand::Label(label) => write!(f, "{}", label),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PseudoInstr<A: Abi> {
    Copy(Register<A>, Register<A>),
}

impl<A: Abi> PseudoInstr<A> {
    pub fn name(&self) -> &'static str {
        match self {
            PseudoInstr::Copy(_, _) => "COPY",
        }
    }

    pub fn ins(&self) -> SmallVec<[InstrOperand<A>; 2]> {
        match self {
            PseudoInstr::Copy(_, to) => {
                smallvec![
                    InstrOperand::Reg(*to),
                ]
            }
        }
    }

    pub fn ins_mut(&mut self) -> SmallVec<[InstrInsMut<'_, A>; 2]> {
        match self {
            PseudoInstr::Copy(_, to) => {
                smallvec![
                    InstrInsMut::Reg(to),
                ]
            }
        }
    }

    pub fn out(&self) -> Register<A> {
        match self {
            PseudoInstr::Copy(dest, _) => *dest,
        }
    }

    pub fn out_mut(&mut self) -> &mut Register<A> {
        match self {
            PseudoInstr::Copy(dest, _) => dest,
        }
    }
}

pub trait Backend {
    type ABI: Abi;

    type P: Pattern<ABI=Self::ABI>;

    fn patterns() -> &'static [Self::P];

    fn expand_pseudo_instruction(instr: &PseudoInstr<Self::ABI>) -> SmallVec<[Instr<Self::ABI>; 2]>;

    fn new() -> Self;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PatternIn {
    Mov(PatternInOutput, PatternInOperand),
    Sub(PatternInOutput, PatternInOperand, PatternInOperand),
    Add(PatternInOutput, PatternInOperand, PatternInOperand),
    Ret(Option<PatternInOperand>),
    Cmp(PatternInOutput, CmpOp, PatternInOperand, PatternInOperand),
    Br,
    CondBr(PatternInOperand),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PatternInOutput {
    Reg(Size)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PatternInOperand {
    Reg(Size),
    Imm(Size),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedMovPattern<A: Abi> {
    pub dest: MatchedPatternOutput<A>,
    pub src: MatchedPatternOperand<A>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedSubPattern<A: Abi> {
    pub dest: MatchedPatternOutput<A>,
    pub lhs: MatchedPatternOperand<A>,
    pub rhs: MatchedPatternOperand<A>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedAddPattern<A: Abi> {
    pub dest: MatchedPatternOutput<A>,
    pub lhs: MatchedPatternOperand<A>,
    pub rhs: MatchedPatternOperand<A>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedRetPattern<A: Abi> {
    pub value: Option<MatchedPatternOperand<A>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedBrPattern {
    pub target: BasicBlockId,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedCmpPattern<A: Abi> {
    pub dest: MatchedPatternOutput<A>,
    pub cmp_op: CmpOp,
    pub lhs: MatchedPatternOperand<A>,
    pub rhs: MatchedPatternOperand<A>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MatchedCondBrPattern<A: Abi> {
    pub cond: MatchedPatternOperand<A>,
    pub true_target: BasicBlockId,
    pub false_target: BasicBlockId,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MatchedPattern<A: Abi> {
    Mov(MatchedMovPattern<A>),
    Sub(MatchedSubPattern<A>),
    Add(MatchedAddPattern<A>),
    Cmp(MatchedCmpPattern<A>),
    Ret(MatchedRetPattern<A>),
    CondBr(MatchedCondBrPattern<A>),
    Br(MatchedBrPattern),
}

impl<A: Abi> MatchedPattern<A> {
    pub fn try_as_mov(&self) -> Option<&MatchedMovPattern<A>> {
        match self {
            MatchedPattern::Mov(mov) => Some(mov),
            _ => None,
        }
    }

    pub fn try_as_sub(&self) -> Option<&MatchedSubPattern<A>> {
        match self {
            MatchedPattern::Sub(sub) => Some(sub),
            _ => None,
        }
    }

    pub fn try_as_add(&self) -> Option<&MatchedAddPattern<A>> {
        match self {
            MatchedPattern::Add(add) => Some(add),
            _ => None,
        }
    }

    pub fn try_as_ret(&self) -> Option<&MatchedRetPattern<A>> {
        match self {
            MatchedPattern::Ret(ret) => Some(ret),
            _ => None,
        }
    }

    pub fn try_as_br(&self) -> Option<&MatchedBrPattern> {
        match self {
            MatchedPattern::Br(br) => Some(br),
            _ => None,
        }
    }

    pub fn try_as_cmp(&self) -> Option<&MatchedCmpPattern<A>> {
        match self {
            MatchedPattern::Cmp(cmp) => Some(cmp),
            _ => None,
        }
    }

    pub fn try_as_cond_br(&self) -> Option<&MatchedCondBrPattern<A>> {
        match self {
            MatchedPattern::CondBr(cond_br) => Some(cond_br),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MatchedPatternOutput<A: Abi> {
    Reg(Register<A>)
}

impl<A: Abi> MatchedPatternOutput<A> {
    pub fn try_as_reg(&self) -> Option<&Register<A>> {
        match self {
            MatchedPatternOutput::Reg(reg) => Some(reg),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MatchedPatternOperand<A: Abi> {
    Reg(Register<A>),
    Imm(Immediate),
}

impl<A: Abi> MatchedPatternOperand<A> {
    pub fn try_as_reg(&self) -> Option<&Register<A>> {
        match self {
            MatchedPatternOperand::Reg(reg) => Some(reg),
            _ => None,
        }
    }

    pub fn try_as_imm(&self) -> Option<&Immediate> {
        match self {
            MatchedPatternOperand::Imm(imm) => Some(imm),
            _ => None,
        }
    }
}

pub trait Pattern: Sized + Debug + Clone + PartialEq + Eq + 'static {
    type ABI: Abi;

    fn in_(&self) -> PatternIn;

    fn into_instr(self, in_: MatchedPattern<Self::ABI>) -> SmallVec<[Instr<<Self as Pattern>::ABI>; 2]>;
}


#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Size {
    Byte,
    Word,
    DWord,
    QWord,
}

impl Size {
    pub fn from_ty(ty: &Type) -> Self {
        let bit_width = ty.size() * 8;
        Self::from_bit_width(bit_width)
    }

    pub fn from_bit_width(bit_width: u32) -> Self {
        if bit_width <= 8 {
            Self::Byte
        } else if bit_width <= 16 {
            Self::Word
        } else if bit_width <= 32 {
            Self::DWord
        } else if bit_width <= 64 {
            Self::QWord
        } else {
            panic!("Invalid bit width: {}", bit_width)
        }
    }

    pub fn bit_width(&self) -> u32 {
        match self {
            Size::Byte => 8,
            Size::Word => 16,
            Size::DWord => 32,
            Size::QWord => 64,
        }
    }
}

impl PartialOrd for Size {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.bit_width().partial_cmp(&other.bit_width())
    }
}

impl From<&Type> for Size {
    fn from(value: &Type) -> Self {
        Self::from_ty(value)
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct VRegInfo {
    pub size: Size,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct FunctionId(u32);

entity_impl!(FunctionId, "fun");

#[derive(Debug, Clone)]
pub struct Function<A: Abi> {
    pub name: String,
    pub basic_blocks: IndexVec<BasicBlockId, BasicBlock<A>>,
    pub(crate) instructions: HopSlotMap<InstrId, Instr<A>>,
    pub(crate) vregs: PrimaryMap<VReg, VRegInfo>,
}

impl<A: Abi> Function<A> {
    pub fn new(
        name: String,
    ) -> Self {
        Self {
            name,
            basic_blocks: IndexVec::default(),
            vregs: PrimaryMap::new(),
            instructions: HopSlotMap::with_key(),
        }
    }

    pub fn alloc_vreg(&mut self, size: Size) -> VReg {
        self.vregs.push(VRegInfo { size })
    }

    pub fn get_vreg(&self, vreg: VReg) -> &VRegInfo {
        &self.vregs[vreg]
    }

    pub fn create_bb(&mut self) -> BasicBlockId {
        self.basic_blocks.push(Default::default())
    }

    pub fn assemble(&self) -> Vec<u8> {
        debug!("Assembling function {}", self.name);
        let mut asm = A::get_assembler();
        for (bb_id, bb) in self.basic_blocks.iter_enumerated() {
            debug!("Assembling basic block {}", bb_id);
            asm.begin_basic_block(bb_id);
            for (_, instr) in bb.instructions(self) {
                debug!("Assembling instruction {:?}", instr);
                asm.assemble(instr.try_as_machine().expect("Pseudo instruction was not expanded"));
            }
        }
        debug!("Finishing assembly for function {}", self.name);
        debug!("{}", asm.format());
        asm.finish()
    }


    pub fn ordered_instructions(&self) -> impl IntoIterator<Item=(BasicBlockId, InstrId)> + '_ {
        self.basic_blocks.iter_enumerated().flat_map(|(bb_id, bb)| bb.instructions.iter().copied().map(move |instr_id| (bb_id, instr_id)))
    }


    pub fn expand_pseudo_instructions<B>(&mut self) where B: Backend<ABI=A> {
        let mut i = 0;
        let instructions = self.ordered_instructions().into_iter().collect::<Vec<_>>();
        while let Some((bb_id, instr_id)) = instructions.get(i) {
            let instr = &mut self.instructions[*instr_id];
            if let Instr::Pseudo(pseudo_instr) = instr {
                let expanded = B::expand_pseudo_instruction(pseudo_instr);
                self.instructions.remove(*instr_id);
                let instr_idx = self.basic_blocks[*bb_id].instructions.iter().position(|id| id == instr_id).unwrap();
                self.basic_blocks[*bb_id].instructions.remove(instr_idx);
                for (n, instr) in expanded.into_iter().enumerate() {
                    let instr_id = self.instructions.insert(instr);
                    self.basic_blocks[*bb_id].instructions.insert(instr_idx + n, instr_id);
                }
            }
            i += 1;
        }
    }

    pub fn remove_instruction(&mut self, bb_id: BasicBlockId, instr_id: InstrId) {
        self.instructions.remove(instr_id);
        let instr_idx = self.basic_blocks[bb_id].instructions.iter().position(|id| id == &instr_id).unwrap();
        self.basic_blocks[bb_id].instructions.remove(instr_idx);
    }
}

impl<A: Abi> Display for Function<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "function {}:", self.name)?;
        for (bb_id, bb) in self.basic_blocks.iter_enumerated() {
            writeln!(f, "{}: ", bb_id)?;
            for (_, instr) in bb.instructions(self) {
                write!(f, "  {}", instr.name())?;
                if let Some(out) = instr.out() {
                    write!(f, " {},", out)?;
                }
                for (i, operand) in instr.ins().into_iter().enumerate() {
                    write!(f, " {}", operand)?;
                }
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

index_vec::define_index_type! {
    pub struct BasicBlockId = u32;

    DISPLAY_FORMAT = "bb{}";
}
#[derive(Debug, Clone, Default)]
pub struct BasicBlock<A: Abi> {
    instructions: Vec<InstrId>,
    marker: std::marker::PhantomData<A>,
}

impl<A: Abi> BasicBlock<A> {
    pub fn instructions<'func>(&'func self, ctx: &'func Function<A>) -> impl ExactSizeIterator<Item=(InstrId, &Instr<A>)> {
        self.instructions.iter().copied().map(move |instr_id| {
            (instr_id, &ctx.instructions[instr_id])
        })
    }
}

#[derive(Debug)]
pub struct FunctionBuilder<B: Backend> {
    function: Function<B::ABI>,
    backend: B,
    bb_mapping: FxHashMap<firc_middle::cfg::BasicBlockId, BasicBlockId>,
}

impl<B: Backend> FunctionBuilder<B> {
    pub fn new() -> Self {
        Self {
            function: Function::new(Default::default()),
            backend: B::new(),
            bb_mapping: FxHashMap::default(),
        }
    }

    pub fn build(mut self, function: &mut firc_middle::Function) -> Function<B::ABI> {
        self.function.name = function.name.clone();
        debug!("Building machine function for function {}", function.name);
        let mut sel_dag_builder = selection_dag::Builder::<B::ABI>::new(&mut self.function);
        let mut sel_dag = sel_dag_builder.build(function);
        for bb in function.cfg.basic_block_ids_ordered() {
            self.create_bb(bb);
        }
        for mbb_id in self.function.basic_blocks.indices() {
            let bb = *self.bb_mapping.iter().find(|(_, mbb)| **mbb == mbb_id).unwrap().0;
            debug!("Building machine basic block for basic block {}", bb);
            let dag = sel_dag.get_bb_dag(bb);
            dag.save_graphviz("out").unwrap();
            let mut node_list = Vec::with_capacity(dag.node_count());
            debug!("Determining traversal order for basic block {}", bb);
            let bfs = Bfs::new(dag.graph(), dag.term_node());
            for n in bfs.iter(dag.graph()) {
                node_list.push(n);
            }
            debug!("Traversal order: {:?}", node_list);
            let mut instructions = Vec::new();
            while let Some(node_id) = node_list.pop() {
                let op = &dag[node_id];
                match op {
                    Op::Pseudo(op) => {
                        debug!("Found pseudo op {:?}", op);
                        match op {
                            PseudoOp::Copy(dest, src) => {
                                instructions.push(
                                    Instr::Pseudo(
                                        PseudoInstr::Copy(
                                            dest.clone(),
                                            src.clone(),
                                        )
                                    )
                                );
                            }
                        }
                    }
                    Op::Machine(op) => {
                        let dag_node_pattern = match op {
                            MachineOp::Mov(dest, src) => {
                                PatternIn::Mov(
                                    PatternInOutput::Reg(dest.size(&self.function)),
                                    self.operand_to_pattern(src),
                                )
                            }
                            MachineOp::Sub(dest, lhs, rhs) => {
                                PatternIn::Sub(
                                    PatternInOutput::Reg(dest.size(&self.function)),
                                    self.operand_to_pattern(lhs),
                                    self.operand_to_pattern(rhs),
                                )
                            }
                            MachineOp::Add(dest, lhs, rhs) => {
                                PatternIn::Add(
                                    PatternInOutput::Reg(dest.size(&self.function)),
                                    self.operand_to_pattern(lhs),
                                    self.operand_to_pattern(rhs),
                                )
                            }
                            MachineOp::Ret(value) => {
                                PatternIn::Ret(
                                    value.as_ref().map(|value| self.operand_to_pattern(value))
                                )
                            }
                            MachineOp::Br(bb_id) => {
                                PatternIn::Br
                            }
                            MachineOp::Cmp(dest, cmp_op, lhs, rhs) => {
                                PatternIn::Cmp(
                                    PatternInOutput::Reg(dest.size(&self.function)),
                                    *cmp_op,
                                    self.operand_to_pattern(lhs),
                                    self.operand_to_pattern(rhs),
                                )
                            }
                            MachineOp::CondBr(cond, true_target, false_target) => {
                                PatternIn::CondBr(
                                    self.operand_to_pattern(cond)
                                )
                            }
                        };
                        let mut matching_pattern = None;
                        debug!("Matching patterns for node {:?}", op);

                        for pattern in B::patterns() {
                            let pattern_in = pattern.in_();
                            debug!("Checking {:?}", pattern_in);
                            debug!("Matching with {:?}", dag_node_pattern);
                            if pattern_in != dag_node_pattern {
                                debug!("Pattern does not match");
                                continue;
                            }
                            debug!("Pattern matches");
                            matching_pattern = Some(pattern.clone());
                            break;
                        }
                        match matching_pattern {
                            None => {
                                panic!("No pattern matched for node {:?}", op);
                            }
                            Some(pattern) => {
                                let generated_instructions = pattern.into_instr(
                                    match op {
                                        MachineOp::Mov(dest, src) => MatchedPattern::Mov(
                                            MatchedMovPattern {
                                                dest: MatchedPatternOutput::Reg(*dest),
                                                src: self.operand_to_matched_pattern_operand(src),
                                            }
                                        ),
                                        MachineOp::Sub(dest, lhs, rhs) => MatchedPattern::Sub(
                                            MatchedSubPattern {
                                                dest: MatchedPatternOutput::Reg(*dest),
                                                lhs: self.operand_to_matched_pattern_operand(lhs),
                                                rhs: self.operand_to_matched_pattern_operand(rhs),
                                            }
                                        ),
                                        MachineOp::Add(dest, lhs, rhs) => MatchedPattern::Add(
                                            MatchedAddPattern {
                                                dest: MatchedPatternOutput::Reg(*dest),
                                                lhs: self.operand_to_matched_pattern_operand(lhs),
                                                rhs: self.operand_to_matched_pattern_operand(rhs),
                                            }
                                        ),
                                        MachineOp::Ret(value) => MatchedPattern::Ret(
                                            MatchedRetPattern {
                                                value: value.as_ref().map(|value| self.operand_to_matched_pattern_operand(value)),
                                            }
                                        ),
                                        MachineOp::Br(target) => MatchedPattern::Br(
                                            MatchedBrPattern {
                                                target: self.bb_mapping[target],
                                            }
                                        ),
                                        MachineOp::Cmp(dest, cmp_op, lhs, rhs) => MatchedPattern::Cmp(
                                            MatchedCmpPattern {
                                                dest: MatchedPatternOutput::Reg(*dest),
                                                cmp_op: *cmp_op,
                                                lhs: self.operand_to_matched_pattern_operand(lhs),
                                                rhs: self.operand_to_matched_pattern_operand(rhs),
                                            }
                                        ),
                                        MachineOp::CondBr(cond, true_target, false_target) => MatchedPattern::CondBr(
                                            MatchedCondBrPattern {
                                                cond: self.operand_to_matched_pattern_operand(cond),
                                                true_target: self.bb_mapping[true_target],
                                                false_target: self.bb_mapping[false_target],
                                            }
                                        ),
                                    }
                                );
                                debug!("Generated instructions {:?}", generated_instructions);
                                instructions.extend(generated_instructions.into_iter());
                            }
                        }
                    }
                }
            }
            for instr in instructions {
                self.push_instruction(instr, mbb_id);
            }
        }
        debug!("Finished building machine function for function {}", function.name);
        debug!("{}", self.function);
        self.function
    }
    
    fn create_bb(&mut self, bb: firc_middle::cfg::BasicBlockId) -> BasicBlockId {
        let mbb = self.function.create_bb();
        self.bb_mapping.insert(bb, mbb);
        mbb
    }

    fn operand_to_matched_pattern_operand(&self, src: &Operand<<B as Backend>::ABI>) -> MatchedPatternOperand<<B as Backend>::ABI> {
        match src {
            Operand::Reg(reg) => MatchedPatternOperand::Reg(reg.clone()),
            Operand::Imm(imm) => MatchedPatternOperand::Imm(imm.clone()),
        }
    }

    fn operand_to_pattern(&self, src: &Operand<<B as Backend>::ABI>) -> PatternInOperand {
        match src {
            Operand::Reg(reg) => PatternInOperand::Reg(reg.size(&self.function)),
            Operand::Imm(imm) => PatternInOperand::Imm(imm.size()),
        }
    }
    fn push_instruction(&mut self, instr: Instr<B::ABI>, bb: BasicBlockId) {
        let instr_id = self.function.instructions.insert(instr);
        self.function.basic_blocks[bb].instructions.push(instr_id);
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::codegen::isa;
    use crate::firc_middle::cfg;
    use crate::firc_middle::cfg::{RetTerm, TerminatorKind};
    use crate::firc_middle::instruction::{Const, Op};
    use crate::test_utils::create_test_function;

    #[test]
    #[traced_test]
    fn test() {
        let mut function = create_test_function();
        let mut builder = cfg::Builder::new(&mut function);
        let bb = builder.start_bb();
        let (val1, _) = builder.op(None, Op::Const(Const::i32(323))).unwrap();
        let (val2, _) = builder.op(None, Op::Const(Const::i32(90))).unwrap();
        let (return_value, _) = builder.sub(None, Op::Value(val1), Op::Value(val2)).unwrap();
        builder.end_bb(TerminatorKind::Ret(RetTerm::new(Op::Value(return_value))));
        drop(builder);
        let function_builder = super::FunctionBuilder::<isa::x86_64::Backend>::new();
        let function = function_builder.build(&function);
        println!("{:?}", function.basic_blocks);
        println!("{}", function);
        function.assemble();
    }
}



