use std::collections::VecDeque;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::Range;

use cranelift_entity::{entity_impl, EntityRef, PrimaryMap};
use daggy::{NodeIndex, Walker};
use daggy::petgraph::{algo, Directed, Direction};
use daggy::petgraph::prelude::StableGraph;
use daggy::petgraph::visit::{Bfs, DfsPostOrder, IntoEdges};
use iced_x86::Instruction;
use index_vec::{index_vec, IndexVec};
use iter_tools::Itertools;
use rustc_hash::{FxHashMap, FxHashSet};
use slotmap::{HopSlotMap, new_key_type};
use smallvec::{SmallVec, smallvec};
use tracing::debug;

pub use abi::Abi;
use natrix_middle;
use natrix_middle::instruction::CmpOp;
use natrix_middle::ty::Type;
pub use module::Module;

use crate::codegen::{machine, selection_dag};
use crate::codegen::isa::{Architecture, Endianness};
use crate::codegen::machine::abi::calling_convention::Slot;
use crate::codegen::machine::abi::CallingConvention;
use crate::codegen::machine::asm::Assembler;
use crate::codegen::register_allocator::{InstrNumbering, InstrUid, LivenessRepr, ProgPoint};
use crate::codegen::selection_dag::{Immediate, MachineOp, Op, Operand, PseudoOp};

pub mod abi;
pub mod asm;
pub mod module;

pub trait TargetMachine {
    type Abi: Abi;

    type Backend: Backend<ABI=Self::Abi>;

    fn backend() -> Self::Backend {
        Self::Backend::new()
    }

    fn endianness() -> Endianness;

    fn arch() -> Architecture;
}

// #[derive(Debug, Clone)]
// pub struct SubReg<P: PhysicalRegister>((P, Range<u16>));

pub trait PhysicalRegister: Debug + Clone + Copy + PartialEq + Eq + Sized {
    fn name(&self) -> &'static str;

    fn all() -> &'static [Self];

    fn is_gp(&self) -> bool;

    fn size(&self) -> Size;

    fn into_unicorn_emu_reg(self) -> impl Into<i32>;

    /// Returns the sub registers of this register.
    ///
    /// E.g. on x86-64, the sub registers of RAX are EAX, AX, AH and AL.
    fn subregs(&self) -> Option<&'static [Self]>;

    fn superregs(&self) -> Option<&'static [Self]>;

    fn regclass(&self) -> impl Iterator<Item=Self> where Self: 'static {
        self.subregs().into_iter().flatten().copied()
            .chain(
                self.superregs().into_iter().flatten().copied()
            )
            .chain(
                std::iter::once(*self)
            )
    }

    fn has_subreg(&self, other: Self) -> bool where Self: 'static {
        self.subregs().map_or(false, |subregs| subregs.contains(&other))
    }

    fn interferes_with(self, other: Self) -> bool where Self: 'static {
        if self == other {
            return true;
        }
        if self.has_subreg(other) || other.has_subreg(self) {
            return true;
        }
        false
    }
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

    pub fn try_as_virtual_mut(&mut self) -> Option<&mut VReg> {
        match self {
            Register::Virtual(virt_reg) => Some(virt_reg),
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

index_vec::define_index_type! {
    pub struct InstrId = u32;

    DISPLAY_FORMAT = "instr{}";
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

    pub fn reads(&self) -> SmallVec<[Register<A>; 2]> {
        match self {
            Instr::Pseudo(pseudo) => pseudo.reads(),
            Instr::Machine(machine) => machine.reads(),
        }
    }

    pub fn reads_implicitly(&self) -> SmallVec<[Register<A>; 2]> {
        let writes = self.writes();
        let reg_operands = self.operands().iter().filter_map(|operand| {
            if let InstrOperand::Reg(reg) = operand {
                Some(*reg)
            } else {
                None
            }
        }).collect::<SmallVec<[_; 2]>>();
        let mut implicit_reads = SmallVec::new();
        for read in self.reads() {
            if !reg_operands.contains(&read) && writes.map_or(true, |writes| writes != read) {
                implicit_reads.push(read);
            }
        }
        implicit_reads
    }

    pub fn writes(&self) -> Option<Register<A>> {
        match self {
            Instr::Pseudo(pseudo) => pseudo.writes(),
            Instr::Machine(machine) => machine.writes().map(|reg| reg.into()),
        }
    }

    pub fn operands(&self) -> SmallVec<[InstrOperand<A>; 3]> {
        match self {
            Instr::Pseudo(pseudo) => pseudo.operands(),
            Instr::Machine(machine) => machine.operands(),
        }
    }

    pub fn written_regs_mut(&mut self) -> SmallVec<[&mut Register<A>; 1]> {
        match self {
            Instr::Pseudo(pseudo) => pseudo.written_regs_mut(),
            Instr::Machine(machine) => machine.written_regs_mut(),
        }
    }
    pub fn read_regs_mut(&mut self) -> SmallVec<[&mut Register<A>; 2]> {
        match self {
            Instr::Pseudo(pseudo) => pseudo.read_regs_mut(),
            Instr::Machine(machine) => machine.read_regs_mut(),
        }
    }
    pub fn try_as_machine(&self) -> Option<&A::I> {
        match self {
            Instr::Pseudo(_) => None,
            Instr::Machine(machine) => Some(machine),
        }
    }

    pub fn is_phi(&self) -> bool {
        match self {
            Instr::Pseudo(PseudoInstr::Phi(_, _)) => true,
            _ => false,
        }
    }
}

pub trait MachineInstr: Debug + PartialEq + Eq + Clone {
    type Abi: Abi;

    fn name(&self) -> &'static str;

    fn writes(&self) -> Option<Register<Self::Abi>>;

    fn reads(&self) -> SmallVec<[Register<Self::Abi>; 2]>;

    fn operands(&self) -> SmallVec<[InstrOperand<Self::Abi>; 3]>;

    fn written_regs_mut(&mut self) -> SmallVec<[&mut Register<Self::Abi>; 1]>;

    fn read_regs_mut(&mut self) -> SmallVec<[&mut Register<Self::Abi>; 2]>;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InstrOperand<A: Abi> {
    Reg(Register<A>),
    Imm(Immediate),
    Label(BasicBlockId),
}

#[derive(Debug)]
pub enum InstrOperandMut<'a, A: Abi> {
    Reg(&'a mut Register<A>),
    Imm(&'a mut Immediate),
    Label(&'a mut BasicBlockId),
}

impl<A: Abi> Display for InstrOperand<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Reg(reg) => write!(f, "{}", reg),
            Self::Imm(imm) => write!(f, "{}", imm),
            Self::Label(label) => write!(f, "{}", label),
        }
    }
}

impl<'op, A: Abi> From<&'op mut InstrOperand<A>> for InstrOperandMut<'op, A> {
    fn from(value: &'op mut InstrOperand<A>) -> Self {
        match value {
            InstrOperand::Reg(reg) => InstrOperandMut::Reg(reg),
            InstrOperand::Imm(imm) => InstrOperandMut::Imm(imm),
            InstrOperand::Label(label) => InstrOperandMut::Label(label),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PseudoInstr<A: Abi> {
    Copy(Register<A>, Register<A>),
    Ret(Option<InstrOperand<A>>),
    Phi(Register<A>, Vec<Register<A>>),
    Def(Register<A>),
}

impl<A: Abi> PseudoInstr<A> {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Copy(_, _) => "COPY",
            Self::Ret(_) => "RET",
            Self::Phi(_, _) => "PHI",
            Self::Def(_) => "DEF",
        }
    }

    pub fn reads(&self) -> SmallVec<[Register<A>; 2]> {
        match self {
            Self::Copy(_, to) => {
                smallvec![
                    *to,
                ]
            }
            Self::Ret(value) => {
                let mut reads = smallvec![];
                if let Some(InstrOperand::Reg(reg)) = value {
                    reads.push(*reg)
                }
                reads
            }
            Self::Phi(_, operands) => {
                operands.clone().into()
            }
            Self::Def(_) => {
                smallvec![]
            }
        }
    }

    pub fn operands(&self) -> SmallVec<[InstrOperand<A>; 3]> {
        match self {
            Self::Copy(dest, src) => {
                smallvec![
                    InstrOperand::Reg(*dest),
                    InstrOperand::Reg(*src),
                ]
            }
            Self::Ret(value) => {
                match value {
                    None => smallvec![],
                    Some(value) => smallvec![value.clone()],
                }
            }
            Self::Phi(dest, operands) => {
                let mut ops = smallvec![
                    InstrOperand::Reg(*dest),
                ];
                ops.extend(operands.iter().map(|reg| InstrOperand::Reg(*reg)));
                ops
            }
            Self::Def(reg) => {
                smallvec![
                    InstrOperand::Reg(*reg),
                ]
            }
        }
    }

    pub fn written_regs_mut(&mut self) -> SmallVec<[&mut Register<A>; 1]> {
        match self {
            Self::Copy(dest, _) => {
                smallvec![
                    dest,
                ]
            }
            Self::Ret(_) => {
                smallvec![]
            }
            Self::Phi(dest, _) => {
                smallvec![
                    dest,
                ]
            }
            Self::Def(reg) => {
                smallvec![
                    reg,
                ]
            }
        }
    }

    pub fn read_regs_mut(&mut self) -> SmallVec<[&mut Register<A>; 2]> {
        match self {
            Self::Copy(_, src) => {
                smallvec![
                    src,
                ]
            }
            Self::Ret(value) => {
                let mut reads = smallvec![];
                if let Some(InstrOperand::Reg(reg)) = value {
                    reads.push(reg)
                }
                reads
            }
            Self::Phi(_, operands) => {
                operands.iter_mut().collect()
            }
            Self::Def(_) => {
                smallvec![]
            }
        }
    }

    pub fn writes(&self) -> Option<Register<A>> {
        match self {
            Self::Copy(dest, _) => Some(*dest),
            Self::Ret(_) => None,
            Self::Phi(dest, _) => Some(*dest),
            Self::Def(dest) => Some(*dest),
        }
    }
}

pub trait Backend {
    type ABI: Abi;

    type P: Pattern<ABI=Self::ABI>;

    fn patterns() -> &'static [Self::P];

    fn mov(dest: <Self::ABI as Abi>::REG, src: <Self::ABI as Abi>::REG) -> <Self::ABI as Abi>::I;

    fn mov_imm(dest: <Self::ABI as Abi>::REG, imm: Immediate) -> <Self::ABI as Abi>::I;

    fn ret() -> <Self::ABI as Abi>::I;

    fn new() -> Self;
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum PatternIn {
    Mov(PatternInOutput, PatternInOperand),
    Sub(PatternInOutput, PatternInOperand, PatternInOperand),
    Add(PatternInOutput, PatternInOperand, PatternInOperand),
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

    fn into_instr(self, function: &mut Function<Self::ABI>, matched: MatchedPattern<Self::ABI>) -> SmallVec<[Instr<<Self as Pattern>::ABI>; 2]>;
}


#[derive(Debug, Copy, Clone, PartialEq, Eq, Display)]
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
pub struct VRegInfo<A: Abi> {
    pub size: Size,
    /// If set, the vreg will be placed in the same location as tied_to
    pub tied_to: Option<VReg>,
    /// If set, the vreg is ensured to be placed in the same location as fixed
    pub fixed: Option<A::REG>,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct FunctionId(u32);

entity_impl!(FunctionId, "fun");

#[derive(Debug, Clone)]
pub struct Function<A: Abi> {
    pub name: String,
    pub basic_blocks: IndexVec<BasicBlockId, BasicBlock<A>>,
    pub(crate) vregs: PrimaryMap<VReg, VRegInfo<A>>,
    pub(crate) params: SmallVec<[VReg; 2]>,
    pub(crate) return_ty_size: Size,
    cfg: Option<Cfg>,
}

impl<A: Abi> Function<A> {
    pub fn new(
        name: String,
    ) -> Self {
        Self {
            name,
            basic_blocks: IndexVec::default(),
            vregs: PrimaryMap::new(),
            cfg: None,
            params: SmallVec::new(),
            return_ty_size: Size::Byte,
        }
    }

    pub fn alloc_vreg(&mut self, size: Size) -> VReg {
        self.vregs.push(VRegInfo { size, tied_to: None, fixed: None })
    }

    pub fn get_vreg(&self, vreg: VReg) -> &VRegInfo<A> {
        &self.vregs[vreg]
    }
    pub fn tie_vreg(&mut self, vreg: VReg, to: VReg) {
        debug!("Tying {vreg} to {to}");
        self.vregs[vreg].tied_to = Some(to);
    }

    pub fn fix_vreg(&mut self, vreg: VReg, to: A::REG) {
        debug!("Fixing {vreg} to {}",to.name());
        self.vregs[vreg].fixed = Some(to);
    }

    pub fn create_bb(&mut self) -> BasicBlockId {
        let id = self.basic_blocks.next_idx();
        self.basic_blocks.push(BasicBlock::new(
            id
        ))
    }

    pub fn assemble(&self, base_addr: u64) -> Vec<u8> {
        debug!("Assembling function {}", self.name);
        let mut asm = A::get_assembler(base_addr);
        for bb_id in self.cfg().ordered() {
            let bb = &self.basic_blocks[bb_id];
            debug!("Assembling basic block {}", bb_id);
            asm.begin_basic_block(bb_id);
            for instr in &bb.instructions {
                debug!("Assembling instruction {:?}", instr);
                asm.assemble(instr.try_as_machine().unwrap_or_else(|| panic!("Pseudo instructions should have been expanded: {:?}", instr)));
            }
        }
        debug!("Finishing assembly for function {}", self.name);
        debug!("{}", asm.format());
        asm.finish()
    }


    pub fn expand_pseudo_instructions<B>(&mut self) where B: Backend<ABI=A> {
        debug!("Expanding pseudo instructions for function {}", self.name);
        for bb in &mut self.basic_blocks {
            if bb.instructions.is_empty() { continue; }
            let mut instr_id = InstrId::new(0);
            while instr_id <= bb.instructions.last_idx() {
                let instr = &mut bb.instructions[instr_id];
                if let Instr::Pseudo(pseudo_instr) = instr {
                    let expanded: SmallVec<[_; 2]> = match pseudo_instr {
                        PseudoInstr::Copy(dest, src) => {
                            let instr = B::mov(dest.try_as_physical().unwrap(), src.try_as_physical().unwrap());
                            smallvec![
                                instr
                            ]
                        }
                        PseudoInstr::Ret(value) => {
                            match value.as_mut() {
                                None => {
                                    smallvec![
                                        B::ret()
                                    ]
                                }
                                Some(value) => {
                                    let return_slot = <B::ABI as Abi>::CallingConvention::return_slot(
                                        match value {
                                            InstrOperand::Reg(reg) => {
                                                reg.try_as_physical().unwrap().size()
                                            }
                                            InstrOperand::Imm(imm) => {
                                                imm.size
                                            }
                                            InstrOperand::Label(_) => unreachable!()
                                        }
                                    );
                                    match return_slot {
                                        Slot::Register(dest) => {
                                            let instr = match value {
                                                InstrOperand::Reg(reg) => {
                                                    let reg = reg.try_as_physical().unwrap();
                                                    if reg == dest {
                                                        None
                                                    } else {
                                                        Some(B::mov(dest, reg))
                                                    }
                                                }
                                                InstrOperand::Imm(imm) => {
                                                    Some(B::mov_imm(dest, *imm))
                                                }
                                                InstrOperand::Label(_) => unreachable!()
                                            };
                                            if let Some(instr) = instr {
                                                smallvec![
                                                    instr,
                                                    B::ret()
                                                ]
                                            } else {
                                                smallvec![
                                                    B::ret()
                                                ]
                                            }
                                        }
                                        Slot::Stack => unimplemented!()
                                    }
                                }
                            }
                        }
                        PseudoInstr::Phi(_, _) => {
                            unreachable!("Phi should have been coalesced away by now")
                        }
                        PseudoInstr::Def(reg) => {
                            assert!(reg.try_as_physical().is_some(), "Def pseudo instruction should have a physical register");
                            smallvec![]
                        }
                    };
                    debug!("Expanded pseudo instruction {:?} to {:?}", pseudo_instr, expanded);
                    bb.instructions.remove(instr_id);
                    let expanded_len = expanded.len();
                    if (expanded_len == 0) {
                        continue;
                    }
                    for (offset, instr) in expanded.into_iter().enumerate() {
                        bb.instructions.insert(instr_id + offset, Instr::Machine(instr));
                    }
                    instr_id += expanded_len - 1;
                }
                instr_id += 1;
            }
        }
    }

    pub fn remove_fallthrough_jumps(&mut self) {
        let ordered = self.cfg().ordered();
        for (i, bb_id) in ordered.iter().copied().enumerate() {
            let bb = &mut self.basic_blocks[bb_id];
            let Some(next_bb_id) = ordered.get(i + 1).copied() else {
                continue;
            };
            if let Some(last_instr) = bb.instructions.last() {
                if let Some(InstrOperand::Label(label)) = last_instr.operands().last() {
                    if *label == next_bb_id {
                        debug!("Removing fallthrough jump from {} to {}", bb_id, next_bb_id);
                        bb.instructions.pop();
                    }
                }
            }
        }
    }

    pub fn build_cfg(&mut self) {
        let cfg = Cfg::build(&self.basic_blocks);
        self.cfg = Some(cfg);
    }

    pub fn cfg(&self) -> &Cfg {
        self.cfg.as_ref().expect("Cfg has not been built yey")
    }
}

impl<A: Abi> Display for Function<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "function {}:", self.name)?;
        let bbs: Box<dyn Iterator<Item=BasicBlockId>> = match &self.cfg {
            Some(cfg) => Box::new(cfg.ordered().into_iter()),
            None => Box::new(self.basic_blocks.indices().into_iter()),
        };
        for bb_id in bbs {
            let bb = &self.basic_blocks[bb_id];
            writeln!(f, "{bb_id}: ")?;
            for instr in &bb.instructions {
                write!(f, "  ")?;
                if let Some(out) = instr.writes() {
                    write!(f, "{out} = ")?;
                }
                write!(f, "{}", instr.name())?;
                let operands = instr.operands();
                let operands_len = operands.len();
                for (i, operand) in operands.into_iter().enumerate() {
                    write!(f, " {operand}")?;
                    if i < operands_len - 1 {
                        write!(f, ",")?;
                    }
                }
                let reads_impl = instr.reads_implicitly();
                let reads_impl_len = reads_impl.len();
                if !reads_impl.is_empty() {
                    write!(f, " {{implicit reads: ")?;
                    for (i, reg) in reads_impl.into_iter().enumerate() {
                        write!(f, "{reg}")?;
                        if i < reads_impl_len - 1 {
                            write!(f, ", ")?;
                        }
                    }
                    write!(f, "}}")?;
                }
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Cfg {
    entry_block: BasicBlockId,
    graph: StableGraph<(), (), Directed>,
    node_to_block_map: FxHashMap<NodeIndex, BasicBlockId>,
    block_to_node_map: FxHashMap<BasicBlockId, NodeIndex>,
}

impl Cfg {
    pub fn build<A: Abi>(bbs: &IndexVec<BasicBlockId, BasicBlock<A>>) -> Self {
        let mut cfg = Self::new(
            BasicBlockId::new(0),
        );
        for bb_id in bbs.indices() {
            let node = cfg.graph.add_node(());
            cfg.node_to_block_map.insert(node, bb_id);
            cfg.block_to_node_map.insert(bb_id, node);
        }
        for (bb_id, bb) in bbs.iter_enumerated() {
            for instr in &bb.instructions {
                let ins = instr.operands();
                for operand in ins {
                    if let InstrOperand::Label(successor_id) = operand {
                        cfg.graph.add_edge(
                            *cfg.block_to_node_map.get(&bb_id).expect("Block not found in block_to_node_map"),
                            *cfg.block_to_node_map.get(&successor_id).expect("Block not found in block_to_node_map"),
                            (),
                        );
                    }
                }
            }
        }
        cfg
    }

    pub fn new(entry_block: BasicBlockId) -> Self {
        Self {
            entry_block,
            graph: StableGraph::new(),
            node_to_block_map: FxHashMap::default(),
            block_to_node_map: FxHashMap::default(),
        }
    }

    /// Traverses the cfg using a post order depth first traversal
    pub fn dfs_postorder(&self) -> impl Iterator<Item=BasicBlockId> + '_ {
        DfsPostOrder::new(
            &self.graph,
            self.entry_node(),
        ).iter(&self.graph).map(
            |node|
                self.node_to_block_map[&node]
        )
    }

    pub fn bfs(&self) -> impl Iterator<Item=BasicBlockId> + '_ {
        Bfs::new(
            &self.graph,
            self.entry_node(),
        ).iter(&self.graph).map(
            |node|
                self.node_to_block_map[&node]
        )
    }

    pub fn predecessors(&self, bb: BasicBlockId) -> impl Iterator<Item=BasicBlockId> + '_ {
        self.graph.neighbors_directed(
            self.block_to_node_map[&bb],
            Direction::Incoming,
        ).map(
            |node| self.node_to_block_map[&node]
        )
    }

    pub fn successors(&self, bb: BasicBlockId) -> impl Iterator<Item=BasicBlockId> + '_ {
        self.graph.neighbors(
            self.block_to_node_map[&bb],
        ).map(
            |node| self.node_to_block_map[&node]
        )
    }


    fn entry_node(&self) -> NodeIndex {
        self.node_to_block_map.iter().find_map(
            |(node, bb)| {
                if *bb == self.entry_block {
                    return Some(*node);
                }
                None
            }
        ).expect("Did not find matching entry in node_to_block_map for entry block")
    }

    /// Returns an ordering of basic block with the following guarantees:
    /// 1. All predecessors of a basic block are visited before the basic block itself (except if the bb is a predecessor of itself)
    pub fn ordered(&self) -> Vec<BasicBlockId> {
        // let mut visited = FxHashSet::default();
        let mut order = self.bfs().collect_vec();
        // let mut stack = VecDeque::new();
        // stack.push_back(self.entry_block);
        // while let Some(bb) = stack.pop_front() {
        //     debug!("Visiting basic block {}:{:?}", bb,visited);
        //     if visited.contains(&bb) {
        //         continue;
        //     }
        //     let mut all_preds_visited = true;
        //     for pred in self.predecessors(bb) {
        //         if !(pred == bb || visited.contains(&pred)) {
        //             debug!("Pred {} of {} has not been visited yet", pred, bb);
        //             stack.push_back(pred);
        //             all_preds_visited = false;
        //         }
        //     }
        //     if all_preds_visited {
        //         visited.insert(bb);
        //         order.push(bb);
        //         for succ in self.successors(bb) {
        //             stack.push_back(succ);
        //         }
        //     } else {
        //         stack.push_back(bb);
        //     }
        // }
        order
    }
}

index_vec::define_index_type! {
    pub struct BasicBlockId = u32;

    DISPLAY_FORMAT = "bb{}";
}
#[derive(Debug, Clone)]
pub struct BasicBlock<A: Abi> {
    pub id: BasicBlockId,
    pub instructions: IndexVec<InstrId, Instr<A>>,
}

impl<A: Abi> BasicBlock<A> {
    pub fn new(id: BasicBlockId) -> Self {
        Self {
            id,
            instructions: IndexVec::default(),
        }
    }

    pub fn entry_pp(&self, liveness_repr: &LivenessRepr) -> ProgPoint {
        let instr_nr = liveness_repr.instr_numbering.get_instr_nr(
            InstrUid {
                bb: self.id,
                instr: 0.into(),
            }
        ).unwrap();
        ProgPoint::Read(instr_nr)
    }

    pub fn exit_pp(&self, instr_numbering: &InstrNumbering) -> ProgPoint {
        let instr_nr = instr_numbering.get_instr_nr(
            InstrUid {
                bb: self.id,
                instr: self.instructions.len_idx() - 1,
            }
        ).unwrap();
        ProgPoint::Write(instr_nr)
    }
}

#[derive(Debug)]
pub struct FunctionBuilder<B: Backend> {
    function: Function<B::ABI>,
    backend: B,
    bb_mapping: FxHashMap<natrix_middle::cfg::BasicBlockId, BasicBlockId>,
}

impl<B: Backend> FunctionBuilder<B> {
    pub fn new() -> Self {
        Self {
            function: Function::new(Default::default()),
            backend: B::new(),
            bb_mapping: FxHashMap::default(),
        }
    }

    pub fn build(mut self, function: &mut natrix_middle::Function) -> Function<B::ABI> {
        self.function.name = function.name.clone();
        self.function.return_ty_size = Size::from_ty(
            &function.ret_ty
        );
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
                            PseudoOp::Ret(operand) => {
                                instructions.push(
                                    Instr::Pseudo(
                                        PseudoInstr::Ret(
                                            operand.as_ref().cloned().map(|operand| match operand {
                                                Operand::Reg(reg) => InstrOperand::Reg(
                                                    reg
                                                ),
                                                Operand::Imm(imm) => InstrOperand::Imm(
                                                    imm
                                                ),
                                            })
                                        )
                                    )
                                );
                            }
                            PseudoOp::Phi(dest, operands) => {
                                instructions.push(
                                    Instr::Pseudo(
                                        PseudoInstr::Phi(
                                            dest.clone(),
                                            operands.clone(),
                                        )
                                    )
                                );
                            }
                            PseudoOp::Def(reg) => {
                                instructions.push(
                                    Instr::Pseudo(
                                        PseudoInstr::Def(
                                            Register::Virtual(
                                                *reg
                                            ),
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
                                let matched = match op {
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
                                };
                                let generated_instructions = pattern.into_instr(
                                    &mut self.function,
                                    matched,
                                );
                                debug!("Generated instructions {:?}", generated_instructions);
                                instructions.extend(generated_instructions.into_iter());
                            }
                        }
                    }
                }
            }
            for instr in instructions {
                self.function.basic_blocks[mbb_id].instructions.push(instr);
            }
        }
        debug!("Finished building machine function for function {}", function.name);
        debug!("{}", self.function);
        self.function
    }

    fn create_bb(&mut self, bb: natrix_middle::cfg::BasicBlockId) -> BasicBlockId {
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
            Operand::Imm(imm) => PatternInOperand::Imm(imm.size),
        }
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::codegen::isa;
    use crate::natrix_middle::cfg;
    use crate::natrix_middle::cfg::{RetTerm, TerminatorKind};
    use crate::natrix_middle::instruction::{Const, Op};
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



