use std::fmt::{
    Display,
    Formatter,
};

pub use cfg::{
    BasicBlock,
    BasicBlockId,
    Cfg,
};
use cranelift_entity::{
    entity_impl,
    PrimaryMap,
};
use daggy::Walker;
use index_vec::IndexVec;
use iter_tools::Itertools;
use smallvec::{
    smallvec,
    SmallVec,
};
use tracing::debug;

use crate::codegen::machine::{
    abi::{
        calling_convention::Slot,
        CallingConvention,
    },
    asm::Assembler,
    backend::Backend,
    instr::{
        InstrOperand,
        PseudoInstr,
    },
    isa::PhysicalRegister,
    reg::VRegInfo,
    Instr,
    InstrId,
    MachInstr,
    Register,
    Size,
    TargetMachine,
    VReg,
};

pub mod builder;
pub mod cfg;

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct FunctionId(u32);

entity_impl!(FunctionId, "fun");

#[derive(Debug, Clone)]
pub struct Function<TM: TargetMachine> {
    pub name: String,
    pub basic_blocks: IndexVec<BasicBlockId, BasicBlock<TM>>,
    pub(crate) vregs: PrimaryMap<VReg, VRegInfo<TM>>,
    pub(crate) params: SmallVec<[VReg; 2]>,
    pub(crate) return_ty_size: Size,
    cfg: Option<Cfg>,
}

impl<TM: TargetMachine> Function<TM> {
    pub fn new(name: String) -> Self {
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
        self.vregs.push(VRegInfo {
            size,
            tied_to: None,
            fixed: None,
        })
    }

    pub fn get_vreg(&self, vreg: VReg) -> &VRegInfo<TM> {
        &self.vregs[vreg]
    }
    pub fn tie_vreg(&mut self, vreg: VReg, to: VReg) {
        debug!("Tying {vreg} to {to}");
        self.vregs[vreg].tied_to = Some(to);
    }

    pub fn fix_vreg(&mut self, vreg: VReg, to: TM::Reg) {
        debug!("Fixing {vreg} to {}", to.name());
        self.vregs[vreg].fixed = Some(to);
    }

    pub fn create_bb(&mut self) -> BasicBlockId {
        let id = self.basic_blocks.next_idx();
        self.basic_blocks.push(BasicBlock::new(id))
    }

    pub fn assemble(&self, base_addr: u64) -> Vec<u8> {
        debug!("Assembling function {}", self.name);
        let mut asm = TM::Assembler::new(base_addr);
        for bb_id in self.cfg().ordered() {
            let bb = &self.basic_blocks[bb_id];
            debug!("Assembling basic block {}", bb_id);
            asm.begin_basic_block(bb_id);
            for instr in &bb.instructions {
                debug!("Assembling instruction {:?}", instr);
                asm.assemble(instr.try_as_machine().unwrap_or_else(|| {
                    panic!("Pseudo instructions should have been expanded: {:?}", instr)
                }));
            }
        }
        debug!("Finishing assembly for function {}", self.name);
        debug!("{}", asm.format());
        asm.finish()
    }

    pub fn expand_pseudo_instructions<B>(&mut self)
    where
        B: Backend<TM = TM>,
    {
        debug!("Expanding pseudo instructions for function {}", self.name);
        for bb in &mut self.basic_blocks {
            if bb.instructions.is_empty() {
                continue;
            }
            let mut instr_id = InstrId::new(0);
            while instr_id <= bb.instructions.last_idx() {
                let instr = &mut bb.instructions[instr_id];
                if let Instr::Pseudo(pseudo_instr) = instr {
                    let expanded: SmallVec<[_; 2]> = match pseudo_instr {
                        PseudoInstr::Copy(dest, src) => {
                            let instr = B::mov(
                                dest.try_as_physical().unwrap(),
                                src.try_as_physical().unwrap(),
                            );
                            smallvec![instr]
                        }
                        PseudoInstr::Ret(value) => match value.as_mut() {
                            None => {
                                smallvec![B::ret()]
                            }
                            Some(value) => {
                                let return_slot = TM::CallingConvention::return_slot(match value {
                                    InstrOperand::Reg(reg) => reg.try_as_physical().unwrap().size(),
                                    InstrOperand::Imm(imm) => imm.size,
                                    InstrOperand::Label(_) => unreachable!(),
                                });
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
                                            InstrOperand::Imm(imm) => Some(B::mov_imm(dest, *imm)),
                                            InstrOperand::Label(_) => unreachable!(),
                                        };
                                        if let Some(instr) = instr {
                                            smallvec![instr, B::ret()]
                                        } else {
                                            smallvec![B::ret()]
                                        }
                                    }
                                    Slot::Stack => unimplemented!(),
                                }
                            }
                        },
                        PseudoInstr::Def(reg) => {
                            assert!(
                                reg.try_as_physical().is_some(),
                                "Def pseudo instruction should have a physical register"
                            );
                            smallvec![]
                        }
                    };
                    debug!(
                        "Expanded pseudo instruction {:?} to {:?}",
                        pseudo_instr, expanded
                    );
                    bb.instructions.remove(instr_id);
                    let expanded_len = expanded.len();
                    if expanded_len == 0 {
                        continue;
                    }
                    for (offset, instr) in expanded.into_iter().enumerate() {
                        bb.instructions
                            .insert(instr_id + offset, Instr::Machine(instr));
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

impl<TM: TargetMachine> Display for Function<TM> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "function {}:", self.name)?;
        let bbs: Box<dyn Iterator<Item = BasicBlockId>> = match &self.cfg {
            Some(cfg) => Box::new(cfg.ordered().into_iter()),
            None => Box::new(self.basic_blocks.indices().into_iter()),
        };
        for bb_id in bbs {
            let bb = &self.basic_blocks[bb_id];
            writeln!(f, "{bb_id}: ")?;
            for (dest, operands) in &bb.phis {
                write!(f, "  {dest} = phi ")?;
                for (i, (reg, bb)) in operands.iter().enumerate() {
                    write!(f, "{reg}:{bb}")?;
                    if i < operands.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                writeln!(f)?;
            }
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
