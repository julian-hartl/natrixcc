use std::collections::VecDeque;
use std::fmt::{Debug, Display, Formatter};

use daggy::{NodeIndex, Walker};
use daggy::petgraph::visit::{Bfs, IntoEdges};
use index_vec::IndexVec;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use tracing::debug;

pub use abi::Abi;

use crate::codegen::{machine, selection_dag};
use crate::codegen::machine::asm::Assembler;
use crate::codegen::selection_dag::{Const, Node, NodeKind, Op};
use crate::middle;
use crate::ty::Type;

pub mod abi;
pub mod asm;

index_vec::define_index_type! {
    pub struct VirtualRegister = usize;

    DISPLAY_FORMAT = "%{}";
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VirtualRegisterData {
    pub ty: Type,
}

pub trait PhysicalRegister: Debug + Clone + Copy + PartialEq + Eq + Sized {
    fn name(&self) -> &'static str;
    
    fn all() -> &'static [Self];
    
    fn is_gp() -> bool;
    
    /// Returns the size of the register in bytes
    fn size(&self) -> u32;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Register<A: Abi> {
    Virtual(VirtualRegister),
    Physical(A::REG),
}

impl <A: Abi> Register<A> {
    pub fn try_as_virtual(&self) -> Option<VirtualRegister> {
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
    pub struct InstrId = usize;
}

pub trait Instr: Debug + PartialEq + Eq {
    type Abi: Abi;

    fn name(&self) -> &'static str;

    fn output(&self) -> Option<Register<Self::Abi>>;

    fn operands(&self) -> SmallVec<[InstrOperand<Self::Abi>; 2]>;

}

#[derive(Debug, Clone)]
pub enum InstrOperand<A: Abi> {
    Reg(Register<A>),
    Imm(i64),
}

impl <A: Abi> Display for InstrOperand<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrOperand::Reg(reg) => write!(f, "{}", reg),
            InstrOperand::Imm(imm) => write!(f, "{}", imm),
        }
    }
}

pub trait Backend {
    type ABI: Abi;

    type P: Pattern<ABI=Self::ABI>;

    fn patterns() -> &'static [Self::P];

    fn new() -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatternIn {
    pub out: PatternOut,
    pub op: Op,
    pub operands: SmallVec<[PatternInOperand; 2]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternOut {
    Reg(Type),
    None,
}

#[derive(Debug, Clone, PartialEq, Eq, EnumTryAs)]
pub enum PatternInOperand {
    Reg(Type),
    Imm(Type),
}

#[derive(Debug, Clone)]
pub enum MatchedPatternOut<A: Abi> {
    Reg(Register<A>),
    None,
}

impl<A: Abi> MatchedPatternOut<A> {
    pub fn try_as_reg(&self) -> Option<Register<A>> {
        match self {
            MatchedPatternOut::Reg(reg) => Some(*reg),
            MatchedPatternOut::None => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MatchedPatternIn<A: Abi> {
    pub out: MatchedPatternOut<A>,
    pub operands: SmallVec<[MatchedPatternInOperand<A>; 2]>,
}

#[derive(Debug, Clone)]
pub enum MatchedPatternInOperand<A: Abi> {
    Reg(Register<A>),
    Imm(i64),
}

impl<A: Abi> MatchedPatternInOperand<A> {
    pub fn try_as_reg(&self) -> Option<Register<A>> {
        match self {
            MatchedPatternInOperand::Reg(reg) => Some(*reg),
            MatchedPatternInOperand::Imm(_) => None,
        }
    }

    pub fn try_as_imm(&self) -> Option<i64> {
        match self {
            MatchedPatternInOperand::Reg(_) => None,
            MatchedPatternInOperand::Imm(imm) => Some(*imm),
        }
    }
}

pub trait Pattern: Sized + Debug + Clone + PartialEq + Eq + 'static {
    type ABI: Abi;

    fn in_(&self) -> PatternIn;

    fn into_instr(self, in_: MatchedPatternIn<Self::ABI>) -> SmallVec<[<<Self as Pattern>::ABI as Abi>::I; 2]>;
}

#[derive(Debug)]
pub struct Function<A: Abi> {
    pub name: String,
    pub basic_blocks: IndexVec<BasicBlockId, BasicBlock<A::I>>,
    vregs: IndexVec<VirtualRegister, VirtualRegisterData>,
}

impl<A: Abi> Function<A> {
    pub fn new(
        name: String,
    ) -> Self {
        Self {
            name,
            basic_blocks: IndexVec::default(),
            vregs: IndexVec::default(),
        }
    }

    pub fn alloc_vreg(&mut self, ty: Type) -> VirtualRegister {
        self.vregs.push(VirtualRegisterData { ty })
    }

    pub fn get_vreg(&self, vreg: VirtualRegister) -> &VirtualRegisterData {
        &self.vregs[vreg]
    }

    pub fn create_bb(&mut self) -> BasicBlockId {
        self.basic_blocks.push(BasicBlock::default())
    }

    pub fn assemble(&self) -> Vec<u8> {
        debug!("Assembling function {}", self.name);
        let mut asm = A::get_assembler();
        for (bb_id, bb) in self.basic_blocks.iter_enumerated() {
            debug!("Assembling basic block {}", bb_id);
            for instr in bb.instructions.iter() {
                debug!("Assembling instruction {:?}", instr);
                asm.assemble(instr);
            }
        }
        debug!("Finishing assembly for function {}", self.name);
        asm.save("out/asm.s").expect("Failed to save assembly to file");
        asm.finish()
    }
}

impl<A: Abi> Display for Function<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "function {}:", self.name)?;
        for (bb_id, bb) in self.basic_blocks.iter_enumerated() {
            writeln!(f, "{}: ", bb_id)?;
            for (instr_id, instr) in bb.instructions.iter_enumerated() {
                write!(f, "  ")?;
                if let Some(output) = instr.output() {
                    write!(f, "{} = ", output)?;
                }
                write!(f, "{}", instr.name())?;
                for (i, operand) in instr.operands().into_iter().enumerate() {
                    if i == 0 {
                        write!(f, " ")?;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", operand)?;
                }
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

index_vec::define_index_type! {
    pub struct BasicBlockId = usize;

    DISPLAY_FORMAT = "bb{}";
}

#[derive(Debug)]
pub struct BasicBlock<I: Instr> {
    pub instructions: IndexVec<InstrId, I>,
}

impl<I: Instr> Default for BasicBlock<I> {
    fn default() -> Self {
        Self {
            instructions: IndexVec::new(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionBuilder<B: Backend> {
    function: Function<B::ABI>,
    backend: B,
    node_to_vreg: FxHashMap<NodeIndex, VirtualRegister>,
}

impl<B: Backend> FunctionBuilder<B> {
    pub fn new() -> Self {
        Self {
            function: Function::new(Default::default()),
            backend: B::new(),
            node_to_vreg: FxHashMap::default(),
        }
    }

    pub fn build(mut self, function: &middle::Function) -> Function<B::ABI> {
        self.function.name = function.name.clone();
        debug!("Building machine function for function {}", function.name);
        let mut sel_dag_builder = selection_dag::Builder::<B::ABI>::default();
        let mut sel_dag = sel_dag_builder.build(function);
        for bb in function.cfg.basic_blocks.iter().flatten() {
            debug!("Building machine basic block for basic block {}", bb.id);
            let mbb_id = self.function.create_bb();
            let dag = sel_dag.get_bb_dag(bb.id);
            dag.save_graphviz("out").unwrap();
            let mut node_list = Vec::with_capacity(dag.node_count());
            let mut listed = vec![false; dag.node_count()];
            debug!("Determining traversal order for basic block {}", bb.id);
            // https://www.brainkart.com/article/Generating-Code-From-DAGs_8107/
            loop {
                let bfs = Bfs::new(dag.graph(), dag.term_node());
                let mut all_interior_nodes_listed = true;
                for mut n in bfs.iter(dag.graph()) {
                    if listed[n.index()] || dag.children(n).walk_next(dag).is_none() {
                        continue;
                    }
                    all_interior_nodes_listed = false;
                    let are_parents_listed = dag.parents(n).iter(dag).all(|(_, parent)| listed[parent.index()]);
                    if are_parents_listed {
                        node_list.push(n);
                        listed[n.index()] = true;
                    }
                    while let Some((_, m)) = dag.children(n).walk_next(dag) {
                        if dag.children(m).walk_next(dag).is_none() {
                            break;
                        }
                        node_list.push(m);
                        listed[m.index()] = true;
                        n = m;
                    }
                }
                if all_interior_nodes_listed {
                    break;
                }
            }
            debug!("Traversal order: {:?}", node_list);
            let mut instructions = Vec::new();
            while let Some(node_id) = node_list.pop() {
                let node = &dag[node_id];
                debug!("Matching patterns for node {:?}", node);
                let mut matching_pattern = None;
                for pattern in B::patterns() {
                    let pattern_in = pattern.in_();
                    debug!("Checking {:?}", pattern_in);
                    let op = match node.kind {
                        NodeKind::Op(op) => op,
                        _ => unreachable!(),
                    };
                    let operands = dag.children(node_id).iter(dag).map(|(_, child)| {
                        let child = &dag[child];
                        match child.kind {
                            NodeKind::Op(_) | NodeKind::Reg(_) => PatternInOperand::Reg(child.ty.as_ref().unwrap().clone()),
                            NodeKind::Const(_) => PatternInOperand::Imm(child.ty.as_ref().unwrap().clone()),
                        }
                    }).collect();
                    let dag_node_pattern = PatternIn {
                        out: node.ty.as_ref().map(|ty| PatternOut::Reg(ty.clone())).unwrap_or(PatternOut::None),
                        op,
                        operands,
                    };
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
                        panic!("No pattern matched for node {:?}", node);
                    }
                    Some(pattern) => {
                        let generated_instructions = pattern.into_instr(
                            MatchedPatternIn {
                                out: node.ty.as_ref().map(|ty| MatchedPatternOut::Reg(Register::Virtual(self.get_or_alloc_vreg(node_id, ty.clone())))).unwrap_or(MatchedPatternOut::None),
                                operands: dag.children(node_id).iter(dag).map(|(_, child)| {
                                    match &dag[child].kind {
                                        NodeKind::Op(_) | NodeKind::Reg(_) => MatchedPatternInOperand::Reg(Register::Virtual(self.get_or_alloc_vreg(child, dag[child].ty.as_ref().unwrap().clone()))),
                                        NodeKind::Const(val) => MatchedPatternInOperand::Imm(match val {
                                            Const::Int(val) => *val,
                                        }),
                                    }
                                }).collect(),
                            }.into(),
                        );
                        debug!("Generated instructions {:?}", generated_instructions);
                        for instr in generated_instructions {
                            instructions.push(instr);
                        }
                    }
                }
            }
            for instr in instructions {
                self.push_instruction(instr, mbb_id);
            }
        }
        self.function
    }

    fn alloc_vreg(&mut self, index: NodeIndex, ty: Type) -> VirtualRegister {
        let vreg = self.function.alloc_vreg(ty);
        assert!(self.node_to_vreg.insert(index, vreg).is_none(), "Overwrote vreg for node index {}", index.index());
        vreg
    }

    fn get_vreg(&self, index: NodeIndex) -> VirtualRegister {
        *self.node_to_vreg.get(&index).expect("Expected vreg")
    }

    fn get_or_alloc_vreg(&mut self, index: NodeIndex, ty: Type) -> VirtualRegister {
        match self.node_to_vreg.get(&index) {
            Some(vreg) => {
                *vreg
            }
            None => {
                self.alloc_vreg(index, ty)
            }
        }
    }

    fn push_instruction(&mut self, instr: <<B as Backend>::ABI as Abi>::I, bb: BasicBlockId) {
        self.function.basic_blocks[bb].instructions.push(instr);
    }
}

#[cfg(test)]
mod tests {
    use tracing_test::traced_test;

    use crate::codegen::isa;
    use crate::middle::cfg;
    use crate::middle::cfg::{RetTerm, TerminatorKind};
    use crate::middle::instruction::{Const, Op};
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



