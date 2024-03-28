use std::ops::Range;

use cranelift_entity::{
    PrimaryMap,
    SecondaryMap,
};
use tracing::{
    debug,
    info,
};

use asm::{FunctionSymbolTable, FunctionSymbolTableEntry};
pub use asm::AsmModule;
pub use builder::Builder;

use crate::codegen::{
    machine::{
        backend::Backend,
        function::{
            Function,
            FunctionId,
        },
    },
    register_allocator,
    register_allocator::RegAllocAlgorithm,
};
use crate::codegen::machine::TargetMachine;

mod builder;
pub mod asm;

#[derive(Debug, Clone)]
pub struct Module<TM: TargetMachine> {
    pub(crate) functions: PrimaryMap<FunctionId, Function<TM>>,
}

impl<TM: TargetMachine> Default for Module<TM> {
    fn default() -> Self {
        Self {
            functions: PrimaryMap::new(),
        }
    }
}

impl<TM: TargetMachine> Module<TM> {
    pub fn add_function(&mut self, function: Function<TM>) -> FunctionId {
        self.functions.push(function)
    }

    pub fn functions(&self) -> impl ExactSizeIterator<Item = (FunctionId, &Function<TM>)> {
        self.functions.iter()
    }

    pub fn assemble(&self, base_addr: u64) -> AsmModule<'_, TM> {
        let mut result = Vec::new();
        let mut addr = base_addr;
        let mut fst = FunctionSymbolTable::new();
        for (function_id, function) in self.functions() {
            let assembled = function.assemble(addr);
            let assembled_len = assembled.len() as u64;
            fst[function_id] = FunctionSymbolTableEntry {
                size: assembled_len,
                offset: addr - base_addr,
            };
            addr += assembled_len;
            result.extend(assembled);
        }
        AsmModule::new(self, base_addr, result, fst)
    }

    pub fn run_register_coalescer(&mut self) {
        let mut coalescer = register_allocator::Coalescer::new(self);
        coalescer.run();
    }

    pub fn run_register_allocator(&mut self) {
        info!("Running register allocator");
        for (function_id, function) in &mut self.functions {
            function.build_cfg();
            debug!("Running register allocator for function {:?}", function_id);
            let liveness_repr = function.liveness_repr();
            let allocator = register_allocator::RegisterAllocator::<
                _,
                register_allocator::linear_scan::RegAlloc<TM>,
            >::new(function, &liveness_repr);
            allocator.run();
            debug!("Register allocator finished for function {:?}", function_id);
            debug!("{}", function);
        }
    }

    pub fn remove_fallthrough_jumps(&mut self) {
        for (_, function) in &mut self.functions {
            function.remove_fallthrough_jumps();
        }
    }

    pub fn expand_pseudo_instructions(&mut self) {
        for (_, function) in &mut self.functions {
            function.expand_pseudo_instructions::<TM::Backend>();
        }
    }
}

impl<TM: TargetMachine> std::fmt::Display for Module<TM> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (id, function) in self.functions() {
            writeln!(f, "{}", function)?;
        }
        Ok(())
    }
}

// pub trait Pass<TM: TargetMachine> {
//     fn run(&mut self, function: &mut Function<TM>);
// }
