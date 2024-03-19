use cranelift_entity::PrimaryMap;
use tracing::{debug, info};

pub use builder::Builder;

use crate::codegen::machine::{Abi, Backend, Function, FunctionId};
use crate::codegen::register_allocator;
use crate::codegen::register_allocator::RegAllocAlgorithm;

mod builder;

#[derive(Debug, Default, Clone)]
pub struct Module<A: Abi> {
    pub(crate) functions: PrimaryMap<FunctionId, Function<A>>,
}

impl<A: Abi> Module<A> {
    pub fn add_function(&mut self, function: Function<A>) -> FunctionId {
        self.functions.push(function)
    }

    pub fn functions(&self) -> impl ExactSizeIterator<Item=(FunctionId, &Function<A>)> {
        self.functions.iter()
    }

    pub fn assemble(&self, base_addr: u64) -> Vec<u8> {
        let mut result = Vec::new();
        let mut addr = base_addr;
        for (_, function) in self.functions() {
            let assembled = function.assemble(addr);
            addr += assembled.len() as u64;
            result.extend(assembled);
        }
        result
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
            let allocator = register_allocator::RegisterAllocator::<_, register_allocator::linear_scan::RegAlloc<A>>::new(function, &liveness_repr);
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

    pub fn expand_pseudo_instructions<B>(&mut self) where B: Backend<ABI=A> {
        for (_, function) in &mut self.functions {
            function.expand_pseudo_instructions::<B>();
        }
    }
}

impl<A: Abi> std::fmt::Display for Module<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (id, function) in self.functions() {
            writeln!(f, "{}", function)?;
        }
        Ok(())
    }
}
