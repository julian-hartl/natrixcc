use std::ops::Range;

use cranelift_entity::SecondaryMap;

use crate::codegen::machine::{
    FunctionId,
    Module,
    TargetMachine,
};

#[derive(Debug, Clone, Eq, PartialEq, Default)]
pub struct FunctionSymbolTableEntry {
    pub offset: u64,
    pub size: u64,
}

pub type FunctionSymbolTable = SecondaryMap<FunctionId, FunctionSymbolTableEntry>;

pub struct AsmModule<'module, TM: TargetMachine> {
    base_addr: u64,
    code: Vec<u8>,
    function_symbol_table: FunctionSymbolTable,
    module: &'module Module<TM>,
}

impl<'module, TM: TargetMachine> AsmModule<'module, TM> {
    pub fn new(
        module: &'module Module<TM>,
        base_addr: u64,
        code: Vec<u8>,
        function_symbol_table: FunctionSymbolTable,
    ) -> Self {
        Self {
            base_addr,
            code,
            function_symbol_table,
            module,
        }
    }

    pub fn base_addr(&self) -> u64 {
        self.base_addr
    }

    pub fn code(&self) -> &[u8] {
        &self.code
    }

    pub fn code_range_of(&self, function_id: FunctionId) -> Option<Range<u64>> {
        self.function_symbol_table.get(function_id).cloned().map(
            |FunctionSymbolTableEntry { offset, size }| {
                let start = self.base_addr + offset;
                let end = start + size;
                start..end
            },
        )
    }

    pub fn machine_module(&self) -> &Module<TM> {
        self.module
    }
}
