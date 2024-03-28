use std::ops::Range;

use anyhow::{
    anyhow,
    bail,
    Result,
};
use tracing::{
    debug,
    warn,
};
use unicorn_engine::{
    SECOND_SCALE,
    unicorn_const::{
        uc_error,
        Arch,
        Mode,
        Permission,
    },
    SECOND_SCALE,
};

use crate::codegen::machine::{
    abi::{
        calling_convention::Slot,
        CallingConvention,
    },
    Architecture,
    function::FunctionId,
    isa::PhysicalRegister,
    module::asm::AsmModule,
    TargetMachine,
};

pub struct Emulator<'module, 'code, 'unicorn, TM: TargetMachine> {
    emu: unicorn_engine::Unicorn<'unicorn, ()>,
    module: &'code AsmModule<'module, TM>,
}

impl<'module, 'code, TM: TargetMachine> Emulator<'module, 'code, '_, TM> {
    pub fn new(module: &'code AsmModule<'module, TM>) -> Self {
        let (arch, mode) = match TM::arch() {
            Architecture::X86_64 => (Arch::X86, Mode::MODE_64),
        };
        debug!(
            "Initializing Unicorn instance with arch {:?} and mode {:?}",
            arch, mode
        );
        let mut emu = unicorn_engine::Unicorn::new(arch, mode)
            .expect("failed to initialize Unicorn instance");
        let unaligned_size = module.code().len() as u64;
        let unaligned_base_addr = module.base_addr();
        let aligned_base_addr = Self::align_down_to_4kb(unaligned_base_addr);
        let aligned_size = Self::align_up_to_4kb(unaligned_size);
        emu.mem_map(aligned_base_addr, aligned_size as usize, Permission::ALL)
            .expect("failed to map code page");
        emu.mem_write(unaligned_base_addr, module.code())
            .expect("failed to write instructions");
        Self { emu, module }
    }

    pub fn run_function(&mut self, func_id: FunctionId, arguments: &[u64]) -> Result<u64> {
        let function = &self.module.machine_module().functions[func_id];
        if function.params.len() != arguments.len() {
            bail!(
                "Invalid number of arguments. Expected {}, got {}",
                function.params.len(),
                arguments.len()
            );
        }
        let parameters = function
            .params
            .iter()
            .copied()
            .map(|vreg| function.vregs[vreg].size);
        let Range {
            start: start_addr,
            end: end_addr,
        } = self
            .module
            .code_range_of(func_id)
            .ok_or_else(|| anyhow!("Function not found"))?;
        let param_slots = TM::CallingConvention::parameter_slots(parameters);
        for (param_slot, arg) in param_slots.zip(arguments.iter().copied()) {
            match param_slot {
                Slot::Register(reg) => {
                    self.emu
                        .reg_write(reg.into_unicorn_emu_reg(), arg)
                        .expect("failed to write argument");
                }
                Slot::Stack => unimplemented!(),
            }
        }
        debug!("Running function at address 0x{:x}", start_addr);
        if let Err(err) = self
            .emu
            .emu_start(start_addr, end_addr, 10 * SECOND_SCALE, 0)
        {
            match err {
                uc_error::READ_UNMAPPED => {
                    warn!("Read from unmapped memory (potentially the return address that was never pushed)");
                }
                _ => bail!("Failed to run function: {:?}", err),
            }
        }
        let ret_slot = TM::CallingConvention::return_slot(function.return_ty_size);
        match ret_slot {
            Slot::Register(reg) => Ok(self
                .emu
                .reg_read(reg.into_unicorn_emu_reg())
                .expect("failed to read reg")),
            Slot::Stack => unimplemented!(),
        }
    }

    const fn align_up_to_4kb(addr: u64) -> u64 {
        (addr + 0xfff) & !0xfff
    }

    const fn align_down_to_4kb(addr: u64) -> u64 {
        addr & !0xfff
    }
}
