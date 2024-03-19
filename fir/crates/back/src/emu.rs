use tracing::debug;
use unicorn_engine::{RegisterARM, RegisterX86, SECOND_SCALE};
use unicorn_engine::unicorn_const::{Arch, Mode, Permission, uc_error};

use crate::codegen::isa::{Architecture, Endianness, Target};

pub struct Emulator<'code, 'unicorn> {
    emu: unicorn_engine::Unicorn<'unicorn, ()>,
    code: &'code [u8],
    base_addr: u64,
}

impl<'code> Emulator<'code, '_> {
    pub fn new(
        target: Target,
        code: &'code [u8],
        base_addr: u64,
    ) -> Self {
        let (arch, mode) = match target.arch {
            Architecture::X86_64 => (Arch::X86, Mode::MODE_64),
        };
        debug!("Initializing Unicorn instance with arch {:?} and mode {:?}", arch, mode);
        let mut emu = unicorn_engine::Unicorn::new(arch, mode).expect("failed to initialize Unicorn instance");
        emu.mem_map(base_addr, 0x4000, Permission::ALL).expect("failed to map code page");
        emu.mem_write(base_addr, code).expect("failed to write instructions");
        Self { emu, code, base_addr }
    }
    pub fn emulate(&mut self, arguments: &[u64]) -> u64 {
        debug!("Emulating code");
        let arg_regs = match self.emu.get_arch() {
            Arch::X86 => &[RegisterX86::RDI, RegisterX86::RSI, RegisterX86::RDX, RegisterX86::RCX, RegisterX86::R8, RegisterX86::R9],
            _ => panic!("Unsupported architecture"),
        };
        for (arg, reg) in arguments.iter().copied().zip(arg_regs.iter().copied()) {
            self.emu.reg_write(reg, arg).expect("failed to write argument");
        }

        match self.emu.emu_start(self.base_addr, self.base_addr + self.code.len() as u64, 10 * SECOND_SCALE, 0) {
            Ok(()) => {}
            Err(err) => {
                assert!(!(err != uc_error::READ_UNMAPPED), "Failed to emulate code: {:?}", err);
            }
        }
        let result = self.emu.reg_read(RegisterX86::RAX).unwrap();
        debug!("Emulation finished with result {}", result);
        result
    }
}