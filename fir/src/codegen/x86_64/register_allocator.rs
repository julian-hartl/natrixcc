use rustc_hash::FxHashMap;

use crate::codegen::x86_64::{FunctionData, InstrKind, PhysicalRegister, Register, VirtualRegister, VirtualRegisterData};
use crate::ty::Type;

pub trait RegisterAllocator {
    fn get_register(&mut self, v_reg: (VirtualRegister, &VirtualRegisterData)) -> Option<PhysicalRegister>;

    fn run(&mut self, function: &mut FunctionData) {
        for bb in &mut function.basic_blocks {
            for instr in &mut bb.instructions {
                let mut maybe_replace_reg = |reg: &mut Register| {
                    if let Register::Virtual(v_reg) = reg {
                        if let Some(p_reg) = self.get_register((*v_reg, &function.registers[*v_reg])) {
                            *reg = Register::Physical(p_reg);
                        }
                    }
                };
                match &mut instr.kind {
                    InstrKind::Mov32RI { dest, .. } => {
                        maybe_replace_reg(dest);
                    }
                    InstrKind::Mov32MI { .. } => {}
                    InstrKind::Mov32RM {
                        dest,
                        ..
                    } => {
                        maybe_replace_reg(dest);
                    }
                    InstrKind::Mov32MR {
                        src,
                        ..
                    } => {
                        maybe_replace_reg(src);
                    }
                    InstrKind::Mov32RR {
                        src,
                        dest,
                    } => {
                        maybe_replace_reg(src);
                        maybe_replace_reg(dest);
                    }
                    InstrKind::Mov64RR {
                        src,
                        dest,
                    } => {
                        maybe_replace_reg(src);
                        maybe_replace_reg(dest);
                    }
                    InstrKind::Sub32RR {
                        src,
                        dest,
                    } => {
                        maybe_replace_reg(src);
                        maybe_replace_reg(dest);
                    }
                    InstrKind::Sub32RI {
                        dest,
                        ..
                    } => {
                        maybe_replace_reg(dest);
                    }
                    InstrKind::Sub64RI {
                        dest,
                        ..
                    } => {
                        maybe_replace_reg(dest);
                    }
                    InstrKind::Add32RI {
                        dest,
                        ..
                    } => {
                        maybe_replace_reg(dest);
                    }
                    InstrKind::Push64R {
                        src,
                    } => {
                        maybe_replace_reg(src);
                    }
                    InstrKind::Neg32R {
                        src,
                    } => {
                        maybe_replace_reg(src);
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FastRegisterAllocator {
    free_registers: Vec<PhysicalRegister>,
    v_reg_to_p_reg: FxHashMap<VirtualRegister, PhysicalRegister>,
}

impl FastRegisterAllocator {
    pub fn new() -> Self {
        Self {
            free_registers: PhysicalRegister::values().collect(),
            v_reg_to_p_reg: FxHashMap::default(),
        }
    }

    fn find_free_reg_for_type(&self, ty: &Type) -> Option<PhysicalRegister> {
        let size = ty.size();
        for reg in &self.free_registers {
            if reg.size() as u32 == size {
                return Some(*reg);
            }
        }
        None
    }
}

impl RegisterAllocator for FastRegisterAllocator {
    fn get_register(&mut self, v_reg: (VirtualRegister, &VirtualRegisterData)) -> Option<PhysicalRegister> {
        if let Some(p_reg) = self.v_reg_to_p_reg.get(&v_reg.0) {
            return Some(*p_reg);
        }
        if let Some(p_reg) = self.find_free_reg_for_type(&v_reg.1.ty) {
            self.v_reg_to_p_reg.insert(v_reg.0, p_reg);
            self.free_registers.retain(|r| *r != p_reg);
            return Some(p_reg);
        }
        None
    }
}


