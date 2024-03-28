use std::fmt::{
    Display,
    Formatter,
};

use cranelift_entity::entity_impl;

use crate::codegen::machine::{function::Function, isa::PhysicalRegister, Size, TargetMachine};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Register<TM: TargetMachine> {
    Virtual(VReg),
    Physical(TM::Reg)
}

impl<TM: TargetMachine> From<VReg> for Register<TM> {
    fn from(vreg: VReg) -> Self {
        Self::Virtual(vreg)
    }
}

impl<TM: TargetMachine> Register<TM> {
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

    pub fn try_as_physical(&self) -> Option<TM::Reg> {
        match self {
            Register::Virtual(_) => None,
            Register::Physical(phys_reg) => Some(*phys_reg),
        }
    }

    pub fn size(&self, func: &Function<TM>) -> Size {
        match self {
            Register::Virtual(vreg) => vreg.size(func),
            Register::Physical(phys_reg) => phys_reg.size(),
        }
    }
}

// impl<TM: TargetMachine> Copy for Register<TM> {}

impl<TM: TargetMachine> Display for Register<TM> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Virtual(virt_reg) => write!(f, "{}", virt_reg),
            Register::Physical(phys_reg) => write!(f, "${}", phys_reg.name()),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct VReg(u32);

impl VReg {
    pub fn size<TM: TargetMachine>(self, func: &Function<TM>) -> Size {
        func.get_vreg(self).size
    }
}

entity_impl!(VReg, "v");

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct VRegInfo<TM: TargetMachine> {
    pub size: Size,
    /// If set, the vreg will be placed in the same location as tied_to
    pub tied_to: Option<VReg>,
    /// If set, the vreg is ensured to be placed in the same location as fixed
    pub fixed: Option<TM::Reg>,
}
