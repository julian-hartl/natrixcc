use std::fmt::{
    Display,
    Formatter,
};

use cranelift_entity::entity_impl;

use crate::codegen::machine::{
    Abi,
    function::Function,
    isa::PhysicalRegister,
    Size,
};
use crate::codegen::machine::isa::Isa;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Register<I: Isa> {
    Virtual(VReg),
    Physical(I::Reg),
}

impl<I: Isa> From<VReg> for Register<I> {
    fn from(vreg: VReg) -> Self {
        Self::Virtual(vreg)
    }
}

impl<I: Isa> Register<I> {
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

    pub fn try_as_physical(&self) -> Option<I::Reg> {
        match self {
            Register::Virtual(_) => None,
            Register::Physical(phys_reg) => Some(*phys_reg),
        }
    }

    pub fn size(&self, func: &Function<I>) -> Size {
        match self {
            Register::Virtual(vreg) => vreg.size(func),
            Register::Physical(phys_reg) => phys_reg.size(),
        }
    }
}

impl<I: Isa> Copy for Register<I> {}

impl<A: Isa> Display for Register<A> {
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
    pub fn size<I: Isa>(self, func: &Function<I>) -> Size {
        func.get_vreg(self).size
    }
}

entity_impl!(VReg, "v");

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct VRegInfo<I: Isa> {
    pub size: Size,
    /// If set, the vreg will be placed in the same location as tied_to
    pub tied_to: Option<VReg>,
    /// If set, the vreg is ensured to be placed in the same location as fixed
    pub fixed: Option<I::Reg>,
}
