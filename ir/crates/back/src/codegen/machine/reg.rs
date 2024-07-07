use std::fmt::{
    Display,
    Formatter,
};

use slotmap::new_key_type;

use crate::codegen::machine::{
    function::Function,
    isa::PhysicalRegister,
    Size,
    TargetMachine,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Register<TM: TargetMachine> {
    Virtual(VRegRef),
    Physical(TM::Reg),
}

impl<TM: TargetMachine> From<VRegRef> for Register<TM> {
    fn from(vreg: VRegRef) -> Self {
        Self::Virtual(vreg)
    }
}

impl<TM: TargetMachine> Register<TM> {
    pub fn try_as_virtual(&self) -> Option<VRegRef> {
        match self {
            Register::Virtual(virt_reg) => Some(*virt_reg),
            Register::Physical(_) => None,
        }
    }

    pub fn try_as_virtual_mut(&mut self) -> Option<&mut VRegRef> {
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

    pub fn display<'func>(&self, func: &'func Function<TM>) -> RegisterDisplay<'func, TM> {
        RegisterDisplay { func, reg: *self }
    }
}

pub struct RegisterDisplay<'func, TM: TargetMachine> {
    func: &'func Function<TM>,
    reg: Register<TM>,
}

impl<TM: TargetMachine> Display for RegisterDisplay<'_, TM> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.reg {
            Register::Virtual(virt_reg) => write!(f, "{}", self.func.vregs[virt_reg]),
            Register::Physical(phys_reg) => write!(f, "${}", phys_reg.name()),
        }
    }
}

new_key_type! {
    pub struct VRegRef;
}

impl VRegRef {
    pub fn size<TM: TargetMachine>(self, func: &Function<TM>) -> Size {
        func.get_vreg(self).size
    }

    pub fn display<TM: TargetMachine>(self, func: &Function<TM>) -> String {
        func.get_vreg(self).symbol.clone()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VReg<TM: TargetMachine> {
    pub size: Size,
    /// If set, the vreg will be placed in the same location as tied_to
    pub tied_to: Option<VRegRef>,
    /// If set, the vreg is ensured to be placed in the same location as fixed
    pub fixed: Option<TM::Reg>,
    pub symbol: String,
}

impl<TM: TargetMachine> Display for VReg<TM> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.symbol)
    }
}
