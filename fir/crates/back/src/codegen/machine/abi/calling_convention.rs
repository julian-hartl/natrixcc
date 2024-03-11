use firc_middle::Type;
use crate::codegen::machine;

pub trait CallingConvention {
    type Reg: machine::PhysicalRegister;
    
    fn parameter_slots(params: impl Iterator<Item=Type>) -> impl Iterator<Item=ParameterSlot<Self::Reg>>;
}

pub enum ParameterSlot<R: machine::PhysicalRegister> {
    Register(R),
    Stack
}