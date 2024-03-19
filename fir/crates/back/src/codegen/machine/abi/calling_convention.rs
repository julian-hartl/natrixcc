use firc_middle::Type;
use crate::codegen::machine;
use crate::codegen::machine::Size;

pub trait CallingConvention {
    type Reg: machine::PhysicalRegister;

    fn parameter_slots(params: impl Iterator<Item=Size>) -> impl Iterator<Item=Slot<Self::Reg>>;

    fn return_slot(size: Size) -> Slot<Self::Reg>;
}

pub enum Slot<R: machine::PhysicalRegister> {
    Register(R),
    Stack
}
