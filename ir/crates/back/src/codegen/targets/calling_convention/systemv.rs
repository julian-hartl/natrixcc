use crate::codegen::{
    machine::{
        abi::{calling_convention::Slot, CallingConvention},
        Size,
    },
    targets::x86_64,
};

#[derive(Default)]
pub struct SystemV;

impl CallingConvention for SystemV {
    type Reg = x86_64::PhysicalRegister;
    fn parameter_slots(
        params: impl Iterator<Item = Size>,
    ) -> impl Iterator<Item = Slot<Self::Reg>> {
        let mut used_regs = 0;
        params.map(move |size| {
            let slot = if used_regs < 6 {
                let reg = match used_regs {
                    0 => match size {
                        Size::Byte => Self::Reg::DIL,
                        Size::Word => Self::Reg::DI,
                        Size::DWord => Self::Reg::EDI,
                        Size::QWord => Self::Reg::RDI,
                    },
                    1 => match size {
                        Size::Byte => Self::Reg::SIL,
                        Size::Word => Self::Reg::SI,
                        Size::DWord => Self::Reg::ESI,
                        Size::QWord => Self::Reg::RSI,
                    },
                    2 => match size {
                        Size::Byte => Self::Reg::DL,
                        Size::Word => Self::Reg::DX,
                        Size::DWord => Self::Reg::EDX,
                        Size::QWord => Self::Reg::RDX,
                    },
                    3 => match size {
                        Size::Byte => Self::Reg::CL,
                        Size::Word => Self::Reg::CX,
                        Size::DWord => Self::Reg::ECX,
                        Size::QWord => Self::Reg::RCX,
                    },
                    4 => match size {
                        Size::Byte => Self::Reg::R8L,
                        Size::Word => Self::Reg::R8W,
                        Size::DWord => Self::Reg::R8D,
                        Size::QWord => Self::Reg::R8,
                    },
                    5 => match size {
                        Size::Byte => Self::Reg::R9L,
                        Size::Word => Self::Reg::R9W,
                        Size::DWord => Self::Reg::R9D,
                        Size::QWord => Self::Reg::R9,
                    },
                    _ => unreachable!("Too many parameters"),
                };
                used_regs += 1;
                Slot::Register(reg)
            } else {
                Slot::Stack
            };
            slot
        })
    }

    fn return_slot(size: Size) -> Slot<Self::Reg> {
        match size {
            Size::Byte => Slot::Register(Self::Reg::AL),
            Size::Word => Slot::Register(Self::Reg::AX),
            Size::DWord => Slot::Register(Self::Reg::EAX),
            Size::QWord => Slot::Register(Self::Reg::RAX),
        }
    }
}
