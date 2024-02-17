use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Int {
        width: u32,
        signed: bool,
    },
    Ptr(Box<Type>),
}

impl Type {

    pub const I1: Self = Self::Int {
        width: 1,
        signed: true,
    };

    pub const I8: Self = Self::Int {
        width: 8,
        signed: true,
    };

    pub const I16: Self = Self::Int {
        width: 16,
        signed: true,
    };

    pub const I32: Self = Self::Int {
        width: 32,
        signed: true,
    };

    pub const I64: Self = Self::Int {
        width: 64,
        signed: true,
    };

    pub const fn size(&self) -> u32 {
        match self {
            Self::Int {
                width,
                signed: _,
            } => width.div_ceil(8),
            Self::Ptr(_) => 8,
        }
    }

    pub const fn alignment(&self) -> u32 {
        match self {
            Self::Int {
                width,
                ..
            } => width.div_ceil(8),
            Self::Ptr(_) => 8,
        }
    }

    pub fn deref(&self) -> &Type {
        match self {
            Type::Ptr(ty) => ty,
            _ => self,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int {
                width,
                signed,
            } => write!(f, "{}{}", if *signed { "i" } else { "u" }, width),
            Type::Ptr(_ty) => write!(f, "ptr"),
        }
    }
}
