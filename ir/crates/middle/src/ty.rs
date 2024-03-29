use std::fmt::{
    Display,
    Formatter,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Bool,
    Void,
    Ptr(Box<Type>),
}

impl Type {
    pub const fn size(&self) -> u32 {
        match self {
            Self::U8 => 1,
            Self::U16 => 2,
            Self::U32 => 4,
            Self::U64 => 8,
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 => 8,
            Self::Bool => 1,
            Self::Ptr(_) => 8,
            Self::Void => 0,
        }
    }

    pub const fn alignment(&self) -> u32 {
        self.size()
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
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::Bool => write!(f, "bool"),
            Type::Ptr(_ty) => write!(f, "ptr"),
            Type::Void => write!(f, "void"),
        }
    }
}
