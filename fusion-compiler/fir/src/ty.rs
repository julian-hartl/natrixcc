use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    I32,
    I8,
    ZeroSized,
    Ptr(Box<Type>),
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::I8 => 1,
            Type::I32 => 4,
            Type::ZeroSized => 0,
            Type::Ptr(_) => 8,
        }
    }

    pub fn alignment(&self) -> usize {
        match self {
            Type::I8 => 1,
            Type::I32 => 4,
            Type::ZeroSized => 1,
            Type::Ptr(_) => 8,
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
            Type::I8 => write!(f, "i8"),
            Type::I32 => write!(f, "i32"),
            Type::Ptr(ty) => write!(f, "ptr"),
            Type::ZeroSized => write!(f, "zerotype"),
        }
    }
}
