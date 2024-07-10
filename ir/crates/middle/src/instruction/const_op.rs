use crate::instruction::CmpOp;
use crate::Type;
use derive_more::Display;
use rand::Rng;
use strum_macros::EnumTryAs;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Display, EnumTryAs)]
pub enum Const {
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),
    Bool(bool),
}

impl Const {
    pub fn from_ty(ty: Type, value: i64) -> Option<Self> {
        match ty {
            Type::I64 => Some(Self::I64(value)),
            Type::I32 => Some(Self::I32(value as i32)),
            Type::I16 => Some(Self::I16(value as i16)),
            Type::I8 => Some(Self::I8(value as i8)),
            Type::U64 => Some(Self::U64(value as u64)),
            Type::U32 => Some(Self::U32(value as u32)),
            Type::U16 => Some(Self::U16(value as u16)),
            Type::U8 => Some(Self::U8(value as u8)),
            Type::Bool => Some(Self::Bool(value != 0)),
            _ => None,
        }
    }

    pub fn cmp(self, other: Const, op: CmpOp) -> Option<Self> {
        fn cmp_internal<T: Eq + Ord>(lhs: T, rhs: T, op: CmpOp) -> bool {
            match op {
                CmpOp::Eq => lhs == rhs,
                CmpOp::Gt => lhs > rhs,
            }
        }
        let result = match (self, other) {
            (Self::I64(lhs), Self::I64(rhs)) => cmp_internal(lhs, rhs, op),
            (Self::I32(lhs), Self::I32(rhs)) => cmp_internal(lhs, rhs, op),
            (Self::I16(lhs), Self::I16(rhs)) => cmp_internal(lhs, rhs, op),
            (Self::I8(lhs), Self::I8(rhs)) => cmp_internal(lhs, rhs, op),
            (Self::U64(lhs), Self::U64(rhs)) => cmp_internal(lhs, rhs, op),
            (Self::U32(lhs), Self::U32(rhs)) => cmp_internal(lhs, rhs, op),
            (Self::U16(lhs), Self::U16(rhs)) => cmp_internal(lhs, rhs, op),
            (Self::U8(lhs), Self::U8(rhs)) => cmp_internal(lhs, rhs, op),
            (Self::Bool(lhs), Self::Bool(rhs)) => cmp_internal(lhs, rhs, op),
            _ => return None,
        };
        Some(Self::Bool(result))
    }

    pub fn checked_sub(self, other: Const) -> Option<Self> {
        match (self, other) {
            (Self::I64(lhs), Self::I64(rhs)) => {
                let res = lhs.checked_sub(rhs)?;
                Some(Self::I64(res))
            }
            (Self::I32(lhs), Self::I32(rhs)) => {
                let res = lhs.checked_sub(rhs)?;
                Some(Self::I32(res))
            }
            (Self::I16(lhs), Self::I16(rhs)) => {
                let res = lhs.checked_sub(rhs)?;
                Some(Self::I16(res))
            }
            (Self::I8(lhs), Self::I8(rhs)) => {
                let res = lhs.checked_sub(rhs)?;
                Some(Self::I8(res))
            }
            (Self::U64(lhs), Self::U64(rhs)) => {
                let res = lhs.checked_sub(rhs)?;
                Some(Self::U64(res))
            }
            (Self::U32(lhs), Self::U32(rhs)) => {
                let res = lhs.checked_sub(rhs)?;
                Some(Self::U32(res))
            }
            (Self::U16(lhs), Self::U16(rhs)) => {
                let res = lhs.checked_sub(rhs)?;
                Some(Self::U16(res))
            }
            (Self::U8(lhs), Self::U8(rhs)) => {
                let res = lhs.checked_sub(rhs)?;
                Some(Self::U8(res))
            }
            _ => None,
        }
    }

    pub fn checked_add(self, other: Const) -> Option<Self> {
        match (self, other) {
            (Self::I64(lhs), Self::I64(rhs)) => {
                let res = lhs.checked_add(rhs)?;
                Some(Self::I64(res))
            }
            (Self::I32(lhs), Self::I32(rhs)) => {
                let res = lhs.checked_add(rhs)?;
                Some(Self::I32(res))
            }
            (Self::I16(lhs), Self::I16(rhs)) => {
                let res = lhs.checked_add(rhs)?;
                Some(Self::I16(res))
            }
            (Self::I8(lhs), Self::I8(rhs)) => {
                let res = lhs.checked_add(rhs)?;
                Some(Self::I8(res))
            }
            (Self::U64(lhs), Self::U64(rhs)) => {
                let res = lhs.checked_add(rhs)?;
                Some(Self::U64(res))
            }
            (Self::U32(lhs), Self::U32(rhs)) => {
                let res = lhs.checked_add(rhs)?;
                Some(Self::U32(res))
            }
            (Self::U16(lhs), Self::U16(rhs)) => {
                let res = lhs.checked_add(rhs)?;
                Some(Self::U16(res))
            }
            (Self::U8(lhs), Self::U8(rhs)) => {
                let res = lhs.checked_add(rhs)?;
                Some(Self::U8(res))
            }
            _ => None,
        }
    }

    pub fn random<R: Rng>(ty: Type, rng: &mut R) -> Option<Self> {
        match ty {
            Type::I8 => Some(Self::I8(rng.gen_range(i8::MIN..=i8::MAX))),
            Type::I16 => Some(Self::I16(rng.gen_range(i16::MIN..=i16::MAX))),
            Type::I32 => Some(Self::I32(rng.gen_range(i32::MIN..=i32::MAX))),
            Type::I64 => Some(Self::I64(rng.gen_range(i64::MIN..=i64::MAX))),
            Type::U8 => Some(Self::U8(rng.gen_range(u8::MIN..=u8::MAX))),
            Type::U16 => Some(Self::U16(rng.gen_range(u16::MIN..=u16::MAX))),
            Type::U32 => Some(Self::U32(rng.gen_range(u32::MIN..=u32::MAX))),
            Type::U64 => Some(Self::U64(rng.gen_range(u64::MIN..=u64::MAX))),
            _ => None,
        }
    }

    pub fn ty(&self) -> Type {
        match self {
            Const::I64(_) => Type::I64,
            Const::I32(_) => Type::I32,
            Const::I16(_) => Type::I16,
            Const::I8(_) => Type::I8,
            Const::U64(_) => Type::U64,
            Const::U32(_) => Type::U32,
            Const::U16(_) => Type::U16,
            Const::U8(_) => Type::U8,
            Const::Bool(_) => Type::Bool,
        }
    }
}

#[cfg(test)]
mod tests {
    use test_case::test_case;

    use super::Const::*;
    use super::*;

    mod from_ty {
        use super::*;

        use super::Const::*;
        use test_case::test_case;

        #[test_case(Type::I64, 10, Some(I64(10)); "should convert i64")]
        #[test_case(Type::I32, 10, Some(I32(10)); "should convert i32")]
        #[test_case(Type::I16, 10, Some(I16(10)); "should convert i16")]
        #[test_case(Type::I8, 10, Some(I8(10)); "should convert i8")]
        #[test_case(Type::U64, 10, Some(U64(10)); "should convert u64")]
        #[test_case(Type::U32, 10, Some(U32(10)); "should convert u32")]
        #[test_case(Type::U16, 10, Some(U16(10)); "should convert u16")]
        #[test_case(Type::U8, 10, Some(U8(10)); "should convert u8")]
        #[test_case(Type::Bool, 1, Some(Bool(true)); "should convert bool true")]
        #[test_case(Type::Bool, 0, Some(Bool(false)); "should convert bool false")]
        fn from_ty(ty: Type, value: i64, expected: Option<Const>) {
            assert_eq!(Const::from_ty(ty, value), expected);
        }
    }

    mod cmp {
        use test_case::test_case;

        use super::CmpOp::*;
        use super::*;

        #[test_case(I64(10), I64(10), Eq, Some(Bool(true)); "should compare equal i64")]
        #[test_case(I64(10), I64(11), Eq, Some(Bool(false)); "should compare not equal i64")]
        #[test_case(I64(10), I64(11), Gt, Some(Bool(false)); "should compare smaller i64")]
        #[test_case(I64(10), I64(9), Gt, Some(Bool(true)); "should compare greater i64")]
        #[test_case(I32(10), I32(10), Eq, Some(Bool(true)); "should compare equal i32")]
        #[test_case(I32(10), I32(11), Eq, Some(Bool(false)); "should compare not equal i32")]
        #[test_case(I32(10), I32(11), Gt, Some(Bool(false)); "should compare smaller i32")]
        #[test_case(I32(10), I32(9), Gt, Some(Bool(true)); "should compare greater i32")]
        #[test_case(I16(10), I16(10), Eq, Some(Bool(true)); "should compare equal i16")]
        #[test_case(I16(10), I16(11), Eq, Some(Bool(false)); "should compare not equal i16")]
        #[test_case(I16(10), I16(11), Gt, Some(Bool(false)); "should compare smaller i16")]
        #[test_case(I16(10), I16(9), Gt, Some(Bool(true)); "should compare greater i16")]
        #[test_case(I8(10), I8(10), Eq, Some(Bool(true)); "should compare equal i8")]
        #[test_case(I8(10), I8(11), Eq, Some(Bool(false)); "should compare not equal i8")]
        #[test_case(I8(10), I8(11), Gt, Some(Bool(false)); "should compare smaller i8")]
        #[test_case(I8(10), I8(9), Gt, Some(Bool(true)); "should compare greater i8")]
        #[test_case(U64(10), U64(10), Eq, Some(Bool(true)); "should compare equal u64")]
        #[test_case(U64(10), U64(11), Eq, Some(Bool(false)); "should compare not equal u64")]
        #[test_case(U64(10), U64(11), Gt, Some(Bool(false)); "should compare smaller u64")]
        #[test_case(U64(10), U64(9), Gt, Some(Bool(true)); "should compare greater u64")]
        #[test_case(U32(10), U32(10), Eq, Some(Bool(true)); "should compare equal u32")]
        #[test_case(U32(10), U32(11), Eq, Some(Bool(false)); "should compare not equal u32")]
        #[test_case(U32(10), U32(11), Gt, Some(Bool(false)); "should compare smaller u32")]
        #[test_case(U32(10), U32(9), Gt, Some(Bool(true)); "should compare greater u32")]
        #[test_case(U16(10), U16(10), Eq, Some(Bool(true)); "should compare equal u16")]
        #[test_case(U16(10), U16(11), Eq, Some(Bool(false)); "should compare not equal u16")]
        #[test_case(U16(10), U16(11), Gt, Some(Bool(false)); "should compare smaller u16")]
        #[test_case(U16(10), U16(9), Gt, Some(Bool(true)); "should compare greater u16")]
        #[test_case(U8(10), U8(10), Eq, Some(Bool(true)); "should compare equal u8")]
        #[test_case(U8(10), U8(11), Eq, Some(Bool(false)); "should compare not equal u8")]
        #[test_case(U8(10), U8(11), Gt, Some(Bool(false)); "should compare smaller u8")]
        #[test_case(U8(10), U8(9), Gt, Some(Bool(true)); "should compare greater u8")]
        #[test_case(Bool(true), Bool(true), Eq, Some(Bool(true)); "should compare equal bool")]
        #[test_case(Bool(true), Bool(false), Eq, Some(Bool(false)); "should compare not equal bool")]
        #[test_case(Bool(true), Bool(false), Gt, Some(Bool(true)); "should compare greater bool")]
        #[test_case(Bool(false), Bool(true), Gt, Some(Bool(false)); "should compare smaller bool")]
        #[test_case(I64(10), I32(10), Eq, None; "should not compare different types")]
        fn compare(lhs: Const, rhs: Const, op: CmpOp, expected: Option<Const>) {
            assert_eq!(lhs.cmp(rhs, op), expected);
        }
    }

    #[test_case(I64(10), I64(10), Some(I64(20)); "should add i64")]
    #[test_case(I32(10), I32(10), Some(I32(20)); "should add i32")]
    #[test_case(I16(10), I16(10), Some(I16(20)); "should add i16")]
    #[test_case(I8(10), I8(10), Some(I8(20)); "should add i8")]
    #[test_case(U64(10), U64(10), Some(U64(20)); "should add u64")]
    #[test_case(U32(10), U32(10), Some(U32(20)); "should add u32")]
    #[test_case(U16(10), U16(10), Some(U16(20)); "should add u16")]
    #[test_case(U8(10), U8(10), Some(U8(20)); "should add u8")]
    #[test_case(I64(10), I32(10), None; "should not add different types")]
    #[test_case(I64(i64::MAX), I64(1), None; "should not add overflow i64")]
    #[test_case(I64(i64::MIN), I64(-1), None; "should not add underflow i64")]
    #[test_case(U64(u64::MAX), U64(1), None; "should not add overflow u64")]
    #[test_case(I32(i32::MAX), I32(1), None; "should not add overflow i32")]
    #[test_case(I16(i16::MAX), I16(1), None; "should not add overflow i16")]
    #[test_case(I8(i8::MAX), I8(1), None; "should not add overflow i8")]
    #[test_case(U32(u32::MAX), U32(1), None; "should not add overflow u32")]
    #[test_case(U16(u16::MAX), U16(1), None; "should not add overflow u16")]
    #[test_case(U8(u8::MAX), U8(1), None; "should not add overflow u8")]
    fn addition(lhs: Const, rhs: Const, expected: Option<Const>) {
        assert_eq!(lhs.checked_add(rhs), expected);
    }

    #[test_case(I64(20), I64(10), Some(I64(10)); "should subtract i64")]
    #[test_case(I32(20), I32(10), Some(I32(10)); "should subtract i32")]
    #[test_case(I16(20), I16(10), Some(I16(10)); "should subtract i16")]
    #[test_case(I8(20), I8(10), Some(I8(10)); "should subtract i8")]
    #[test_case(U64(20), U64(10), Some(U64(10)); "should subtract u64")]
    #[test_case(U32(20), U32(10), Some(U32(10)); "should subtract u32")]
    #[test_case(U16(20), U16(10), Some(U16(10)); "should subtract u16")]
    #[test_case(U8(20), U8(10), Some(U8(10)); "should subtract u8")]
    #[test_case(I64(10), I32(10), None; "should not subtract different types")]
    #[test_case(I64(i64::MIN), I64(1), None; "should not subtract underflow i64")]
    #[test_case(U64(0), U64(1), None; "should not subtract underflow u64")]
    #[test_case(I32(i32::MIN), I32(1), None; "should not subtract underflow i32")]
    #[test_case(I16(i16::MIN), I16(1), None; "should not subtract underflow i16")]
    #[test_case(I8(i8::MIN), I8(1), None; "should not subtract underflow i8")]
    #[test_case(U32(0), U32(1), None; "should not subtract underflow u32")]
    #[test_case(U16(0), U16(1), None; "should not subtract underflow u16")]
    #[test_case(U8(0), U8(1), None; "should not subtract underflow u8")]
    fn subtraction(lhs: Const, rhs: Const, expected: Option<Const>) {
        assert_eq!(lhs.checked_sub(rhs), expected);
    }
}
