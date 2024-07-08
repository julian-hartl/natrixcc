use crate::grammar;

pub fn parse(
    input: &str,
) -> Result<Module, lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, &str>> {
    grammar::ModuleParser::new().parse(input)
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub functions: Vec<Function>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub ret_ty: Type,
    pub args: Vec<Type>,
    pub basic_blocks: Vec<BasicBlock>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Arg {
    pub id: Identifier,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BasicBlock {
    pub args: Vec<Arg>,
    pub id: Identifier,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    Add(Identifier, Type, Operand, Operand),
    Sub(Identifier, Type, Operand, Operand),
    Cmp(Identifier, CmpOp, Type, Operand, Operand),
    Op(Identifier, Type, Operand),
    Ret(Option<Operand>),
    Condbr(Operand, Target, Target),
    Br(Target),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CmpOp {
    Eq,
    Gt,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Target(pub Identifier, pub Option<Vec<Operand>>);

#[derive(Debug, PartialEq, Eq)]
pub enum Operand {
    Literal(Literal),
    Value(Identifier),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Bool,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Void,
    Ptr(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Int(i64, Type),
    Bool(bool),
}

pub type Identifier = String;

#[cfg(test)]
mod tests {
    use crate::{
        grammar,
        module::{Arg, BasicBlock, Function, Instruction, Operand, Type},
    };

    #[test]
    fn should_parse_function() {
        let function = grammar::FunctionParser::new()
            .parse(
                r"
fun i32 @add(i32, i32) {
bb0(i32 %v0, i32 %v1):
    i32 %v2 = add %v0, %v1;
    i32 %v3 = add %v2, %v1;
    ret %v3;
}
        ",
            )
            .unwrap();
        assert_eq!(
            function,
            Function {
                name: "add".to_string(),
                ret_ty: Type::I32,
                args: vec![Type::I32, Type::I32,],
                basic_blocks: vec![BasicBlock {
                    id: "bb0".into(),
                    instructions: vec![
                        Instruction::Add(
                            "v2".into(),
                            Type::I32,
                            Operand::Value("v0".into()),
                            Operand::Value("v1".into())
                        ),
                        Instruction::Add(
                            "v3".into(),
                            Type::I32,
                            Operand::Value("v2".into()),
                            Operand::Value("v1".into())
                        ),
                        Instruction::Ret(Some(Operand::Value("v3".into()))),
                    ],
                    args: vec![
                        Arg {
                            id: "v0".into(),
                            ty: Type::I32,
                        },
                        Arg {
                            id: "v1".into(),
                            ty: Type::I32,
                        },
                    ],
                },],
            }
        )
    }
}
