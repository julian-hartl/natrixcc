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
        module::{
            Arg,
            BasicBlock,
            Function,
            Instruction,
            Operand,
            Type,
            ValueId,
        },
    };

    #[test]
    fn should_parse_function() {
        let function = grammar::FunctionParser::new()
            .parse(
                r"
fun i32 @add(i32, i32) {
bb0(i32 v0, i32 v1):
    v2 = add i32 v0, v1;
    v3 = add i32 v2, v1;
    ret v3;
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
                    id: BasicBlockId(0),
                    instructions: vec![
                        Instruction::Add(
                            ValueId(2),
                            Type::I32,
                            Operand::Value(ValueId(0)),
                            Operand::Value(ValueId(1))
                        ),
                        Instruction::Add(
                            ValueId(3),
                            Type::I32,
                            Operand::Value(ValueId(2)),
                            Operand::Value(ValueId(1))
                        ),
                        Instruction::Ret(Type::I32, Some(Operand::Value(ValueId(3)))),
                    ],
                    args: vec![
                        Arg {
                            id: ValueId(0),
                            ty: Type::I32,
                        },
                        Arg {
                            id: ValueId(1),
                            ty: Type::I32,
                        },
                    ],
                },],
            }
        )
    }
}
