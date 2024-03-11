use crate::grammar;

pub fn parse(input: &str) -> Result<Module, lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token, &str>> {
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
    pub id: RegId,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BasicBlock {
    pub args: Vec<Arg>,
    pub id: BasicBlockId,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    Add(RegId, Type, Operand, Operand),
    Sub(RegId, Type, Operand, Operand),
    ICmp(RegId, Type, CmpOp, Operand, Operand),
    Op(RegId, Type, Operand),
    Ret(Type, Option<Operand>),
    Condbr(Type, Operand, Target, Target),
    Br(Target),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum CmpOp {
    Eq,
    Gt
}

#[derive(Debug, PartialEq, Eq)]
pub struct Target(pub BasicBlockId, pub Option<Vec<Operand>>);

#[derive(Debug, PartialEq, Eq)]
pub enum Operand {
    Literal(Literal),
    Register(RegId),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RegId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BasicBlockId(pub u32);

#[cfg(test)]
mod tests {
    use crate::grammar;
    use crate::module::{Arg, BasicBlock, BasicBlockId, Function, Instruction, Operand, RegId, Type};

    #[test]
    fn should_parse_function() {
        let function = grammar::FunctionParser::new().parse(r"
fun i32 @add(i32, i32) {
bb0(i32 v0, i32 v1):
    v2 = add i32 v0, v1;
    v3 = add i32 v2, v1;
    ret i32 v3;
}
        ").unwrap();
        assert_eq!(
            function,
            Function {
                name: "add".to_string(),
                ret_ty: Type::I32,
                args: vec![
                    Type::I32,
                    Type::I32,
                ],
                basic_blocks: vec![
                    BasicBlock {
                        id: BasicBlockId(0),
                        instructions: vec![
                            Instruction::Add(RegId(2), Type::I32, Operand::Register(RegId(0)), Operand::Register(RegId(1))),
                            Instruction::Add(RegId(3), Type::I32, Operand::Register(RegId(2)), Operand::Register(RegId(1))),
                        ],
                        args: vec![
                            Arg {
                                id: RegId(0),
                                ty: Type::I32,
                            },
                            Arg {
                                id: RegId(1),
                                ty: Type::I32,
                            },
                        ],
                    },
                ],
            }
        )
    }
}
