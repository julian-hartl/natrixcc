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
    pub args: Vec<Arg>,
    pub basic_blocks: Vec<BasicBlock>,
}


#[derive(Debug, PartialEq, Eq)]
pub struct Arg {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BasicBlock {
    pub name: String,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Instruction {
    Add(String, Type, Operand, Operand),
    Sub(String, Type, Operand, Operand),
    Ret(Option<Operand>),
}


#[derive(Debug, PartialEq, Eq)]
pub enum Operand {
    Literal(Type, Literal),
    Register(String),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Type {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
}

#[cfg(test)]
mod tests {
    use crate::grammar;
    use crate::module::{Arg, BasicBlock, Function, Instruction, Literal, Operand, Type};

    #[test]
    fn should_parse_function() {
        let function = grammar::FunctionParser::new().parse(r"
fun i32 @add(i32 %0, u8 %1):
.entry:
    %val = add i32 %0, %1;
    %val = add i32 %2, %3;
        ").unwrap();
        assert_eq!(
            function,
            Function {
                name: "add".to_string(),
                ret_ty: Type::I32,
                args: vec![
                    Arg {
                        name: "0".to_string(),
                        ty: Type::I32,
                    },
                    Arg {
                        name: "1".to_string(),
                        ty: Type::U8,
                    },
                ],
                basic_blocks: vec![
                    BasicBlock {
                        name: "entry".to_string(),
                        instructions: vec![
                            Instruction::Add("val".to_string(), Type::I32, Operand::Register("0".to_string()), Operand::Register("1".to_string())),
                            Instruction::Add("val".to_string(), Type::I32, Operand::Register("2".to_string()), Operand::Register("3".to_string())),
                        ],
                    },
                ],
            }
        )
    }

    #[test]
    fn should_parse_basic_block() {
        let basic_block = grammar::BasicBlockParser::new().parse(r"
.entry:
    %val = add i32 %0, %1;
    %val =  add i32 %2, %3;
        ").unwrap();
        assert_eq!(basic_block.name, "entry".to_string());
        assert_eq!(basic_block.instructions, vec![
            Instruction::Add("val".to_string(), Type::I32, Operand::Register("0".to_string()), Operand::Register("1".to_string())),
            Instruction::Add("val".to_string(), Type::I32, Operand::Register("2".to_string()), Operand::Register("3".to_string())),
        ]);
    }

    #[test]
    fn should_parse_add_instruction() {
        let instruction = grammar::InstructionParser::new().parse("%val = add i32 %0, %1;").unwrap();
        assert_eq!(instruction,
            Instruction::Add("val".to_string(), Type::I32, Operand::Register("0".to_string()), Operand::Register("1".to_string())),
        );
    }

    #[test]
    fn should_parse_instruction_with_int_literal() {
        let instruction = grammar::InstructionParser::new().parse("%val = add i32 %0, u8 42;").unwrap();
        assert_eq!(instruction,
             Instruction::Add("val".to_string(), Type::I32, Operand::Register("0".to_string()), Operand::Literal(Type::U8, Literal::Int(42))),
        );
    }

    #[test]
    fn should_parse_instruction_with_decl() {
        let instruction = grammar::InstructionParser::new().parse("%0 = add u8 u8 42, %0;").unwrap();
        assert_eq!(instruction,
             Instruction::Add("0".to_string(), Type::U8, Operand::Literal(Type::U8, Literal::Int(42)), Operand::Register("0".to_string())),
        );
    }

    #[test]
    fn should_not_allow_reg_id_not_starting_with_percent() {
        let result = grammar::OperandParser::new().parse("0");
        assert!(result.is_err());
    }

    #[test]
    fn should_not_allow_func_id_not_starting_with_at() {
        let result = grammar::FunctionParser::new().parse("fun i32 add():");
        assert!(result.is_err());
    }

    #[test]
    fn should_parse_a_module() {
        let module = grammar::ModuleParser::new().parse(r"
fun i32 @add(i32 %0, u8 %1):
.entry:
    %val = add i32 %0, %1;
    %val = add i32 %2, %3;
fun i32 @sub(i32 %0, u8 %1):
.entry:
    %val = add i32 %0, %1;
    %val = add i32 %2, %3;
        ").unwrap();
        assert_eq!(module.functions.len(), 2);
    }
}
