use crate::code::OpCode;
use monkey_lang_interpreter::object::Object;

pub struct Compiler {
    instructions: Vec<u8>,
    constants: Vec<Object>,
}

pub struct Bytecode {
    instructions: Vec<OpCode>,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: vec![],
            constants: vec![],
        }
    }

    pub fn compile(&mut self, program: monkey_lang_interpreter::ast::Program) -> Bytecode {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::code;
    use monkey_lang_interpreter::lexer::Tokenizer;
    use monkey_lang_interpreter::object::Object;
    use monkey_lang_interpreter::parser::Parser;
    #[ignore]
    #[test]
    fn test_integer_arithmetic() {
        let input = "1 + 2";
        let expected_constants = vec![Object::Integer(1), Object::Integer(2)];
        let expected_instructions = vec![code::OpCode::OpConstant(0), code::OpCode::OpConstant(1)];

        let tokenizer = Tokenizer::new(input);
        let mut parser = Parser::new(tokenizer);
        let program = parser.parse_program().unwrap();

        let bytecode = super::Compiler::new().compile(program);

        assert_eq!(bytecode.constants, expected_constants);
        assert_eq!(bytecode.instructions, expected_instructions);
    }
}
