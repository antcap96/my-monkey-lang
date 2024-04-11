use monkey_lang_interpreter::object::Object;

use crate::{code::OpCode, compiler::Bytecode};

pub struct Vm {
    constants: Vec<Object>,
    instructions: Vec<OpCode>,
    stack: Vec<Object>,
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Vm {
        Vm {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::new(),
        }
    }

    pub fn stack_top(&self) -> Option<Object> {
        self.stack.last().cloned()
    }

    pub fn run(&mut self) {
        for op in &self.instructions {
            match op {
                OpCode::OpConstant(const_index) => {
                    let object = self.constants[*const_index as usize].clone();
                    self.stack.push(object);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use monkey_lang_core::{lexer::Tokenizer, parser::Parser};
    use monkey_lang_interpreter::object::Object;

    use crate::compiler::Compiler;

    #[test]
    fn test_integer_arithmetic() {
        let inputs = [
            ("1", Object::Integer(1)),
            ("2", Object::Integer(2)),
            ("1 + 2", Object::Integer(2)), // FIXME
        ];

        for (input, output) in inputs {
            let tokenizer = Tokenizer::new(input);
            let mut parser = Parser::new(tokenizer);
            let program = parser.parse_program().unwrap();

            let bytecode = Compiler::new().compile(program).unwrap();

            let mut vm = Vm::new(bytecode);
            vm.run();

            assert_eq!(vm.stack_top(), Some(output))
        }
    }
}
