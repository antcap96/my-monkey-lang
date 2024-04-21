use monkey_lang_interpreter::object::Object;

use crate::{code::OpCode, compiler::Bytecode};

pub struct Vm {
    constants: Vec<Object>,
    instructions: Vec<OpCode>,
    stack: Vec<Object>,
    pub last_popped_stack_element: Option<Object>,
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Vm {
        Vm {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::new(),
            last_popped_stack_element: None,
        }
    }

    pub fn stack_top(&self) -> Option<Object> {
        self.stack.last().cloned()
    }

    pub fn run(&mut self) {
        for op in &self.instructions {
            match op {
                OpCode::Constant(const_index) => {
                    let object = self.constants[*const_index as usize].clone();
                    self.stack.push(object);
                }
                OpCode::True => self.stack.push(Object::Boolean(true)),
                OpCode::False => self.stack.push(Object::Boolean(false)),
                OpCode::Add => {
                    let msg = "Not enough elements to execute Add";
                    let right = self.stack.pop().expect(msg);
                    let left = self.stack.pop().expect(msg);
                    match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.stack.push(Object::Integer(l + r))
                        }
                        _ => todo!(),
                    }
                }
                OpCode::Subtract => {
                    let msg = "Not enough elements to execute Subtract";
                    let right = self.stack.pop().expect(msg);
                    let left = self.stack.pop().expect(msg);
                    match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.stack.push(Object::Integer(l - r))
                        }
                        _ => todo!(),
                    }
                }
                OpCode::Multiply => {
                    let msg = "Not enough elements to execute Multiply";
                    let right = self.stack.pop().expect(msg);
                    let left = self.stack.pop().expect(msg);
                    match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.stack.push(Object::Integer(l * r))
                        }
                        _ => todo!(),
                    }
                }
                OpCode::Divide => {
                    let msg = "Not enough elements to execute Divide";
                    let right = self.stack.pop().expect(msg);
                    let left = self.stack.pop().expect(msg);
                    match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.stack.push(Object::Integer(l / r))
                        }
                        _ => todo!(),
                    }
                }
                OpCode::Pop => {
                    let popped = self.stack.pop();
                    if popped.is_none() {
                        panic!("Pop used on empty stack")
                    }
                    self.last_popped_stack_element = popped;
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

    fn validate_expression(input: &str, output: Object) {
        let tokenizer = Tokenizer::new(input);
        let mut parser = Parser::new(tokenizer);
        let program = parser.parse_program().unwrap();

        let bytecode = Compiler::new().compile(program).unwrap();

        let mut vm = Vm::new(bytecode);
        vm.run();

        assert_eq!(vm.last_popped_stack_element, Some(output))
    }

    #[test]
    fn test_integer_arithmetic() {
        let inputs = [
            ("1", Object::Integer(1)),
            ("2", Object::Integer(2)),
            ("1 + 2", Object::Integer(3)),
            ("1 - 2", Object::Integer(-1)),
            ("1 * 2", Object::Integer(2)),
            ("5 / 2", Object::Integer(2)),
            ("50 / 2 * 2 + 10 - 5", Object::Integer(55)),
            ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("5 * 2 + 10", Object::Integer(20)),
            ("5 + 2 * 10", Object::Integer(25)),
            ("5 * (2 + 10)", Object::Integer(60)),
        ];

        for (input, output) in inputs {
            validate_expression(input, output)
        }
    }
    #[test]
    fn test_booleans() {
        let inputs = [
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
        ];

        for (input, output) in inputs {
            validate_expression(input, output)
        }
    }
}
