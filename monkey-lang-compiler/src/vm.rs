use monkey_lang_interpreter::object::Object;

use crate::{
    code::{InstructionReadError, Instructions, OpCode},
    compiler::Bytecode,
};

#[derive(Debug)]
pub enum VmError {
    EmptyStack(OpCode),
    InvalidOperation(OpCode, Vec<Object>),
    InstructionReadError(InstructionReadError),
}

pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,
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

    // TODO: should this consume the VM?
    pub fn run(&mut self) -> Result<(), VmError> {
        for op in self.instructions.iter() {
            let op = op.map_err(|e| VmError::InstructionReadError(e))?;

            match op {
                OpCode::Constant(const_index) => {
                    let object = self.constants[const_index as usize].clone();
                    self.stack.push(object);
                }
                OpCode::True => self.stack.push(Object::Boolean(true)),
                OpCode::False => self.stack.push(Object::Boolean(false)),
                OpCode::Add => {
                    let right = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    let left = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    match (&left, &right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.stack.push(Object::Integer(l + r))
                        }
                        _ => return Err(VmError::InvalidOperation(op.clone(), vec![left, right])),
                    }
                }
                OpCode::Subtract => {
                    let right = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    let left = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    match (&left, &right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.stack.push(Object::Integer(l - r))
                        }
                        _ => return Err(VmError::InvalidOperation(op.clone(), vec![left, right])),
                    }
                }
                OpCode::Multiply => {
                    let right = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    let left = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    match (&left, &right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.stack.push(Object::Integer(l * r))
                        }
                        _ => return Err(VmError::InvalidOperation(op.clone(), vec![left, right])),
                    }
                }
                OpCode::Divide => {
                    let right = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    let left = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    match (&left, &right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.stack.push(Object::Integer(l / r))
                        }
                        _ => return Err(VmError::InvalidOperation(op.clone(), vec![left, right])),
                    }
                }
                OpCode::Equal => {
                    let right = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    let left = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    self.stack.push(Object::Boolean(left == right));
                }
                OpCode::NotEqual => {
                    let right = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    let left = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    self.stack.push(Object::Boolean(left != right));
                }
                OpCode::GreaterThan => {
                    let right = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    let left = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    match (&left, &right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.stack.push(Object::Boolean(l > r))
                        }
                        _ => return Err(VmError::InvalidOperation(op.clone(), vec![left, right])),
                    }
                }
                OpCode::Minus => {
                    let object = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    match object {
                        Object::Integer(i) => self.stack.push(Object::Integer(-i)),
                        _ => return Err(VmError::InvalidOperation(op.clone(), vec![object])),
                    }
                }
                OpCode::Bang => {
                    let object = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    match object {
                        Object::Boolean(b) => self.stack.push(Object::Boolean(!b)),
                        _ => return Err(VmError::InvalidOperation(op.clone(), vec![object])),
                    }
                }
                OpCode::Pop => {
                    let popped = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    self.last_popped_stack_element = Some(popped);
                }
                _ => todo!(),
            }
        }
        Ok(())
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
        vm.run().unwrap();

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
            ("-5", Object::Integer(-5)),
            ("-10", Object::Integer(-10)),
            ("-50 + 100 + -50", Object::Integer(0)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
        ];

        for (input, output) in inputs {
            validate_expression(input, output)
        }
    }
    #[test]
    fn test_boolean_expressions() {
        let inputs = [
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
            ("1 < 2", Object::Boolean(true)),
            ("1 > 2", Object::Boolean(false)),
            ("1 < 1", Object::Boolean(false)),
            ("1 > 1", Object::Boolean(false)),
            ("1 == 1", Object::Boolean(true)),
            ("1 != 1", Object::Boolean(false)),
            ("1 == 2", Object::Boolean(false)),
            ("1 != 2", Object::Boolean(true)),
            ("true == true", Object::Boolean(true)),
            ("false == false", Object::Boolean(true)),
            ("true == false", Object::Boolean(false)),
            ("true != false", Object::Boolean(true)),
            ("false != true", Object::Boolean(true)),
            ("(1 < 2) == true", Object::Boolean(true)),
            ("(1 < 2) == false", Object::Boolean(false)),
            ("(1 > 2) == true", Object::Boolean(false)),
            ("(1 > 2) == false", Object::Boolean(true)),
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
        ];

        for (input, output) in inputs {
            validate_expression(input, output)
        }
    }
}
