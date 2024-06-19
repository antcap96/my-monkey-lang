use std::collections::HashMap;

use crate::object::Object;
use monkey_lang_core::ast::HashKey;
use thiserror::Error;

use crate::{
    code::{InstructionReadError, Instructions, OpCode},
    compiler::Bytecode,
};

#[derive(Debug, Error)]
pub enum VmError {
    #[error("Empty stack when executing opcode: {0:?}")]
    EmptyStack(OpCode),
    #[error("Invalid operation: {0:?} with operands: {1:?}")]
    InvalidOperation(OpCode, Vec<Object>),
    #[error("Error reading instruction")]
    InstructionReadError(#[from] InstructionReadError),
    #[error("Invalid HashKey {0:?}. Only Integer, String and Boolean are valid keys")]
    InvalidHashKey(Object),
}

pub struct Vm {
    constants: Vec<Object>,
    instructions: Instructions,
    stack: Vec<Object>,
    pub globals: Vec<Object>,
    pub last_popped_stack_element: Option<Object>,
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Vm {
        Vm {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::new(),
            last_popped_stack_element: None,
            globals: Vec::new(),
        }
    }

    pub fn new_with_global_store(bytecode: Bytecode, globals: Vec<Object>) -> Vm {
        Vm {
            constants: bytecode.constants,
            instructions: bytecode.instructions,
            stack: Vec::new(),
            last_popped_stack_element: None,
            globals,
        }
    }

    pub fn stack_top(&self) -> Option<Object> {
        self.stack.last().cloned()
    }

    // TODO: should this consume the VM?
    pub fn run(&mut self) -> Result<(), VmError> {
        let mut iter = self.instructions.iter();
        while let Some(op) = iter.next() {
            let op = op?;

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
                    match (left, right) {
                        (Object::Integer(l), Object::Integer(r)) => {
                            self.stack.push(Object::Integer(l + r))
                        }
                        (Object::String(l), Object::String(r)) => {
                            self.stack.push(Object::String(l + &r))
                        }
                        (left, right) => {
                            return Err(VmError::InvalidOperation(op.clone(), vec![left, right]))
                        }
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
                OpCode::Jump(offset) => {
                    iter = self.instructions.iter_at(offset as usize);
                }
                OpCode::JumpFalse(offset) => {
                    let condition = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    if Object::Boolean(false) == condition {
                        iter = self.instructions.iter_at(offset as usize);
                    }
                }
                OpCode::Null => self.stack.push(Object::Null),
                OpCode::SetGlobal(index) => {
                    let object = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    // TODO: what if globals is not populated enough?
                    self.globals.insert(index as usize, object);
                }
                OpCode::GetGlobal(index) => {
                    let object = self
                        .globals
                        .get(index as usize)
                        .cloned()
                        .ok_or(VmError::EmptyStack(op.clone()))?;
                    self.stack.push(object);
                }
                OpCode::Array(size) => {
                    let mut output = Vec::with_capacity(size as usize);

                    for _ in 0..size {
                        let top = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                        output.push(top);
                    }
                    output.reverse();

                    self.stack.push(Object::Array(output))
                }
                OpCode::Hash(size) => {
                    let mut output = HashMap::with_capacity(size as usize);

                    for _ in 0..size {
                        // NOTE: could have used Vec::drain to avoid repeatedly updating
                        // the size of the Vec.
                        let value = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                        let key = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                        output.insert(
                            key.clone()
                                .try_into()
                                .map_err(VmError::InvalidHashKey)?,
                            (key, value),
                        );
                    }

                    self.stack.push(Object::Hash(output))
                }
                OpCode::Index => {
                    let index = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    let left = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;

                    match (&left, &index) {
                        (Object::Array(arr), Object::Integer(i)) => self
                            .stack
                            .push(arr.get(*i as usize).cloned().unwrap_or(Object::Null)),
                        (Object::Hash(hash), index) => {
                            let key: HashKey = index
                                .clone()
                                .try_into()
                                .map_err(|_| VmError::InvalidHashKey(index.clone()))?;
                            self.stack.push(
                                hash.get(&key)
                                    .map(|(_, value)| value.clone())
                                    .unwrap_or(Object::Null),
                            )
                        }
                        _ => {
                            return Err(VmError::InvalidOperation(
                                OpCode::Index,
                                vec![left.clone(), index.clone()],
                            ))
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::Object;
    use monkey_lang_core::{ast::HashKey, lexer::Tokenizer, parser::Parser};

    use crate::compiler::Compiler;

    fn validate_expression(input: &str, output: Object) {
        let tokenizer = Tokenizer::new(input);
        let mut parser = Parser::new(tokenizer);
        let program = parser.parse_program().unwrap();

        let bytecode = Compiler::new().compile(&program).unwrap();

        let mut vm = Vm::new(bytecode);
        vm.run().expect("Error running VM");

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

    #[test]
    fn test_conditionals() {
        let inputs = [
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (true) { 10 } else { 20 }", Object::Integer(10)),
            ("if (false) { 10 } else { 20 } ", Object::Integer(20)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (false) { 10 }", Object::Null),
        ];

        for (input, output) in inputs {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_global_set_statement() {
        let tests = [
            ("let one = 1; one", Object::Integer(1)),
            ("let one = 1; let two = 2; one + two", Object::Integer(3)),
            (
                "let one = 1; let two = one + one; one + two",
                Object::Integer(3),
            ),
        ];

        for (input, output) in tests {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_string_expressions() {
        let tests = [
            (r#""monkey""#, Object::String("monkey".into())),
            (r#""mon" + "key""#, Object::String("monkey".into())),
            (
                r#""mon" + "key" + "banana""#,
                Object::String("monkeybanana".into()),
            ),
        ];
        for (input, output) in tests {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_array_literal() {
        let tests = [
            ("[]", Object::Array(Vec::new())),
            (
                "[1, 2, 3]",
                Object::Array(vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                ]),
            ),
            (
                "[1 + 2, 3 * 4, 5 + 6]",
                Object::Array(vec![
                    Object::Integer(3),
                    Object::Integer(12),
                    Object::Integer(11),
                ]),
            ),
        ];
        for (input, output) in tests {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_hash_literal() {
        let tests = [
            ("{}", Object::Hash(HashMap::new())),
            (
                "{1: 2, 2: 3}",
                Object::Hash(HashMap::from([
                    (
                        HashKey::Integer(1),
                        (Object::Integer(1), Object::Integer(2)),
                    ),
                    (
                        HashKey::Integer(2),
                        (Object::Integer(2), Object::Integer(3)),
                    ),
                ])),
            ),
            (
                "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
                Object::Hash(HashMap::from([
                    (
                        HashKey::Integer(2),
                        (Object::Integer(2), Object::Integer(4)),
                    ),
                    (
                        HashKey::Integer(6),
                        (Object::Integer(6), Object::Integer(16)),
                    ),
                ])),
            ),
        ];
        for (input, output) in tests {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_index_expressions() {
        let tests = [
            ("[1, 2, 3][1]", Object::Integer(2)),
            ("[1, 2, 3][0 + 2]", Object::Integer(3)),
            ("[[1, 1, 1]][0][0]", Object::Integer(1)),
            ("[][0]", Object::Null),
            ("[1, 2, 3][99]", Object::Null),
            ("[1][-1]", Object::Null),
            ("{1: 1, 2: 2}[1]", Object::Integer(1)),
            ("{1: 1, 2: 2}[2]", Object::Integer(2)),
            ("{1: 1}[0]", Object::Null),
            ("{}[0]", Object::Null),
        ];
        for (input, output) in tests {
            validate_expression(input, output)
        }
    }
}
