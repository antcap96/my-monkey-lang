use std::collections::HashMap;

use crate::{
    builtins::BUILTINS,
    object::{CompiledFunction, Object},
};
use monkey_lang_core::ast::HashKey;
use thiserror::Error;

use crate::{
    code::{InstructionReadError, OpCode},
    compiler::Bytecode,
    frame::Frame,
};

#[derive(Debug, Error, PartialEq)]
pub enum VmError {
    #[error("Empty stack when executing opcode: {0:?}")]
    EmptyStack(OpCode),
    #[error("Invalid operation: {0:?} with operands: {1:?}")]
    InvalidOperation(OpCode, Vec<Object>),
    #[error("Error reading instruction")]
    InstructionReadError(#[from] InstructionReadError),
    #[error("Invalid HashKey {0:?}. Only Integer, String and Boolean are valid keys")]
    InvalidHashKey(Object),
    #[error("Builtin Function error {0}")]
    BuiltinFunctionError(String),
    #[error("Undefined builtin function with index {0}")]
    UndefinedBuiltin(u8),
    #[error("Wrong number of arguments. Expected {expected} got {got}")]
    WrongNumberOfArguments { expected: u8, got: u8 },
}

pub struct Vm {
    constants: Vec<Object>,
    stack: Vec<Object>,
    pub globals: Vec<Object>,
    pub last_popped_stack_element: Option<Object>,
    frames: Vec<Frame>,
}

impl Vm {
    pub fn new(bytecode: Bytecode) -> Vm {
        Vm {
            constants: bytecode.constants,
            stack: Vec::new(),
            last_popped_stack_element: None,
            globals: Vec::new(),
            frames: vec![Frame::new(CompiledFunction {
                instructions: bytecode.instructions,
                num_locals: 0,
                num_parameters: 0,
            })],
        }
    }

    pub fn new_with_global_store(bytecode: Bytecode, globals: Vec<Object>) -> Vm {
        Vm {
            constants: bytecode.constants,
            stack: Vec::new(),
            last_popped_stack_element: None,
            globals,
            frames: vec![Frame::new(CompiledFunction {
                instructions: bytecode.instructions,
                num_locals: 0,
                num_parameters: 0,
            })],
        }
    }

    // TODO: should this consume the VM?
    pub fn run(&mut self) -> Result<(), VmError> {
        while let Some(op) = self.frames.last_mut().unwrap().next() {
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
                    self.frames.last_mut().unwrap().ip = offset as usize;
                }
                OpCode::JumpFalse(offset) => {
                    let condition = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    if Object::Boolean(false) == condition {
                        self.frames.last_mut().unwrap().ip = offset as usize;
                    }
                }
                OpCode::Null => self.stack.push(Object::Null),
                OpCode::SetGlobal(index) => {
                    let object = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    // If globals is not populated enough, fill it with `Object::Null`
                    for _ in self.globals.len()..index as usize {
                        self.globals.push(Object::Null);
                    }
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
                            key.clone().try_into().map_err(VmError::InvalidHashKey)?,
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
                OpCode::Call(n_args) => {
                    let object = self
                        .stack
                        .get(self.stack.len() - 1 - n_args as usize)
                        .ok_or(VmError::EmptyStack(op.clone()))?;
                    match object {
                        Object::CompiledFunction(function) => {
                            if n_args != function.num_parameters as u8 {
                                return Err(VmError::WrongNumberOfArguments {
                                    expected: function.num_parameters as u8,
                                    got: n_args,
                                });
                            }
                            // TODO: Validate number of arguments somehow
                            self.frames.push(Frame {
                                function: function.clone(),
                                ip: 0,
                                base_pointer: self.stack.len() - n_args as usize,
                            });
                            // Set aside space for locals
                            for _ in 0..(function.num_locals - n_args as usize) {
                                self.stack.push(Object::Null);
                            }
                        }
                        Object::Builtin(index) => {
                            let callable = BUILTINS
                                .get(*index as usize)
                                .ok_or(VmError::UndefinedBuiltin(*index))?
                                .1;
                            let split_at = self.stack.len() - n_args as usize - 1;
                            let args = self.stack.split_off(split_at);

                            let output = callable(&args[1..])?;
                            self.stack.push(output);
                        }
                        _ => return Err(VmError::InvalidOperation(op, vec![object.clone()])),
                    }
                }
                OpCode::Return => {
                    let base_pointer = self.frames.last().unwrap().base_pointer;
                    self.frames.pop();
                    self.stack.truncate(base_pointer - 1);
                    self.stack.push(Object::Null);
                }
                OpCode::ReturnValue => {
                    let base_pointer = self.frames.last().unwrap().base_pointer;
                    let return_value = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    self.frames.pop();
                    self.stack.truncate(base_pointer - 1);
                    self.stack.push(return_value);
                }
                OpCode::SetLocal(index) => {
                    let frame = self.frames.last().unwrap();
                    let object_index = frame.base_pointer + index as usize;
                    let object = self.stack.pop().ok_or(VmError::EmptyStack(op.clone()))?;
                    self.stack[object_index] = object;
                }
                OpCode::GetLocal(index) => {
                    let frame = self.frames.last().unwrap();
                    let object_index = frame.base_pointer + index as usize;
                    let object = self.stack[object_index].clone();
                    self.stack.push(object);
                }
                OpCode::GetBuiltin(index) => {
                    self.stack.push(Object::Builtin(index));
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

    fn validate_expression_errors(input: &str) {
        let tokenizer = Tokenizer::new(input);
        let mut parser = Parser::new(tokenizer);
        let program = parser.parse_program().unwrap();

        let bytecode = Compiler::new().compile(&program).unwrap();

        let mut vm = Vm::new(bytecode);
        let result = vm.run();

        assert!(result.is_err())
    }

    fn validate_expression(input: &str, output: Object) {
        let tokenizer = Tokenizer::new(input);
        let mut parser = Parser::new(tokenizer);
        let program = parser.parse_program().unwrap();

        let bytecode = Compiler::new().compile(&program).unwrap();

        let mut vm = Vm::new(bytecode);
        vm.run().expect("Error running VM");

        assert_eq!(vm.last_popped_stack_element, Some(output));
        assert_eq!(vm.stack.len(), 0);
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

    #[test]
    fn test_calling_function_with_no_arguments() {
        let tests = [
            (
                "
                let fivePlusTen = fn() { 5 + 10; };
                fivePlusTen();",
                Object::Integer(15),
            ),
            (
                "
                let one = fn() { 1; };
                let two = fn() { 2; };
                one() + two()",
                Object::Integer(3),
            ),
            (
                "
                let a = fn() { 1 };
                let b = fn() { a() + 1 };
                let c = fn() { b() + 1 };
                c();",
                Object::Integer(3),
            ),
        ];
        for (input, output) in tests {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_functions_with_return_statements() {
        let tests = [
            (
                "
                let earlyExit = fn() { return 99; 100; };
                earlyExit();",
                Object::Integer(99),
            ),
            (
                "
                let earlyExit = fn() { return 99; return 100; };
                earlyExit();",
                Object::Integer(99),
            ),
        ];
        for (input, output) in tests {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_functions_without_return_value() {
        let tests = [
            (
                "
                let empty = fn() {};
                empty();",
                Object::Null,
            ),
            (
                "
                let let_statement = fn() { let x = 1; };
                let_statement();",
                Object::Null,
            ),
        ];
        for (input, output) in tests {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_first_class_functions() {
        let tests = [(
            "
            let returnsOne = fn () { 1 };
            let returnsOneReturner = fn() { returnsOne };
            returnsOneReturner()()",
            Object::Integer(1),
        )];
        for (input, output) in tests {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_calling_functions_with_bindings() {
        let tests = [
            (
                "let one = fn() { let one = 1; one };
                 one();",
                Object::Integer(1),
            ),
            (
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                 oneAndTwo();",
                Object::Integer(3),
            ),
            (
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                 let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
                 oneAndTwo() + threeAndFour();",
                Object::Integer(10),
            ),
            (
                "let firstFoobar = fn() { let foobar = 50; foobar; };
                 let secondFoobar = fn() { let foobar = 100; foobar; };
                 firstFoobar() + secondFoobar();",
                Object::Integer(150),
            ),
            (
                "let globalSeed = 50;
                 let minusOne = fn() {
                     let num = 1;
                     globalSeed - num;
                 };
                 let minusTwo = fn() {
                     let num = 2;
                     globalSeed - num;
                 };
                 minusOne() + minusTwo();",
                Object::Integer(97),
            ),
        ];

        for (input, output) in tests {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_calling_functions_with_arguments_and_bindings() {
        let tests = [
            (
                "let identity = fn(a) { a; };
                 identity(4)",
                Object::Integer(4),
            ),
            (
                "let sum = fn(a, b) { a + b; };
                 sum(1, 2)",
                Object::Integer(3),
            ),
        ];

        for (input, output) in tests {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_builtin_functions() {
        let tests = [
            (r#"len("")"#, Object::Integer(0)),
            (r#"len("four")"#, Object::Integer(4)),
            (r#"len("hello world")"#, Object::Integer(11)),
            (r#"len([1, 2, 3])"#, Object::Integer(3)),
            (r#"len([])"#, Object::Integer(0)),
            (
                r#"to_string("hello world!")"#,
                Object::String("hello world!".into()),
            ),
            (r#"first([1, 2, 3])"#, Object::Integer(1)),
            (r#"first([])"#, Object::Null),
            (r#"last([1, 2, 3])"#, Object::Integer(3)),
            (r#"last([])"#, Object::Null),
            (
                r#"tail([1, 2, 3])"#,
                Object::Array(vec![Object::Integer(2), Object::Integer(3)]),
            ),
            (r#"tail([])"#, Object::Array(vec![])),
        ];
        for (input, output) in tests {
            validate_expression(input, output)
        }
    }

    #[test]
    fn test_builtin_functions_errors() {
        let tests = [
            r#"len(1)"#,
            r#"len("one", "two")"#,
            r#"first(1)"#,
            r#"last(1)"#,
            r#"tail(1)"#,
        ];

        for input in tests {
            validate_expression_errors(input)
        }
    }
}
