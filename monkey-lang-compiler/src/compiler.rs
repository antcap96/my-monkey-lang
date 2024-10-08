use std::error;
use std::rc::Rc;

use crate::builtins::BUILTINS;
use crate::object::{CompiledFunction, Object};
use crate::symbol_table::Scope;
use crate::{
    code::{Instructions, OpCode},
    symbol_table::SymbolTable,
};
use monkey_lang_core::ast::{
    BlockStatement, Expression, LetStatement, Program, ReturnStatement, Statement,
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompilationError {
    #[error("Unknown identifier: {0}")]
    UnknownIdentifier(Rc<str>),
    #[error("Functions can have at most 255 arguments, got {0}")]
    TooManyArguments(usize),
    #[error("TODO")]
    TODO,
}

#[derive(Debug, Clone)]
pub struct EmittedInstruction {
    op: OpCode,
    position: usize,
}

#[derive(Debug, Clone)]
pub struct Compiler {
    pub constants: Vec<Object>,
    pub symbol_table: SymbolTable,
    scopes: Vec<CompilationScope>,
    scope_index: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let mut compiler = Compiler {
            constants: Vec::new(),
            symbol_table: SymbolTable::new(),
            scopes: vec![CompilationScope::new()],
            scope_index: 0,
        };

        for (index, (name, _func)) in BUILTINS.iter().enumerate() {
            compiler.symbol_table.define_builtin((*name).into(), index);
        }
        compiler
    }

    pub fn new_with_state(constants: Vec<Object>, symbol_table: SymbolTable) -> Self {
        Compiler {
            constants,
            symbol_table,
            scopes: vec![CompilationScope::new()],
            scope_index: 0,
        }
    }

    pub fn add_constant(&mut self, constant: Object) -> u16 {
        self.constants.push(constant);
        self.constants.len() as u16 - 1
    }

    pub fn emit(&mut self, op: OpCode) -> usize {
        let position = self.add_instruction(&op);

        self.set_last_instruction(EmittedInstruction { op, position });

        position
    }

    fn enter_scope(&mut self) {
        self.scopes.push(CompilationScope::new());
        self.scope_index += 1;
        self.symbol_table = SymbolTable::new_enclosed(std::mem::take(&mut self.symbol_table));
    }

    fn leave_scope(&mut self) -> Instructions {
        // Panics if scopes is empty
        let scope = self.scopes.pop().unwrap();
        self.scope_index -= 1;
        self.symbol_table = *self.symbol_table.outer.take().unwrap();

        scope.instructions
    }

    fn current_scope_mut(&mut self) -> &mut CompilationScope {
        &mut self.scopes[self.scope_index]
    }

    fn current_scope(&self) -> &CompilationScope {
        &self.scopes[self.scope_index]
    }

    fn current_instructions_mut(&mut self) -> &mut Instructions {
        &mut self.current_scope_mut().instructions
    }

    fn current_instructions(&self) -> &Instructions {
        &self.current_scope().instructions
    }

    fn last_opcode(&self) -> Option<&OpCode> {
        self.current_scope()
            .last_instruction
            .as_ref()
            .map(|i| &i.op)
    }

    fn set_last_instruction(&mut self, op: EmittedInstruction) {
        self.current_scope_mut().previous_instruction =
            std::mem::replace(&mut self.current_scope_mut().last_instruction, Some(op));
    }

    fn remove_last_instruction(&mut self) {
        // This will crash if there is no last instruction
        let to = self
            .current_scope()
            .last_instruction
            .as_ref()
            .unwrap()
            .position;
        self.current_instructions_mut().pop_to(to);
    }

    pub fn add_instruction(&mut self, op: &OpCode) -> usize {
        let position = self.current_instructions().len();
        self.current_instructions_mut().push(op);
        position
    }

    pub fn compile(&mut self, program: &Program) -> Result<Bytecode, CompilationError> {
        for statement in &program.statements {
            self.compile_statement(statement)?;
        }
        Ok(Bytecode {
            constants: self.constants.clone(),
            instructions: self.current_instructions().clone(),
        })
    }

    pub fn compile_statement(&mut self, statement: &Statement) -> Result<(), CompilationError> {
        match statement {
            Statement::Expression(expression) => {
                self.compile_expression(expression)?;
                self.emit(OpCode::Pop);
            }
            Statement::Let(let_statement) => self.compile_let_statement(let_statement)?,
            Statement::Return(return_statement) => {
                self.compile_return_statement(return_statement)?
            }
        };
        Ok(())
    }

    pub fn compile_expression(&mut self, expression: &Expression) -> Result<(), CompilationError> {
        match expression {
            Expression::InfixOperation(kind, left, right) => {
                use monkey_lang_core::ast::InfixOperationKind as K;
                if kind == &K::LessThan {
                    self.compile_expression(right)?;
                    self.compile_expression(left)?;
                    self.emit(OpCode::GreaterThan);
                } else {
                    self.compile_expression(left)?;
                    self.compile_expression(right)?;
                    match kind {
                        K::Plus => self.emit(OpCode::Add),
                        K::Minus => self.emit(OpCode::Subtract),
                        K::Multiply => self.emit(OpCode::Multiply),
                        K::Divide => self.emit(OpCode::Divide),
                        K::Equal => self.emit(OpCode::Equal),
                        K::NotEqual => self.emit(OpCode::NotEqual),
                        K::GreaterThan => self.emit(OpCode::GreaterThan),
                        K::LessThan => unreachable!("Case handled above"),
                    };
                }
            }
            Expression::PrefixOperation(kind, expression) => {
                use monkey_lang_core::ast::PrefixOperationKind as K;
                self.compile_expression(expression)?;
                match kind {
                    K::Minus => self.emit(OpCode::Minus),
                    K::Bang => self.emit(OpCode::Bang),
                };
            }
            Expression::IntegerLiteral(literal) => {
                let obj_id = self.add_constant(Object::Integer(*literal));
                self.emit(OpCode::Constant(obj_id));
            }
            Expression::StringLiteral(string) => {
                let obj_id = self.add_constant(Object::String(string.clone()));
                self.emit(OpCode::Constant(obj_id));
            }
            Expression::BooleanLiteral(true) => {
                self.emit(OpCode::True);
            }
            Expression::BooleanLiteral(false) => {
                self.emit(OpCode::False);
            }
            Expression::NullLiteral => {
                self.emit(OpCode::Null);
            }
            Expression::ArrayLiteral(array) => {
                for element in array {
                    self.compile_expression(element)?;
                }
                self.emit(OpCode::Array(array.len() as u16));
            }
            Expression::HashLiteral(hash) => {
                for element in hash {
                    self.compile_expression(&element.0)?;
                    self.compile_expression(&element.1)?;
                }
                self.emit(OpCode::Hash(hash.len() as u16));
            }
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                self.compile_expression(condition)?;
                let first_jump_pos = self.emit(OpCode::JumpFalse(u16::MAX));
                self.compile_block_statement(consequence)?;
                if let Some(OpCode::Pop) = self.last_opcode() {
                    self.remove_last_instruction();
                }
                let second_jump_pos = self.emit(OpCode::Jump(u16::MAX));
                let len = self.current_instructions().len() as u16;
                self.current_instructions_mut()
                    .replace(first_jump_pos, OpCode::JumpFalse(len));

                if let Some(alternative) = alternative {
                    self.compile_block_statement(alternative)?;
                    if let Some(OpCode::Pop) = self.last_opcode() {
                        self.remove_last_instruction();
                    }
                } else {
                    self.emit(OpCode::Null);
                }
                let len = self.current_instructions().len() as u16;
                self.current_instructions_mut()
                    .replace(second_jump_pos, OpCode::Jump(len));
            }
            Expression::Identifier(identifier) => {
                let symbol = self
                    .symbol_table
                    .resolve(&identifier.name)
                    .ok_or(CompilationError::UnknownIdentifier(identifier.name.clone()))?;
                match symbol.scope {
                    Scope::Global => self.emit(OpCode::GetGlobal(symbol.index as u16)),
                    Scope::Local => self.emit(OpCode::GetLocal(symbol.index as u8)),
                    Scope::Builtin => self.emit(OpCode::GetBuiltin(symbol.index as u8)),
                };
            }
            Expression::IndexExpression { left, index } => {
                self.compile_expression(left)?;
                self.compile_expression(index)?;

                self.emit(OpCode::Index);
            }
            Expression::FunctionLiteral { parameters, body } => {
                self.enter_scope();
                for param in parameters {
                    self.symbol_table.define(param.name.clone());
                }
                self.compile_block_statement(body)?;

                match self.last_opcode() {
                    // Function ended with an explicit return, nothing to do
                    Some(OpCode::ReturnValue) => {}
                    // Function ended in a `ExpressionStatement`, we replace `Pop` with
                    // `ReturnValue` to handle implicit returns.
                    Some(OpCode::Pop) => {
                        self.remove_last_instruction();
                        self.emit(OpCode::ReturnValue);
                    }
                    // Function ended with a statement that is not a `ReturnStatement`
                    // or a `ExpressionStatement`, so we return null.
                    _ => {
                        self.emit(OpCode::Return);
                    }
                }

                let num_locals = self.symbol_table.num_definitions;
                let instructions = self.leave_scope();
                let id = self.add_constant(Object::CompiledFunction(CompiledFunction {
                    instructions,
                    num_locals,
                    num_parameters: parameters
                        .len()
                        .try_into()
                        .map_err(|_| CompilationError::TooManyArguments(parameters.len()))?,
                }));
                self.emit(OpCode::Constant(id));
            }
            Expression::CallExpression {
                function,
                arguments,
            } => {
                self.compile_expression(function)?;
                for arg in arguments {
                    self.compile_expression(arg)?;
                }
                self.emit(OpCode::Call(arguments.len().try_into().map_err(|_| {
                    CompilationError::TooManyArguments(arguments.len())
                })?));
            }
            Expression::MatchExpression { expression, cases } => todo!(),
        }
        Ok(())
    }

    pub fn compile_block_statement(
        &mut self,
        statements: &BlockStatement,
    ) -> Result<(), CompilationError> {
        for statement in &statements.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    pub fn compile_let_statement(
        &mut self,
        statement: &LetStatement,
    ) -> Result<(), CompilationError> {
        self.compile_expression(&statement.value)?;
        let symbol = self.symbol_table.define(statement.identifier.name.clone());
        match symbol.scope {
            Scope::Global => {
                let index = symbol.index;
                self.emit(OpCode::SetGlobal(index as u16))
            }
            Scope::Local => {
                let index = symbol.index;
                self.emit(OpCode::SetLocal(index as u8))
            }
            Scope::Builtin => panic!("Trying to define a builtin"),
        };

        Ok(())
    }

    pub fn compile_return_statement(
        &mut self,
        statement: &ReturnStatement,
    ) -> Result<(), CompilationError> {
        self.compile_expression(&statement.value)?;
        self.emit(OpCode::ReturnValue);
        Ok(())
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Default)]
struct CompilationScope {
    instructions: Instructions,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl CompilationScope {
    fn new() -> Self {
        CompilationScope {
            instructions: Instructions::new(),
            last_instruction: None,
            previous_instruction: None,
        }
    }
}

#[derive(Debug)]
pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

#[cfg(test)]
mod tests {
    use crate::code::OpCode;
    use crate::object::{CompiledFunction, Object};
    use monkey_lang_core::lexer::Tokenizer;
    use monkey_lang_core::parser::Parser;

    fn validate_expression(input: &str, constants: Vec<Object>, instructions: Vec<OpCode>) {
        let tokenizer = Tokenizer::new(input);
        let mut parser = Parser::new(tokenizer);
        let program = parser.parse_program().unwrap();

        let bytecode = super::Compiler::new().compile(&program).unwrap();

        assert_eq!(bytecode.constants, constants);
        assert_eq!(
            bytecode
                .instructions
                .iter()
                .collect::<Result<Vec<_>, _>>()
                .unwrap(),
            instructions
        );
    }
    #[test]
    fn test_integer_arithmetic() {
        let tests = [
            (
                "1 + 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Add,
                    OpCode::Pop,
                ],
            ),
            (
                "1 - 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Subtract,
                    OpCode::Pop,
                ],
            ),
            (
                "1 * 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Multiply,
                    OpCode::Pop,
                ],
            ),
            (
                "1 / 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Divide,
                    OpCode::Pop,
                ],
            ),
            (
                "-2",
                vec![Object::Integer(2)],
                vec![OpCode::Constant(0), OpCode::Minus, OpCode::Pop],
            ),
        ];

        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_boolean_expressions() {
        let tests = [
            ("true", vec![], vec![OpCode::True, OpCode::Pop]),
            ("false", vec![], vec![OpCode::False, OpCode::Pop]),
            (
                "1 > 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::GreaterThan,
                    OpCode::Pop,
                ],
            ),
            (
                "1 < 2",
                vec![Object::Integer(2), Object::Integer(1)],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::GreaterThan,
                    OpCode::Pop,
                ],
            ),
            (
                "1 == 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Equal,
                    OpCode::Pop,
                ],
            ),
            (
                "1 != 2",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::NotEqual,
                    OpCode::Pop,
                ],
            ),
            (
                "true == false",
                vec![],
                vec![OpCode::True, OpCode::False, OpCode::Equal, OpCode::Pop],
            ),
            (
                "true != false",
                vec![],
                vec![OpCode::True, OpCode::False, OpCode::NotEqual, OpCode::Pop],
            ),
            (
                "!true",
                vec![],
                vec![OpCode::True, OpCode::Bang, OpCode::Pop],
            ),
        ];

        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_conditionals() {
        let tests = [
            (
                "if (true) { 10 }; 3333;",
                vec![Object::Integer(10), Object::Integer(3333)],
                vec![
                    OpCode::True,
                    OpCode::JumpFalse(10),
                    OpCode::Constant(0),
                    OpCode::Jump(11),
                    OpCode::Null,
                    OpCode::Pop,
                    OpCode::Constant(1),
                    OpCode::Pop,
                ],
            ),
            (
                "if (true) { 10 } else { 20 }; 3333;",
                vec![
                    Object::Integer(10),
                    Object::Integer(20),
                    Object::Integer(3333),
                ],
                vec![
                    OpCode::True,
                    OpCode::JumpFalse(10),
                    OpCode::Constant(0),
                    OpCode::Jump(13),
                    OpCode::Constant(1),
                    OpCode::Pop,
                    OpCode::Constant(2),
                    OpCode::Pop,
                ],
            ),
        ];
        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_global_set_statement() {
        let tests = [
            (
                "
                let one = 1;
                let two = 2;",
                vec![Object::Integer(1), Object::Integer(2)],
                vec![
                    OpCode::Constant(0),
                    OpCode::SetGlobal(0),
                    OpCode::Constant(1),
                    OpCode::SetGlobal(1),
                ],
            ),
            (
                "
                let one = 1;
                one;",
                vec![Object::Integer(1)],
                vec![
                    OpCode::Constant(0),
                    OpCode::SetGlobal(0),
                    OpCode::GetGlobal(0),
                    OpCode::Pop,
                ],
            ),
            (
                "
                let one = 1;
                let two = one;
                two;",
                vec![Object::Integer(1)],
                vec![
                    OpCode::Constant(0),
                    OpCode::SetGlobal(0),
                    OpCode::GetGlobal(0),
                    OpCode::SetGlobal(1),
                    OpCode::GetGlobal(1),
                    OpCode::Pop,
                ],
            ),
        ];
        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_string() {
        let tests = [
            (
                r#""string""#,
                vec![Object::String("string".into())],
                vec![OpCode::Constant(0), OpCode::Pop],
            ),
            (
                r#""mon"+"key""#,
                vec![Object::String("mon".into()), Object::String("key".into())],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Add,
                    OpCode::Pop,
                ],
            ),
        ];
        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_array_literal() {
        let tests = [
            ("[]", vec![], vec![(OpCode::Array(0)), (OpCode::Pop)]),
            (
                "[1, 2, 3]",
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Constant(2),
                    OpCode::Array(3),
                    OpCode::Pop,
                ],
            ),
            (
                "[1 + 2, 3 - 4, 5 * 6]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Add,
                    OpCode::Constant(2),
                    OpCode::Constant(3),
                    OpCode::Subtract,
                    OpCode::Constant(4),
                    OpCode::Constant(5),
                    OpCode::Multiply,
                    OpCode::Array(3),
                    OpCode::Pop,
                ],
            ),
        ];
        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_hash_literal() {
        let tests = [
            ("{}", vec![], vec![OpCode::Hash(0), OpCode::Pop]),
            (
                "{1: 2, 3: 4, 5: 6}",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Constant(2),
                    OpCode::Constant(3),
                    OpCode::Constant(4),
                    OpCode::Constant(5),
                    OpCode::Hash(3),
                    OpCode::Pop,
                ],
            ),
            (
                "{1: 2 + 3, 4: 5 * 6}",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(4),
                    Object::Integer(5),
                    Object::Integer(6),
                ],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Constant(2),
                    OpCode::Add,
                    OpCode::Constant(3),
                    OpCode::Constant(4),
                    OpCode::Constant(5),
                    OpCode::Multiply,
                    OpCode::Hash(2),
                    OpCode::Pop,
                ],
            ),
        ];

        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_index_expressions() {
        let tests = [
            (
                "[1, 2, 3][1 + 1]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(3),
                    Object::Integer(1),
                    Object::Integer(1),
                ],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Constant(2),
                    OpCode::Array(3),
                    OpCode::Constant(3),
                    OpCode::Constant(4),
                    OpCode::Add,
                    OpCode::Index,
                    OpCode::Pop,
                ],
            ),
            (
                "{1: 2}[2 - 1]",
                vec![
                    Object::Integer(1),
                    Object::Integer(2),
                    Object::Integer(2),
                    Object::Integer(1),
                ],
                vec![
                    OpCode::Constant(0),
                    OpCode::Constant(1),
                    OpCode::Hash(1),
                    OpCode::Constant(2),
                    OpCode::Constant(3),
                    OpCode::Subtract,
                    OpCode::Index,
                    OpCode::Pop,
                ],
            ),
        ];
        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_functions() {
        let tests = [
            (
                "fn() { return 5 + 10 }",
                vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    Object::CompiledFunction(CompiledFunction {
                        instructions: [
                            OpCode::Constant(0),
                            OpCode::Constant(1),
                            OpCode::Add,
                            OpCode::ReturnValue,
                        ]
                        .into(),
                        num_locals: 0,
                        num_parameters: 0,
                    }),
                ],
                vec![OpCode::Constant(2), OpCode::Pop],
            ),
            (
                "fn() { 5 + 10 }",
                vec![
                    Object::Integer(5),
                    Object::Integer(10),
                    Object::CompiledFunction(CompiledFunction {
                        instructions: [
                            OpCode::Constant(0),
                            OpCode::Constant(1),
                            OpCode::Add,
                            OpCode::ReturnValue,
                        ]
                        .into(),
                        num_locals: 0,
                        num_parameters: 0,
                    }),
                ],
                vec![OpCode::Constant(2), OpCode::Pop],
            ),
        ];

        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_function_calls() {
        let tests = [
            (
                "fn() { 24 }();",
                vec![
                    Object::Integer(24),
                    Object::CompiledFunction(CompiledFunction {
                        instructions: [OpCode::Constant(0), OpCode::ReturnValue].into(),
                        num_locals: 0,
                        num_parameters: 0,
                    }),
                ],
                vec![OpCode::Constant(1), OpCode::Call(0), OpCode::Pop],
            ),
            (
                "
                let noArg = fn() { 24 };
                noArg();
                ",
                vec![
                    Object::Integer(24),
                    Object::CompiledFunction(CompiledFunction {
                        instructions: [OpCode::Constant(0), OpCode::ReturnValue].into(),
                        num_locals: 0,
                        num_parameters: 0,
                    }),
                ],
                vec![
                    OpCode::Constant(1),
                    OpCode::SetGlobal(0),
                    OpCode::GetGlobal(0),
                    OpCode::Call(0),
                    OpCode::Pop,
                ],
            ),
            (
                "
                let oneArg = fn(a) { a };
                oneArg(24);
                ",
                vec![
                    Object::CompiledFunction(CompiledFunction {
                        instructions: [OpCode::GetLocal(0), OpCode::ReturnValue].into(),
                        num_locals: 1,
                        num_parameters: 1,
                    }),
                    Object::Integer(24),
                ],
                vec![
                    OpCode::Constant(0),
                    OpCode::SetGlobal(0),
                    OpCode::GetGlobal(0),
                    OpCode::Constant(1),
                    OpCode::Call(1),
                    OpCode::Pop,
                ],
            ),
            (
                "
                let manyArg = fn(a, b, c) { a; b; c };
                manyArg(24, 25, 26);
                ",
                vec![
                    Object::CompiledFunction(CompiledFunction {
                        instructions: [
                            OpCode::GetLocal(0),
                            OpCode::Pop,
                            OpCode::GetLocal(1),
                            OpCode::Pop,
                            OpCode::GetLocal(2),
                            OpCode::ReturnValue,
                        ]
                        .into(),
                        num_locals: 3,
                        num_parameters: 3,
                    }),
                    Object::Integer(24),
                    Object::Integer(25),
                    Object::Integer(26),
                ],
                vec![
                    OpCode::Constant(0),
                    OpCode::SetGlobal(0),
                    OpCode::GetGlobal(0),
                    OpCode::Constant(1),
                    OpCode::Constant(2),
                    OpCode::Constant(3),
                    OpCode::Call(3),
                    OpCode::Pop,
                ],
            ),
        ];
        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_let_statements_scopes() {
        let tests = [
            (
                "let num = 55;
                 fn() { num }",
                vec![
                    Object::Integer(55),
                    Object::CompiledFunction(CompiledFunction {
                        instructions: [OpCode::GetGlobal(0), OpCode::ReturnValue].into(),
                        num_locals: 0,
                        num_parameters: 0,
                    }),
                ],
                vec![
                    OpCode::Constant(0),
                    OpCode::SetGlobal(0),
                    OpCode::Constant(1),
                    OpCode::Pop,
                ],
            ),
            (
                "
                fn() {
                    let num = 55;
                    num
                }",
                vec![
                    Object::Integer(55),
                    Object::CompiledFunction(CompiledFunction {
                        instructions: [
                            OpCode::Constant(0),
                            OpCode::SetLocal(0),
                            OpCode::GetLocal(0),
                            OpCode::ReturnValue,
                        ]
                        .into(),
                        num_locals: 1,
                        num_parameters: 0,
                    }),
                ],
                vec![OpCode::Constant(1), OpCode::Pop],
            ),
            (
                "
                fn() {
                    let a = 55;
                    let b = 77;
                    a + b
                }",
                vec![
                    Object::Integer(55),
                    Object::Integer(77),
                    Object::CompiledFunction(CompiledFunction {
                        instructions: [
                            OpCode::Constant(0),
                            OpCode::SetLocal(0),
                            OpCode::Constant(1),
                            OpCode::SetLocal(1),
                            OpCode::GetLocal(0),
                            OpCode::GetLocal(1),
                            OpCode::Add,
                            OpCode::ReturnValue,
                        ]
                        .into(),
                        num_locals: 2,
                        num_parameters: 0,
                    }),
                ],
                vec![OpCode::Constant(2), OpCode::Pop],
            ),
        ];

        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_builtins() {
        let tests = [
            (
                "len([]);
                 push([], 1);",
                vec![Object::Integer(1)],
                vec![
                    OpCode::GetBuiltin(0),
                    OpCode::Array(0),
                    OpCode::Call(1),
                    OpCode::Pop,
                    OpCode::GetBuiltin(5),
                    OpCode::Array(0),
                    OpCode::Constant(0),
                    OpCode::Call(2),
                    OpCode::Pop,
                ],
            ),
            (
                "fn() { len([]) }",
                vec![Object::CompiledFunction(CompiledFunction {
                    instructions: [
                        OpCode::GetBuiltin(0),
                        OpCode::Array(0),
                        OpCode::Call(1),
                        OpCode::ReturnValue,
                    ]
                    .into(),
                    num_locals: 0,
                    num_parameters: 0,
                })],
                vec![OpCode::Constant(0), OpCode::Pop],
            ),
        ];

        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }
}
