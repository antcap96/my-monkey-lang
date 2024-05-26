use crate::code::{Instructions, OpCode};
use monkey_lang_core::ast::{
    BlockStatement, Expression, LetStatement, Program, ReturnStatement, Statement,
};
use monkey_lang_interpreter::object::Object;

#[derive(Debug)]
pub enum CompilationError {
    TODO,
}

#[derive(Debug)]
pub struct EmittedInstruction {
    op: OpCode,
    position: usize,
}

#[derive(Debug)]
pub struct Compiler {
    instructions: Instructions,
    constants: Vec<Object>,
    last_instruction: Option<EmittedInstruction>,
    previous_instruction: Option<EmittedInstruction>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions::new(),
            constants: Vec::new(),
            last_instruction: None,
            previous_instruction: None,
        }
    }

    pub fn add_constant(&mut self, constant: Object) -> u16 {
        self.constants.push(constant);
        return self.constants.len() as u16 - 1;
    }

    pub fn emit(&mut self, op: OpCode) -> usize {
        let position = self.add_instruction(&op);

        self.set_last_instruction(EmittedInstruction { op, position });

        position
    }

    fn set_last_instruction(&mut self, op: EmittedInstruction) {
        self.previous_instruction = std::mem::replace(&mut self.last_instruction, Some(op));
    }

    fn remove_last_instruction(&mut self) {
        // This will crash if there is no last instruction
        self.instructions
            .pop_to(self.last_instruction.as_ref().unwrap().position);
    }

    pub fn add_instruction(&mut self, op: &OpCode) -> usize {
        let position = self.instructions.len();
        self.instructions.push(op);
        position
    }

    pub fn compile(mut self, program: Program) -> Result<Bytecode, CompilationError> {
        for statement in program.statements {
            self.compile_statement(statement)?;
        }
        Ok(Bytecode {
            constants: self.constants,
            instructions: self.instructions,
        })
    }

    pub fn compile_statement(&mut self, statement: Statement) -> Result<(), CompilationError> {
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

    pub fn compile_expression(&mut self, expression: Expression) -> Result<(), CompilationError> {
        match expression {
            Expression::InfixOperation(kind, left, right) => {
                use monkey_lang_core::ast::InfixOperationKind as K;
                if kind == K::LessThan {
                    self.compile_expression(*right)?;
                    self.compile_expression(*left)?;
                    self.emit(OpCode::GreaterThan);
                } else {
                    self.compile_expression(*left)?;
                    self.compile_expression(*right)?;
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
                self.compile_expression(*expression)?;
                match kind {
                    K::Minus => self.emit(OpCode::Minus),
                    K::Bang => self.emit(OpCode::Bang),
                };
            }
            Expression::IntegerLiteral(literal) => {
                let obj_id = self.add_constant(Object::Integer(literal));
                self.add_instruction(&OpCode::Constant(obj_id));
            }
            Expression::BooleanLiteral(true) => {
                self.add_instruction(&OpCode::True);
            }
            Expression::BooleanLiteral(false) => {
                self.add_instruction(&OpCode::False);
            }
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                self.compile_expression(*condition)?;
                let first_jump_pos = self.emit(OpCode::JumpFalse(u16::MAX));
                self.compile_block_statement(consequence)?;
                if let Some(OpCode::Pop) = self.last_instruction.as_ref().map(|i| &i.op) {
                    self.remove_last_instruction();
                }
                let mut first_jump_to = self.instructions.len() as u16;

                if let Some(alternative) = alternative {
                    let second_jump_pos = self.emit(OpCode::Jump(u16::MAX));
                    first_jump_to = self.instructions.len() as u16;
                    self.compile_block_statement(alternative)?;
                    if let Some(OpCode::Pop) = self.last_instruction.as_ref().map(|i| &i.op) {
                        self.remove_last_instruction();
                    }
                    self.instructions.replace(
                        second_jump_pos,
                        OpCode::Jump(self.instructions.len() as u16),
                    );
                }
                self.instructions
                    .replace(first_jump_pos, OpCode::JumpFalse(first_jump_to));
            }
            _ => {}
        }
        Ok(())
    }

    pub fn compile_block_statement(
        &mut self,
        statements: BlockStatement,
    ) -> Result<(), CompilationError> {
        for statement in statements.statements {
            self.compile_statement(statement)?;
        }
        Ok(())
    }

    pub fn compile_let_statement(
        &mut self,
        statement: LetStatement,
    ) -> Result<(), CompilationError> {
        Err(CompilationError::TODO)
    }

    pub fn compile_return_statement(
        &mut self,
        statement: ReturnStatement,
    ) -> Result<(), CompilationError> {
        Err(CompilationError::TODO)
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
    use monkey_lang_core::lexer::Tokenizer;
    use monkey_lang_core::parser::Parser;
    use monkey_lang_interpreter::object::Object;

    fn validate_expression(input: &str, constants: Vec<Object>, instructions: Vec<OpCode>) {
        let tokenizer = Tokenizer::new(input);
        let mut parser = Parser::new(tokenizer);
        let program = parser.parse_program().unwrap();

        let bytecode = super::Compiler::new().compile(program).unwrap();

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
                    OpCode::JumpFalse(7),
                    OpCode::Constant(0),
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
}
