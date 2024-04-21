use crate::code::OpCode;
use monkey_lang_core::ast::{Expression, LetStatement, Program, ReturnStatement, Statement};
use monkey_lang_interpreter::object::Object;

pub struct Compiler {
    instructions: Vec<OpCode>,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, constant: Object) -> u16 {
        self.constants.push(constant);
        return self.constants.len() as u16 - 1;
    }

    pub fn emit(&mut self, op: OpCode) -> usize {
        self.add_instruction(op)
    }

    pub fn add_instruction(&mut self, op: OpCode) -> usize {
        self.instructions.push(op);
        self.instructions.len() - 1
    }

    pub fn compile(mut self, program: Program) -> Result<Bytecode, ()> {
        for statement in program.statements {
            self.compile_statement(statement)?;
        }
        Ok(Bytecode {
            constants: self.constants,
            instructions: self.instructions,
        })
    }

    pub fn compile_statement(&mut self, statement: Statement) -> Result<(), ()> {
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

    pub fn compile_expression(&mut self, expression: Expression) -> Result<(), ()> {
        match expression {
            Expression::InfixOperation(kind, left, right) => {
                self.compile_expression(*left)?;
                self.compile_expression(*right)?;
                use monkey_lang_core::ast::InfixOperationKind as K;
                match kind {
                    K::Plus => self.emit(OpCode::Add),
                    K::Minus => self.emit(OpCode::Subtract),
                    K::Multiply => self.emit(OpCode::Multiply),
                    K::Divide => self.emit(OpCode::Divide),
                    _ => todo!(),
                };
            }
            Expression::IntegerLiteral(literal) => {
                let obj_id = self.add_constant(Object::Integer(literal));
                self.add_instruction(OpCode::Constant(obj_id));
            }
            Expression::BooleanLiteral(true) => {
                self.add_instruction(OpCode::True);
            }
            Expression::BooleanLiteral(false) => {
                self.add_instruction(OpCode::False);
            }
            _ => {}
        }
        Ok(())
    }

    pub fn compile_let_statement(&mut self, statement: LetStatement) -> Result<(), ()> {
        todo!()
    }

    pub fn compile_return_statement(&mut self, statement: ReturnStatement) -> Result<(), ()> {
        todo!()
    }
}

pub struct Bytecode {
    pub instructions: Vec<OpCode>,
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
        assert_eq!(bytecode.instructions, instructions);
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
        ];

        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }

    #[test]
    fn test_booleans() {
        let tests = [
            ("true", vec![], vec![OpCode::True, OpCode::Pop]),
            ("false", vec![], vec![OpCode::False, OpCode::Pop]),
        ];

        for (input, constants, instructions) in tests {
            validate_expression(input, constants, instructions);
        }
    }
}
