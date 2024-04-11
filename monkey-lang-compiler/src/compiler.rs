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
            instructions: vec![],
            constants: vec![],
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
            Statement::Expression(expression) => self.compile_expression(expression)?,
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
                Ok(())
            }
            Expression::IntegerLiteral(literal) => {
                let obj_id = self.add_constant(Object::Integer(literal));
                self.add_instruction(OpCode::OpConstant(obj_id));
                Ok(())
            }
            _ => Ok(()),
        }
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
    use crate::code;
    use monkey_lang_core::lexer::Tokenizer;
    use monkey_lang_core::parser::Parser;
    use monkey_lang_interpreter::object::Object;
    #[test]
    fn test_integer_arithmetic() {
        let input = "1 + 2";
        let expected_constants = vec![Object::Integer(1), Object::Integer(2)];
        let expected_instructions = vec![code::OpCode::OpConstant(0), code::OpCode::OpConstant(1)];

        let tokenizer = Tokenizer::new(input);
        let mut parser = Parser::new(tokenizer);
        let program = parser.parse_program().unwrap();

        let bytecode = super::Compiler::new().compile(program).unwrap();

        assert_eq!(bytecode.constants, expected_constants);
        assert_eq!(bytecode.instructions, expected_instructions);
    }
}
