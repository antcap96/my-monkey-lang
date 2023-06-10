use crate::{
    ast::{Identifier, Statement},
    lexer::Token,
    token_extensions::HasInfixOperation,
    token_extensions::HasPrecedence,
    token_extensions::HasPrefixOperation
};

#[derive(PartialOrd, PartialEq, Debug)]
pub enum Precedence {
    Lowest = 0,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[derive(Debug)]
pub enum ParseError {
    PrematureEndOfInput,
    UnexpectedToken(Token),
    ParseIntError(std::num::ParseIntError),
    NoPrefixParseError(Token),
}

impl From<std::num::ParseIntError> for ParseError {
    fn from(value: std::num::ParseIntError) -> Self {
        ParseError::ParseIntError(value)
    }
}

pub struct Parser<'a> {
    pub iter: std::iter::Peekable<crate::lexer::Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: crate::lexer::Tokenizer<'a>) -> Self {
        let iter = tokenizer.peekable();
        Self { iter }
    }

    pub fn parse_program(&mut self) -> Result<crate::ast::Program, Vec<ParseError>> {
        let mut statements = Vec::new();

        let mut errors = Vec::new();

        while self.iter.peek().is_some() {
            let maybe_statement = self.parse_statement();
            match maybe_statement {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(err) => {
                    errors.push(err);
                    for token in self.iter.by_ref() {
                        match token {
                            Token::SemiColon => break,
                            _ => {}
                        }
                    }
                }
            }
        }
        if errors.is_empty() {
            Ok(crate::ast::Program { statements })
        } else {
            Err(errors)
        }
    }

    fn parse_statement(&mut self) -> Result<crate::ast::Statement, ParseError> {
        let first_token = self.iter.next().ok_or(ParseError::PrematureEndOfInput)?;
        match first_token {
            Token::Let => Ok(Statement::Let(self.parse_let_statement()?)),
            Token::Return => Ok(Statement::Return(self.parse_return_statement()?)),
            _ => Ok(Statement::Expression(
                self.parse_expression_statement(first_token)?,
            )),
        }
    }

    fn parse_let_statement(&mut self) -> Result<crate::ast::LetStatement, ParseError> {
        let ident = self.iter.next().ok_or(ParseError::PrematureEndOfInput)?;

        let name = match ident {
            Token::Ident(name) => name,
            _ => return Err(ParseError::UnexpectedToken(ident)),
        };

        // TODO: Parse the expression
        for token in self.iter.by_ref() {
            match token {
                Token::SemiColon => break,
                _ => {}
            }
        }
        Ok(crate::ast::LetStatement {
            name: Identifier { name },
            value: crate::ast::Expression::NotYetImplemented,
        })
    }

    fn parse_return_statement(&mut self) -> Result<crate::ast::ReturnStatement, ParseError> {
        // TODO: Parse the expression
        for token in self.iter.by_ref() {
            match token {
                Token::SemiColon => break,
                _ => {}
            }
        }
        Ok(crate::ast::ReturnStatement {
            value: crate::ast::Expression::NotYetImplemented,
        })
    }
    fn parse_expression_statement(
        &mut self,
        token: Token,
    ) -> Result<crate::ast::Expression, ParseError> {
        let expression = self.parse_expression(Precedence::Lowest, token)?;

        match self.iter.next() {
            Some(Token::SemiColon) | None => Ok(expression),
            Some(t) => Err(ParseError::UnexpectedToken(t)),
        }
    }

    pub fn parse_expression(
        &mut self,
        precedence: Precedence,
        token: Token,
    ) -> Result<crate::ast::Expression, ParseError> {
        let mut left_expression = token.prefix_parsing(self)?;

        loop {
            //TODO: do i need statement_continues? SemiColon would have Precedence::Lowest
            let statement_ended = self
                .iter
                .peek()
                .map(|token| *token == Token::SemiColon)
                .unwrap_or(true);
            let next_precedence = self
                .iter
                .peek()
                .map(|token| token.precedence())
                .unwrap_or(Precedence::Lowest);
            if statement_ended || precedence >= next_precedence {
                break;
            }

            let next_token = self.iter.peek();
            let infix_parse_function = next_token.and_then(|token| token.infix_parsing_function());

            left_expression = match infix_parse_function {
                None => break,
                Some(parse_function) => {
                    self.iter.next();
                    parse_function(left_expression, self)?
                }
            };
        }

        Ok(left_expression)
    }
}


#[cfg(test)]
mod tests {
    #[test]
    fn test_expression_1() {
        let tests = vec![
            ("-a * b", "((-a) * b);\n"),
            ("!-a", "(!(-a));\n"),
            ("a + b + c", "((a + b) + c);\n"),
            ("a + b - c", "((a + b) - c);\n"),
            ("a * b * c", "((a * b) * c);\n"),
            ("a * b / c", "((a * b) / c);\n"),
            ("a + b / c", "(a + (b / c));\n"),
            (
                "a + b * c + d / e - f",
                "(((a + (b * c)) + (d / e)) - f);\n",
            ),
            ("3 + 4; -5 * 5", "(3 + 4);\n((-5) * 5);\n"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4));\n"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4));\n"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));\n",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));\n",
            ),
        ];

        for (input, expected) in tests {
            let tokenizer = crate::lexer::Tokenizer::new(input);
            let mut parser = crate::parser::Parser::new(tokenizer);

            let program = parser.parse_program().unwrap();

            assert_eq!(program.to_string(), expected)
        }
    }

    #[test]
    fn test_let_statement() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;";

        let tokenizer = crate::lexer::Tokenizer::new(input);
        let mut parser = crate::parser::Parser::new(tokenizer);

        let program = parser.parse_program().unwrap();

        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got={}",
                program.statements.len()
            );
        }

        for (name, statement) in ["x", "y", "foobar"]
            .iter()
            .zip(program.statements.into_iter())
        {
            test_let_statement_(statement, name);
        }
    }

    #[allow(dead_code)]
    fn test_let_statement_(s: crate::ast::Statement, name: &str) {
        if let crate::ast::Statement::Let(let_statement) = s {
            assert_eq!(let_statement.name.name, name);
        } else {
            panic!("s is not LET. got={:?}", s);
        }
    }
}
