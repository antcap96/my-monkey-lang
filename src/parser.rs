use crate::{
    ast::{Identifier, Statement},
    lexer::Token,
    token_extensions::HasInfixOperation,
    token_extensions::HasPrecedence,
    token_extensions::HasPrefixOperation,
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
    PrematureEndOfInput { expected: Expected },
    UnexpectedToken { expected: Expected, got: Token },
    ParseIntError(std::num::ParseIntError),
    NoPrefixFunction(Token),
}

impl ParseError {
    pub fn premature_end_expected_expression() -> Self {
        ParseError::PrematureEndOfInput {
            expected: Expected::Expression,
        }
    }
}

#[derive(Debug)]
pub enum Expected {
    Token(Token),
    Identifier,
    Expression,
}

impl From<std::num::ParseIntError> for ParseError {
    fn from(value: std::num::ParseIntError) -> Self {
        ParseError::ParseIntError(value)
    }
}

impl ParseError {
    pub fn unexpected_token(expected: Token, got: Option<Token>) -> ParseError {
        match got {
            Some(got) => ParseError::UnexpectedToken {
                expected: Expected::Token(expected),
                got,
            },
            None => ParseError::PrematureEndOfInput {
                expected: Expected::Token(expected),
            },
        }
    }

    pub fn unexpected_other(expected: Expected, got: Option<Token>) -> ParseError {
        match got {
            Some(got) => ParseError::UnexpectedToken { expected, got },
            None => ParseError::PrematureEndOfInput { expected },
        }
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

        // XXX: would some like this be possible? `for token in self.iter.by_ref() {`
        while let Some(token) = self.iter.next() {
            let mut maybe_statement = self.parse_statement(token);
            match self.iter.peek() {
                Some(Token::SemiColon) => {
                    self.iter.next();
                }
                None => {}
                Some(_) => {
                    maybe_statement = Err(ParseError::unexpected_other(
                        Expected::Token(Token::SemiColon),
                        self.iter.next(),
                    ))
                }
            }
            match maybe_statement {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(err) => {
                    errors.push(err);
                    for token in self.iter.by_ref() {
                        if token == Token::SemiColon {
                            break;
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

    pub fn parse_statement(&mut self, token: Token) -> Result<crate::ast::Statement, ParseError> {
        match token {
            Token::Let => Ok(Statement::Let(self.parse_let_statement()?)),
            Token::Return => Ok(Statement::Return(self.parse_return_statement()?)),
            _ => Ok(Statement::Expression(
                self.parse_expression_statement(token)?,
            )),
        }
    }

    fn parse_let_statement(&mut self) -> Result<crate::ast::LetStatement, ParseError> {
        let next = self.iter.next();
        let Some(Token::Ident(name)) = next else {
            return Err(ParseError::unexpected_other(
                Expected::Identifier,
                next,
            ))
        };

        let next = self.iter.next();
        let Some(Token::Assign) = next else {
            return Err(ParseError::unexpected_token(Token::Assign, next))
        };

        let next = self
            .iter
            .next()
            .ok_or(ParseError::premature_end_expected_expression())?;
        let value = self.parse_expression(Precedence::Lowest, next)?;

        Ok(crate::ast::LetStatement {
            identifier: Identifier { name },
            value,
        })
    }

    fn parse_return_statement(&mut self) -> Result<crate::ast::ReturnStatement, ParseError> {
        let next = self
            .iter
            .next()
            .ok_or(ParseError::premature_end_expected_expression())?;
        let value = self.parse_expression(Precedence::Lowest, next)?;

        Ok(crate::ast::ReturnStatement { value })
    }
    fn parse_expression_statement(
        &mut self,
        token: Token,
    ) -> Result<crate::ast::Expression, ParseError> {
        self.parse_expression(Precedence::Lowest, token)
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
    fn test_expression_precedence() {
        let tests = vec![
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4);\n"),
            ("(5 + 5) * 2", "((5 + 5) * 2);\n"),
            ("2 / (5 + 5)", "(2 / (5 + 5));\n"),
            ("-(5 + 5)", "(-(5 + 5));\n"),
            ("!(true == true)", "(!(true == true));\n"),
        ];
        for (input, expected) in tests {
            let tokenizer = crate::lexer::Tokenizer::new(input);
            let mut parser = crate::parser::Parser::new(tokenizer);

            let program = parser.parse_program().unwrap();

            assert_eq!(program.to_string(), expected)
        }
    }

    #[test]
    fn test_call_expression() {
        let tests = vec![
            ("a + add(b * c) + d", "((a + add((b * c))) + d);\n"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)));\n",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g));\n",
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
    fn test_conditional() {
        let tests = vec![
            ("if (x < y) { x }", "if (x < y) {\n  x;\n};\n"),
            (
                "if (x < y) { x } else { y }",
                "if (x < y) {\n  x;\n} else {\n  y;\n};\n",
            ),
        ];

        for (input, expected) in tests {
            let tokenizer = crate::lexer::Tokenizer::new(input);
            let mut parser = crate::parser::Parser::new(tokenizer);

            let program = parser.parse_program().unwrap();

            assert_eq!(program.to_string(), expected)
        }
    }
}
