use crate::{
    ast::{Identifier, Pattern, Statement},
    expression_parsing::Precedence,
    lexer::{Token, TokenKind},
};

#[derive(Debug)]
pub enum ParseError {
    PrematureEndOfInput { expected: Expected },
    UnexpectedToken { expected: Expected, got: Token },
    ParseIntError(std::num::ParseIntError),
    NoPrefixFunction(Token),
    InvalidPattern(Token), // TODO: should have more information about the error
    InvalidLiteral(Token),
}

#[derive(Debug)]
pub enum Expected {
    Token(TokenKind),
    Identifier,
    Expression,
    Pattern,
}

impl From<std::num::ParseIntError> for ParseError {
    fn from(value: std::num::ParseIntError) -> Self {
        ParseError::ParseIntError(value)
    }
}

impl ParseError {
    pub fn premature_end_expected_expression() -> Self {
        ParseError::PrematureEndOfInput {
            expected: Expected::Expression,
        }
    }

    pub fn unexpected_token(expected: TokenKind, got: Option<Token>) -> ParseError {
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
            match self.parse_statement(token) {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(err) => {
                    errors.push(err);
                }
            }
            // Clear until the next semicolon, giving an error if there are
            // other tokens in between
            match self.iter.peek() {
                Some(Token {
                    kind: TokenKind::SemiColon,
                    ..
                }) => {
                    self.iter.next();
                }
                None => {}
                Some(token) => {
                    errors.push(ParseError::UnexpectedToken {
                        expected: Expected::Token(TokenKind::SemiColon),
                        got: token.clone(),
                    });
                    for token in self.iter.by_ref() {
                        if token.kind == TokenKind::SemiColon {
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
        match token.kind {
            TokenKind::Let => Ok(Statement::Let(self.parse_let_statement()?)),
            TokenKind::Return => Ok(Statement::Return(self.parse_return_statement()?)),
            _ => Ok(Statement::Expression(
                self.parse_expression_statement(token)?,
            )),
        }
    }

    fn parse_let_statement(&mut self) -> Result<crate::ast::LetStatement, ParseError> {
        let next = self.iter.next();
        let Some(Token {
            kind: TokenKind::Ident(name),
            ..
        }) = next
        else {
            return Err(ParseError::unexpected_other(Expected::Identifier, next));
        };

        let next = self.iter.next();
        let Some(Token {
            kind: TokenKind::Assign,
            ..
        }) = next
        else {
            return Err(ParseError::unexpected_token(TokenKind::Assign, next));
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
        token: Token, // consider taking an Option<Token> instead, and returning an error if it's None
    ) -> Result<crate::ast::Expression, ParseError> {
        let mut left_expression = crate::expression_parsing::prefix_parsing(token, self)?;

        loop {
            let Some(next_token) = self.iter.peek() else {
                break;
            };

            //TODO: do i need statement_ended? SemiColon would have Precedence::Lowest
            let statement_ended = next_token.kind == TokenKind::SemiColon;
            let next_precedence = crate::expression_parsing::precedence(&next_token.kind);
            if statement_ended || precedence >= next_precedence {
                break;
            }

            let Some(next_token) = self.iter.next() else {
                break;
            };
            let Some(infix_parse_function) =
                crate::expression_parsing::infix_parsing_function(next_token.kind)
            else {
                break;
            };
            left_expression = infix_parse_function(left_expression, self)?;
        }

        Ok(left_expression)
    }

    pub fn parse_pattern(&mut self, token: Token) -> Result<Pattern, ParseError> {
        match token.kind {
            TokenKind::Ident(ident) => {
                Ok(Pattern::Identifier(crate::ast::Identifier { name: ident }))
            }
            TokenKind::Int(val) => Ok(Pattern::IntegerLiteral(val.parse()?)),
            TokenKind::String(val) => Ok(Pattern::StringLiteral(val.trim_matches('\"').to_owned())),
            TokenKind::True => Ok(Pattern::BooleanLiteral(true)),
            TokenKind::False => Ok(Pattern::BooleanLiteral(false)),
            TokenKind::Null => Ok(Pattern::NullLiteral),
            TokenKind::LBracket => Ok(Pattern::ArrayPattern(self.parse_array_pattern()?)),
            TokenKind::LBrace => Ok(Pattern::HashPattern(self.parse_hash_pattern()?)),
            _ => Err(ParseError::InvalidPattern(token)),
        }
    }

    pub fn parse_array_pattern(&mut self) -> Result<crate::ast::ArrayPattern, ParseError> {
        let mut contents = Vec::new();
        let mut remainder = None;

        loop {
            match self.iter.next() {
                Some(Token {
                    kind: TokenKind::RBracket,
                    ..
                }) => break,
                Some(Token {
                    kind: TokenKind::Ellipsis,
                    ..
                }) => {
                    let next = self.iter.next();
                    let Some(Token {
                        kind: TokenKind::Ident(ident),
                        ..
                    }) = next
                    else {
                        return Err(ParseError::unexpected_other(Expected::Identifier, next));
                    };
                    remainder = Some(crate::ast::Identifier { name: ident });

                    let next = self.iter.next();
                    let Some(Token {
                        kind: TokenKind::RBracket,
                        ..
                    }) = next
                    else {
                        return Err(ParseError::unexpected_token(TokenKind::RBracket, next));
                    };
                    break;
                }
                Some(next) => {
                    contents.push(self.parse_pattern(next)?);
                }
                None => {
                    return Err(ParseError::PrematureEndOfInput {
                        expected: Expected::Pattern,
                    })
                }
            }

            match self.iter.next() {
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) => {}
                Some(Token {
                    kind: TokenKind::RBracket,
                    ..
                }) => break,
                next => return Err(ParseError::unexpected_token(TokenKind::RBracket, next)),
            }
        }

        // TODO: validate pattern:
        // - no duplicate identifiers
        Ok(crate::ast::ArrayPattern {
            contents,
            remainder,
        })
    }

    pub fn parse_hash_pattern(&mut self) -> Result<crate::ast::HashPattern, ParseError> {
        let mut contents = Vec::new();
        let mut remainder = None;

        loop {
            match self.iter.next() {
                Some(Token {
                    kind: TokenKind::RBrace,
                    ..
                }) => break,
                Some(Token {
                    kind: TokenKind::Ellipsis,
                    ..
                }) => {
                    let next = self.iter.next();
                    let Some(Token {
                        kind: TokenKind::Ident(ident),
                        ..
                    }) = next
                    else {
                        return Err(ParseError::unexpected_other(Expected::Identifier, next));
                    };
                    remainder = Some(crate::ast::Identifier { name: ident });

                    let next = self.iter.next();
                    let Some(Token {
                        kind: TokenKind::RBrace,
                        ..
                    }) = next
                    else {
                        return Err(ParseError::unexpected_token(TokenKind::RBrace, next));
                    };
                    break;
                }
                Some(next) => {
                    //1. parse key literal
                    let key = match next.kind {
                        TokenKind::Int(val) => crate::ast::HashKey::Integer(val.parse()?),
                        TokenKind::String(val) => {
                            crate::ast::HashKey::String(val.trim_matches('\"').to_owned())
                        }
                        TokenKind::True => crate::ast::HashKey::Boolean(true),
                        TokenKind::False => crate::ast::HashKey::Boolean(false),
                        _ => return Err(ParseError::InvalidLiteral(next)),
                    };

                    //2. consume colon
                    let next = self.iter.next();
                    let Some(Token {
                        kind: TokenKind::Colon,
                        ..
                    }) = next
                    else {
                        return Err(ParseError::unexpected_token(TokenKind::Colon, next));
                    };

                    //3. parse value
                    let maybe_next = self.iter.next();
                    let Some(next) = maybe_next else {
                        return Err(ParseError::PrematureEndOfInput {
                            expected: Expected::Pattern,
                        });
                    };
                    let value = self.parse_pattern(next)?;
                    contents.push((key, value));
                }
                None => {
                    return Err(ParseError::PrematureEndOfInput {
                        expected: Expected::Pattern,
                    })
                }
            }
            // TODO: validate pattern:
            // - no duplicate keys
            // - no duplicate identifiers
            match self.iter.next() {
                Some(Token {
                    kind: TokenKind::Comma,
                    ..
                }) => {}
                Some(Token {
                    kind: TokenKind::RBrace,
                    ..
                }) => break,
                next => return Err(ParseError::unexpected_token(TokenKind::RBrace, next)),
            }
        }

        Ok(crate::ast::HashPattern {
            contents,
            remainder,
        })
    }
}

#[cfg(test)]
mod tests {
    fn test_parsing(tests: Vec<(&str, &str)>) {
        for (input, expected) in tests {
            let tokenizer = crate::lexer::Tokenizer::new(input);
            let mut parser = crate::parser::Parser::new(tokenizer);

            let program = parser.parse_program().unwrap();

            assert_eq!(program.to_string(), expected)
        }
    }

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

        test_parsing(tests)
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

        test_parsing(tests)
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

        test_parsing(tests)
    }

    #[test]
    fn test_function() {
        let tests = vec![
            (
                "let getName = fn(person) { person[\"name\"]; };",
                "let getName = fn(person) {\n  (person[\"name\"]);\n};\n",
            ),
            (
                "let getName = fn(person) { person[\"name\"] };",
                "let getName = fn(person) {\n  (person[\"name\"]);\n};\n",
            ),
        ];

        test_parsing(tests)
    }
}
