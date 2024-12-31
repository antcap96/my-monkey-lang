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

    pub(crate) fn parse_ident(&mut self) -> Result<std::rc::Rc<str>, ParseError> {
        let token = self.iter.next();
        match token {
            Some(Token {
                kind: TokenKind::Ident(name),
                ..
            }) => Ok(name),
            _ => Err(ParseError::unexpected_other(Expected::Identifier, token)),
        }
    }

    pub(crate) fn expect_token(&mut self, token_kind: TokenKind) -> Result<(), ParseError> {
        let token = self.iter.next();
        match token {
            Some(Token { kind, .. }) if kind == token_kind => Ok(()),
            _ => Err(ParseError::unexpected_token(token_kind, token)),
        }
    }

    pub fn parse_program(&mut self) -> Result<crate::ast::Program, Vec<ParseError>> {
        let mut statements = Vec::new();

        let mut errors = Vec::new();

        // XXX: would some like this be possible? `for token in self.iter.by_ref() {`
        while self.iter.peek().is_some() {
            match self.parse_statement() {
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

    pub fn parse_statement(&mut self) -> Result<crate::ast::Statement, ParseError> {
        let token = self.iter.peek();
        match token.map(|t| &t.kind) {
            Some(TokenKind::Let) => Ok(Statement::Let(self.parse_let_statement()?)),
            Some(TokenKind::Return) => Ok(Statement::Return(self.parse_return_statement()?)),
            _ => Ok(Statement::Expression(self.parse_expression_statement()?)),
        }
    }

    fn parse_let_statement(&mut self) -> Result<crate::ast::LetStatement, ParseError> {
        self.expect_token(TokenKind::Let)?;
        let name = self.parse_ident()?;
        self.expect_token(TokenKind::Assign)?;
        let value = self.parse_expression(Precedence::Lowest)?;

        Ok(crate::ast::LetStatement {
            identifier: Identifier { name },
            value,
        })
    }

    fn parse_return_statement(&mut self) -> Result<crate::ast::ReturnStatement, ParseError> {
        self.expect_token(TokenKind::Return)?;
        let value = self.parse_expression(Precedence::Lowest)?;

        Ok(crate::ast::ReturnStatement { value })
    }
    fn parse_expression_statement(&mut self) -> Result<crate::ast::Expression, ParseError> {
        self.parse_expression(Precedence::Lowest)
    }

    pub fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<crate::ast::Expression, ParseError> {
        let Some(token) = self.iter.next() else {
            return Err(ParseError::premature_end_expected_expression());
        };
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

    pub fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let Some(token) = self.iter.next() else {
            return Err(ParseError::PrematureEndOfInput {
                expected: Expected::Pattern,
            });
        };
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
            match self.iter.peek().map(|t| &t.kind) {
                Some(TokenKind::RBracket) => {
                    self.iter.next();
                    break;
                }
                Some(TokenKind::Ellipsis) => {
                    self.iter.next();
                    let name = self.parse_ident()?;
                    remainder = Some(crate::ast::Identifier { name });

                    self.expect_token(TokenKind::RBracket)?;
                    break;
                }
                _ => contents.push(self.parse_pattern()?),
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
            match self.iter.peek() {
                Some(Token {
                    kind: TokenKind::RBrace,
                    ..
                }) => {
                    self.iter.next();
                    break;
                }
                Some(Token {
                    kind: TokenKind::Ellipsis,
                    ..
                }) => {
                    self.iter.next();
                    let name = self.parse_ident()?;
                    remainder = Some(crate::ast::Identifier { name });
                    self.expect_token(TokenKind::RBrace)?;
                    break;
                }
                Some(_) => {
                    let next = self.iter.next().expect("peeked");
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
                    self.expect_token(TokenKind::Colon)?;

                    //3. parse value
                    let value = self.parse_pattern()?;
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
            ("if (x < y) { x }", "if (x < y) {x;};\n"),
            (
                "if (x < y) { x } else { y }",
                "if (x < y) {x;} else {y;};\n",
            ),
        ];

        test_parsing(tests)
    }

    #[test]
    fn test_function() {
        let tests = vec![
            (
                "let getName = fn(person) { person[\"name\"]; };",
                "let getName = fn(person) {(person[\"name\"]);};\n",
            ),
            (
                "let getName = fn(person) { person[\"name\"] };",
                "let getName = fn(person) {(person[\"name\"]);};\n",
            ),
        ];

        test_parsing(tests)
    }

    #[test]
    fn test_match_expression() {
        let tests = vec![
            ("match 1 {};\n", "match 1 {};\n"),
            ("match 1 {1 => {1}};", "match 1 {1 => {1;}};\n"),
            (
                "match 1 {1 => {1}, 2 => {2}};",
                "match 1 {1 => {1;}, 2 => {2;}};\n",
            ),
            (
                "match 1 {1 => {1}, 2 => {2},};",
                "match 1 {1 => {1;}, 2 => {2;}};\n",
            ),
        ];

        test_parsing(tests)
    }
}
