use crate::lexer::{Token, TokenKind};

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
