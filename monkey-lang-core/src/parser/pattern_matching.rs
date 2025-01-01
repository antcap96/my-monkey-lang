use super::error;
use super::error::ParseError;
use crate::ast::Pattern;
use crate::lexer::{Token, TokenKind};
use crate::parser::Parser;

pub fn parse_pattern(parser: &mut Parser) -> Result<Pattern, ParseError> {
    let Some(token) = parser.iter.next() else {
        return Err(ParseError::PrematureEndOfInput {
            expected: error::Expected::Pattern,
        });
    };
    match token.kind {
        TokenKind::Ident(name) => Ok(Pattern::Identifier(crate::ast::Identifier { name })),
        TokenKind::Int(val) => Ok(Pattern::IntegerLiteral(val.parse()?)),
        TokenKind::String(val) => Ok(Pattern::StringLiteral(val.trim_matches('\"').to_owned())),
        TokenKind::True => Ok(Pattern::BooleanLiteral(true)),
        TokenKind::False => Ok(Pattern::BooleanLiteral(false)),
        TokenKind::Null => Ok(Pattern::NullLiteral),
        TokenKind::LBracket => Ok(Pattern::ArrayPattern(parse_array_pattern(parser)?)),
        TokenKind::LBrace => Ok(Pattern::HashPattern(parse_hash_pattern(parser)?)),
        _ => Err(ParseError::InvalidPattern(token)),
    }
}

pub fn parse_array_pattern(parser: &mut Parser) -> Result<crate::ast::ArrayPattern, ParseError> {
    let mut contents = Vec::new();
    let mut remainder = None;

    loop {
        match parser.iter.peek().map(|t| &t.kind) {
            Some(TokenKind::RBracket) => {
                parser.iter.next();
                break;
            }
            Some(TokenKind::Ellipsis) => {
                parser.iter.next();
                let name = parser.parse_ident()?;
                remainder = Some(crate::ast::Identifier { name });

                parser.expect_token(TokenKind::RBracket)?;
                break;
            }
            _ => contents.push(parse_pattern(parser)?),
        }

        match parser.iter.next() {
            Some(Token {
                kind: TokenKind::Comma,
                ..
            }) => continue,
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

pub fn parse_hash_pattern(parser: &mut Parser) -> Result<crate::ast::HashPattern, ParseError> {
    let mut contents = Vec::new();
    let mut remainder = None;

    loop {
        match parser.iter.peek() {
            Some(Token {
                kind: TokenKind::RBrace,
                ..
            }) => {
                parser.iter.next();
                break;
            }
            Some(Token {
                kind: TokenKind::Ellipsis,
                ..
            }) => {
                parser.iter.next();
                let name = parser.parse_ident()?;
                remainder = Some(crate::ast::Identifier { name });
                parser.expect_token(TokenKind::RBrace)?;
                break;
            }
            Some(_) => {
                let next = parser.iter.next().expect("peeked");
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
                parser.expect_token(TokenKind::Colon)?;

                //3. parse value
                let value = parse_pattern(parser)?;
                contents.push((key, value));
            }
            None => {
                return Err(ParseError::PrematureEndOfInput {
                    expected: error::Expected::Pattern,
                })
            }
        }
        // TODO: validate pattern:
        // - no duplicate keys
        // - no duplicate identifiers
        match parser.iter.next() {
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
