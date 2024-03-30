use crate::ast::{BlockStatement, Expression};
use crate::lexer::{Token, TokenKind};
use crate::parser::{Expected, ParseError, Parser};

#[derive(PartialOrd, PartialEq, Debug)]
pub enum Precedence {
    Lowest = 0,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

pub fn precedence(token: &TokenKind) -> Precedence {
    match token {
        TokenKind::Equal => Precedence::Equals,
        TokenKind::NotEqual => Precedence::Equals,
        TokenKind::LessThan => Precedence::LessGreater,
        TokenKind::GreaterThan => Precedence::LessGreater,
        TokenKind::Plus => Precedence::Sum,
        TokenKind::Minus => Precedence::Sum,
        TokenKind::Asterisk => Precedence::Product,
        TokenKind::Slash => Precedence::Product,
        TokenKind::LParen => Precedence::Call,
        TokenKind::LBracket => Precedence::Index,
        _ => Precedence::Lowest,
    }
}

fn prefix_operation(
    kind: crate::ast::PrefixOperationKind,
) -> impl FnOnce(&mut Parser) -> Result<Expression, ParseError> {
    move |parser| {
        let next_token = parser
            .iter
            .next()
            .ok_or(ParseError::premature_end_expected_expression())?;
        Ok(Expression::PrefixOperation(
            kind,
            Box::new(parser.parse_expression(Precedence::Prefix, next_token)?),
        ))
    }
}

fn parse_grouped_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
    let next_token = parser
        .iter
        .next()
        .ok_or(ParseError::premature_end_expected_expression())?;

    let expression = parser.parse_expression(Precedence::Lowest, next_token)?;

    match parser.iter.next() {
        Some(Token {
            kind: TokenKind::RParen,
            ..
        }) => Ok(expression),
        next => Err(ParseError::unexpected_token(TokenKind::RParen, next)),
    }
}

fn parse_expression_list(
    parser: &mut Parser,
    terminator: TokenKind,
) -> Result<Vec<Expression>, ParseError> {
    let mut elements = Vec::new();

    loop {
        match parser.iter.next() {
            Some(next) if next.kind == terminator => return Ok(elements),
            Some(next) => {
                elements.push(parser.parse_expression(Precedence::Lowest, next)?);
            }
            None => return Err(ParseError::premature_end_expected_expression()),
        }

        match parser.iter.next() {
            Some(Token {
                kind: TokenKind::Comma,
                ..
            }) => {}
            Some(next) if next.kind == terminator => return Ok(elements),
            next => return Err(ParseError::unexpected_token(terminator, next)),
        }
    }
}

fn parse_array_literal(parser: &mut Parser) -> Result<Expression, ParseError> {
    let expressions = parse_expression_list(parser, TokenKind::RBracket)?;
    Ok(Expression::ArrayLiteral(expressions))
}

fn parse_hash_literal(parser: &mut Parser) -> Result<Expression, ParseError> {
    let mut pairs = Vec::new();

    loop {
        let next = parser.iter.next();
        let key = match next {
            Some(Token {
                kind: TokenKind::RBrace,
                ..
            }) => return Ok(Expression::HashLiteral(pairs)),
            None => {
                return Err(ParseError::PrematureEndOfInput {
                    expected: Expected::Token(TokenKind::RBrace),
                })
            }
            Some(token) => parser.parse_expression(Precedence::Lowest, token)?,
        };

        let next = parser.iter.next();
        let Some(Token {
            kind: TokenKind::Colon,
            ..
        }) = next
        else {
            return Err(ParseError::unexpected_token(TokenKind::Colon, next));
        };

        let Some(next) = parser.iter.next() else {
            return Err(ParseError::premature_end_expected_expression());
        };
        let value = parser.parse_expression(Precedence::Lowest, next)?;

        pairs.push((key, value));

        let next = parser.iter.next();
        match next {
            Some(Token {
                kind: TokenKind::Comma,
                ..
            }) => {}
            Some(Token {
                kind: TokenKind::RBrace,
                ..
            }) => return Ok(Expression::HashLiteral(pairs)),
            next => return Err(ParseError::unexpected_token(TokenKind::RBrace, next)),
        }
    }
}

fn parse_if_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
    let token = parser
        .iter
        .next()
        .ok_or(ParseError::premature_end_expected_expression())?;
    let condition = Box::new(parser.parse_expression(Precedence::Lowest, token)?);
    let mut alternative = None;

    let next = parser.iter.next();
    let Some(Token {
        kind: TokenKind::LBrace,
        ..
    }) = next
    else {
        return Err(ParseError::unexpected_token(TokenKind::LBrace, next));
    };

    let consequence = parse_block_statement(parser)?;

    if parser
        .iter
        .next_if(|token| token.kind == TokenKind::Else)
        .is_some()
    {
        let next = parser.iter.next();
        let Some(Token {
            kind: TokenKind::LBrace,
            ..
        }) = next
        else {
            return Err(ParseError::unexpected_token(TokenKind::LBrace, next));
        };
        alternative = Some(parse_block_statement(parser)?);
    }

    Ok(Expression::IfExpression {
        condition,
        consequence,
        alternative,
    })
}

fn parse_block_statement(parser: &mut Parser) -> Result<BlockStatement, ParseError> {
    let mut statements = Vec::new();

    loop {
        let next = parser.iter.next();
        match next {
            Some(Token {
                kind: TokenKind::RBrace,
                ..
            }) => return Ok(BlockStatement { statements }),
            None => {
                return Err(ParseError::PrematureEndOfInput {
                    expected: Expected::Token(TokenKind::RBrace),
                })
            }
            Some(token) => {
                let statement = parser.parse_statement(token)?;
                statements.push(statement);
            }
        }
        let next = parser.iter.next();
        match next {
            Some(Token {
                kind: TokenKind::SemiColon,
                ..
            }) => continue,
            Some(Token {
                kind: TokenKind::RBrace,
                ..
            }) => return Ok(BlockStatement { statements }),
            _ => return Err(ParseError::unexpected_token(TokenKind::RBrace, next)),
        }
    }
}

fn parse_function_literal(parser: &mut Parser) -> Result<Expression, ParseError> {
    let next = parser.iter.next();
    let Some(Token {
        kind: TokenKind::LParen,
        ..
    }) = next
    else {
        return Err(ParseError::unexpected_token(TokenKind::LParen, next));
    };

    let parameters = parse_parameters(parser)?;

    let next = parser.iter.next();
    let Some(Token {
        kind: TokenKind::LBrace,
        ..
    }) = next
    else {
        return Err(ParseError::unexpected_token(TokenKind::LBrace, next));
    };

    let body = parse_block_statement(parser)?;

    Ok(Expression::FunctionLiteral { parameters, body })
}

fn parse_parameters(parser: &mut Parser) -> Result<Vec<crate::ast::Identifier>, ParseError> {
    let mut identifiers = Vec::new();

    loop {
        let next = parser.iter.next();
        match next {
            Some(Token {
                kind: TokenKind::Ident(name),
                ..
            }) => identifiers.push(crate::ast::Identifier { name }),
            Some(Token {
                kind: TokenKind::RParen,
                ..
            }) => return Ok(identifiers), // empty parameter list or tailing comma
            _ => Err(ParseError::unexpected_token(TokenKind::RParen, next))?,
        }

        let next = parser.iter.next();
        match next {
            Some(Token {
                kind: TokenKind::Comma,
                ..
            }) => continue,
            Some(Token {
                kind: TokenKind::RParen,
                ..
            }) => return Ok(identifiers),
            _ => Err(ParseError::unexpected_token(TokenKind::RParen, next))?,
        }
    }
}

fn parse_match_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
    let Some(token) = parser.iter.next() else {
        return Err(ParseError::premature_end_expected_expression());
    };
    let expression = Box::new(parser.parse_expression(Precedence::Lowest, token)?);

    let next = parser.iter.next();
    let Some(Token {
        kind: TokenKind::LBrace,
        ..
    }) = next
    else {
        return Err(ParseError::unexpected_token(TokenKind::LBrace, next));
    };

    let mut cases = Vec::new();

    loop {
        let next = parser.iter.next();
        match next {
            Some(Token {
                kind: TokenKind::RBrace,
                ..
            }) => return Ok(Expression::MatchExpression { expression, cases }),
            None => {
                return Err(ParseError::PrematureEndOfInput {
                    expected: Expected::Token(TokenKind::RBrace),
                })
            }
            Some(token) => {
                let case = parse_match_case(parser, token)?;
                cases.push(case);
            }
        }
    }
}

fn parse_match_case(
    parser: &mut Parser,
    token: Token,
) -> Result<crate::ast::MatchCase, ParseError> {
    let pattern = parser.parse_pattern(token)?;

    let next = parser.iter.next();
    let Some(Token {
        kind: TokenKind::FatArrow,
        ..
    }) = next
    else {
        return Err(ParseError::unexpected_token(TokenKind::FatArrow, next));
    };

    let next = parser.iter.next();
    let Some(Token {
        kind: TokenKind::LBrace,
        ..
    }) = next
    else {
        return Err(ParseError::unexpected_token(TokenKind::LBrace, next));
    };

    let body = parse_block_statement(parser)?;

    Ok(crate::ast::MatchCase { pattern, body })
}

pub fn prefix_parsing(token: Token, parser: &mut Parser) -> Result<Expression, ParseError> {
    match token.kind {
        TokenKind::Ident(name) => Ok(Expression::Identifier(crate::ast::Identifier { name })),
        TokenKind::Int(val) => Ok(Expression::IntegerLiteral(val.parse()?)),
        TokenKind::String(val) => Ok(Expression::StringLiteral(val.trim_matches('\"').to_owned())),
        TokenKind::True => Ok(Expression::BooleanLiteral(true)),
        TokenKind::False => Ok(Expression::BooleanLiteral(false)),
        TokenKind::Null => Ok(Expression::NullLiteral),
        TokenKind::Bang => prefix_operation(crate::ast::PrefixOperationKind::Bang)(parser),
        TokenKind::Minus => prefix_operation(crate::ast::PrefixOperationKind::Minus)(parser),
        TokenKind::LParen => parse_grouped_expression(parser),
        TokenKind::LBracket => parse_array_literal(parser),
        TokenKind::LBrace => parse_hash_literal(parser),
        TokenKind::If => parse_if_expression(parser),
        TokenKind::Function => parse_function_literal(parser),
        TokenKind::Match => parse_match_expression(parser),
        _ => Err(ParseError::NoPrefixFunction(token)),
    }
}

type InfixFunction = Box<dyn FnOnce(Expression, &mut Parser) -> Result<Expression, ParseError>>;

fn infix_operation(token: TokenKind, kind: crate::ast::InfixOperationKind) -> InfixFunction {
    Box::new(
        move |left: Expression, parser: &mut Parser| -> Result<Expression, ParseError> {
            let new_precedence = precedence(&token);

            let new_token = parser
                .iter
                .next()
                .ok_or(ParseError::premature_end_expected_expression())?;
            Ok(Expression::InfixOperation(
                kind,
                Box::new(left),
                Box::new(parser.parse_expression(new_precedence, new_token)?),
            ))
        },
    )
}

fn parse_call_function(left: Expression, parser: &mut Parser) -> Result<Expression, ParseError> {
    let arguments = parse_expression_list(parser, TokenKind::RParen)?;

    Ok(Expression::CallExpression {
        function: Box::new(left),
        arguments,
    })
}

fn parse_index_expression(left: Expression, parser: &mut Parser) -> Result<Expression, ParseError> {
    let next = parser
        .iter
        .next()
        .ok_or(ParseError::premature_end_expected_expression())?;
    let index = parser.parse_expression(Precedence::Lowest, next)?;

    let next = parser.iter.next();
    let Some(Token {
        kind: TokenKind::RBracket,
        ..
    }) = next
    else {
        return Err(ParseError::unexpected_token(TokenKind::RBracket, next));
    };

    Ok(Expression::IndexExpression {
        left: Box::new(left),
        index: Box::new(index),
    })
}

pub fn infix_parsing_function(token: TokenKind) -> Option<InfixFunction> {
    use crate::ast::InfixOperationKind as InfixKind;

    match token {
        TokenKind::Plus => Some(infix_operation(TokenKind::Plus, InfixKind::Plus)),
        TokenKind::Minus => Some(infix_operation(TokenKind::Minus, InfixKind::Minus)),
        TokenKind::LessThan => Some(infix_operation(TokenKind::LessThan, InfixKind::LessThan)),
        TokenKind::GreaterThan => Some(infix_operation(
            TokenKind::GreaterThan,
            InfixKind::GreaterThan,
        )),
        TokenKind::Equal => Some(infix_operation(TokenKind::Equal, InfixKind::Equal)),
        TokenKind::NotEqual => Some(infix_operation(TokenKind::NotEqual, InfixKind::NotEqual)),
        TokenKind::Asterisk => Some(infix_operation(TokenKind::Asterisk, InfixKind::Multiply)),
        TokenKind::Slash => Some(infix_operation(TokenKind::Slash, InfixKind::Divide)),
        TokenKind::LParen => Some(Box::new(parse_call_function)),
        TokenKind::LBracket => Some(Box::new(parse_index_expression)),
        _ => None,
    }
}
