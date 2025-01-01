use super::error;
use super::error::ParseError;
use super::pattern_matching::parse_pattern;
use super::statements::parse_statement;
use crate::ast::{BlockStatement, Expression, Identifier};
use crate::lexer::{Token, TokenKind};
use crate::parser::Parser;

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

pub fn precedence_of(token: &TokenKind) -> Precedence {
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

pub fn parse_expression(
    parser: &mut Parser,
    precedence: Precedence,
) -> Result<crate::ast::Expression, ParseError> {
    let Some(token) = parser.iter.next() else {
        return Err(ParseError::premature_end_expected_expression());
    };
    let mut left_expression = prefix_parsing(token, parser)?;

    loop {
        let Some(next_token) = parser.iter.peek() else {
            break;
        };

        //TODO: do i need statement_ended? SemiColon would have Precedence::Lowest
        let statement_ended = next_token.kind == TokenKind::SemiColon;
        let next_precedence = precedence_of(&next_token.kind);
        if statement_ended || precedence >= next_precedence {
            break;
        }

        let Some(next_token) = parser.iter.next() else {
            break;
        };
        let Some(infix_parse_function) = infix_parsing_function(next_token.kind) else {
            break;
        };
        left_expression = infix_parse_function(left_expression, parser)?;
    }

    Ok(left_expression)
}

fn prefix_operation(
    kind: crate::ast::PrefixOperationKind,
) -> impl FnOnce(&mut Parser) -> Result<Expression, ParseError> {
    move |parser| {
        Ok(Expression::PrefixOperation(
            kind,
            Box::new(parse_expression(parser, Precedence::Prefix)?),
        ))
    }
}

fn parse_grouped_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
    let expression = parse_expression(parser, Precedence::Lowest)?;
    parser.expect_token(TokenKind::RParen)?;

    Ok(expression)
}

fn parse_sequence<T>(
    parser: &mut Parser,
    parse_element: impl Fn(&mut Parser) -> Result<T, ParseError>,
    separator: TokenKind,
    terminator: TokenKind,
) -> Result<Vec<T>, ParseError> {
    let mut elements = Vec::new();

    loop {
        match parser.iter.peek() {
            Some(next) if next.kind == terminator => {
                parser.iter.next();
                return Ok(elements);
            }
            None => {
                return Err(ParseError::PrematureEndOfInput {
                    expected: error::Expected::Token(terminator),
                })
            }
            _ => {
                elements.push(parse_element(parser)?);
            }
        }

        match parser.iter.next() {
            Some(next) if next.kind == separator => continue,
            Some(next) if next.kind == terminator => return Ok(elements),
            next => return Err(ParseError::unexpected_token(separator, next)),
        }
    }
}

fn parse_array_literal(parser: &mut Parser) -> Result<Expression, ParseError> {
    let expressions = parse_sequence(
        parser,
        |parser| parse_expression(parser, Precedence::Lowest),
        TokenKind::Comma,
        TokenKind::RBracket,
    )?;
    Ok(Expression::ArrayLiteral(expressions))
}

fn parse_hash_literal(parser: &mut Parser) -> Result<Expression, ParseError> {
    let pairs = parse_sequence(
        parser,
        |parser| {
            let key = parse_expression(parser, Precedence::Lowest)?;
            parser.expect_token(TokenKind::Colon)?;
            let value = parse_expression(parser, Precedence::Lowest)?;
            return Ok((key, value));
        },
        TokenKind::Comma,
        TokenKind::RBrace,
    )?;
    Ok(Expression::HashLiteral(pairs))
}

fn parse_if_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
    let condition = Box::new(parse_expression(parser, Precedence::Lowest)?);

    parser.expect_token(TokenKind::LBrace)?;

    let consequence = parse_block_statement(parser)?;

    let alternative = if parser
        .iter
        .next_if(|token| token.kind == TokenKind::Else)
        .is_some()
    {
        parser.expect_token(TokenKind::LBrace)?;
        Some(parse_block_statement(parser)?)
    } else {
        None
    };

    Ok(Expression::IfExpression {
        condition,
        consequence,
        alternative,
    })
}

fn parse_block_statement(parser: &mut Parser) -> Result<BlockStatement, ParseError> {
    let statements = parse_sequence(
        parser,
        parse_statement,
        TokenKind::SemiColon,
        TokenKind::RBrace,
    )?;
    Ok(BlockStatement { statements })
}

fn parse_function_literal(parser: &mut Parser) -> Result<Expression, ParseError> {
    parser.expect_token(TokenKind::LParen)?;
    let parameters = parse_parameters(parser)?;

    parser.expect_token(TokenKind::LBrace)?;
    let body = parse_block_statement(parser)?;

    Ok(Expression::FunctionLiteral { parameters, body })
}

fn parse_parameters(parser: &mut Parser) -> Result<Vec<crate::ast::Identifier>, ParseError> {
    let identifiers = parse_sequence(
        parser,
        |parser| parser.parse_ident().map(|name| Identifier { name }),
        TokenKind::Comma,
        TokenKind::RParen,
    )?;

    Ok(identifiers)
}

fn parse_match_expression(parser: &mut Parser) -> Result<Expression, ParseError> {
    let expression = Box::new(parse_expression(parser, Precedence::Lowest)?);

    parser.expect_token(TokenKind::LBrace)?;

    let cases = parse_sequence(
        parser,
        parse_match_case,
        TokenKind::Comma,
        TokenKind::RBrace,
    )?;

    return Ok(Expression::MatchExpression { expression, cases });
}

fn parse_match_case(parser: &mut Parser) -> Result<crate::ast::MatchCase, ParseError> {
    let pattern = parse_pattern(parser)?;

    parser.expect_token(TokenKind::FatArrow)?;
    parser.expect_token(TokenKind::LBrace)?;

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
            let new_precedence = precedence_of(&token);

            Ok(Expression::InfixOperation(
                kind,
                Box::new(left),
                Box::new(parse_expression(parser, new_precedence)?),
            ))
        },
    )
}

fn parse_call_function(left: Expression, parser: &mut Parser) -> Result<Expression, ParseError> {
    let arguments = parse_sequence(
        parser,
        |parser| parse_expression(parser, Precedence::Lowest),
        TokenKind::Comma,
        TokenKind::RParen,
    )?;

    Ok(Expression::CallExpression {
        function: Box::new(left),
        arguments,
    })
}

fn parse_index_expression(left: Expression, parser: &mut Parser) -> Result<Expression, ParseError> {
    let index = parse_expression(parser, Precedence::Lowest)?;
    parser.expect_token(TokenKind::RBracket)?;

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
