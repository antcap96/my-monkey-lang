use crate::ast::{BlockStatement, Expression};
use crate::lexer::Token;
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

pub fn precedence(token: &Token) -> Precedence {
    match token {
        Token::Equal => Precedence::Equals,
        Token::NotEqual => Precedence::Equals,
        Token::LessThan => Precedence::LessGreater,
        Token::GreaterThan => Precedence::LessGreater,
        Token::Plus => Precedence::Sum,
        Token::Minus => Precedence::Sum,
        Token::Asterisk => Precedence::Product,
        Token::Slash => Precedence::Product,
        Token::LParen => Precedence::Call,
        Token::LBracket => Precedence::Index,
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
        Some(Token::RParen) => Ok(expression),
        next => Err(ParseError::unexpected_token(Token::RParen, next)),
    }
}

fn parse_expression_list(
    parser: &mut Parser,
    terminator: Token,
) -> Result<Vec<Expression>, ParseError> {
    let mut elements = Vec::new();

    loop {
        match parser.iter.next() {
            Some(next) if next == terminator => return Ok(elements),
            Some(next) => {
                elements.push(parser.parse_expression(Precedence::Lowest, next)?);
            }
            None => return Err(ParseError::premature_end_expected_expression()),
        }

        match parser.iter.next() {
            Some(Token::Comma) => {}
            Some(next) if next == terminator => return Ok(elements),
            next => return Err(ParseError::unexpected_token(terminator, next)),
        }
    }
}

fn parse_array_literal(parser: &mut Parser) -> Result<Expression, ParseError> {
    let expressions = parse_expression_list(parser, Token::RBracket)?;
    Ok(Expression::ArrayLiteral(expressions))
}

fn parse_hash_literal(parser: &mut Parser) -> Result<Expression, ParseError> {
    let mut pairs = Vec::new();

    loop {
        let next = parser.iter.next();
        let key = match next {
            Some(Token::RBrace) => return Ok(Expression::HashLiteral(pairs)),
            None => {
                return Err(ParseError::PrematureEndOfInput {
                    expected: Expected::Token(Token::RBrace),
                })
            }
            Some(token) => parser.parse_expression(Precedence::Lowest, token)?,
        };

        let next = parser.iter.next();
        let Some(Token::Colon) = next else {return Err(ParseError::unexpected_token(Token::Colon, next))};

        let Some(next) = parser.iter.next() else {return Err(ParseError::premature_end_expected_expression())};
        let value = parser.parse_expression(Precedence::Lowest, next)?;

        pairs.push((key, value));

        let next = parser.iter.next();
        match next {
            Some(Token::Comma) => {}
            Some(Token::RBrace) => return Ok(Expression::HashLiteral(pairs)),
            next => return Err(ParseError::unexpected_token(Token::RBrace, next)),
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
    let Some(Token::LBrace) = next else {return Err(ParseError::unexpected_token(Token::LBrace, next))};

    let consequence = parse_block_statement(parser)?;

    if parser.iter.next_if(|token| *token == Token::Else).is_some() {
        let next = parser.iter.next();
        let Some(Token::LBrace) = next else {return Err(ParseError::unexpected_token(Token::LBrace, next))};
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

    if let Some(Token::RBrace) = parser.iter.next_if(|token| token == &Token::RBrace) {
        return Ok(BlockStatement { statements });
    }

    loop {
        let next = parser.iter.next();
        match next {
            None => {
                return Err(ParseError::PrematureEndOfInput {
                    expected: Expected::Token(Token::RBrace),
                })
            }
            Some(token) => {
                let statement = parser.parse_statement(token)?;
                statements.push(statement);
            }
        }
        let next = parser.iter.next();
        match next {
            Some(Token::SemiColon) => continue,
            Some(Token::RBrace) => return Ok(BlockStatement { statements }),
            _ => return Err(ParseError::unexpected_token(Token::RBrace, next)),
        }
    }
}

fn parse_function_literal(parser: &mut Parser) -> Result<Expression, ParseError> {
    let next = parser.iter.next();
    let Some(Token::LParen) = next else {return Err(ParseError::unexpected_token(Token::LParen, next))};

    let parameters = parse_parameters(parser)?;

    let next = parser.iter.next();
    let Some(Token::LBrace) = next else {return Err(ParseError::unexpected_token(Token::LBrace, next))};

    let body = parse_block_statement(parser)?;

    Ok(Expression::FunctionLiteral { parameters, body })
}

fn parse_parameters(parser: &mut Parser) -> Result<Vec<crate::ast::Identifier>, ParseError> {
    let mut identifiers = Vec::new();

    loop {
        let next = parser.iter.next();
        match next {
            Some(Token::Ident(name)) => identifiers.push(crate::ast::Identifier { name }),
            Some(Token::RParen) => return Ok(identifiers), // empty parameter list or tailing comma
            _ => Err(ParseError::unexpected_token(Token::RParen, next))?,
        }

        let next = parser.iter.next();
        match next {
            Some(Token::Comma) => continue,
            Some(Token::RParen) => return Ok(identifiers),
            _ => Err(ParseError::unexpected_token(Token::RParen, next))?,
        }
    }
}

pub fn prefix_parsing(token: Token, parser: &mut Parser) -> Result<Expression, ParseError> {
    match token {
        Token::Ident(name) => Ok(Expression::Identifier(crate::ast::Identifier { name })),
        Token::Int(val) => Ok(Expression::IntegerLiteral(val.parse()?)),
        Token::String(val) => Ok(Expression::StringLiteral(val.trim_matches('\"').to_owned())),
        Token::True => Ok(Expression::BooleanLiteral(true)),
        Token::False => Ok(Expression::BooleanLiteral(false)),
        Token::Bang => prefix_operation(crate::ast::PrefixOperationKind::Bang)(parser),
        Token::Minus => prefix_operation(crate::ast::PrefixOperationKind::Minus)(parser),
        Token::LParen => parse_grouped_expression(parser),
        Token::LBracket => parse_array_literal(parser),
        Token::LBrace => parse_hash_literal(parser),
        Token::If => parse_if_expression(parser),
        Token::Function => parse_function_literal(parser),
        _ => Err(ParseError::NoPrefixFunction(token)),
    }
}

type InfixFunction = Box<dyn FnOnce(Expression, &mut Parser) -> Result<Expression, ParseError>>;

fn infix_operation(token: Token, kind: crate::ast::InfixOperationKind) -> InfixFunction {
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
    let arguments = parse_expression_list(parser, Token::RParen)?;

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
    let Some(Token::RBracket) = next else {
        return Err(ParseError::unexpected_token(Token::RBracket, next));
    };

    Ok(Expression::IndexExpression {
        left: Box::new(left),
        index: Box::new(index),
    })
}

pub fn infix_parsing_function(token: Token) -> Option<InfixFunction> {
    use crate::ast::InfixOperationKind as InfixKind;

    match token {
        Token::Plus => Some(infix_operation(Token::Plus, InfixKind::Plus)),
        Token::Minus => Some(infix_operation(Token::Minus, InfixKind::Minus)),
        Token::LessThan => Some(infix_operation(Token::LessThan, InfixKind::LessThan)),
        Token::GreaterThan => Some(infix_operation(Token::GreaterThan, InfixKind::GreaterThan)),
        Token::Equal => Some(infix_operation(Token::Equal, InfixKind::Equal)),
        Token::NotEqual => Some(infix_operation(Token::NotEqual, InfixKind::NotEqual)),
        Token::Asterisk => Some(infix_operation(Token::Asterisk, InfixKind::Multiply)),
        Token::Slash => Some(infix_operation(Token::Slash, InfixKind::Divide)),
        Token::LParen => Some(Box::new(parse_call_function)),
        Token::LBracket => Some(Box::new(parse_index_expression)),
        _ => None,
    }
}
