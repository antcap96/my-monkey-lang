use crate::ast::{BlockStatement, Expression};
use crate::lexer::Token;
use crate::parser::{Expected, ParseError, Parser, Precedence};
pub trait HasPrecedence {
    fn precedence(&self) -> Precedence;
}

impl HasPrecedence for crate::lexer::Token {
    fn precedence(&self) -> Precedence {
        match self {
            Token::Equal => Precedence::Equals,
            Token::NotEqual => Precedence::Equals,
            Token::LessThan => Precedence::LessGreater,
            Token::GreaterThan => Precedence::LessGreater,
            Token::Plus => Precedence::Sum,
            Token::Minus => Precedence::Sum,
            Token::Asterisk => Precedence::Product,
            Token::Slash => Precedence::Product,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

pub trait HasPrefixOperation {
    fn prefix_parsing(self, parser: &mut Parser) -> Result<Expression, ParseError>;
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
            None => return Err(ParseError::PrematureEndOfInput {
                expected: Expected::Token(Token::RBrace),
            }),
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

impl HasPrefixOperation for Token {
    fn prefix_parsing(self, parser: &mut Parser) -> Result<Expression, ParseError> {
        match self {
            Token::Ident(name) => Ok(Expression::Identifier(crate::ast::Identifier { name })),
            Token::Int(val) => Ok(Expression::IntegerLiteral(val.parse()?)),
            Token::String(val) => Ok(Expression::StringLiteral(val.trim_matches('\"').to_owned())),
            Token::True => Ok(Expression::BooleanLiteral(true)),
            Token::False => Ok(Expression::BooleanLiteral(false)),
            Token::Bang => prefix_operation(crate::ast::PrefixOperationKind::Bang)(parser),
            Token::Minus => prefix_operation(crate::ast::PrefixOperationKind::Minus)(parser),
            Token::LParen => parse_grouped_expression(parser),
            Token::If => parse_if_expression(parser),
            Token::Function => parse_function_literal(parser),
            _ => Err(ParseError::NoPrefixFunction(self)),
        }
    }
}

type InfixFunction = Box<dyn FnOnce(Expression, &mut Parser) -> Result<Expression, ParseError>>;

pub trait HasInfixOperation {
    fn infix_parsing_function(&self) -> Option<InfixFunction>;
}

fn infix_operation(token: Token, kind: crate::ast::InfixOperationKind) -> InfixFunction {
    Box::new(
        move |left: Expression, parser: &mut Parser| -> Result<Expression, ParseError> {
            let new_precedence = token.precedence();

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
    let arguments = parse_call_arguments(parser)?;

    Ok(Expression::CallExpression {
        function: Box::new(left),
        arguments,
    })
}

fn parse_call_arguments(parser: &mut Parser) -> Result<Vec<Expression>, ParseError> {
    let mut arguments = Vec::new();

    loop {
        let next = parser.iter.next();
        match next {
            Some(Token::RParen) => return Ok(arguments), // empty argument list or tailing comma
            Some(token) => arguments.push(parser.parse_expression(Precedence::Lowest, token)?),
            _ => Err(ParseError::unexpected_token(Token::RParen, next))?,
        }

        let next = parser.iter.next();
        match next {
            Some(Token::Comma) => continue,
            Some(Token::RParen) => return Ok(arguments),
            _ => Err(ParseError::unexpected_token(Token::RParen, next))?,
        }
    }
}

impl HasInfixOperation for Token {
    fn infix_parsing_function(&self) -> Option<InfixFunction> {
        use crate::ast::InfixOperationKind as InfixKind;

        match self {
            Token::Plus => Some(infix_operation(Token::Plus, InfixKind::Plus)),
            Token::Minus => Some(infix_operation(Token::Minus, InfixKind::Minus)),
            Token::LessThan => Some(infix_operation(Token::LessThan, InfixKind::LessThan)),
            Token::GreaterThan => Some(infix_operation(Token::GreaterThan, InfixKind::GreaterThan)),
            Token::Equal => Some(infix_operation(Token::Equal, InfixKind::Equal)),
            Token::NotEqual => Some(infix_operation(Token::NotEqual, InfixKind::NotEqual)),
            Token::Asterisk => Some(infix_operation(Token::Asterisk, InfixKind::Multiply)),
            Token::Slash => Some(infix_operation(Token::Slash, InfixKind::Divide)),
            Token::LParen => Some(Box::new(parse_call_function)),
            _ => None,
        }
    }
}
