use crate::ast::Expression;
use crate::lexer::Token;
use crate::parser::{ParseError, Parser, Precedence};
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
            _ => Precedence::Lowest,
        }
    }
}

pub trait HasPrefixOperation {
    fn prefix_parsing(self, parser: &mut Parser) -> Result<Expression, ParseError>;
}

fn prefix_operation(kind: crate::ast::PrefixOperationKind) -> impl FnOnce(&mut Parser) -> Result<Expression, ParseError> {
    move |parser| {
        let next_token = parser.iter.next().ok_or(ParseError::PrematureEndOfInput)?;
        Ok(Expression::PrefixOperation(
            kind,
            Box::new(parser.parse_expression(Precedence::Prefix, next_token)?),
        ))
    }
}

impl HasPrefixOperation for Token {
    fn prefix_parsing(self, parser: &mut Parser) -> Result<Expression, ParseError> {
        match self {
            Token::Ident(name) => Ok(Expression::Identifier(crate::ast::Identifier { name })),
            Token::Int(val) => Ok(Expression::IntegerLiteral(val.parse()?)),
            Token::Bang => prefix_operation(crate::ast::PrefixOperationKind::Bang)(parser),
            Token::Minus => prefix_operation(crate::ast::PrefixOperationKind::Minus)(parser),
            _ => Err(ParseError::NoPrefixParseError(self)),
        }
    }
}

pub trait HasInfixOperation {
    fn infix_parsing_function(
        &self,
    ) -> Option<Box<dyn FnOnce(Expression, &mut Parser) -> Result<Expression, ParseError>>>;
}

fn infix_operation(
    token: Token,
    kind: crate::ast::InfixOperationKind,
) -> Box<dyn FnOnce(Expression, &mut Parser) -> Result<Expression, ParseError>> {
    Box::new(
        move |left: Expression, parser: &mut Parser| -> Result<Expression, ParseError> {
            let new_precedence = token.precedence();

            let new_token = parser
                .iter
                .next()
                .ok_or(crate::parser::ParseError::PrematureEndOfInput)?;
            Ok(Expression::InfixOperation(
                kind,
                Box::new(left),
                Box::new(parser.parse_expression(new_precedence, new_token)?),
            ))
        },
    )
}

impl HasInfixOperation for Token {
    fn infix_parsing_function(
        &self,
    ) -> Option<Box<dyn FnOnce(Expression, &mut Parser) -> Result<Expression, ParseError>>> {
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
            _ => None,
        }
    }
}
