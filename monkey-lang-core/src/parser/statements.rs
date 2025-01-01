use crate::ast::{Identifier, Statement};
use crate::lexer::TokenKind;
use crate::parser::expressions::{parse_expression, Precedence};
use crate::parser::{ParseError, Parser};

pub fn parse_statement(parser: &mut Parser) -> Result<crate::ast::Statement, ParseError> {
    let token = parser.iter.peek();
    match token.map(|t| &t.kind) {
        Some(TokenKind::Let) => Ok(Statement::Let(parse_let_statement(parser)?)),
        Some(TokenKind::Return) => Ok(Statement::Return(parse_return_statement(parser)?)),
        _ => Ok(Statement::Expression(parse_expression_statement(parser)?)),
    }
}

fn parse_let_statement(parser: &mut Parser) -> Result<crate::ast::LetStatement, ParseError> {
    parser.expect_token(TokenKind::Let)?;
    let name = parser.parse_ident()?;
    parser.expect_token(TokenKind::Assign)?;
    let value = parse_expression(parser, Precedence::Lowest)?;

    Ok(crate::ast::LetStatement {
        identifier: Identifier { name },
        value,
    })
}

fn parse_return_statement(parser: &mut Parser) -> Result<crate::ast::ReturnStatement, ParseError> {
    parser.expect_token(TokenKind::Return)?;
    let value = parse_expression(parser, Precedence::Lowest)?;

    Ok(crate::ast::ReturnStatement { value })
}
fn parse_expression_statement(parser: &mut Parser) -> Result<crate::ast::Expression, ParseError> {
    parse_expression(parser, Precedence::Lowest)
}
