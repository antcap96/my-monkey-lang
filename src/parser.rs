use crate::{
    ast::{Expression, Identifier, Statement},
    lexer::Token,
};

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    Lowest = 0,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

#[derive(Debug)]
pub enum ParseError {
    PrematureEndOfInput,
    UnexpectedToken(Token),
    ParseIntError(std::num::ParseIntError),
    NoPrefixParseError(Token),
}

impl From<std::num::ParseIntError> for ParseError {
    fn from(value: std::num::ParseIntError) -> Self {
        ParseError::ParseIntError(value)
    }
}

pub struct Parser<'a> {
    iter: std::iter::Peekable<crate::lexer::Tokenizer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: crate::lexer::Tokenizer<'a>) -> Self {
        let iter = tokenizer.peekable();
        Self { iter }
    }

    pub fn parse_program(&mut self) -> Result<crate::ast::Program, Vec<ParseError>> {
        let mut statements = Vec::new();

        let mut errors = Vec::new();

        while self.iter.peek().is_some() {
            let maybe_statement = self.parse_statement();
            match maybe_statement {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(err) => {
                    errors.push(err);
                    for token in self.iter.by_ref() {
                        match token {
                            Token::SemiColon => break,
                            _ => {}
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

    fn parse_statement(&mut self) -> Result<crate::ast::Statement, ParseError> {
        let first_token = self.iter.next().ok_or(ParseError::PrematureEndOfInput)?;
        match first_token {
            Token::Let => Ok(Statement::Let(self.parse_let_statement()?)),
            Token::Return => Ok(Statement::Return(self.parse_return_statement()?)),
            _ => Ok(Statement::Expression(
                self.parse_expression_statement(first_token)?,
            )),
        }
    }

    fn parse_let_statement(&mut self) -> Result<crate::ast::LetStatement, ParseError> {
        let ident = self.iter.next().ok_or(ParseError::PrematureEndOfInput)?;

        let name = match ident {
            Token::Ident(name) => name,
            _ => return Err(ParseError::UnexpectedToken(ident)),
        };

        // TODO: Parse the expression
        for token in self.iter.by_ref() {
            match token {
                Token::SemiColon => break,
                _ => {}
            }
        }
        Ok(crate::ast::LetStatement {
            name: Identifier { name },
            value: crate::ast::Expression::NotYetImplemented,
        })
    }

    fn parse_return_statement(&mut self) -> Result<crate::ast::ReturnStatement, ParseError> {
        // TODO: Parse the expression
        for token in self.iter.by_ref() {
            match token {
                Token::SemiColon => break,
                _ => {}
            }
        }
        Ok(crate::ast::ReturnStatement {
            value: crate::ast::Expression::NotYetImplemented,
        })
    }
    fn parse_expression_statement(
        &mut self,
        token: Token,
    ) -> Result<crate::ast::Expression, ParseError> {
        let expression = self.parse_expression(Precedence::Lowest, token)?;

        match self.iter.next() {
            Some(Token::SemiColon) | None => Ok(expression),
            Some(t) => Err(ParseError::UnexpectedToken(t)),
        }
    }

    fn parse_expression(
        &mut self,
        precedence: Precedence,
        token: Token,
    ) -> Result<crate::ast::Expression, ParseError> {
        let mut left_expression = parse_prefix_expression(token, self)?;

        let statement_continues = self
            .iter
            .peek()
            .map(|token| *token != Token::SemiColon)
            .unwrap_or(false);
        let lower_precedence = precedence
            < self
                .iter
                .peek()
                .map(precedences)
                .unwrap_or(Precedence::Lowest);
        while statement_continues && lower_precedence {
            let result = parse_infix_expression(left_expression, self)?;
            left_expression = result.0;
            if result.1 {
                break;
            }
        }

        Ok(left_expression)
    }
}

fn precedences(token: &Token) -> Precedence {
    match token {
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

fn parse_infix_expression(
    left: Expression,
    parser: &mut Parser,
) -> Result<(Expression, bool), ParseError> {
    use crate::ast::InfixOperationKind as InfixKind;
    let infix_operation = |token: Token, kind: InfixKind, left: Expression, parser: &mut Parser| {
        let new_token = parser.iter.next().ok_or(ParseError::PrematureEndOfInput)?;
        let new_precedence = precedences(&token);
        Ok((
            Expression::InfixOperation(
                kind,
                Box::new(left),
                Box::new(parser.parse_expression(new_precedence, new_token)?),
            ),
            false,
        ))
    };

    let infix_token = parser.iter.next_if(|token| {
        *token == Token::Plus
            || *token == Token::Minus
            || *token == Token::LessThan
            || *token == Token::GreaterThan
            || *token == Token::Equal
            || *token == Token::NotEqual
            || *token == Token::Asterisk
            || *token == Token::Slash
    });
    match infix_token {
        Some(Token::Plus) => infix_operation(Token::Plus, InfixKind::Plus, left, parser),
        Some(Token::Minus) => infix_operation(Token::Minus, InfixKind::Minus, left, parser),
        Some(Token::LessThan) => {
            infix_operation(Token::LessThan, InfixKind::LessThan, left, parser)
        }
        Some(Token::GreaterThan) => {
            infix_operation(Token::GreaterThan, InfixKind::GreaterThan, left, parser)
        }
        Some(Token::Equal) => infix_operation(Token::Equal, InfixKind::Equal, left, parser),
        Some(Token::NotEqual) => {
            infix_operation(Token::NotEqual, InfixKind::NotEqual, left, parser)
        }
        Some(Token::Asterisk) => {
            infix_operation(Token::Asterisk, InfixKind::Multiply, left, parser)
        }
        Some(Token::Slash) => infix_operation(Token::Slash, InfixKind::Divide, left, parser),
        _ => Ok((left, true)),
    }
}

fn parse_prefix_expression(token: Token, parser: &mut Parser) -> Result<Expression, ParseError> {
    match token {
        Token::Ident(name) => Ok(Expression::Identifier(crate::ast::Identifier { name })),
        Token::Int(val) => Ok(Expression::IntegerLiteral(val.parse()?)),
        Token::Bang => {
            let next_token = parser.iter.next().ok_or(ParseError::PrematureEndOfInput)?;
            Ok(Expression::PrefixOperation(
                crate::ast::PrefixOperationKind::Bang,
                Box::new(parser.parse_expression(Precedence::Prefix, next_token)?),
            ))
        }
        Token::Minus => {
            let next_token = parser.iter.next().ok_or(ParseError::PrematureEndOfInput)?;
            Ok(Expression::PrefixOperation(
                crate::ast::PrefixOperationKind::Minus,
                Box::new(parser.parse_expression(Precedence::Prefix, next_token)?),
            ))
        }
        _ => Err(ParseError::NoPrefixParseError(token)),
    }
}

#[cfg(test)]
mod tests {
    #[test] 
    fn test_expression_1() {
        let input = "10 + 10";
        let tokenizer = crate::lexer::Tokenizer::new(input);
        let mut parser = crate::parser::Parser::new(tokenizer);

        let program = parser.parse_program().unwrap();

        assert_eq!(program.to_string(), "(10 + 10);\n")
    }

    #[test]
    fn test_let_statement() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;";

        let tokenizer = crate::lexer::Tokenizer::new(input);
        let mut parser = crate::parser::Parser::new(tokenizer);

        let program = parser.parse_program().unwrap();

        if program.statements.len() != 3 {
            panic!(
                "program.statements does not contain 3 statements. got={}",
                program.statements.len()
            );
        }

        for (name, statement) in ["x", "y", "foobar"]
            .iter()
            .zip(program.statements.into_iter())
        {
            test_let_statement_(statement, name);
        }
    }

    #[allow(dead_code)]
    fn test_let_statement_(s: crate::ast::Statement, name: &str) {
        if let crate::ast::Statement::Let(let_statement) = s {
            assert_eq!(let_statement.name.name, name);
        } else {
            panic!("s is not LET. got={:?}", s);
        }
    }
}
