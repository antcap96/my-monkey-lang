pub mod error;
pub mod expressions;
pub mod pattern_matching;
pub mod statements;

use crate::lexer::{Token, TokenKind};
pub use error::ParseError;
use statements::parse_statement;

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
            _ => Err(ParseError::unexpected_other(
                error::Expected::Identifier,
                token,
            )),
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
            match parse_statement(self) {
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
                        expected: error::Expected::Token(TokenKind::SemiColon),
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
