use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Illegal(Rc<str>),
    Ident(Rc<str>),
    Int(Rc<str>),
    String(Rc<str>),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Equal,
    NotEqual,

    GreaterThan,
    LessThan,

    Comma,
    Colon,
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    FatArrow,
    Ellipsis,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Match,
    Null,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub start: usize,
    pub end: usize,
}

fn keywords(ident: &str) -> Option<TokenKind> {
    match ident {
        "fn" => Some(TokenKind::Function),
        "let" => Some(TokenKind::Let),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        "if" => Some(TokenKind::If),
        "else" => Some(TokenKind::Else),
        "return" => Some(TokenKind::Return),
        "match" => Some(TokenKind::Match),
        "null" => Some(TokenKind::Null),
        _ => None,
    }
}

#[derive(Clone)]
pub struct Tokenizer<'a> {
    input: &'a str,
    iter: std::iter::Peekable<std::str::CharIndices<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        let iter = input.char_indices().peekable();
        Self { input, iter }
    }

    fn is_letter(ch: char) -> bool {
        ch.is_ascii_alphabetic() || ch == '_'
    }

    fn read_identifier(&mut self, start: usize) -> Token {
        while self.iter.next_if(|(_, ch)| Self::is_letter(*ch)).is_some() {}

        let end = self.next_idx();
        let ident = &self.input[start..end];
        Token {
            kind: keywords(ident).unwrap_or_else(|| TokenKind::Ident(ident.into())),
            start,
            end,
        }
    }

    fn read_number(&mut self, start: usize) -> Token {
        while self.iter.next_if(|(_, ch)| ch.is_ascii_digit()).is_some() {}

        let end = self.next_idx();
        let ident = &self.input[start..end];

        Token {
            kind: TokenKind::Int(ident.into()),
            start,
            end,
        }
    }

    fn read_string(&mut self, start: usize) -> Token {
        loop {
            match self.iter.next() {
                Some((_, '"')) => break,
                None => {
                    return Token {
                        kind: TokenKind::Illegal("Unterminated string".into()),
                        start,
                        end: self.next_idx(),
                    }
                }
                _ => {}
            }
        }

        let end = self.next_idx();
        let string = &self.input[start..end];
        Token {
            kind: TokenKind::String(string.into()),
            start,
            end,
        }
    }

    fn next_idx(&mut self) -> usize {
        self.iter
            .peek()
            .map(|(idx, _)| *idx)
            .unwrap_or(self.input.len())
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let mut iter = self.iter.by_ref().skip_while(|(_, ch)| ch.is_whitespace());

        if let Some((idx, ch)) = iter.next() {
            let tok = match ch {
                '=' => {
                    if self.iter.next_if(|(_, ch)| *ch == '=').is_some() {
                        Token {
                            kind: TokenKind::Equal,
                            start: idx,
                            end: self.next_idx(),
                        }
                    } else if self.iter.next_if(|(_, ch)| *ch == '>').is_some() {
                        Token {
                            kind: TokenKind::FatArrow,
                            start: idx,
                            end: self.next_idx(),
                        }
                    } else {
                        Token {
                            kind: TokenKind::Assign,
                            start: idx,
                            end: self.next_idx(),
                        }
                    }
                }
                '+' => Token {
                    kind: TokenKind::Plus,
                    start: idx,
                    end: self.next_idx(),
                },
                ',' => Token {
                    kind: TokenKind::Comma,
                    start: idx,
                    end: self.next_idx(),
                },
                '.' => {
                    if self.iter.next_if(|(_, ch)| *ch == '.').is_some() {
                        if self.iter.next_if(|(_, ch)| *ch == '.').is_some() {
                            Token {
                                kind: TokenKind::Ellipsis,
                                start: idx,
                                end: self.next_idx(),
                            }
                        } else {
                            Token {
                                kind: TokenKind::Illegal("..".into()),
                                start: idx,
                                end: self.next_idx(),
                            }
                        }
                    } else {
                        Token {
                            kind: TokenKind::Illegal(".".into()),
                            start: idx,
                            end: self.next_idx(),
                        }
                    }
                }
                ':' => Token {
                    kind: TokenKind::Colon,
                    start: idx,
                    end: self.next_idx(),
                },
                ';' => Token {
                    kind: TokenKind::SemiColon,
                    start: idx,
                    end: self.next_idx(),
                },
                '(' => Token {
                    kind: TokenKind::LParen,
                    start: idx,
                    end: self.next_idx(),
                },
                ')' => Token {
                    kind: TokenKind::RParen,
                    start: idx,
                    end: self.next_idx(),
                },
                '{' => Token {
                    kind: TokenKind::LBrace,
                    start: idx,
                    end: self.next_idx(),
                },
                '}' => Token {
                    kind: TokenKind::RBrace,
                    start: idx,
                    end: self.next_idx(),
                },
                '[' => Token {
                    kind: TokenKind::LBracket,
                    start: idx,
                    end: self.next_idx(),
                },
                ']' => Token {
                    kind: TokenKind::RBracket,
                    start: idx,
                    end: self.next_idx(),
                },
                '-' => Token {
                    kind: TokenKind::Minus,
                    start: idx,
                    end: self.next_idx(),
                },
                '!' => {
                    if self.iter.next_if(|(_, ch)| *ch == '=').is_some() {
                        Token {
                            kind: TokenKind::NotEqual,
                            start: idx,
                            end: self.next_idx(),
                        }
                    } else {
                        Token {
                            kind: TokenKind::Bang,
                            start: idx,
                            end: self.next_idx(),
                        }
                    }
                }
                '*' => Token {
                    kind: TokenKind::Asterisk,
                    start: idx,
                    end: self.next_idx(),
                },
                '/' => Token {
                    kind: TokenKind::Slash,
                    start: idx,
                    end: self.next_idx(),
                },
                '<' => Token {
                    kind: TokenKind::LessThan,
                    start: idx,
                    end: self.next_idx(),
                },
                '>' => Token {
                    kind: TokenKind::GreaterThan,
                    start: idx,
                    end: self.next_idx(),
                },
                '"' => self.read_string(idx),
                c if Tokenizer::is_letter(c) => self.read_identifier(idx),
                c if c.is_ascii_digit() => self.read_number(idx),
                _ => Token {
                    kind: TokenKind::Illegal(ch.to_string().into()),
                    start: idx,
                    end: self.next_idx(),
                },
            };
            Some(tok)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test1() {
        let input = "=+(){},;";
        let output = Tokenizer::new(input).collect::<Vec<_>>();

        assert_eq!(
            output,
            vec![
                Token {
                    kind: TokenKind::Assign,
                    start: 0,
                    end: 1
                },
                Token {
                    kind: TokenKind::Plus,
                    start: 1,
                    end: 2
                },
                Token {
                    kind: TokenKind::LParen,
                    start: 2,
                    end: 3
                },
                Token {
                    kind: TokenKind::RParen,
                    start: 3,
                    end: 4
                },
                Token {
                    kind: TokenKind::LBrace,
                    start: 4,
                    end: 5
                },
                Token {
                    kind: TokenKind::RBrace,
                    start: 5,
                    end: 6
                },
                Token {
                    kind: TokenKind::Comma,
                    start: 6,
                    end: 7
                },
                Token {
                    kind: TokenKind::SemiColon,
                    start: 7,
                    end: 8
                }
            ]
        );
    }

    #[test]
    fn test2() {
        let input = "let five = 5;
    let ten = 10;
    let add = fn(x, y) {
    x + y;
    };
    let result = add(five, ten);
    ";
        let expected_output = vec![
            TokenKind::Let,
            TokenKind::Ident("five".into()),
            TokenKind::Assign,
            TokenKind::Int("5".into()),
            TokenKind::SemiColon,
            TokenKind::Let,
            TokenKind::Ident("ten".into()),
            TokenKind::Assign,
            TokenKind::Int("10".into()),
            TokenKind::SemiColon,
            TokenKind::Let,
            TokenKind::Ident("add".into()),
            TokenKind::Assign,
            TokenKind::Function,
            TokenKind::LParen,
            TokenKind::Ident("x".into()),
            TokenKind::Comma,
            TokenKind::Ident("y".into()),
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Ident("x".into()),
            TokenKind::Plus,
            TokenKind::Ident("y".into()),
            TokenKind::SemiColon,
            TokenKind::RBrace,
            TokenKind::SemiColon,
            TokenKind::Let,
            TokenKind::Ident("result".into()),
            TokenKind::Assign,
            TokenKind::Ident("add".into()),
            TokenKind::LParen,
            TokenKind::Ident("five".into()),
            TokenKind::Comma,
            TokenKind::Ident("ten".into()),
            TokenKind::RParen,
            TokenKind::SemiColon,
        ];

        let output = Tokenizer::new(input).collect::<Vec<_>>();
        assert_eq!(
            output
                .into_iter()
                .map(|token| token.kind)
                .collect::<Vec<_>>(),
            expected_output
        )
    }

    #[test]
    fn test3() {
        let input = "
    !-/*5;
    5 < 10 > 5;
    ";

        let output = Tokenizer::new(input).collect::<Vec<_>>();

        let expected_output = vec![
            TokenKind::Bang,
            TokenKind::Minus,
            TokenKind::Slash,
            TokenKind::Asterisk,
            TokenKind::Int("5".into()),
            TokenKind::SemiColon,
            TokenKind::Int("5".into()),
            TokenKind::LessThan,
            TokenKind::Int("10".into()),
            TokenKind::GreaterThan,
            TokenKind::Int("5".into()),
            TokenKind::SemiColon,
        ];

        assert_eq!(
            output
                .into_iter()
                .map(|token| token.kind)
                .collect::<Vec<_>>(),
            expected_output
        )
    }

    #[test]
    fn test4() {
        let input = "if (5 < 10) {
    return true;
    } else {
    return false;
    }";

        let output = Tokenizer::new(input).collect::<Vec<_>>();

        let expected_output = vec![
            TokenKind::If,
            TokenKind::LParen,
            TokenKind::Int("5".into()),
            TokenKind::LessThan,
            TokenKind::Int("10".into()),
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Return,
            TokenKind::True,
            TokenKind::SemiColon,
            TokenKind::RBrace,
            TokenKind::Else,
            TokenKind::LBrace,
            TokenKind::Return,
            TokenKind::False,
            TokenKind::SemiColon,
            TokenKind::RBrace,
        ];

        assert_eq!(
            output
                .into_iter()
                .map(|token| token.kind)
                .collect::<Vec<_>>(),
            expected_output
        )
    }

    #[test]
    fn test5() {
        let input = "10 == 10;
    10 != 9;";

        let output = Tokenizer::new(input).collect::<Vec<_>>();
        let expected_output = vec![
            TokenKind::Int("10".into()),
            TokenKind::Equal,
            TokenKind::Int("10".into()),
            TokenKind::SemiColon,
            TokenKind::Int("10".into()),
            TokenKind::NotEqual,
            TokenKind::Int("9".into()),
            TokenKind::SemiColon,
        ];

        assert_eq!(
            output
                .into_iter()
                .map(|token| token.kind)
                .collect::<Vec<_>>(),
            expected_output
        )
    }

    #[test]
    fn test6() {
        let input = "{1: 2}";

        let output = Tokenizer::new(input).collect::<Vec<_>>();
        let expected_output = vec![
            TokenKind::LBrace,
            TokenKind::Int("1".into()),
            TokenKind::Colon,
            TokenKind::Int("2".into()),
            TokenKind::RBrace,
        ];

        assert_eq!(
            output
                .into_iter()
                .map(|token| token.kind)
                .collect::<Vec<_>>(),
            expected_output
        )
    }
}
