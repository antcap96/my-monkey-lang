#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Illegal(String),
    Ident(String),
    Int(String),
    String(String),

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
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

static KEYWORDS: phf::Map<&str, Token> = phf::phf_map! {
    "fn" => Token::Function,
    "let" => Token::Let,
    "true" => Token::True,
    "false" => Token::False,
    "if" => Token::If,
    "else" => Token::Else,
    "return" => Token::Return,
};

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
        KEYWORDS
            .get(ident)
            .cloned()
            .unwrap_or_else(|| Token::Ident(ident.to_owned()))
    }
    
    fn read_number(&mut self, start: usize) -> Token {
        while self.iter.next_if(|(_, ch)| ch.is_ascii_digit()).is_some() {}

        let end = self.next_idx();
        let ident = &self.input[start..end];

        Token::Int(ident.to_owned())
    }

    fn read_string(&mut self, start: usize) -> Token {
        loop {
            match self.iter.next() {
                Some((_, '"')) => break,
                None => return Token::Illegal("Unterminated string".to_owned()),
                _ => {}
            }
        }

        let end = self.next_idx();
        let string = &self.input[start..end];
        Token::String(string.to_owned())
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
            let tok: Token = match ch {
                '=' => {
                    if self.iter.next_if(|(_, ch)| *ch == '=').is_some() {
                        Token::Equal
                    } else {
                        Token::Assign
                    }
                }
                '+' => Token::Plus,
                ',' => Token::Comma,
                ';' => Token::SemiColon,
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '-' => Token::Minus,
                '!' => {
                    if self.iter.next_if(|(_, ch)| *ch == '=').is_some() {
                        Token::NotEqual
                    } else {
                        Token::Bang
                    }
                }
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                '<' => Token::LessThan,
                '>' => Token::GreaterThan,
                '"' => self.read_string(idx),
                c if Tokenizer::is_letter(c) => self.read_identifier(idx),
                c if c.is_ascii_digit() => self.read_number(idx),
                _c => Token::Illegal(ch.to_string()),
            };
            Some(tok)
        } else {
            None
        }
    }
}

#[cfg(test)]
#[test]
fn test1() {
    let input = "=+(){},;";
    let output = Tokenizer::new(input).collect::<Vec<_>>();

    assert_eq!(
        output,
        vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::SemiColon
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
        Token::Let,
        Token::Ident("five".to_owned()),
        Token::Assign,
        Token::Int("5".to_owned()),
        Token::SemiColon,
        Token::Let,
        Token::Ident("ten".to_owned()),
        Token::Assign,
        Token::Int("10".to_owned()),
        Token::SemiColon,
        Token::Let,
        Token::Ident("add".to_owned()),
        Token::Assign,
        Token::Function,
        Token::LParen,
        Token::Ident("x".to_owned()),
        Token::Comma,
        Token::Ident("y".to_owned()),
        Token::RParen,
        Token::LBrace,
        Token::Ident("x".to_owned()),
        Token::Plus,
        Token::Ident("y".to_owned()),
        Token::SemiColon,
        Token::RBrace,
        Token::SemiColon,
        Token::Let,
        Token::Ident("result".to_owned()),
        Token::Assign,
        Token::Ident("add".to_owned()),
        Token::LParen,
        Token::Ident("five".to_owned()),
        Token::Comma,
        Token::Ident("ten".to_owned()),
        Token::RParen,
        Token::SemiColon,
    ];

    let output = Tokenizer::new(input).collect::<Vec<_>>();
    assert_eq!(output, expected_output)
}

#[test]
fn test3() {
    let input = "
    !-/*5;
    5 < 10 > 5;
    ";

    let output = Tokenizer::new(input).collect::<Vec<_>>();

    let expected_output = vec![
        Token::Bang,
        Token::Minus,
        Token::Slash,
        Token::Asterisk,
        Token::Int("5".to_owned()),
        Token::SemiColon,
        Token::Int("5".to_owned()),
        Token::LessThan,
        Token::Int("10".to_owned()),
        Token::GreaterThan,
        Token::Int("5".to_owned()),
        Token::SemiColon,
    ];

    assert_eq!(output, expected_output)
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
        Token::If,
        Token::LParen,
        Token::Int("5".to_owned()),
        Token::LessThan,
        Token::Int("10".to_owned()),
        Token::RParen,
        Token::LBrace,
        Token::Return,
        Token::True,
        Token::SemiColon,
        Token::RBrace,
        Token::Else,
        Token::LBrace,
        Token::Return,
        Token::False,
        Token::SemiColon,
        Token::RBrace,
    ];

    assert_eq!(output, expected_output)
}

#[test]
fn test5() {
    let input = "10 == 10;
    10 != 9;";

    let output = Tokenizer::new(input).collect::<Vec<_>>();
    let expected_output = vec![
        Token::Int("10".to_owned()),
        Token::Equal,
        Token::Int("10".to_owned()),
        Token::SemiColon,
        Token::Int("10".to_owned()),
        Token::NotEqual,
        Token::Int("9".to_owned()),
        Token::SemiColon,
    ];

    assert_eq!(output, expected_output)
}
