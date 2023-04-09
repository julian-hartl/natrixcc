use std::fmt::{Display, Formatter, write};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    // Literals
    Number(i64),
    // Operators
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equals,
    Ampersand,
    Pipe,
    Caret,
    DoubleAsterisk,
    Tilde,
    GreaterThan,
    LessThan,
    GreaterThanEquals,
    LessThanEquals,
    EqualsEquals,
    BangEquals,
    // Keywords
    Let,
    If,
    Else,
    True,
    False,
    While,
    Func,
    Return,
    // Separators
    LeftParen,
    RightParen,
    OpenBrace,
    CloseBrace,
    Comma,
    // Other
    Bad,
    Whitespace,
    Identifier,
    Eof,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Number(_) => write!(f, "Number"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::LeftParen => write!(f, "("),
            TokenKind::RightParen => write!(f, ")"),
            TokenKind::Bad => write!(f, "Bad"),
            TokenKind::Whitespace => write!(f, "Whitespace"),
            TokenKind::Eof => write!(f, "Eof"),
            TokenKind::Let => write!(f, "Let"),
            TokenKind::Identifier => write!(f, "Identifier"),
            TokenKind::Equals => write!(f, "="),
            TokenKind::Ampersand => write!(f, "&"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::Caret => write!(f, "^"),
            TokenKind::DoubleAsterisk => write!(f, "**"),
            TokenKind::Tilde => write!(f, "~"),
            TokenKind::If => write!(f, "If"),
            TokenKind::Else => write!(f, "Else"),
            TokenKind::GreaterThan => write!(f, ">"),
            TokenKind::LessThan => write!(f, "<"),
            TokenKind::GreaterThanEquals => write!(f, ">="),
            TokenKind::LessThanEquals => write!(f, "<="),
            TokenKind::EqualsEquals => write!(f, "=="),
            TokenKind::BangEquals => write!(f, "!="),
            TokenKind::OpenBrace => write!(f, "{{"),
            TokenKind::CloseBrace => write!(f, "}}"),
            TokenKind::True => write!(f, "True"),
            TokenKind::False => write!(f, "False"),
            TokenKind::While => write!(f, "While"),
            TokenKind::Func => write!(f, "Func"),
            TokenKind::Return => write!(f, "Return"),
            TokenKind::Comma => write!(f, "Comma"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TextSpan {
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) literal: String,
}

impl TextSpan {
    pub fn new(start: usize, end: usize, literal: String) -> Self {
        Self { start, end, literal }
    }

    pub fn length(&self) -> usize {
        self.end - self.start
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) span: TextSpan,
}

impl Token {
    pub fn new(kind: TokenKind, span: TextSpan) -> Self {
        Self { kind, span }
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    current_pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, current_pos: 0 }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if self.current_pos == self.input.len() {
            let eof_char: char = '\0';
            self.current_pos += 1;
            return Some(Token::new(
                TokenKind::Eof,
                TextSpan::new(0, 0, eof_char.to_string()),
            ));
        }
        let c = self.current_char();
        return c.map(|c| {
            let start = self.current_pos;
            let mut kind = TokenKind::Bad;
            if Self::is_number_start(&c) {
                let number: i64 = self.consume_number();
                kind = TokenKind::Number(number);
            } else if Self::is_whitespace(&c){
                self.consume();
                kind = TokenKind::Whitespace;
            } else if Self::is_identifier_start(&c){
                let identifier = self.consume_identifier();
                kind = match identifier.as_str() {
                    "let" => TokenKind::Let,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "while" => TokenKind::While,
                    "func" => TokenKind::Func,
                    "return" => TokenKind::Return,
                    _ => TokenKind::Identifier,
                }

            } else  {
                kind = self.consume_punctuation();
            }

            let end = self.current_pos;
            let literal = self.input[start..end].to_string();
            let span = TextSpan::new(start, end, literal);
            Token::new(kind, span)
        });
    }

    fn consume_punctuation(&mut self) -> TokenKind {
        let c = self.consume().unwrap();
        match c {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => {
                self.lex_potential_double_char_operator('*', TokenKind::Asterisk, TokenKind::DoubleAsterisk)
            },
            '/' => TokenKind::Slash,
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '=' => {
                self.lex_potential_double_char_operator('=', TokenKind::Equals, TokenKind::EqualsEquals)
            },
            '&' => TokenKind::Ampersand,
            '|' => TokenKind::Pipe,
            '^' => TokenKind::Caret,
            '~' => TokenKind::Tilde,
            '>' => {
                self.lex_potential_double_char_operator('=', TokenKind::GreaterThan, TokenKind::GreaterThanEquals)
            },
            '<' => {
                self.lex_potential_double_char_operator('=', TokenKind::LessThan, TokenKind::LessThanEquals)
            },
            '!' => {
                self.lex_potential_double_char_operator('=', TokenKind::Bad, TokenKind::BangEquals)
            },
            '{' => {
                TokenKind::OpenBrace
            },
            '}' => {
                TokenKind::CloseBrace
            },
            ',' => {
                TokenKind::Comma
            },

            _ => TokenKind::Bad,
        }
    }

    fn lex_potential_double_char_operator(&mut self, expected: char, one_char_kind: TokenKind, double_char_kind: TokenKind) -> TokenKind {
        if let Some(next) = self.current_char() {
            if next == expected {
                self.consume();
                double_char_kind
            } else {
                one_char_kind
            }
        } else {
            one_char_kind
        }
    }

    fn is_number_start(c: &char) -> bool {
        c.is_digit(10)
    }

    fn is_identifier_start(c: &char) -> bool {
        c.is_alphabetic()
    }

    fn is_whitespace(c: &char) -> bool {
        c.is_whitespace()
    }

    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.current_pos)
    }

    fn consume(&mut self) -> Option<char> {
        if self.current_pos >= self.input.len() {
            return None;
        }
        let c = self.current_char();
        self.current_pos += 1;

        c
    }

    fn consume_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some(c) = self.current_char() {
            if Self::is_identifier_start(&c) {
                self.consume().unwrap();
                identifier.push(c);
            } else {
                break;
            }
        }
        identifier
    }

    fn consume_number(&mut self) -> i64 {
        let mut number: i64 = 0;
        while let Some(c) = self.current_char() {
            if c.is_digit(10) {
                self.consume().unwrap();
                number = number * 10 + c.to_digit(10).unwrap() as i64;
            } else {
                break;
            }
        }
        number
    }
}