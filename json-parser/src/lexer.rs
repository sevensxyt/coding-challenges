use std::ops::Range;
use thiserror::Error;

#[derive(Debug)]
pub enum Token {
    Colon,   // :
    Comma,   // ,
    Lcurl,   // {
    Rcurl,   // }
    Lsquare, // [
    Rsquare, // [
    Eof,

    String(Range<usize>),
    Number(Range<usize>),
    True,
    False,
    Null,
}

impl Token {
    pub fn from_byte(b: u8) -> Option<Self> {
        Some(match b {
            b':' => Self::Colon,
            b',' => Self::Comma,
            b'{' => Self::Lcurl,
            b'}' => Self::Rcurl,
            b'[' => Self::Lsquare,
            b']' => Self::Rsquare,
            _ => return None,
        })
    }
}

pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [u8]) -> Self {
        Self {
            input,
            pos: 0,
            line: 1,
            col: 1,
        }
    }

    pub fn lex(&mut self) -> std::result::Result<Vec<Token>, LexerError> {
        let mut tokens = vec![];

        loop {
            match self.next_token() {
                Err(e) => {
                    return Err(LexerError {
                        kind: e,
                        line: self.line,
                        col: self.col,
                    });
                }
                Ok(Token::Eof) => return Ok(tokens),
                Ok(token) => tokens.push(token),
            }
        }
    }

    fn next_token(&mut self) -> Result<Token> {
        use Token::*;

        self.skip_whitespace();

        let Some(b) = self.curr() else {
            return Ok(Eof);
        };

        if let Some(token) = Token::from_byte(b) {
            self.next()?;
            return Ok(token);
        }

        let token = match b {
            b'"' => self.read_string()?,
            b't' => self.read_literal("true")?,
            b'f' => self.read_literal("false")?,
            b'n' => self.read_literal("null")?,
            byte if byte == b'-' || byte.is_ascii_digit() => self.read_number()?,
            byte => return Err(LexerErrorKind::InvalidToken(byte)),
        };

        Ok(token)
    }

    fn read_literal(&mut self, keyword: &str) -> Result<Token> {
        let start = self.pos;
        let end = start + keyword.len();

        if self.input.get(start..end) == Some(keyword.as_bytes()) {
            self.pos += keyword.len();

            match keyword {
                "true" => Ok(Token::True),
                "false" => Ok(Token::False),
                "null" => Ok(Token::Null),
                _ => Err(LexerErrorKind::InvalidToken(keyword.as_bytes()[0])),
            }
        } else {
            Err(LexerErrorKind::InvalidToken(keyword.as_bytes()[0]))
        }
    }

    fn read_number(&mut self) -> Result<Token> {
        let start = self.pos;

        if self.curr() == Some(b'0') {
            match self.peek() {
                Some(b'.') | Some(b'e') | Some(b'E') => (),
                Some(byte) if byte.is_ascii_digit() => {
                    return Err(LexerErrorKind::InvalidNumber(NumberError::LeadingZero));
                }
                _ => {
                    self.next()?;
                    let end = self.pos;
                    return Ok(Token::Number(start..end));
                }
            }
        }

        if self.curr() == Some(b'-') {
            match self.peek() {
                Some(b) if b.is_ascii_digit() => (),
                Some(b'0') => {
                    return Err(LexerErrorKind::InvalidNumber(NumberError::LeadingZero));
                }
                None => {
                    return Err(LexerErrorKind::InvalidNumber(
                        NumberError::InvalidNegative {
                            reason: "'-' cannot be the last character",
                        },
                    ));
                }
                _ => {
                    return Err(LexerErrorKind::InvalidNumber(
                        NumberError::InvalidNegative {
                            reason: "'-' must be followed by a digit",
                        },
                    ));
                }
            }

            self.next()?;
        }

        let mut found_decimal = false;
        let mut found_exponent = false;
        loop {
            let Some(b) = self.curr() else {
                break;
            };

            match b {
                b'.' => {
                    if found_decimal {
                        return Err(LexerErrorKind::InvalidNumber(NumberError::InvalidDecimal {
                            reason: "multiple decimal points found",
                        }));
                    }

                    found_decimal = true;
                    self.next()?;

                    if !self.curr().is_some_and(|b| b.is_ascii_digit()) {
                        return Err(LexerErrorKind::InvalidNumber(NumberError::InvalidDecimal {
                            reason: "decimal point must be followed by a digit",
                        }));
                    }
                }
                b'e' | b'E' => {
                    if found_exponent {
                        return Err(LexerErrorKind::InvalidNumber(
                            NumberError::InvalidExponent {
                                reason: "multiple exponents found",
                            },
                        ));
                    }

                    found_exponent = true;
                    self.next()?;

                    match self.curr() {
                        Some(b'+') | Some(b'-') => (),
                        Some(byte) if byte.is_ascii_digit() => (),
                        _ => {
                            return Err(LexerErrorKind::InvalidNumber(
                                NumberError::InvalidExponent {
                                    reason: "exponent must be followed by '+' or '-' or a digit",
                                },
                            ));
                        }
                    }
                }
                byte if byte.is_ascii_digit() => (),
                _ => break,
            };

            self.next()?;
        }

        let end = self.pos;
        Ok(Token::Number(start..end))
    }

    fn read_string(&mut self) -> Result<Token> {
        // consume starting quote
        self.next()?;

        let start = self.pos;

        loop {
            let Some(b) = self.curr() else {
                return Err(LexerErrorKind::InvalidString(StringError::Unterminated));
            };

            match b {
                b'"' => {
                    let end = self.pos;
                    // consume closing quote
                    self.next()?;
                    return Ok(Token::String(start..end));
                }
                b'\\' => {
                    self.next()?;
                    self.next()?;
                }
                _ => {
                    self.next()?;
                }
            }
        }
    }

    fn next(&mut self) -> Result<u8> {
        let Some(b) = self.curr() else {
            return Err(LexerErrorKind::Eof);
        };

        self.pos += 1;

        if b == b'\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }

        Ok(b)
    }

    fn curr(&self) -> Option<u8> {
        self.input.get(self.pos).copied()
    }

    fn peek(&self) -> Option<u8> {
        if self.pos + 1 < self.input.len() {
            Some(self.input[self.pos + 1])
        } else {
            None
        }
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len() && self.curr().is_some_and(|b| b.is_ascii_whitespace()) {
            self.next().unwrap();
        }
    }
}

pub struct LexerError {
    kind: LexerErrorKind,
    line: usize,
    col: usize,
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Error at line {}, col {}: {}",
            self.line, self.col, self.kind
        )
    }
}

#[derive(Debug, Error)]
pub enum LexerErrorKind {
    #[error("invalid string: {0}")]
    InvalidString(StringError),
    #[error("invalid number: {0}")]
    InvalidNumber(NumberError),
    #[error("invalid token: {0}")]
    InvalidToken(u8),
    #[error("eof")]
    Eof,
}

#[derive(Error, Debug)]
pub enum StringError {
    #[error("string not terminated, missing \"")]
    Unterminated,
}

#[derive(Error, Debug)]
pub enum NumberError {
    #[error("{reason}")]
    InvalidDecimal { reason: &'static str },

    #[error("{reason}")]
    InvalidExponent { reason: &'static str },

    #[error("numbers cannot have leading zeros")]
    LeadingZero,

    #[error("{reason}")]
    InvalidNegative { reason: &'static str },
}

type Result<T> = std::result::Result<T, LexerErrorKind>;
