use std::ops::Range;
use thiserror::Error;

#[derive(Debug, PartialEq, Eq, Clone)]
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

    pub fn print(&self, input: &[u8]) {
        let s = match self {
            Token::Colon
            | Token::Comma
            | Token::Lcurl
            | Token::Rcurl
            | Token::Lsquare
            | Token::Rsquare
            | Token::Eof
            | Token::True
            | Token::False
            | Token::Null => format!("{self:?}"),
            Token::String(range) | Token::Number(range) => {
                let bytes = &input[range.clone()];
                let s = str::from_utf8(bytes).unwrap_or("invalid utf8");
                let ident = match self {
                    Token::String(_) => "String",
                    Token::Number(_) => "Number",
                    _ => unreachable!(),
                };
                format!("{ident}({s})")
            }
        };

        println!("{s}")
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
                    self.next()?;
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

                _ => {
                    self.next()?;
                    return Err(LexerErrorKind::InvalidNumber(
                        NumberError::InvalidNegative {
                            reason: INVALID_NEGATIVE_POSTFIX,
                        },
                    ));
                }
            }

            self.next()?;
        }

        let mut found_decimal = false;
        let mut found_exponent = false;

        while let Some(b) = self.curr() {
            match b {
                b'.' => {
                    if found_decimal {
                        return Err(LexerErrorKind::InvalidNumber(NumberError::InvalidDecimal {
                            reason: MULTIPLE_DECIMAL_POINTS,
                        }));
                    }

                    found_decimal = true;
                    self.next()?;

                    // dangling . or not followed by digit
                    if !self.curr().is_some_and(|b| b.is_ascii_digit()) {
                        return Err(LexerErrorKind::InvalidNumber(NumberError::InvalidDecimal {
                            reason: INVALID_DECIMAL_POSTFIX,
                        }));
                    }
                }
                b'e' | b'E' => {
                    if found_exponent {
                        return Err(LexerErrorKind::InvalidNumber(
                            NumberError::InvalidExponent {
                                reason: MULTIPLE_EXPONENTS,
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
                                    reason: INVALID_EXPONENT_POSTFIX,
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

        while let Some(b) = self.curr() {
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
                b'\n' => {
                    return Err(LexerErrorKind::InvalidString(StringError::UnescapedNewline));
                }
                _ => {
                    self.next()?;
                }
            }
        }

        Err(LexerErrorKind::InvalidString(StringError::Unterminated))
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

#[derive(Debug)]
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
    #[error("[invalid string] {0}")]
    InvalidString(StringError),
    #[error("[invalid number] {0}")]
    InvalidNumber(NumberError),
    #[error("[invalid token] {0}")]
    InvalidToken(u8),
    #[error("eof")]
    Eof,
}

#[derive(Error, Debug)]
pub enum StringError {
    #[error("string not terminated, missing \"")]
    Unterminated,
    #[error("string contains unescaped newline")]
    UnescapedNewline,
}

const MULTIPLE_DECIMAL_POINTS: &str = "multiple decimal points found";
const INVALID_DECIMAL_POSTFIX: &str = "decimal point must be followed by a digit";

const MULTIPLE_EXPONENTS: &str = "multiple exponents found";
const INVALID_EXPONENT_POSTFIX: &str = "exponent must be followed by '+' or '-' or a digit";

const INVALID_NEGATIVE_POSTFIX: &str = "'-' must be followed by a digit";

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

#[cfg(test)]
mod tests {
    use super::*;
    use LexerErrorKind::*;

    fn expect_success(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(input.as_bytes());
        lexer.lex().expect("expected lexer to succeed")
    }

    fn expect_error(input: &str) -> LexerError {
        let mut lexer = Lexer::new(input.as_bytes());
        lexer.lex().expect_err("expected lexer to error")
    }

    #[test]
    fn punctuation() {
        let tokens = expect_success(":,{}[]");
        let expected = [
            Token::Colon,
            Token::Comma,
            Token::Lcurl,
            Token::Rcurl,
            Token::Lsquare,
            Token::Rsquare,
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn string() {
        let input = "\"hello world\"";
        let tokens = expect_success(input);

        // quotes should not be included in range
        let range = 1..input.len() - 1;
        let expected = [Token::String(range)];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn number() {
        let input = "-1.2e+3";
        let tokens = expect_success(input);

        let range = 0..input.len();
        let expected = [Token::Number(range)];

        assert_eq!(tokens, expected)
    }

    #[test]
    fn literals() {
        [
            ("true", [Token::True]),
            ("false", [Token::False]),
            ("null", [Token::Null]),
        ]
        .iter()
        .for_each(|(input, expected)| {
            let tokens = expect_success(input);

            assert_eq!(tokens, expected);
        });
    }

    #[test]
    fn empty_input() {
        let tokens = expect_success("");

        assert_eq!(tokens, []);
    }

    #[test]
    fn rejects_unterminated_string() {
        let LexerError { kind, line, col } = expect_error("\"hello");

        assert!(matches!(kind, InvalidString(StringError::Unterminated)));
        assert_eq!(line, 1);
        assert_eq!(col, 7);
    }

    #[test]
    fn rejects_string_with_unescaped_newline() {
        let LexerError { kind, line, col } = expect_error("\"hello\nworld\"");

        assert!(matches!(kind, InvalidString(StringError::UnescapedNewline)));
        assert_eq!(line, 1);
        assert_eq!(col, 7);
    }

    #[test]
    fn rejects_number_with_leading_zero() {
        let LexerError { kind, line, col } = expect_error("01");

        assert!(matches!(kind, InvalidNumber(NumberError::LeadingZero)));
        assert_eq!(line, 1);
        assert_eq!(col, 2);
    }

    #[test]
    fn rejects_decimal_with_multiple_decimal_points() {
        let LexerError { kind, line, col } = expect_error("1.2.3");

        assert!(matches!(
            kind,
            InvalidNumber(NumberError::InvalidDecimal {
                reason: MULTIPLE_DECIMAL_POINTS
            })
        ));
        assert_eq!(line, 1);
        assert_eq!(col, 4);
    }

    #[test]
    fn rejects_decimal_with_invalid_postfix() {
        let LexerError { kind, line, col } = expect_error("1.");

        assert!(matches!(
            kind,
            InvalidNumber(NumberError::InvalidDecimal {
                reason: INVALID_DECIMAL_POSTFIX
            })
        ));
        assert_eq!(line, 1);
        assert_eq!(col, 3);
    }

    #[test]
    fn rejects_exponent_with_invalid_postfix() {
        let LexerError { kind, line, col } = expect_error("1e");

        assert!(matches!(
            kind,
            InvalidNumber(NumberError::InvalidExponent {
                reason: INVALID_EXPONENT_POSTFIX
            })
        ));
        assert_eq!(line, 1);
        assert_eq!(col, 3);
    }

    #[test]
    fn rejects_negative_without_following_digit() {
        let LexerError { kind, line, col } = expect_error("-a");

        assert!(matches!(
            kind,
            InvalidNumber(NumberError::InvalidNegative {
                reason: INVALID_NEGATIVE_POSTFIX
            })
        ));
        assert_eq!(line, 1);
        assert_eq!(col, 2);
    }
}

type Result<T> = std::result::Result<T, LexerErrorKind>;
