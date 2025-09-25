use std::{collections::HashMap, ops::Range};

use crate::lexer::Token;
use thiserror::Error;

#[allow(unused)]
#[derive(Clone, Debug)]
pub enum Value<'a> {
    Object(HashMap<&'a str, Value<'a>>),
    Array(Vec<Value<'a>>),
    String(&'a str),
    Number(&'a str),
    Boolean(bool),
    Null,
}

pub struct Parser<'a> {
    input: &'a [u8],
    tokens: Vec<Token>,
    position: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, input: &'a [u8]) -> Self {
        Self {
            input,
            tokens,
            position: 0,
        }
    }

    pub fn parse(&mut self) -> Option<std::result::Result<Value<'a>, ParserError>> {
        let token = self.curr()?;

        if matches!(token, Token::Lcurl | Token::Lsquare) {
            self.parse_value()
        } else {
            Err(ParserError::InvalidStart {
                found: token.clone(),
            })
        }
        .into()
    }

    fn parse_value(&mut self) -> std::result::Result<Value<'a>, ParserError> {
        let Some(token) = self.curr() else {
            return Err(ParserError::UnexpectedEof);
        };

        match token {
            Token::Lcurl => self.parse_object(),
            Token::Lsquare => self.parse_array(),
            Token::String(range) => {
                let s = Self::read_str(self.input, range)?;
                self.next()?;
                Ok(Value::String(s))
            }
            Token::Number(range) => {
                let s = Self::read_str(self.input, range)?;
                self.next()?;
                Ok(Value::Number(s))
            }
            Token::True => {
                self.next()?;
                Ok(Value::Boolean(true))
            }
            Token::False => {
                self.next()?;
                Ok(Value::Boolean(false))
            }
            Token::Null => {
                self.next()?;
                Ok(Value::Null)
            }
            _ => Err(ParserError::InvalidValue {
                found: token.clone(),
            }),
        }
    }

    fn parse_object(&mut self) -> Result<'a> {
        let _ = self.next(); // consume lbrace
        let mut object: HashMap<&str, Value<'a>> = HashMap::new();

        while let Some(token) = self.curr() {
            if token == &Token::Rcurl {
                let _ = self.next()?;
                return Ok(Value::Object(object));
            }

            // key must be string
            let Some(Token::String(range)) = self.curr() else {
                return Err(ParserError::InvalidKey);
            };

            let key = Self::read_str(self.input, range)?;

            if object.contains_key(&key) {
                return Err(ParserError::DuplicateKey);
            };

            let _ = self.next(); // consume key

            let Ok(Token::Colon) = self.next() else {
                return Err(ParserError::MissingColon);
            };

            let value = self.parse_value()?;
            object.insert(key, value);

            match self.curr() {
                Some(Token::Comma) => {
                    if self.peek() == Some(&Token::Rcurl) {
                        return Err(ParserError::TrailingComma);
                    }

                    let _ = self.next()?;
                }
                Some(Token::Rcurl) => {
                    let _ = self.next()?;
                    return Ok(Value::Object(object));
                }
                other => {
                    return Err(ParserError::InvalidValue {
                        found: other.cloned().unwrap_or(Token::Null),
                    });
                }
            }
        }

        Err(ParserError::UnexpectedEof)
    }

    fn parse_array(&mut self) -> Result<'a> {
        let mut array = Vec::new();

        let _ = self.next(); // consume lsquare

        if self.curr() == Some(&Token::Rsquare) {
            let _ = self.next()?;
            return Ok(Value::Array(array));
        }

        while let Some(token) = self.curr() {
            if token == &Token::Rsquare {
                let _ = self.next()?;
                return Ok(Value::Array(array));
            }

            let value = self.parse_value()?;
            array.push(value);

            match self.curr() {
                Some(Token::Comma) => {
                    let _ = self.next()?;
                }
                Some(Token::Rsquare) => {
                    let _ = self.next()?;
                    return Ok(Value::Array(array));
                }
                _ => {
                    return Err(ParserError::InvalidArray);
                }
            }
        }

        Err(ParserError::UnexpectedEof)
    }

    fn next(&mut self) -> std::result::Result<&Token, ParserError> {
        if self.curr().is_some() {
            let token = self
                .tokens
                .get(self.position)
                .ok_or(ParserError::UnexpectedEof)?;

            self.position += 1;

            Ok(token)
        } else {
            Err(ParserError::UnexpectedEof)
        }
    }

    fn curr(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position + 1)
    }

    fn read_str<'b>(
        input: &'b [u8],
        range: &Range<usize>,
    ) -> std::result::Result<&'b str, ParserError> {
        std::str::from_utf8(&input[range.clone()]).map_err(|_| ParserError::NonUTF8String)
    }
}

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Json must start with an object or array, found {found:?}")]
    InvalidStart { found: Token },

    #[error(
        "Json values can only be an object, array, number, string, true, false, or null, found: {found:?}"
    )]
    InvalidValue { found: Token },

    #[error("Json keys must be followed by a colon")]
    MissingColon,

    #[error("Json strings must be valid UTF-8")]
    NonUTF8String,

    #[error("Unexpected end of input")]
    UnexpectedEof,

    #[error("Keys must be strings")]
    InvalidKey,

    #[error("Keys must be unique within an object")]
    DuplicateKey,

    #[error("Array value must either be terminated or followed by a comma")]
    InvalidArray,

    #[error("Trailing commas are not allowed")]
    TrailingComma,
}

type Result<'a> = std::result::Result<Value<'a>, ParserError>;

#[cfg(test)]
mod tests {
    use super::*;
    use ParserError::*;

    fn expect_success(input: &str) -> Value {
        let bytes = input.as_bytes();
        let mut lexer = crate::lexer::Lexer::new(bytes);
        let tokens = lexer.lex().expect("Lexing failed");

        let mut parser = Parser::new(tokens, bytes);
        let result = parser.parse();

        match result {
            Some(Ok(value)) => value,
            Some(Err(e)) => panic!("Expected success, but failed: {e}"),
            None => panic!("Missing result"),
        }
    }

    fn expect_failure(input: &str) -> ParserError {
        let bytes = input.as_bytes();
        let mut lexer = crate::lexer::Lexer::new(bytes);
        let tokens = lexer.lex().expect("Lexing failed");

        let mut parser = Parser::new(tokens, bytes);
        let result = parser.parse();

        match result {
            Some(Ok(_)) => panic!("Expected failure, but succeeded"),
            Some(Err(e)) => e,
            None => panic!("Missing result"),
        }
    }

    #[test]
    fn empty_object() {
        let value = expect_success("{}");

        let Value::Object(obj) = value else {
            panic!("Expected object, got {value:?}");
        };

        assert!(obj.is_empty());
    }

    #[test]
    fn empty_array() {
        let value = expect_success("[]");

        let Value::Array(arr) = value else {
            panic!("Expected array, got {value:?}");
        };

        assert!(arr.is_empty());
    }

    #[test]
    fn string_value() {
        let value = expect_success(r#"{"key": "value"}"#);

        let Value::Object(obj) = value else {
            panic!("Expected object, got {value:?}");
        };

        let Some(Value::String(s)) = obj.get("key") else {
            panic!("Expected string value");
        };

        assert_eq!(*s, "value");
    }

    #[test]
    fn number_value() {
        let value = expect_success(r#"{"num": 42}"#);

        let Value::Object(obj) = value else {
            panic!("Expected object, got {value:?}");
        };

        let Some(Value::Number(n)) = obj.get("num") else {
            panic!("Expected number value");
        };

        assert_eq!(*n, "42");
    }

    #[test]
    fn boolean_values() {
        [("true", true), ("false", false)]
            .iter()
            .for_each(|(input, expected)| {
                let input = format!(r#"{{"flag": {input}}}"#);
                let value = expect_success(&input);

                let Value::Object(obj) = value else {
                    panic!("Expected object, got {value:?}");
                };

                let Some(Value::Boolean(b)) = obj.get("flag") else {
                    panic!("Expected boolean value");
                };

                assert_eq!(*b, *expected);
            });
    }

    #[test]
    fn null_value() {
        let value = expect_success(r#"{"empty": null}"#);
        let Value::Object(obj) = value else {
            panic!("Expected object, got {value:?}");
        };

        let Some(Value::Null) = obj.get("empty") else {
            panic!("Expected null value");
        };
    }

    #[test]
    fn invalid_start() {
        let error = expect_failure(r#""invalid""#);
        assert!(matches!(error, InvalidStart { .. }));
    }

    #[test]
    fn duplicate_key() {
        let error = expect_failure(r#"{"key": 1, "key": 2}"#);
        assert!(matches!(error, DuplicateKey));
    }

    #[test]
    fn missing_colon() {
        let error = expect_failure(r#"{"key" "value"}"#);
        assert!(matches!(error, MissingColon));
    }

    #[test]
    fn invalid_key() {
        let error = expect_failure(r#"{123: "value"}"#);
        assert!(matches!(error, InvalidKey));
    }

    #[test]
    fn trailing_comma() {
        let error = expect_failure(r#"{"key": "value",}"#);
        assert!(matches!(error, TrailingComma));
    }

    #[test]
    fn missing_comma() {
        let error = expect_failure(r#"{"key1": "value1" "key2": "value2"}"#);
        assert!(matches!(error, InvalidValue { .. }));
    }
}
