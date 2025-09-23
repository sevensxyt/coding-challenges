use std::{collections::HashMap, ops::Range};

use crate::lexer::Token;
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum Value {
    Object(HashMap<String, Value>),
    Array(Vec<Value>),
    String(String),
    Number(String),
    Boolean(bool),
    Null,
}

pub struct Parser<'a> {
    input: &'a [u8],
    tokens: Vec<Token>,
    position: usize,
}

const VALUES: [Token; 5] = [
    Token::Number(0..0),
    Token::String(0..0),
    Token::True,
    Token::False,
    Token::Null,
];

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, input: &'a [u8]) -> Self {
        Self {
            input,
            tokens,
            position: 0,
        }
    }

    pub fn parse(&mut self) -> Option<std::result::Result<Value, ParserError>> {
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

    fn parse_value(&mut self) -> std::result::Result<Value, ParserError> {
        let Some(token) = self.curr() else {
            return Err(ParserError::UnexpectedEof);
        };

        match token {
            Token::Lcurl => self.parse_object(),
            Token::Lsquare => self.parse_array(),
            Token::String(range) => {
                let s = self.read_string(range)?;
                self.next()?;
                Ok(Value::String(s))
            }
            Token::Number(range) => {
                let s = self.read_string(range)?;
                self.next()?;
                Ok(Value::String(s))
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
            _ => Err(ParserError::InvalidValue),
        }
    }

    fn parse_object(&mut self) -> Result {
        let _ = self.next(); // consume lbrace

        // key must be string
        let Some(Token::String(range)) = self.curr() else {
            return Err(ParserError::NonStringKey);
        };

        let mut object: HashMap<String, Value> = HashMap::new();
        let key = self.read_string(range)?;

        if object.contains_key(&key) {
            return Err(ParserError::DuplicateKey);
        };

        let _ = self.next(); // consume key

        let Ok(Token::Colon) = self.next() else {
            return Err(ParserError::MissingColon);
        };

        let value = self.parse_value()?;
        object.insert(key, value);

        Ok(Value::Object(object))
    }

    fn parse_array(&mut self) -> Result {
        let mut array = Vec::new();

        let _ = self.next(); // consume lsquare

        if self.peek() == Some(&Token::Rsquare) {
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

    fn read_string(&self, range: &Range<usize>) -> std::result::Result<String, ParserError> {
        String::from_utf8(self.input[range.clone()].to_vec())
            .map_err(|_| ParserError::NonUTF8String)
    }
}

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Json must start with an object or array, found {found:?}")]
    InvalidStart { found: Token },

    #[error("Json values can only be an object, array, number, string, true, false, or null")]
    InvalidValue,

    #[error("Json keys must be followed by a colon")]
    MissingColon,

    #[error("Json strings must be valid UTF-8")]
    NonUTF8String,

    #[error("Unexpected end of input")]
    UnexpectedEof,

    #[error("Keys must be strings")]
    NonStringKey,

    #[error("Keys must be unique within an object")]
    DuplicateKey,

    #[error("Array value must either be terminated or followed by a comma")]
    InvalidArray,
}

type Result = std::result::Result<Value, ParserError>;
