use json_parser::{Lexer, Parser};
use std::fs;
use std::path::Path;

fn validate(path: &Path) -> Result<(), String> {
    let input = fs::read(&path).expect("Error reading input");

    let mut lexer = Lexer::new(&input);
    let tokens = lexer.lex().map_err(|e| format!("[lexer] {e}"))?;

    let mut parser = Parser::new(tokens, &input);
    match parser.parse() {
        Some(Ok(_)) => Ok(()),
        Some(Err(e)) => Err(format!("[parser]: {e}")),
        None => Err("Empty input".to_string()),
    }
}

fn assert_valid(path: &str) {
    let path = Path::new(path);

    if let Err(e) = validate(path) {
        panic!("Expected valid JSON, but got error: {}", e);
    }
}

fn assert_invalid(path: &str) {
    let path = Path::new(path);

    if validate(path).is_ok() {
        panic!("Expected invalid JSON, but it was valid");
    }
}

#[cfg(test)]
mod step1_tests {
    use super::*;

    #[test]
    fn test_step1_valid() {
        assert_valid("data/tests/step1/valid.json");
    }

    #[test]
    fn test_step1_invalid() {
        assert_invalid("data/tests/step1/invalid.json");
    }
}

#[cfg(test)]
mod step2_tests {
    use super::*;

    #[test]
    fn test_step2_valid() {
        assert_valid("data/tests/step2/valid.json");
    }

    #[test]
    fn test_step2_valid2() {
        assert_valid("data/tests/step2/valid2.json");
    }

    #[test]
    fn test_step2_invalid() {
        assert_invalid("data/tests/step2/invalid.json");
    }

    #[test]
    fn test_step2_invalid2() {
        assert_invalid("data/tests/step2/invalid2.json");
    }
}

#[cfg(test)]
mod step3_tests {
    use super::*;

    #[test]
    fn test_step3_valid() {
        assert_valid("data/tests/step3/valid.json");
    }

    #[test]
    fn test_step3_invalid() {
        assert_invalid("data/tests/step3/invalid.json");
    }
}

#[cfg(test)]
mod step4_tests {
    use super::*;

    #[test]
    fn test_step4_valid() {
        assert_valid("data/tests/step4/valid.json");
    }

    #[test]
    fn test_step4_valid2() {
        assert_valid("data/tests/step4/valid2.json");
    }

    #[test]
    fn test_step4_invalid() {
        assert_invalid("data/tests/step4/invalid.json");
    }
}
