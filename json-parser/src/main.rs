use std::fs;
use std::time::Instant;

use humansize::{DECIMAL, format_size};
use num_format::{Locale, ToFormattedString};
use parser::Parser;

mod lexer;
mod parser;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    let Some(path) = args.get(1) else {
        eprintln!("Usage: ./json-parser <file> --verbose");
        return;
    };

    let verbose = args
        .get(2)
        .is_some_and(|arg| arg == "--verbose" || arg == "-v");

    let input = match fs::read(path) {
        Ok(input) => input,
        Err(e) => {
            eprintln!("Error reading input: {e}");
            return;
        }
    };

    let mut lexer = lexer::Lexer::new(&input);

    let lex_start = Instant::now();
    let tokens = match lexer.lex() {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("{e}");
            return;
        }
    };
    let lex_end = lex_start.elapsed();

    let size = format_size(input.len(), DECIMAL);
    let lex_duration = lex_end.as_secs_f64();
    let count = tokens.len().to_formatted_string(&Locale::en);

    if verbose {
        tokens.iter().for_each(|t| t.print(&input));
        println!();
    }

    println!("File size: {size}");
    println!("Lex duration: {lex_duration}s");
    println!("Tokens: {count}");

    let mut parser = Parser::new(tokens, &input);
    let parse_start = Instant::now();
    let value = parser.parse();
    let parse_end = parse_start.elapsed();

    let Some(value) = value else {
        return;
    };

    let parsed = match value {
        Ok(value) => value,
        Err(e) => {
            println!("Outcome: invalid");
            eprintln!("{e}");
            return;
        }
    };

    let parse_duration = parse_end.as_secs_f64();
    println!("Outcome: valid");
    println!("Parse duration: {parse_duration}s");
    println!("Total duration: {}s", lex_duration + parse_duration);
}
