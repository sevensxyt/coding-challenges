use std::fs;
use std::time::Instant;

use humansize::{DECIMAL, format_size};
use num_format::{Locale, ToFormattedString};

mod lexer;
mod parser;
mod types;

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

    let start = Instant::now();
    let tokens = match lexer.lex() {
        Ok(tokens) => tokens,
        Err(e) => {
            eprintln!("{e}");
            return;
        }
    };
    let end = start.elapsed();

    let size = format_size(input.len(), DECIMAL);
    let duration = end.as_secs_f64();
    let count = tokens.len().to_formatted_string(&Locale::en);

    if verbose {
        tokens.iter().for_each(|t| t.print(&input));
        println!();
    }

    println!("File size: {size}");
    println!("Duration: {duration}s");
    println!("Tokens: {count}");
}
