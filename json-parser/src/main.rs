use std::fs;
use std::time::Instant;

use humansize::{DECIMAL, format_size};
use num_format::{Locale, ToFormattedString};

mod lexer;
mod types;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    let Some(path) = args.get(1) else {
        eprintln!("Usage: ./json-parser <file>");
        return;
    };

    let input = fs::read(path).expect("Failed to read input");
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

    if false {
        for token in &tokens {
            use lexer::Token::*;
            match token {
                String(range) | Number(range) => {
                    let s = str::from_utf8(&input[range.clone()]).unwrap_or("invalid utf-8");
                    let token = match token {
                        String(_) => "String",
                        Number(_) => "Number",
                        _ => unreachable!(),
                    };
                    println!("{token}({s})");
                }
                other => println!("{other:?}"),
            }
        }
    }
    let size = format_size(input.len(), DECIMAL);
    let duration = end.as_secs_f64();
    let count = tokens.len().to_formatted_string(&Locale::en);

    println!("{}", "-".repeat(30));
    println!("Statistics");
    println!("File size: {size}");
    println!("Duration: {duration}s");
    println!("Tokens: {count}");
}
