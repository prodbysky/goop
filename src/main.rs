mod config;
mod lexer;
mod parser;

use colored::Colorize;

fn main() {
    let config = match config::Config::from_args(std::env::args()) {
        Some(c) => c,
        None => return,
    };
    let input = match std::fs::read_to_string(&config.input_name) {
        Ok(i) => i,
        Err(e) => {
            eprintln!(
                "[{}]: Failed to read {input_name}: {e}",
                "Error".red(),
                input_name = config.input_name,
            );
            usage(&config.program_name);
            return;
        }
    };

    let input_chars: Vec<_> = input.chars().collect();

    let lexer = lexer::Lexer::new(&input_chars);
    let elements: Vec<_> = lexer.collect();
    let lexer_errors: Vec<_> = elements
        .iter()
        .filter_map(|e| match e {
            Err(e) => Some(e),
            Ok(_) => None,
        })
        .collect();

    for e in &lexer_errors {
        eprintln!("{}", e.v);
        display_diagnostic_info(&input, &config.input_name, e);
    }

    if !lexer_errors.is_empty() {
        return;
    }
    let tokens: Vec<_> = elements
        .iter()
        .filter_map(|e| match e {
            Err(_) => None,
            Ok(t) => Some(t.clone()),
        })
        .collect();

    let parser = parser::Parser::new(&tokens);
    let (exprs, parser_errors) = parser.parse();
    for e in &parser_errors {
        eprintln!("{}", e.v);
        display_diagnostic_info(&input, &config.input_name, e);
    }
    if !parser_errors.is_empty() {
        return;
    }
    for e in exprs {
        dbg!(e);
    }
}

fn display_diagnostic_info<T>(input: &str, input_name: &str, e: &Spanned<T>) {
    let line_offset = e.offset - e.line_beginning;
    let line_end = input[e.line_beginning..].find('\n').unwrap_or(input.len());
    let line = &input[e.line_beginning..line_end];

    let line_count = {
        let upto = &input[0..e.line_beginning];
        upto.chars().filter(|c| *c == '\n').count() + 1
    };

    let prefix = format!("  ./{}:{}:{}", input_name, line_count, line_offset + 1);
    eprintln!("{prefix}\n{}", line);
    eprintln!(
        "{}{}",
        " ".repeat(e.offset - e.line_beginning),
        "^".repeat(e.len)
    );
}

fn usage(prog_name: &str) {
    eprintln!("  [{}]: Usage:", "Info".blue());
    eprintln!("  [{}]: ./{prog_name} <input.gp>", "Info".blue());
}

pub type Span = std::ops::Range<usize>;
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub offset: usize,
    pub len: usize,
    pub line_beginning: usize,
    pub v: T,
}
