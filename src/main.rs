mod config;
mod lexer;

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

    for t in lexer::Lexer::new(&input_chars) {
        match t {
            Ok(t) => {
                dbg!(t);
            }
            Err(e) => {
                match &e.v {
                    lexer::Error::UnexpectedChar(c) => {
                        eprintln!(
                            "[{}]\n  Unexpected char found during lexing `{c}`",
                            "Error".red()
                        );
                    }
                    lexer::Error::InvalidNumberLiteral => {
                        eprintln!(
                            "[{}]\n  Invalid number literal found during lexing",
                            "Error".red()
                        );
                        eprintln!(
                            "[{}]\n  Numbers must be separated by whitespace or other characters that are not a..z etc.\n  For example this `123 123` is two valid number literals\n  `123a 123a` is not.",
                            "Note".green()
                        )
                    }
                }
                let line_offset = e.offset - e.line_beginning;
                let line_end =
                    &input[e.line_beginning..].find('\n').unwrap_or(input.len()) + e.offset;
                let line = &input[e.line_beginning..line_end];

                let line_count = {
                    let upto = &input[0..e.line_beginning];
                    upto.chars().filter(|c| *c == '\n').count() + 1
                };

                let prefix = format!(
                    "./{}:{}:{}",
                    &config.input_name,
                    line_count,
                    line_offset + 1
                );
                eprintln!("{prefix}\n{}", line);
                eprintln!(
                    "{}{}",
                    " ".repeat(e.offset - e.line_beginning),
                    "^".repeat(e.len)
                );
                break;
            }
        }
    }
}

#[derive(Debug)]
pub struct Parser {}

fn usage(prog_name: &str) {
    eprintln!("  [{}]: Usage:", "Info".blue());
    eprintln!("  [{}]: ./{prog_name} <input.gp>", "Info".blue());
}

pub type Span = std::ops::Range<usize>;
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    offset: usize,
    len: usize,
    line_beginning: usize,
    v: T,
}
