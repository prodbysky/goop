mod config;
use colored::Colorize;

fn main() {
    let config = match config::Config::from_args(std::env::args()) {
        Some(c) => c,
        None => return,
    };
    let input: Vec<_> = match std::fs::read_to_string(&config.input_name) {
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
    }
    .chars()
    .collect();

    for t in Lexer::new(&input) {
        dbg!(t);
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a [char],
    offset: usize,
    line_beginning: usize,
}

#[derive(Debug, Clone)]
pub enum Error {
    UnexpectedChar(char)
}
#[derive(Debug, Clone)]
pub enum Token {
    Number(u64)
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    span: std::ops::Range<usize>,
    v: T,
}

pub type Span = std::ops::Range<usize>;

impl<'a> Lexer<'a> {
    pub fn new(src: &'a [char]) -> Self {
        Self {
            input: src,
            offset: 0,
            line_beginning: 0,
        }
    }

    fn skip_ws(&mut self) {
        while !self.finished() && self.input[self.offset].is_whitespace() {
            match self.input[self.offset] {
                ' ' | '\t' => self.offset += 1,
                '\n' => {
                    self.offset += 1;
                    self.line_beginning += self.offset;
                }
                _ => unreachable!(),
            }
        }
    }
    fn finished(&self) -> bool {
        self.offset >= self.input.len()
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Spanned<Token>, Spanned<Error>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.skip_ws();
        if self.finished() {
            return None;
        }
        match self.input[self.offset] {
            c if c.is_ascii_digit() => {
                todo!()
            }
            _ => 
        }
    }
}

fn usage(prog_name: &str) {
    eprintln!("  [{}]: Usage:", "Info".blue());
    eprintln!("  [{}]: ./{prog_name} <input.gp>", "Info".blue());
}
