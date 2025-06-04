mod config;
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

    for t in Lexer::new(&input_chars) {
        match t {
            Ok(t) => {}
            Err(e) => {
                match &e.v {
                    Error::UnexpectedChar(c) => {
                        eprintln!("[{}]: Unexpected char found during lexing", "Error".red());
                    }
                }
                let line_offset = e.offset - e.line_beginning;
                let line_end = &input[e.line_beginning..].find('\n').unwrap_or(input.len());
                let line = &input[e.line_beginning..*line_end];
                let prefix = format!("./{}:{}:{}", &config.input_name, 0, line_offset);
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
pub struct Lexer<'a> {
    input: &'a [char],
    offset: usize,
    line_beginning: usize,
}

#[derive(Debug, Clone)]
pub enum Error {
    UnexpectedChar(char),
}
#[derive(Debug, Clone)]
pub enum Token {
    Number(u64),
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    offset: usize,
    len: usize,
    line_beginning: usize,
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
            c => Some(Err(Spanned {
                offset: self.offset,
                line_beginning: self.line_beginning,
                len: 1,
                v: Error::UnexpectedChar(c),
            })),
        }
    }
}

fn usage(prog_name: &str) {
    eprintln!("  [{}]: Usage:", "Info".blue());
    eprintln!("  [{}]: ./{prog_name} <input.gp>", "Info".blue());
}
