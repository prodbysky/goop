mod codegen;
mod config;
mod ir;
mod lexer;
mod logging;
mod parser;

use clap::Parser;
use colored::Colorize;

fn main() -> Result<(), ()> {
    let args = config::Args::parse();
    let mut objects = vec![];
    let pre = std::time::Instant::now();
    for name in &args.input {
        let input = match std::fs::read_to_string(name) {
            Ok(i) => i,
            Err(e) => {
                eprintln!("[{}]: Failed to read {name}: {e}", "Error".red(),);
                return Err(());
            }
        };
        println!("[{}]: {name}", "Build".purple());

        let program = parse_source(&input, name)?;

        let module = match ir::Module::from_ast(program) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("{}", e.v);
                display_diagnostic_info(&input, name, &e);
                return Err(());
            }
        };

        if args.dump_ir {
            println!("{module}")
        }

        let no_ext = std::path::Path::new(name)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or(name);

        // generate object into this file
        codegen::gccjit::generate_module(module, no_ext);
        objects.push(format!("{no_ext}.o"));
    }

    std::process::Command::new("gcc")
        .arg("-o")
        .arg(&args.output)
        .args(&objects)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    objects
        .iter()
        .for_each(|o| std::fs::remove_file(o).unwrap());

    println!(
        "[{}]:  Compilation took: {:.2?}",
        "Info".green(),
        pre.elapsed()
    );
    Ok(())
}

fn parse_source(input: &str, name: &str) -> Result<parser::Module, ()> {
    let tokens: Vec<_> = match lexer::Lexer::new(&input.chars().collect::<Vec<_>>()).lex() {
        Ok(ts) => ts,
        Err(e) => {
            eprintln!("{}", e.v);
            display_diagnostic_info(input, name, &e);
            return Err(());
        }
    };

    let program = match parser::Parser::new(&tokens).parse() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{}", e.v);
            display_diagnostic_info(input, name, &e);
            return Err(());
        }
    };
    Ok(program)
}

fn display_diagnostic_info<T: std::fmt::Debug>(input: &str, input_name: &str, e: &Spanned<T>) {
    let line_begin = input[0..e.begin()].rfind('\n').map(|i| i + 1).unwrap_or(0);
    let line_end = input[line_begin..]
        .find('\n')
        .map(|i| i + line_begin)
        .unwrap_or(input.len());
    let line = &input[line_begin..line_end];

    println!(
        "./{}:{}:{}",
        input_name,
        &input[0..e.begin()].chars().filter(|c| *c == '\n').count() + 1,
        &input[line_begin..e.begin()].chars().count() + 1
    );
    println!("{line}");
    let begin_of_bad_place = e.begin() - line_begin;

    let len = e.end() - e.begin();
    println!("{}{}", " ".repeat(begin_of_bad_place), "^".repeat(len));
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Span {
    begin: usize,
    end: usize,
}

impl Span {
    pub fn len(&self) -> usize {
        assert!(self.begin <= self.end);
        self.end - self.begin
    }

    pub fn new(begin: usize, end: usize) -> Self {
        assert!(begin <= end);
        Self { begin, end }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    s: Span,
    v: T,
}

impl<T> Spanned<T> {
    pub fn new(v: T, s: Span) -> Self {
        Self { v, s }
    }

    pub fn inner(&self) -> &T {
        &self.v
    }

    pub fn span(&self) -> &Span {
        &self.s
    }

    pub fn begin(&self) -> usize {
        self.span().begin
    }

    pub fn end(&self) -> usize {
        self.span().end
    }

    pub fn map<U>(self, f: fn(Spanned<T>) -> U) -> Spanned<U> {
        Spanned {
            s: self.s,
            v: f(self),
        }
    }
}
