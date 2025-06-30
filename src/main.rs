mod codegen;
mod ir;
mod config;
mod lexer;
mod logging;
mod parser;

use clap::Parser;
use colored::Colorize;

fn main() -> Result<(), ()> {
    let args = config::Args::parse();
    let mut objects = vec![];
    let pre = std::time::Instant::now();
    for name in args.input {
        let input = match std::fs::read_to_string(&name) {
            Ok(i) => i,
            Err(e) => {
                eprintln!(
                    "[{}]: Failed to read {name}: {e}",
                    "Error".red(),
                );
                return Err(());
            }
        };
        println!("[{}]: {name}", "Build".purple());

        let program = parse_source(&input, &name)?;

        let module = match ir::Module::from_ast(program) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("{}", e.v);
                display_diagnostic_info(&input, &name, &e);
                return Err(());
            }
        };

        let no_ext = &name[..name.len() - 3];

        codegen::inkwell::generate_code(module, no_ext);
        objects.push(format!("{no_ext}.o"));
    }
    std::process::Command::new("clang").args(&objects).arg("-o").arg(&args.output).spawn().unwrap().wait().unwrap();
    println!(
        "[{}]:  Compilation took: {:.2?}",
        "Info".green(),
        pre.elapsed()
    );
    std::process::Command::new("rm").args(&objects).spawn().unwrap().wait().unwrap();

    Ok(())
}

fn parse_source(input: &str, name: &str) -> Result<parser::Module, ()> {
    let tokens: Vec<_> = match lexer::Lexer::new(&input.chars().collect::<Vec<_>>()).lex() {
        Ok(ts) => ts,
        Err(e) => {
            eprintln!("{}", e.v);
            display_diagnostic_info(&input, name, &e);
            return Err(());
        }
    };

    let program = match parser::Parser::new(&tokens).parse() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("{}", e.v);
            display_diagnostic_info(&input, name, &e);
            return Err(());
        }
    };
    Ok(program)
}

fn display_diagnostic_info<T>(input: &str, input_name: &str, e: &Spanned<T>) {
    let line_offset = e.offset - e.line_beginning;
    let line_end = input[e.line_beginning..].find('\n').unwrap_or(input.len()) + e.line_beginning;
    let line = &input[e.line_beginning..line_end];

    let line_count = {
        let upto = &input[0..e.line_beginning];
        upto.chars().filter(|c| *c == '\n').count() + 1
    };

    let prefix = format!("  ./{}:{}:{}", input_name, line_count, line_offset + 1);
    eprintln!("{prefix}\n{}", line);
    eprintln!(
        "{}{}",
        " ".repeat(e.offset - e.line_beginning + 7),
        "^".repeat(e.len)
    );
}

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub offset: usize,
    pub len: usize,
    pub line_beginning: usize,
    pub v: T,
}
