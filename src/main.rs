mod codegen;
mod config;
mod ir;
mod lexer;
mod parser;
mod type_check;

use clap::Parser;
use colored::Colorize;

fn main() -> Result<(), ()> {
    let args = config::Args::parse();
    let input = match std::fs::read_to_string(&args.input) {
        Ok(i) => i,
        Err(e) => {
            eprintln!(
                "[{}]: Failed to read {input_name}: {e}",
                "Error".red(),
                input_name = args.input,
            );
            return Err(());
        }
    };

    let program = parse_source(&input, &args.input)?;
    type_check(&program, &args.input, &input)?;

    let module = ir::Module::from_ast(&program).unwrap();

    let no_ext = &args.input[..args.input.len() - 3];

    let pre = std::time::Instant::now();
    codegen::inkwell::generate_code(module, no_ext, &args.output);
    println!(
        "[{}]: Compilation took: {:.2?}",
        "Info".green(),
        pre.elapsed()
    );
    Ok(())
}

fn parse_source(input: &str, name: &str) -> Result<parser::AstModule, ()> {
    let pre_parsing = std::time::Instant::now();
    let tokens: Vec<_> = match lexer::Lexer::new(&input.chars().collect::<Vec<_>>()).lex() {
        Ok(ts) => ts,
        Err(e) => {
            eprintln!("{}", e.v);
            display_diagnostic_info(&input, input, &e);
            return Err(());
        }
    };

    let (program, parser_errors) = parser::Parser::new(&tokens).parse();
    for e in &parser_errors {
        eprintln!("{}", e.v);
        display_diagnostic_info(&input, name, e);
    }
    if !parser_errors.is_empty() {
        return Err(());
    }
    println!(
        "[{}]: Parsing source code took: {:.2?}",
        "Info".green(),
        pre_parsing.elapsed()
    );
    Ok(program)
}

fn type_check(program: &parser::AstModule, name: &str, input: &str) -> Result<(), ()> {
    let pre_t_check = std::time::Instant::now();
    let errs = type_check::type_check(&program);
    println!(
        "[{}]: Type checking took: {:.2?}",
        "Info".green(),
        pre_t_check.elapsed()
    );
    for e in &errs {
        eprintln!("{}", e.v);
        display_diagnostic_info(&input, name, e);
    }
    if !errs.is_empty() {
        return Err(());
    }
    Ok(())
}

fn display_diagnostic_info<T: std::fmt::Debug>(input: &str, input_name: &str, e: &Spanned<T>) {
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
        " ".repeat(e.offset - e.line_beginning),
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
