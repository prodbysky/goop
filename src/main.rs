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
    let line_count = input[..e.line_beginning]
        .chars()
        .filter(|&c| c == '\n')
        .count()
        + 1;
    println!("./{}:{}:{}", input_name, line_count, {
        input[e.line_beginning..e.offset].chars().count() + 1
    });
}

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T> {
    pub offset: usize,
    pub len: usize,
    pub line_beginning: usize,
    pub v: T,
}
