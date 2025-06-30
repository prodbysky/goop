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

        let no_ext = std::path::Path::new(&name).file_stem().and_then(|s| s.to_str()).unwrap_or(&name);

        codegen::inkwell::generate_code(module, no_ext);
        objects.push(format!("{no_ext}.o"));
    }
    let out = std::process::Command::new("clang").args(&objects).arg("-o").arg(&args.output).output().map_err(|e| {
        println!("[{}]: Failed to execute clang: {e}", "Error".red());
        
    })?;
    if !out.status.success() {
        eprintln!("[{}]: Linking failed", "Error".red());
        eprintln!("{}", String::from_utf8_lossy(&out.stderr));
        return Err(());
    }
    println!(
        "[{}]:  Compilation took: {:.2?}",
        "Info".green(),
        pre.elapsed()
    );
    for o in &objects {
        match std::fs::remove_file(o) {
            Ok(_) => {}
            Err(e) => {
                println!("[{}]: Failed to remove file {o}: {e}", "Error".red())
            }
        };
    }

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

fn display_diagnostic_info<T>(input: &str, input_name: &str, e: &Spanned<T>) {
    let line_count = input[..e.line_beginning].chars().filter(|&c| c == '\n').count() + 1;

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
