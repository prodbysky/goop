mod config;
mod lexer;
mod parser;
mod type_check;
mod codegen;

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
            config::usage(&config.program_name);
            return;
        }
    };


    let input_chars: Vec<_> = input.chars().collect();
    let lexer = lexer::Lexer::new(&input_chars);
    let elements: Vec<_> = lexer.collect();
    let lexer_errors: Vec<_> = elements
        .iter()
        .filter_map(|e| e.as_ref().err())
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
    let (program, parser_errors) = parser.parse();
    for e in &parser_errors {
        eprintln!("{}", e.v);
        display_diagnostic_info(&input, &config.input_name, e);
    }
    if !parser_errors.is_empty() {
        return;
    }


    let pre_t_check = std::time::Instant::now();
    let errs = type_check::type_check(&program);
    println!("Type checking took: {:.2?}", pre_t_check.elapsed());
    for e in &errs {
        eprintln!("{}", e.v);
        display_diagnostic_info(&input, &config.input_name, e);
    }
    if !errs.is_empty() {
        return;
    }

    let pre_cg = std::time::Instant::now();
    let module = codegen::generate_qbe_module(&program);
    println!("Code geneneration took: {:.2?}", pre_cg.elapsed());
    println!("{}", module);
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
