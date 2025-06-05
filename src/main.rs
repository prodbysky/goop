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
        display_expression(&e.v, 0);
        let ir = generate_expr_ir(&e.v);
        dbg!(ir);
    }
}

#[derive(Debug)]
pub enum Instr {
    PushConstInt(u64),
    Add,
    Sub,
    Mul,
    Div,
}

pub fn generate_expr_ir(e: &parser::Expression) -> Vec<Instr> {
    let mut ir = vec![];

    match e {
        parser::Expression::Integer(i) => {
            ir.push(Instr::PushConstInt(*i));
        }
        parser::Expression::Binary { left, op, right } => {
            ir.extend(generate_expr_ir(&left.v));
            ir.extend(generate_expr_ir(&right.v));
            match op {
                lexer::Operator::Plus => {
                    ir.push(Instr::Add);
                }
                lexer::Operator::Minus => {
                    ir.push(Instr::Sub);
                }
                lexer::Operator::Star => {
                    ir.push(Instr::Mul);
                }
                lexer::Operator::Slash => {
                    ir.push(Instr::Div);
                }
            }
        }
    }

    ir
}

fn display_expression(e: &parser::Expression, indent: usize) {
    match e {
        parser::Expression::Integer(i) => {
            println!("{}Integer {i}", "  ".repeat(indent))
        }
        parser::Expression::Binary { left, op, right } => {
            println!("{}{op:?}", "  ".repeat(indent));
            display_expression(&left.v, indent + 1);
            display_expression(&right.v, indent + 1);
        }
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
