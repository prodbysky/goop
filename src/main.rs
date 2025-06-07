mod config;
mod lexer;
mod parser;

use std::io::Write;

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

    let expr_to_gen = &exprs[0];
    let expr_ir = generate_expr_ir(&expr_to_gen.v);

    match config.backend {
        config::Backend::LLVM => {
            let mod_string = generate_llvm_ir_module(&expr_ir).unwrap();
            std::fs::OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open("main.ll")
                .unwrap()
                .write(mod_string.as_bytes())
                .unwrap();
            std::process::Command::new("clang")
                .arg("main.ll")
                .spawn()
                .unwrap()
                .wait()
                .unwrap();
        }
    }
}

pub fn generate_llvm_ir_module(ir: &[Instr]) -> Result<String, inkwell::builder::BuilderError> {
    let ctx = inkwell::context::Context::create();
    let module = ctx.create_module("expr");
    let builder = ctx.create_builder();
    let i32_type = ctx.i32_type();

    let main_type = i32_type.fn_type(&[], false);
    let main_func = module.add_function("main", main_type, None);
    let block = ctx.append_basic_block(main_func, "entry");
    builder.position_at_end(block);

    let mut temps = vec![];

    for i in ir {
        match i {
            Instr::PushConstInt(n) => {
                let ptr = builder.build_alloca(i32_type, "temp")?;
                builder.build_store(ptr, i32_type.const_int(*n, false))?;
                temps.push((ptr, i32_type));
            }
            Instr::Add => {
                let left_idx = temps.len() - 2;
                let right_idx = temps.len() - 1;
                let l_value = builder.build_load(temps[left_idx].1, temps[left_idx].0, "load_l")?;
                let r_value =
                    builder.build_load(temps[right_idx].1, temps[right_idx].0, "load_r")?;
                let ptr = builder.build_alloca(i32_type, "temp")?;

                let result = builder.build_int_add(
                    l_value.into_int_value(),
                    r_value.into_int_value(),
                    "result",
                )?;
                builder.build_store(ptr, result)?;
                temps.push((ptr, i32_type));
            }

            Instr::Sub => {
                let left_idx = temps.len() - 2;
                let right_idx = temps.len() - 1;
                let l_value = builder.build_load(temps[left_idx].1, temps[left_idx].0, "load_l")?;
                let r_value =
                    builder.build_load(temps[right_idx].1, temps[right_idx].0, "load_r")?;
                let ptr = builder.build_alloca(i32_type, "temp")?;

                let result = builder.build_int_sub(
                    l_value.into_int_value(),
                    r_value.into_int_value(),
                    "result",
                )?;
                builder.build_store(ptr, result)?;
                temps.push((ptr, i32_type));
            }

            Instr::Mul => {
                let left_idx = temps.len() - 2;
                let right_idx = temps.len() - 1;
                let l_value = builder.build_load(temps[left_idx].1, temps[left_idx].0, "load_l")?;
                let r_value =
                    builder.build_load(temps[right_idx].1, temps[right_idx].0, "load_r")?;
                let ptr = builder.build_alloca(i32_type, "temp")?;

                let result = builder.build_int_mul(
                    l_value.into_int_value(),
                    r_value.into_int_value(),
                    "result",
                )?;
                builder.build_store(ptr, result)?;
                temps.push((ptr, i32_type));
            }

            Instr::Div => {
                let left_idx = temps.len() - 2;
                let right_idx = temps.len() - 1;
                let l_value = builder.build_load(temps[left_idx].1, temps[left_idx].0, "load_l")?;
                let r_value =
                    builder.build_load(temps[right_idx].1, temps[right_idx].0, "load_r")?;
                let ptr = builder.build_alloca(i32_type, "temp")?;

                let result = builder.build_int_signed_div(
                    l_value.into_int_value(),
                    r_value.into_int_value(),
                    "result",
                )?;
                builder.build_store(ptr, result)?;
                temps.push((ptr, i32_type));
            }
            _ => todo!(),
        }
    }
    let result =
        builder.build_load(temps.first().unwrap().1, temps.first().unwrap().0, "result")?;
    builder.build_return(Some(&result))?;
    module.verify().unwrap();
    Ok(module.to_string())
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

pub type Span = std::ops::Range<usize>;
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub offset: usize,
    pub len: usize,
    pub line_beginning: usize,
    pub v: T,
}
