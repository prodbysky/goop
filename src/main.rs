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
    let (program, parser_errors) = parser.parse();
    for e in &parser_errors {
        eprintln!("{}", e.v);
        display_diagnostic_info(&input, &config.input_name, e);
    }
    if !parser_errors.is_empty() {
        return;
    }
    // display_ast(&program);

    use qbe::{Module, Function, Instr, Value, Type, Block, Linkage};

    let mut module = Module::new();
    let func = module.add_function(Function::new(Linkage::public(), "main", vec![], Some(Type::Word)));
    let mut func_temp_count = 0;
    func.add_block("entry");

    fn make_temp (t_count: &mut usize) -> Value {
        *t_count += 1;
        Value::Temporary(format!("t_{}", t_count))
    }

    fn eval_expr(func: &mut Function, e: &parser::Expression, t_count: &mut usize) -> Value {
        match e {
            parser::Expression::Integer(i) => Value::Const(*i),
            parser::Expression::Binary { left, op, right } => {
                let left = eval_expr(func, &left.v, t_count);
                let right = eval_expr(func, &right.v, t_count);
                match (&left, &right, op) {
                    (Value::Const(l), Value::Const(r), lexer::Operator::Plus) => return Value::Const(l + r),
                    (Value::Const(l), Value::Const(r), lexer::Operator::Minus) => return Value::Const(l - r),
                    (Value::Const(l), Value::Const(r), lexer::Operator::Star) => return Value::Const(l * r),
                    (Value::Const(l), Value::Const(r), lexer::Operator::Slash) => return Value::Const(l / r),
                    _ => {}
                }
                let result_place = make_temp(t_count);
                match op {
                    lexer::Operator::Plus => {
                        func.assign_instr(result_place.clone(), Type::Word, Instr::Add(left, right));
                        return result_place;
                    }
                    lexer::Operator::Minus => {
                        func.assign_instr(result_place.clone(), Type::Word, Instr::Sub(left, right));
                        return result_place;
                    }
                    lexer::Operator::Star => {
                        func.assign_instr(result_place.clone(), Type::Word, Instr::Mul(left, right));
                        return result_place;
                    }
                    lexer::Operator::Slash => {
                        func.assign_instr(result_place.clone(), Type::Word, Instr::Div(left, right));
                        return result_place;
                    }
                    _ => todo!()
                }
            }
        }
    }

    for s in &program {
        match &s.v {
            parser::Statement::Return(v) => {
                let value = eval_expr(func, &v.v, &mut func_temp_count);
                func.add_instr(Instr::Ret(Some(value)));
            }
            _ => todo!()
        }
    }
    println!("{}", module.to_string());

}

fn display_ast(ast: &[Spanned<parser::Statement>]) {
    for s in ast {
        match &s.v{
            parser::Statement::Return(v) => {
                println!("Return:");
                display_expression(&v.v, 1);
            }
            parser::Statement::VarAssign { name, t, expr } => {
                println!("Assign ({name} : {t}):");
                display_expression(&expr.v, 1);
            }
        }
    }
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
