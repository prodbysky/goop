mod config;
mod lexer;
mod parser;

use colored::Colorize;
use std::collections::HashMap;

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

    #[derive(Debug, PartialEq, Clone, Copy)]
    enum Type {
        Int
    }

    fn type_from_type_name(name: &str) -> Type {
        if name == "i32" {
            return Type::Int;
        }
        todo!()
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    enum TypeError {
        UndefinedBinding,
        TypeMismatch
    }

    fn get_expr_type(e: &Spanned<parser::Expression>, vars: &HashMap<String, Type>) -> Result<Type, Spanned<TypeError>> {
        match &e.v{
            parser::Expression::Integer(_) => Ok(Type::Int),
            parser::Expression::Binary { left, op: _, right } => {
                match (get_expr_type(left, vars)?, get_expr_type(right, vars)?) {
                    (Type::Int, Type::Int) => Ok(Type::Int)
                }
            }
            parser::Expression::Identifier(ident) => {
                match vars.get(ident) {
                    None => Err(Spanned {
                        offset: e.offset,
                        len: e.len,
                        line_beginning: e.line_beginning,
                        v: TypeError::UndefinedBinding
                    }),
                    Some(t) => Ok(*t)
                }
            }
        }
    }

    fn type_check(ast: &[Spanned<parser::Statement>]) -> Vec<Spanned<TypeError>> {
        let mut vars = HashMap::new();
        let mut errs = vec![];

        for s in ast {
            match &s.v {
                parser::Statement::VarAssign { name, t, expr } => {
                    let expr_type = match get_expr_type(expr, &vars) {
                        Ok(t) => t,
                        Err(e) => {errs.push(e); continue;},
                    };
                    let expected = type_from_type_name(t);
                    if expr_type != expected {
                        errs.push(Spanned { offset: s.offset, len: s.len, line_beginning: s.line_beginning, v: TypeError::TypeMismatch });
                        continue;
                    }
                    vars.insert(name.to_string(), expr_type);
                }
                parser::Statement::Return(v) => {
                    let expr_type = match get_expr_type(v, &vars) {
                        Ok(t) => t,
                        Err(e) => {errs.push(e); continue;},
                    };
                    assert!(expr_type == Type::Int)
                }
            }
        }
        errs
    }

    let pre_t_check = std::time::Instant::now();
    let errs = type_check(&program);
    println!("Type checking took: {:.2?}", pre_t_check.elapsed());
    for e in &errs {
        dbg!(e);
    }
    if !errs.is_empty() {
        return;
    }

    // display_ast(&program);

    use qbe::{Module, Function, Instr, Value, Linkage};


    fn make_temp (t_count: &mut usize) -> Value {
        *t_count += 1;
        Value::Temporary(format!("t_{}", t_count))
    }

    fn eval_expr(func: &mut Function, e: &parser::Expression, t_count: &mut usize, vars: &HashMap<String, Value>) -> Value {
        match e {
            parser::Expression::Integer(i) => Value::Const(*i),
            parser::Expression::Identifier(i) => {
                vars.get(i).unwrap().clone()
            }
            parser::Expression::Binary { left, op, right } => {
                let left = eval_expr(func, &left.v, t_count, vars);
                let right = eval_expr(func, &right.v, t_count, vars);
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
                        func.assign_instr(result_place.clone(), qbe::Type::Word, Instr::Add(left, right));
                        result_place
                    }
                    lexer::Operator::Minus => {
                        func.assign_instr(result_place.clone(), qbe::Type::Word, Instr::Sub(left, right));
                        result_place
                    }
                    lexer::Operator::Star => {
                        func.assign_instr(result_place.clone(), qbe::Type::Word, Instr::Mul(left, right));
                        result_place
                    }
                    lexer::Operator::Slash => {
                        func.assign_instr(result_place.clone(), qbe::Type::Word, Instr::Div(left, right));
                        result_place
                    }
                }
            }
        }
    }
    let pre_cg = std::time::Instant::now();
    let mut module = Module::new();
    let func = module.add_function(Function::new(Linkage::public(), "main", vec![], Some(qbe::Type::Word)));
    let mut func_temp_count = 0;
    let mut vars: HashMap<String, Value> = HashMap::new();
    func.add_block("entry");

    for s in &program {
        match &s.v {
            parser::Statement::Return(v) => {
                let value = eval_expr(func, &v.v, &mut func_temp_count, &vars);
                func.add_instr(Instr::Ret(Some(value)));
            }
            parser::Statement::VarAssign { name, t: _, expr } => {
                let value = eval_expr(func, &expr.v, &mut func_temp_count, &vars);
                vars.insert(name.to_string(), value);
            }
        }
    }

    println!("Code geneneration took: {:.2?}", pre_cg.elapsed());
    println!("{}", module);

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
