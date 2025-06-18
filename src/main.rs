mod config;
mod lexer;
mod parser;
mod type_check;
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
    let lexer_errors: Vec<_> = elements.iter().filter_map(|e| e.as_ref().err()).collect();

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
    println!("[{}]: Type checking took: {:.2?}", "Info".green(), pre_t_check.elapsed());
    for e in &errs {
        eprintln!("{}", e.v);
        display_diagnostic_info(&input, &config.input_name, e);
    }
    if !errs.is_empty() {
        return;
    }


    let ir = optimize_ir(generate_ir(&program));
    display_ir(&ir);

    let mut module = qbe::Module::new();
    let main = module.add_function(qbe::Function::new(qbe::Linkage::public(), "main", vec![], Some(qbe::Type::Word)));
    main.add_block("entry");

    for i in ir {
        match i {
            Instr::Assign { index, v } => {
                let place = qbe::Value::Temporary(format!("{index}_ptr"));
                main.assign_instr(place.clone(), qbe::Type::Long, qbe::Instr::Alloc8(8));
                match v {
                    Value::Const(i) => main.add_instr(qbe::Instr::Store(qbe::Type::Word, place, qbe::Value::Const(i))),
                    Value::Temp(i) => {
                        let from = qbe::Value::Temporary(format!("{i}_ptr"));
                        main.assign_instr(place, qbe::Type::Word, qbe::Instr::Load(qbe::Type::Word, from));
                    }
                }
            }
            Instr::Return { value } => {
                match value {
                    Value::Const(i) => main.add_instr(qbe::Instr::Ret(Some(qbe::Value::Const(i)))),
                    Value::Temp(i) => {
                        let place = qbe::Value::Temporary(format!("{i}"));
                        let from = qbe::Value::Temporary(format!("{i}_ptr"));
                        main.assign_instr(place.clone(), qbe::Type::Word, qbe::Instr::Load(qbe::Type::Word, from));
                        main.add_instr(qbe::Instr::Ret(Some(place)));
                    }
                }
            }
            _ => {}
        }
    }
    println!("{module}");


    // let pre_cg = std::time::Instant::now();
    // let module = codegen::generate_qbe_module(&program);
    // println!("[{}]: Code geneneration took: {:.2?}", "Info".green(), pre_cg.elapsed());
    // if let Err(e) = compile_qbe_module(module, &config.input_name[0..config.input_name.len() - 3]) {
    //     eprintln!("[{}]\n Failed to compile code module: {e}", "Error".red());
    // }
}

#[derive(Debug, Clone, Copy)]
enum Value {
    Const(u64),
    Temp(ValueIndex)
}

type ValueIndex = usize;
type LabelIndex = usize;

#[derive(Debug, Clone)]
enum Instr {
    Assign {
        index: ValueIndex,
        v: Value
    },
    BinaryOp {
        op: lexer::Operator,
        l: Value,
        r: Value,
        into: ValueIndex
    },
    UnaryOp {
        op: lexer::Operator,
        r: ValueIndex,
        into: ValueIndex
    },
    Return {
        value: Value,
    },
    Label(LabelIndex),
    Jump(LabelIndex),
    JumpNotZero { 
        index: ValueIndex, 
        to: LabelIndex,
        otherwise: LabelIndex
    },
}

fn generate_ir(module: &[Spanned<parser::Statement>]) -> Vec<Instr> {
    let mut ir = vec![];
    let mut vars: HashMap<String, ValueIndex> = HashMap::new();

    let mut t_count = 0;
    let mut l_count = 0;
    
    for s in module {
        generate_statement(&mut ir, &mut vars, &s.v, &mut t_count, &mut l_count);
    }

    ir
}

fn alloc_new(t: &mut usize) -> usize {
    *t += 1;
    *t - 1
}

fn optimize_ir(mut ir: Vec<Instr>) -> Vec<Instr>{
    let pre = std::time::Instant::now();
    constant_fold_ir(&mut ir);
    println!("[{}]: Optimizations took {:.2?}", "Info".green(), pre.elapsed());
    ir
}

fn constant_fold_ir(ir: &mut [Instr]) {
    let blocks = ir.split_mut(|t| matches!(t, Instr::Label(_)));
    for block in blocks {
        let mut temps: HashMap<ValueIndex, u64>= HashMap::new();  
        for i in block.iter_mut() {
            match i {
                Instr::Assign { index, v: Value::Const(i) } => {temps.insert(*index, *i);}
                Instr::BinaryOp { op, l: Value::Temp(l), r: Value::Temp(r), into } => {
                    if let (Some(l), Some(r)) = (temps.get(l), temps.get(r)) {
                        let v = match op {
                            lexer::Operator::Plus => l + r,
                            lexer::Operator::Minus => l - r,
                            lexer::Operator::Star => l * r,
                            lexer::Operator::Slash => l / r,
                            lexer::Operator::Percent => l % r,
                            lexer::Operator::More => (l > r) as u64,
                            lexer::Operator::Less => (l < r) as u64,
                            _ => unreachable!()
                        };
                        temps.insert(*into, v);
                        *i = Instr::Assign { index: *into, v: Value::Const(v) }
                    }
                },
                Instr::Return { value } => {
                    if let Value::Temp(i) = value {
                        if let Some(v) = temps.get(i) {
                            *value = Value::Const(*v);
                        }
                    }
                }
                _ => {}
            };
        }
    }
}

fn generate_statement(ir: &mut Vec<Instr>, vars: &mut HashMap<String, ValueIndex>, s: &parser::Statement, t_count: &mut ValueIndex, l_count: &mut LabelIndex) {
    match s {
        parser::Statement::Return(v) => {
            let e = generate_expr(ir, vars, &v.v, t_count);
            ir.push(Instr::Return { value: Value::Temp(e) });
        }
        parser::Statement::VarAssign { name, t: _, expr } => {
            let e = generate_expr(ir, vars, &expr.v, t_count);
            vars.insert(name.to_string(), e);
        }
        parser::Statement::If { cond, body } => {
            let e = generate_expr(ir, vars, &cond.v, t_count);
            let body_idx = alloc_new(l_count);
            let over = alloc_new(l_count);
            ir.push(Instr::JumpNotZero {
                to: body_idx,
                otherwise: over,
                index: e,
            });
            ir.push(Instr::Label(body_idx));
            for i in body {
                generate_statement(ir, vars, &i.v, t_count, l_count);
            }
            ir.push(Instr::Label(over));
        }
        parser::Statement::VarReassign { name, expr } => {
            let new = generate_expr(ir, vars, &expr.v, t_count);
            vars.insert(name.to_string(), new);
        }
        parser::Statement::While { cond, body } => {
            let header_idx = alloc_new(l_count);
            let body_idx = alloc_new(l_count);
            let over_idx = alloc_new(l_count);
            ir.push(Instr::Label(header_idx));
            let e = generate_expr(ir, vars, &cond.v, t_count);
            ir.push(Instr::JumpNotZero { index: e, to: body_idx, otherwise: over_idx });
            ir.push(Instr::Label(body_idx));
            for b in body {
                generate_statement(ir, vars, &b.v, t_count, l_count);
            }
            ir.push(Instr::Jump(header_idx));
            ir.push(Instr::Label(over_idx));
        }
    }
}

fn generate_expr(ir: &mut Vec<Instr>, vars: &mut HashMap<String, ValueIndex>, e: &parser::Expression, t_count: &mut usize) -> ValueIndex {
    match e {
        parser::Expression::Integer(i) => {
            let place = alloc_new(t_count);
            ir.push(Instr::Assign { index: place, v: Value::Const(*i) });
            place
        }
        parser::Expression::Char(i) => {
            let place = alloc_new(t_count);
            ir.push(Instr::Assign { index: place, v: Value::Const(*i as u64) });
            place
        }
        parser::Expression::Bool(b) => {
            let place = alloc_new(t_count);
            ir.push(Instr::Assign { index: place, v: Value::Const(*b as u64) });
            place
        }
        parser::Expression::Identifier(name) => {
            *vars.get(name).unwrap()
        }
        parser::Expression::Binary { left, op, right } => {
            let left = generate_expr(ir, vars, &left.v, t_count);
            let right = generate_expr(ir, vars, &right.v, t_count);
            let place = alloc_new(t_count);
            ir.push(Instr::BinaryOp { op: *op, l: Value::Temp(left), r: Value::Temp(right), into: place });
            place
        }
        parser::Expression::Unary { op, right } => {
            let right = generate_expr(ir, vars, &right.v, t_count);
            let place = alloc_new(t_count);
            ir.push(Instr::UnaryOp { op: *op, r: right, into: place } );
            place
        }
    }
}

fn display_ir(ir: &[Instr]) {
    for i in ir {
        match i {
            Instr::Assign { index, v } => {
                println!("Temp[{index}] = {v:?}")
            }
            Instr::BinaryOp { op, l, r, into } => {
                println!("Temp[{into}] = {l:?} {op:?} {r:?}")
            }
            Instr::UnaryOp { op, r, into } => {
                println!("Temp[{into}] = {op:?} Temp[{r}]")
            }
            Instr::Return { value } => {
                println!("Return {value:?}")
            }
            Instr::Label(l) => {
                println!("Label[{l}]");
            }
            Instr::JumpNotZero { index, to, otherwise} => {
                println!("JumpNotZero Label[{to}] else Label[{otherwise}] Temp[{index}]");
            }
            Instr::Jump(i) => {
                println!("Jump Label[{i}]");

            }
        }
    }
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
