use crate::Spanned;
use crate::{parser, lexer};
use std::collections::HashMap;

pub fn generate_qbe_module(program: &[Spanned<parser::Statement>]) -> qbe::Module {
    let mut module = qbe::Module::new();
    let func = module.add_function(qbe::Function::new(qbe::Linkage::public(), "main", vec![], Some(qbe::Type::Word)));
    let mut func_temp_count = 0;
    let mut label_count = 0;
    let mut vars: HashMap<String, qbe::Value> = HashMap::new();
    func.add_block("entry");

    for s in program {
        generate_statement(func, &mut func_temp_count, &mut label_count, &mut vars, &s.v);
    }
    module 
}

fn generate_statement(func: &mut qbe::Function, func_temp_count: &mut usize, label_count: &mut usize, vars: &mut HashMap<String, qbe::Value>, s: &parser::Statement) {
    match s {
        parser::Statement::Return(v) => {
            let value = eval_expr(func, &v.v, func_temp_count, &vars);
            func.add_instr(qbe::Instr::Ret(Some(value)));
        }
        parser::Statement::VarAssign { name, t: _, expr } => {
            let value = eval_expr(func, &expr.v, func_temp_count, &vars);
            vars.insert(name.to_string(), value);
        }
        parser::Statement::If { cond, body } => {
            let cond = eval_expr(func, &cond.v, func_temp_count, &vars);
            match cond {
                qbe::Value::Const(1) => {
                    for st in body {
                        generate_statement(func, func_temp_count, label_count, vars, &st.v);
                    }
                    return
                },
                qbe::Value::Const(0) => {
                    return
                },
                _ => {}
            }

            let into = format!("{}", *label_count);
            *label_count += 1;
            let skip = format!("{}", *label_count);
            *label_count += 1;
            func.add_instr(qbe::Instr::Jnz(cond, into.clone(), skip.clone()));
            func.add_block(into);
            for st in body {
                generate_statement(func, func_temp_count, label_count, vars, &st.v);
            }
            func.add_block(skip);
        }
    }
}

fn eval_expr(func: &mut qbe::Function, e: &parser::Expression, t_count: &mut usize, vars: &HashMap<String, qbe::Value>) -> qbe::Value {
    fn make_temp (t_count: &mut usize) -> qbe::Value {
        *t_count += 1;
        qbe::Value::Temporary(format!("{}", t_count))
    }
    match e {
        parser::Expression::Integer(i) => qbe::Value::Const(*i),
        parser::Expression::Bool(i) => qbe::Value::Const(*i as u64),
        parser::Expression::Identifier(i) => {
            vars.get(i).unwrap().clone()
        }
        parser::Expression::Binary { left, op, right } => {
            let left = eval_expr(func, &left.v, t_count, vars);
            let right = eval_expr(func, &right.v, t_count, vars);
            match (&left, &right, op) {
                (qbe::Value::Const(l), qbe::Value::Const(r), lexer::Operator::Plus) => return qbe::Value::Const(l + r),
                (qbe::Value::Const(l), qbe::Value::Const(r), lexer::Operator::Minus) => return qbe::Value::Const(l - r),
                (qbe::Value::Const(l), qbe::Value::Const(r), lexer::Operator::Star) => return qbe::Value::Const(l * r),
                (qbe::Value::Const(l), qbe::Value::Const(r), lexer::Operator::Slash) => return qbe::Value::Const(l / r),
                (qbe::Value::Const(l), qbe::Value::Const(r), lexer::Operator::Less) => return qbe::Value::Const((l < r) as u64),
                (qbe::Value::Const(l), qbe::Value::Const(r), lexer::Operator::More) => return qbe::Value::Const((l > r) as u64),
                _ => {}
            }
            let result_place = make_temp(t_count);
            match op {
                lexer::Operator::Plus => {
                    func.assign_instr(result_place.clone(), qbe::Type::Word, qbe::Instr::Add(left, right));
                    result_place
                }
                lexer::Operator::Minus => {
                    func.assign_instr(result_place.clone(), qbe::Type::Word, qbe::Instr::Sub(left, right));
                    result_place
                }
                lexer::Operator::Star => {
                    func.assign_instr(result_place.clone(), qbe::Type::Word, qbe::Instr::Mul(left, right));
                    result_place
                }
                lexer::Operator::Slash => {
                    func.assign_instr(result_place.clone(), qbe::Type::Word, qbe::Instr::Div(left, right));
                    result_place
                }
                lexer::Operator::Less => {
                    func.assign_instr(result_place.clone(), qbe::Type::Word, qbe::Instr::Cmp(qbe::Type::Word, qbe::Cmp::Slt, left, right));
                    result_place
                }
                lexer::Operator::More => {
                    func.assign_instr(result_place.clone(), qbe::Type::Word, qbe::Instr::Cmp(qbe::Type::Word, qbe::Cmp::Sgt, left, right));
                    result_place
                }
            }
        }
    }
}

