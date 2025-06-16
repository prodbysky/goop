use crate::Spanned;
use crate::{lexer, parser};
use std::collections::HashMap;

pub fn generate_qbe_module(program: &[Spanned<parser::Statement>]) -> qbe::Module {
    let mut module = qbe::Module::new();
    let func = module.add_function(qbe::Function::new(
        qbe::Linkage::public(),
        "main",
        vec![],
        Some(qbe::Type::Word),
    ));
    let mut func_temp_count = 0;
    let mut label_count = 0;
    let mut vars: HashMap<String, qbe::Value> = HashMap::new();
    func.add_block("entry");

    for s in program {
        generate_statement(
            func,
            &mut func_temp_count,
            &mut label_count,
            &mut vars,
            &s.v,
        );
    }
    module
}

fn generate_statement(
    func: &mut qbe::Function,
    func_temp_count: &mut usize,
    label_count: &mut usize,
    vars: &mut HashMap<String, qbe::Value>,
    s: &parser::Statement,
) {
    match s {
        parser::Statement::Return(v) => {
            let value = eval_expr(func, &v.v, func_temp_count, vars);
            func.add_instr(qbe::Instr::Ret(Some(value)));
        }
        parser::Statement::VarAssign { name, t: _, expr } => {
            let value = eval_expr(func, &expr.v, func_temp_count, vars);
            
            let var_slot = make_temp(func_temp_count);
            func.assign_instr(
                var_slot.clone(),
                qbe::Type::Long,
                qbe::Instr::Alloc8(8), 
            );
            
            func.add_instr(qbe::Instr::Store(qbe::Type::Word, var_slot.clone(), value));
            
            vars.insert(name.to_string(), var_slot);
        }
        parser::Statement::VarReassign { name, expr } => {
            let value = eval_expr(func, &expr.v, func_temp_count, vars);
            let var_slot = vars.get(name).unwrap().clone();
            
            func.add_instr(qbe::Instr::Store(qbe::Type::Word, var_slot, value));
        }
        parser::Statement::If { cond, body } => {
            let cond = eval_expr(func, &cond.v, func_temp_count, vars);
            let into = format!("l_{}", *label_count);
            *label_count += 1;
            let skip = format!("l_{}", *label_count);
            *label_count += 1;
            func.add_instr(qbe::Instr::Jnz(cond, into.clone(), skip.clone()));
            func.add_block(into);
            for st in body {
                generate_statement(func, func_temp_count, label_count, vars, &st.v);
            }
            func.add_block(skip);
        }
        parser::Statement::While { cond, body } => {
            let header = format!("l_{}", *label_count);
            *label_count += 1;
            let body_l = format!("l_{}", *label_count);
            *label_count += 1;
            let exit = format!("l_{}", *label_count);
            *label_count += 1;
            
            func.add_instr(qbe::Instr::Jmp(header.clone()));
            
            func.add_block(header.clone());
            let cond_val = eval_expr(func, &cond.v, func_temp_count, vars);
            func.add_instr(qbe::Instr::Jnz(cond_val, body_l.clone(), exit.clone()));
            
            func.add_block(body_l);
            for st in body {
                generate_statement(func, func_temp_count, label_count, vars, &st.v);
            }
            func.add_instr(qbe::Instr::Jmp(header));
            
            func.add_block(exit);
        }
    }
}

fn eval_expr(
    func: &mut qbe::Function,
    e: &parser::Expression,
    t_count: &mut usize,
    vars: &HashMap<String, qbe::Value>,
) -> qbe::Value {
    match e {
        parser::Expression::Integer(i) => qbe::Value::Const(*i),
        parser::Expression::Bool(i) => qbe::Value::Const(*i as u64),
        parser::Expression::Identifier(i) => {
            let var_slot = vars.get(i).unwrap().clone();
            let result = make_temp(t_count);
            func.assign_instr(
                result.clone(),
                qbe::Type::Word,
                qbe::Instr::Load(qbe::Type::Word, var_slot),
            );
            result
        }
        parser::Expression::Unary { op, right } => {
            let right = eval_expr(func, &right.v, t_count, vars);
            let result_place = make_temp(t_count);
            match op {
                lexer::Operator::Not => {
                    func.assign_instr(result_place.clone(), qbe::Type::Word, qbe::Instr::Cmp(qbe::Type::Word, qbe::Cmp::Eq, right, qbe::Value::Const(0)));
                    result_place
                },
                lexer::Operator::Minus => {
                    func.assign_instr(result_place.clone(), qbe::Type::Word, qbe::Instr::Sub(qbe::Value::Const(0), right));
                    result_place
                },
                _ => unreachable!()
            }
        }
        parser::Expression::Binary { left, op, right } => {
            let left = eval_expr(func, &left.v, t_count, vars);
            let right = eval_expr(func, &right.v, t_count, vars);
            let result_place = make_temp(t_count);
            match op {
                lexer::Operator::Plus => {
                    func.assign_instr(
                        result_place.clone(),
                        qbe::Type::Word,
                        qbe::Instr::Add(left, right),
                    );
                    result_place
                }
                lexer::Operator::Minus => {
                    func.assign_instr(
                        result_place.clone(),
                        qbe::Type::Word,
                        qbe::Instr::Sub(left, right),
                    );
                    result_place
                }
                lexer::Operator::Star => {
                    func.assign_instr(
                        result_place.clone(),
                        qbe::Type::Word,
                        qbe::Instr::Mul(left, right),
                    );
                    result_place
                }
                lexer::Operator::Slash => {
                    func.assign_instr(
                        result_place.clone(),
                        qbe::Type::Word,
                        qbe::Instr::Div(left, right),
                    );
                    result_place
                }
                lexer::Operator::Less => {
                    func.assign_instr(
                        result_place.clone(),
                        qbe::Type::Word,
                        qbe::Instr::Cmp(qbe::Type::Word, qbe::Cmp::Slt, left, right),
                    );
                    result_place
                }
                lexer::Operator::More => {
                    func.assign_instr(
                        result_place.clone(),
                        qbe::Type::Word,
                        qbe::Instr::Cmp(qbe::Type::Word, qbe::Cmp::Sgt, left, right),
                    );
                    result_place
                }
                lexer::Operator::Percent => {
                    func.assign_instr(
                        result_place.clone(),
                        qbe::Type::Word,
                        qbe::Instr::Rem(left, right)
                    );
                    result_place
                }
                lexer::Operator::Not => unreachable!()
            }
        }
    }
}

fn make_temp(t_count: &mut usize) -> qbe::Value {
    *t_count += 1;
    qbe::Value::Temporary(format!("t_{}", *t_count))
}
