use crate::Spanned;
use crate::parser;
use std::collections::HashMap;
use colored::Colorize;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    Int
}

pub fn type_from_type_name(name: &str) -> Type {
    if name == "i32" {
        return Type::Int;
    }
    todo!()
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeError {
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

pub fn type_check(ast: &[Spanned<parser::Statement>]) -> Vec<Spanned<TypeError>> {
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

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch => {
                writeln!(f, "[{}]\n Found a type mismatch during type checking", "Error".red())?;
                write!(f, "[{}]\n We only have integers for now so this is a bug somewhere :3", "Note".green())
            }
            Self::UndefinedBinding => {
                writeln!(f, "[{}]\n Found a undefined binding", "Error".red())?;
                write!(f, "[{}]\n Maybe you have misspeled it?", "Note".green())
            }

        }
        
    }
}
