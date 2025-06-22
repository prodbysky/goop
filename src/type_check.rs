use crate::Spanned;
use crate::lexer;
use crate::parser;
use colored::Colorize;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    Int,
    Bool,
    Char,
}

pub fn type_from_type_name(name: &str) -> Type {
    if name == "i32" {
        return Type::Int;
    }
    if name == "bool" {
        return Type::Bool;
    }
    if name == "char" {
        return Type::Char;
    }
    todo!()
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeError {
    UndefinedBinding,
    TypeMismatch,
    BoolBinaryOp,
    NonBoolIfCond,
    BindingRedefinition,
}

fn get_expr_type(
    e: &Spanned<parser::Expression>,
    vars: &HashMap<String, Type>,
) -> Result<Type, Spanned<TypeError>> {
    match &e.v {
        parser::Expression::Integer(_) => Ok(Type::Int),
        parser::Expression::Bool(_) => Ok(Type::Bool),
        parser::Expression::Char(_) => Ok(Type::Char),
        parser::Expression::Unary { op, right } => {
            let right_type = get_expr_type(right, vars)?;
            match (op, right_type) {
                (lexer::Operator::Minus, Type::Int) => Ok(Type::Int),
                (lexer::Operator::Not, Type::Bool) => Ok(Type::Bool),
                _ => Err(Spanned {
                    offset: right.offset,
                    len: right.len,
                    line_beginning: right.line_beginning,
                    v: TypeError::TypeMismatch,
                }),
            }
        }
        parser::Expression::Binary { left, op, right } => {
            let (left, right) = (get_expr_type(left, vars)?, get_expr_type(right, vars)?);
            match op {
                lexer::Operator::Plus
                | lexer::Operator::Minus
                | lexer::Operator::Star
                | lexer::Operator::Percent
                | lexer::Operator::Slash => match (left, right) {
                    (Type::Int, Type::Int) => Ok(Type::Int),
                    (Type::Char, Type::Int) => Ok(Type::Char),
                    (Type::Int, Type::Char) => Ok(Type::Int),
                    _ => Err(Spanned {
                        offset: e.offset,
                        len: e.len,
                        line_beginning: e.line_beginning,
                        v: TypeError::BoolBinaryOp,
                    }),
                },
                lexer::Operator::Less | lexer::Operator::More => match (left, right) {
                    (Type::Int, Type::Int) => Ok(Type::Bool),
                    (Type::Char, Type::Char) => Ok(Type::Bool),
                    _ => Err(Spanned {
                        offset: e.offset,
                        len: e.len,
                        line_beginning: e.line_beginning,
                        v: TypeError::BoolBinaryOp,
                    }),
                },
                lexer::Operator::Not => unreachable!(),
            }
        }
        parser::Expression::Identifier(ident) => match vars.get(ident) {
            None => Err(Spanned {
                offset: e.offset,
                len: e.len,
                line_beginning: e.line_beginning,
                v: TypeError::UndefinedBinding,
            }),
            Some(t) => Ok(*t),
        },
        parser::Expression::FuncCall { name, args } => todo!("type check function call as expr")
    }
}

pub fn type_check(ast: &[Spanned<parser::Statement>]) -> Vec<Spanned<TypeError>> {
    let mut vars = HashMap::new();
    let mut errs = vec![];

    for s in ast {
        if let Err(e) = type_check_statement(s, &mut vars) {
            errs.push(e);
        }
    }
    errs
}

fn type_check_statement(
    s: &Spanned<parser::Statement>,
    vars: &mut HashMap<String, Type>,
) -> Result<(), Spanned<TypeError>> {
    match &s.v {
        parser::Statement::VarAssign { name, t, expr } => {
            if vars.get(name).is_some() {
                return Err(Spanned {
                    offset: s.offset,
                    len: s.len,
                    line_beginning: s.line_beginning,
                    v: TypeError::BindingRedefinition,
                });
            }
            let expr_type = get_expr_type(expr, vars)?;
            let expected = type_from_type_name(t);
            if expr_type != expected {
                return Err(Spanned {
                    offset: s.offset,
                    len: s.len,
                    line_beginning: s.line_beginning,
                    v: TypeError::TypeMismatch,
                });
            }
            vars.insert(name.to_string(), expr_type);
        }
        parser::Statement::VarReassign { name, expr } => {
            if vars.get(name).is_none() {
                return Err(Spanned {
                    offset: s.offset,
                    len: s.len,
                    line_beginning: s.line_beginning,
                    v: TypeError::UndefinedBinding,
                });
            }
            let expr_type = get_expr_type(expr, vars)?;
            if expr_type != *vars.get(name).unwrap() {
                return Err(Spanned {
                    offset: s.offset,
                    len: s.len,
                    line_beginning: s.line_beginning,
                    v: TypeError::TypeMismatch,
                });
            }
        }
        parser::Statement::Return(v) => {
            let expr_type = get_expr_type(v, vars)?;
            assert!(expr_type == Type::Int)
        }
        parser::Statement::If { cond, body } | parser::Statement::While { cond, body } => {
            let cond_type = get_expr_type(cond, vars)?;
            match cond_type {
                Type::Bool => {}
                _ => {
                    return Err(Spanned {
                        offset: cond.offset,
                        len: cond.len,
                        line_beginning: cond.line_beginning,
                        v: TypeError::NonBoolIfCond,
                    });
                }
            };
            for s in body {
                type_check_statement(s, vars)?;
            }
        }
        parser::Statement::FuncCall { name, args } => todo!("type check function calls")
    }
    Ok(())
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeMismatch => {
                writeln!(
                    f,
                    "[{}]\n Found a type mismatch during type checking",
                    "Error".red()
                )?;
                write!(
                    f,
                    "[{}]\n We only have integers for now so this is a bug somewhere :3",
                    "Note".green()
                )
            }
            Self::UndefinedBinding => {
                writeln!(f, "[{}]\n Found a undefined binding", "Error".red())?;
                write!(f, "[{}]\n Maybe you have misspeled it?", "Note".green())
            }
            Self::BindingRedefinition => {
                writeln!(
                    f,
                    "[{}]\n Found an attempt to redefine a binding",
                    "Error".red()
                )?;
                write!(f, "[{}]\n Maybe you meant to reassign it?", "Note".green())
            }
            Self::BoolBinaryOp => {
                writeln!(
                    f,
                    "[{}]\n Found an attempt to do binary operations on booleans",
                    "Error".red()
                )?;
                write!(
                    f,
                    "[{}]\n Since there are no logical comparison operators you can't do this yet",
                    "Note".green()
                )
            }
            Self::NonBoolIfCond => {
                writeln!(
                    f,
                    "[{}]\n Found an attempt to use a non-boolean condition in an `if` statement",
                    "Error".red()
                )?;
                write!(
                    f,
                    "[{}]\n For now you can't do any type casting so you have to use a comparison result for the condition here",
                    "Note".green()
                )
            }
        }
    }
}
