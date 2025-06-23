use crate::Spanned;
use crate::lexer;
use crate::parser;
use colored::Colorize;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Bool,
    Char,
    Void,
}


#[derive(Debug)]
pub struct FunctionType {
    ret: Type,
    args: Vec<Type>,
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


fn get_expr_type(
    e: &Spanned<parser::Expression>,
    vars: &HashMap<String, Type>,
    funcs: &HashMap<String, FunctionType>,
) -> Result<Type, Spanned<TypeError>> {
    match &e.v {
        parser::Expression::Integer(_) => Ok(Type::Int),
        parser::Expression::Bool(_) => Ok(Type::Bool),
        parser::Expression::Char(_) => Ok(Type::Char),
        parser::Expression::Unary { op, right } => {
            let right_type = get_expr_type(right, vars, funcs)?;
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
            let (left, right) = (
                get_expr_type(left, vars, funcs)?,
                get_expr_type(right, vars, funcs)?,
            );
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
            Some(t) => Ok(t.clone()),
        },
        parser::Expression::FuncCall { name, args } => {
            let mut arg_types = vec![];
            for a in args {
                arg_types.push(get_expr_type(a, vars, funcs)?);
            }

            let called = match funcs.get(name) {
                Some(f) => f,
                None => {
                    return Err(Spanned {
                        offset: e.offset,
                        len: e.len,
                        line_beginning: e.line_beginning,
                        v: TypeError::UndefinedFunction,
                    });
                }
            };

            if called.args.len() != arg_types.len() {
                return Err(Spanned {
                    offset: e.offset,
                    len: e.len,
                    line_beginning: e.line_beginning,
                    v: TypeError::ArgCountMismatch,
                });
            }
            for i in 0..called.args.len() {
                if called.args[i] != arg_types[i] {
                    return Err(Spanned {
                        offset: e.offset,
                        len: e.len,
                        line_beginning: e.line_beginning,
                        v: TypeError::TypeMismatch,
                    });
                }
            }
            Ok(called.ret.clone())
        }
    }
}

pub fn type_check(ast: &[Spanned<parser::Statement>]) -> Vec<Spanned<TypeError>> {
    let mut vars = HashMap::new();
    let mut funcs = HashMap::new();
    // TODO: HACK
    funcs.insert(
        "putchar".to_string(),
        FunctionType {
            ret: Type::Void,
            args: vec![Type::Char],
        },
    );
    funcs.insert(
        "getchar".to_string(),
        FunctionType {
            ret: Type::Char,
            args: vec![],
        },
    );
    let mut errs = vec![];

    for s in ast {
        if let Err(e) = type_check_statement(s, &mut vars, &mut funcs) {
            errs.push(e);
        }
    }
    errs
}

fn type_check_statement(
    s: &Spanned<parser::Statement>,
    vars: &mut HashMap<String, Type>,
    funcs: &mut HashMap<String, FunctionType>,
) -> Result<(), Spanned<TypeError>> {
    dbg!(&s);
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
            let expr_type = get_expr_type(expr, vars, funcs)?;
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
            let expr_type = get_expr_type(expr, vars, funcs)?;
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
            let expr_type = get_expr_type(v, vars, funcs)?;
            assert!(expr_type == Type::Int)
        }
        parser::Statement::If { cond, body } | parser::Statement::While { cond, body } => {
            let cond_type = get_expr_type(cond, vars, funcs)?;
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
                type_check_statement(s, vars, funcs)?;
            }
        }
        parser::Statement::FuncCall { name, args } => {
            let mut arg_types = vec![];
            for a in args {
                arg_types.push(get_expr_type(a, vars, funcs)?);
            }

            let called = match funcs.get(name) {
                Some(f) => f,
                None => {
                    return Err(Spanned {
                        offset: s.offset,
                        len: s.len,
                        line_beginning: s.line_beginning,
                        v: TypeError::UndefinedFunction,
                    });
                }
            };

            if called.args.len() != arg_types.len() {
                return Err(Spanned {
                    offset: s.offset,
                    len: s.len,
                    line_beginning: s.line_beginning,
                    v: TypeError::ArgCountMismatch,
                });
            }
            for i in 0..called.args.len() {
                if called.args[i] != arg_types[i] {
                    return Err(Spanned {
                        offset: s.offset,
                        len: s.len,
                        line_beginning: s.line_beginning,
                        v: TypeError::TypeMismatch,
                    });
                }
            }
        }
        parser::Statement::FuncDefinition { name, body, ret_type } => {
            if funcs.get(name).is_some() {
                return Err(Spanned {
                    offset: s.offset,
                    len: s.len,
                    line_beginning: s.line_beginning,
                    v: TypeError::FunctionRedefinition,
                });
            }
            funcs.insert(name.to_string(), FunctionType { ret: Type::Void, args: vec![] });

            let mut vars = HashMap::new();
            for s in body {
                type_check_statement(s, &mut vars, funcs)?;
            }
        }
    }
    Ok(())
}
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TypeError {
    UndefinedBinding,
    UndefinedFunction,
    TypeMismatch,
    BoolBinaryOp,
    NonBoolIfCond,
    BindingRedefinition,
    ArgCountMismatch,
    FunctionRedefinition
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
            Self::UndefinedFunction => {
                writeln!(
                    f,
                    "[{}]\n Found a call to an undefined function",
                    "Error".red()
                )?;
                write!(f, "[{}]\n Maybe you have misspeled it?", "Note".green())
            }
            Self::ArgCountMismatch => {
                writeln!(
                    f,
                    "[{}]\n Found a call to an function, but the amount of arguments was invalid",
                    "Error".red()
                )
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
            Self::FunctionRedefinition => {
                write!(f, "[{}]\n Found a redefinition of a function", "Error".red())

            }
        }
    }
}
