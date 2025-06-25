use std::collections::HashMap;

use crate::{Spanned, parser};
use colored::Colorize;

#[derive(Debug, Clone)]
pub struct Module {
    functions: Vec<Function>,
}

impl Module {
    pub fn from_ast(ast_module: parser::AstModule) -> Result<Self, Spanned<Error>> {
        let mut s = Self { functions: vec![] };

        let mut func_types = HashMap::new();
        func_types.insert(
            "getchar".to_string(),
            FunctionType {
                name: "getchar".to_string(),
                ret: Type::Char,
                args: vec![],
            },
        );
        func_types.insert(
            "putchar".to_string(),
            FunctionType {
                name: "putchar".to_string(),
                ret: Type::Void,
                args: vec![Type::Char],
            },
        );
        for f in ast_module.funcs() {
            func_types.insert(f.v.name.clone(), f.v.get_type());
        }

        for f in ast_module.funcs() {
            let func = s.add_function(f.v.name.clone(), f.v.get_type());
            for st in f.v.body() {
                func.add_statement(&st, &func_types)?;
            }
        }

        Ok(s)
    }

    fn add_function(&mut self, name: String, fn_type: FunctionType) -> &mut Function {
        self.functions.push(Function {
            name,
            body: vec![],
            ret_type: fn_type.ret,
            values: vec![],
            max_labels: 0,
            vars: HashMap::new(),
        });
        self.functions.last_mut().unwrap()
    }

    fn get_function_mut(&mut self, name: &str) -> Option<&mut Function> {
        self.functions.iter_mut().find(|v| v.name.as_str() == name)
    }

    pub fn functions(&self) -> &[Function] {
        &self.functions
    }
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub name: String,
    pub ret: Type,
    pub args: Vec<Type>,
}

impl std::fmt::Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "func {}(", self.name)?;
        for arg in &self.args {
            write!(f, "{arg:?},")?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    ret_type: Type,
    name: String,
    body: Vec<Instr>,
    values: Vec<Value>,
    vars: HashMap<String, Value>,
    max_labels: LabelIndex,
}

impl Function {
    pub fn get_type(&self) -> FunctionType {
        FunctionType { name: self.name.clone(), ret: self.ret_type.clone(), args: vec![] }
    }
    fn add_statement(
        &mut self,
        s: &Spanned<parser::Statement>,
        funcs: &HashMap<String, FunctionType>,
    ) -> Result<(), Spanned<Error>> {
        match &s.v {
            parser::Statement::Return(v) => {
                let v_ir = self.add_expr(&v, funcs)?;
                if *v_ir.get_type() != self.ret_type {
                    return Err(Spanned {
                        offset: v.offset,
                        len: v.len,
                        line_beginning: v.line_beginning,
                        v: Error::MismatchedReturnType {
                            got: v_ir.get_type().clone(),
                            expect: self.ret_type.clone(),
                        },
                    });
                }
                self.body.push(Instr::Return { v: Some(v_ir) });
            }
            parser::Statement::VarAssign { name, t, expr } => {
                if self.vars.get(name).is_some() {
                    return Err(Spanned {
                        offset: s.offset,
                        len: s.len,
                        line_beginning: s.line_beginning,
                        v: Error::VariableRedefinition,
                    });
                }
                let v = self.add_expr(&expr, funcs)?;
                if v.get_type() != &type_from_type_name(t) {
                    return Err(Spanned {
                        offset: s.offset,
                        len: s.len,
                        line_beginning: s.line_beginning,
                        v: Error::UnexpectedType {
                            got: v.get_type().clone(),
                            expect: type_from_type_name(t),
                        },
                    });
                }
                self.vars.insert(name.to_string(), v);
            }
            parser::Statement::VarReassign { name, expr } => {
                if self.vars.get(name).is_none() {
                    return Err(Spanned {
                        offset: s.offset,
                        len: s.len,
                        line_beginning: s.line_beginning,
                        v: Error::UndefinedVariableRedefinition,
                    });
                }
                let v = self.add_expr(&expr, funcs)?;
                let prev = match self.vars.get(name).unwrap() {
                    Value::Temp { t: _, i } => *i,
                    _ => unreachable!(),
                };
                self.body.push(Instr::Assign { index: prev, v });
            }
            parser::Statement::If { cond, body } => {
                let cond = self.add_expr(&cond, funcs)?;
                if cond.get_type() != &Type::Bool {
                    return Err(Spanned {
                        offset: s.offset,
                        len: s.len,
                        line_beginning: s.line_beginning,
                        v: Error::NotBooleanCondition,
                    });
                }
                let into = self.alloc_label();
                let over = self.alloc_label();
                self.body.push(Instr::Jnz {
                    cond,
                    to: into,
                    otherwise: over,
                });
                self.body.push(Instr::Label(into));
                for s in body {
                    self.add_statement(&s, funcs)?;
                }
                self.body.push(Instr::Label(over));
            }
            parser::Statement::While { cond, body } => {
                let header = self.alloc_label();
                let into = self.alloc_label();
                let over = self.alloc_label();
                self.body.push(Instr::Label(header));
                let cond = self.add_expr(&cond, funcs)?;
                self.body.push(Instr::Jnz {
                    cond: cond,
                    to: into,
                    otherwise: over,
                });
                self.body.push(Instr::Label(into));
                for s in body {
                    self.add_statement(&s, funcs)?;
                }
                self.body.push(Instr::Jump(header));
                self.body.push(Instr::Label(over));
            }
            parser::Statement::FuncCall { name, args } => {
                let mut args_into = vec![];

                let receiver = match funcs.get(name) {
                    None => {
                        return Err(Spanned {
                            offset: s.offset,
                            len: s.len,
                            line_beginning: s.line_beginning,
                            v: Error::UndefinedFunction,
                        });
                    }
                    Some(r) => r,
                };

                if receiver.args.len() != args.len() {
                    return Err(Spanned {
                        offset: s.offset,
                        len: s.len,
                        line_beginning: s.line_beginning,
                        v: Error::MismatchedArgumentCount {
                            callee_type: receiver.clone(),
                            expect: receiver.args.len(),
                            got: args.len(),
                        },
                    });
                }

                for arg in args {
                    args_into.push(self.add_expr(&arg, funcs)?);
                }

                for i in 0..args.len() {
                    if args_into[i].get_type() != &receiver.args[i] {
                        return Err(Spanned {
                            offset: s.offset,
                            len: s.len,
                            line_beginning: s.line_beginning,
                            v: Error::MismatchedArgumentTypes {
                                callee_type: receiver.clone(),
                                expect: receiver.args.len(),
                                got: args.len(),
                            },
                        });
                    }
                }
                self.body.push(Instr::Call {
                    name: name.to_string(),
                    args: args_into,
                    into: None,
                });
            }
        }
        Ok(())
    }

    fn add_expr(
        &mut self,
        e: &Spanned<parser::Expression>,
        funcs: &HashMap<String, FunctionType>,
    ) -> Result<Value, Spanned<Error>> {
        match &e.v {
            parser::Expression::Integer(i) => Ok(Value::Const {
                t: Type::U64,
                v: *i,
            }),
            parser::Expression::Char(c) => Ok(Value::Const {
                t: Type::Char,
                v: *c as u64,
            }),
            parser::Expression::Bool(b) => Ok(Value::Const {
                t: Type::Bool,
                v: *b as u64,
            }),
            parser::Expression::Identifier(id) => match self.vars.get(id) {
                None => {
                    return Err(Spanned {
                        offset: e.offset,
                        len: e.len,
                        line_beginning: e.line_beginning,
                        v: Error::UndefinedVariable,
                    });
                }
                Some(v) => Ok(v.clone()),
            },
            parser::Expression::Binary { left, op, right } => {
                let l = self.add_expr(&left, funcs)?;
                let r = self.add_expr(&right, funcs)?;
                use crate::lexer::Operator as Op;
                let result_type = match (l.get_type(), op, r.get_type()) {
                    (
                        Type::U64,
                        Op::Plus | Op::Minus | Op::Star | Op::Slash,
                        Type::U64 | Type::Char | Type::Bool,
                    ) => Type::U64,
                    (_, Op::Less | Op::More, _) => Type::Bool,
                    (
                        Type::Char,
                        Op::Plus | Op::Minus | Op::Star | Op::Slash,
                        Type::U64 | Type::Char,
                    ) => Type::U64,
                    _ => todo!("report invalid values to binary operation"),
                };
                let place = self.alloc_temp(result_type.clone());
                match op {
                    Op::Plus => self.body.push(Instr::Add { l, r, into: place }),
                    Op::Minus => self.body.push(Instr::Sub { l, r, into: place }),
                    Op::Star => self.body.push(Instr::Mul { l, r, into: place }),
                    Op::Slash => self.body.push(Instr::Div { l, r, into: place }),
                    Op::Percent => self.body.push(Instr::Mod { l, r, into: place }),
                    Op::More => self.body.push(Instr::More { l, r, into: place }),
                    Op::Less => self.body.push(Instr::Less { l, r, into: place }),
                    _ => unreachable!(),
                };
                Ok(Value::Temp {
                    t: result_type,
                    i: place,
                })
            }
            parser::Expression::Unary { op, right } => {
                let r = self.add_expr(&right, funcs)?;
                let result_type = r.get_type().clone();
                let place = self.alloc_temp(result_type.clone());

                use crate::lexer::Operator as Op;
                match op {
                    Op::Not => self.body.push(Instr::LogicalNot { r, into: place }),
                    Op::Minus => self.body.push(Instr::Sub {
                        l: Value::Const { t: Type::U64, v: 0 },
                        r,
                        into: place,
                    }),
                    _ => unreachable!(),
                }
                Ok(Value::Temp {
                    t: result_type.clone(),
                    i: place,
                })
            }
            parser::Expression::FuncCall { name, args } => {
                let receiver = match funcs.get(name) {
                    None => {
                        return Err(Spanned {
                            offset: e.offset,
                            len: e.len,
                            line_beginning: e.line_beginning,
                            v: Error::UndefinedFunction,
                        });
                    }
                    Some(f_type) => f_type,
                };
                if args.len() != receiver.args.len() {
                    return Err(Spanned {
                        offset: e.offset,
                        len: e.len,
                        line_beginning: e.line_beginning,
                        v: Error::MismatchedArgumentCount {
                            callee_type: receiver.clone(),
                            expect: receiver.args.len(),
                            got: args.len(),
                        },
                    });
                }
                let mut send_load = vec![];
                for arg in args {
                    send_load.push(self.add_expr(&arg, funcs)?);
                }
                for i in 0..args.len() {
                    if send_load[i].get_type() != &receiver.args[i] {
                        return Err(Spanned {
                            offset: e.offset,
                            len: e.len,
                            line_beginning: e.line_beginning,
                            v: Error::MismatchedArgumentTypes {
                                callee_type: receiver.clone(),
                                expect: receiver.args.len(),
                                got: args.len(),
                            },
                        });
                    }
                }

                let place = self.alloc_temp(receiver.ret.clone());

                self.body.push(Instr::Call {
                    name: name.to_string(),
                    args: send_load,
                    into: Some(place),
                });

                Ok(Value::Temp {
                    t: receiver.ret.clone(),
                    i: place,
                })
            }
        }
    }
    fn alloc_temp(&mut self, t: Type) -> TempIndex {
        let idx = self.values.len();
        self.values.push(Value::Temp { t, i: idx });
        idx
    }
    fn alloc_label(&mut self) -> LabelIndex {
        let idx = self.max_labels;
        self.max_labels += 1;
        idx
    }
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn temps(&self) -> &[Value] {
        &self.values
    }

    pub fn body(&self) -> &[Instr] {
        &self.body
    }
}

#[derive(Debug)]
pub enum Error {
    MismatchedReturnType {
        got: Type,
        expect: Type,
    },
    VariableRedefinition,
    UnexpectedType {
        got: Type,
        expect: Type,
    },
    UndefinedVariableRedefinition,
    NotBooleanCondition,
    UndefinedFunction,
    MismatchedArgumentCount {
        callee_type: FunctionType,
        expect: usize,
        got: usize,
    },
    MismatchedArgumentTypes {
        callee_type: FunctionType,
        expect: usize,
        got: usize,
    },
    UndefinedVariable,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UndefinedFunction => {
                writeln!(
                    f,
                    "[{}]\n  You tried to call an undefined function",
                    "Error".red()
                )?;
                write!(
                    f,
                    "[{}]\n  Maybe you have misspelled the name?",
                    "Help".green()
                )
            }
            Self::UndefinedVariable => {
                writeln!(
                    f,
                    "[{}]\n  You tried to use an undefined variable",
                    "Error".red()
                )?;
                write!(
                    f,
                    "[{}]\n  Maybe you have misspelled the name?",
                    "Help".green()
                )
            }
            Self::NotBooleanCondition => {
                write!(
                    f,
                    "[{}]\n  You tried to use a non-boolean condition in a `if` or `while` statement",
                    "Error".red()
                )
            }
            Self::UndefinedVariableRedefinition => {
                writeln!(
                    f,
                    "[{}]\n  You tried to redefine an undefined variable",
                    "Error".red()
                )?;
                write!(
                    f,
                    "[{}]\n  Maybe you have misspelled the name?",
                    "Help".green()
                )
            }
            Self::MismatchedArgumentTypes { callee_type, .. } => {
                writeln!(
                    f,
                    "[{}]\n  Called a function with mismatched argument types",
                    "Error".red()
                )?;
                write!(f, "[{}]\n  Callee type: {callee_type}", "Note".blue())
            }
            Self::MismatchedArgumentCount { callee_type, .. } => {
                writeln!(
                    f,
                    "[{}]\n  Called a function with either not enough or too many arguments ",
                    "Error".red()
                )?;
                write!(f, "[{}]\n  Callee type: {callee_type}", "Note".blue())
            }
            Self::UnexpectedType { got, expect } => {
                writeln!(f, "[{}]\n  You mismatched some types", "Error".red())?;
                write!(
                    f,
                    "[{}]\n  Expected: {expect:?}, got: {got:?}",
                    "Note".blue()
                )
            }
            Self::MismatchedReturnType { got, expect } => {
                writeln!(
                    f,
                    "[{}]\n  You tried to return a value that does not match the functions expected return type",
                    "Error".red()
                )?;
                write!(
                    f,
                    "[{}]\n  Expected: {expect:?}, got: {got:?}",
                    "Note".blue()
                )
            }
            Self::VariableRedefinition => {
                write!(f, "[{}]\n  You tried to redefine a variable", "Error".red())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    // Binary Operations
    Assign {
        index: TempIndex,
        v: Value,
    },
    Add {
        l: Value,
        r: Value,
        into: TempIndex,
    },
    Sub {
        l: Value,
        r: Value,
        into: TempIndex,
    },
    Mul {
        l: Value,
        r: Value,
        into: TempIndex,
    },
    Div {
        l: Value,
        r: Value,
        into: TempIndex,
    },
    Mod {
        l: Value,
        r: Value,
        into: TempIndex,
    },
    More {
        l: Value,
        r: Value,
        into: TempIndex,
    },
    Less {
        l: Value,
        r: Value,
        into: TempIndex,
    },
    LogicalNot {
        r: Value,
        into: TempIndex,
    },
    Return {
        v: Option<Value>,
    },
    Label(LabelIndex),
    Jump(LabelIndex),
    Jnz {
        cond: Value,
        to: LabelIndex,
        otherwise: LabelIndex,
    },
    /// If `into` is Some(_) then it returns something and
    /// the result is used later
    Call {
        name: String,
        args: Vec<Value>,
        into: Option<TempIndex>,
    },
}

#[derive(Debug, Clone)]
pub enum Value {
    Const { t: Type, v: u64 },
    Temp { t: Type, i: TempIndex },
}

impl Value {
    pub fn get_type(&self) -> &Type {
        match self {
            Self::Temp { t, i: _ } => t,
            Self::Const { t, v: _ } => t,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    U64,
    Char,
    Bool,
    Void,
}

pub fn type_from_type_name(n: &str) -> Type {
    match n {
        "i32" => Type::U64,
        "char" => Type::Char,
        "bool" => Type::Bool,
        _ => todo!(),
    }
}

pub type TempIndex = usize;
pub type LabelIndex = usize;
