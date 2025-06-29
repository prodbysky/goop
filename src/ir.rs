use std::collections::HashMap;

use crate::{Spanned, logging, parser};
use colored::Colorize;

#[derive(Debug, Clone)]
pub struct Module {
    functions: Vec<Function>,
}

impl Module {
    pub fn from_ast<'a>(ast_module: parser::Module) -> Result<Self, Spanned<Error>> {
        let mut s = Self { functions: vec![] };

        let mut func_types = HashMap::new();
        for f in ast_module.funcs() {
            let t = f.v.get_type();
            func_types.insert(t.name.to_string(), t);
        }

        for f in ast_module.funcs() {
            let f_type = f.v.get_type();
            let ext = match f.v.body() {
                Some(_) => false,
                None => true
            };
            let func = s.add_function(f.v.name.clone(), f.v.get_type(), ext);
            match f.v.body() {
                Some(b) => {
                for arg in &f_type.args {
                    let index = func.alloc_temp(arg.1.clone());
                    func.vars.insert(arg.0.clone(), Value::Temp{t: arg.1.to_owned(), i: index});
                }
                for st in b {
                    func.add_statement(&st, &func_types)?;
                }
                },
                None => {}
            }
        }

        Ok(s)
    }

    fn add_function(&mut self, name: String, fn_type: parser::FunctionType, external: bool) -> &mut Function {
        self.functions.push(Function {
            name,
            body: vec![],
            ret_type: fn_type.ret,
            values: vec![],
            max_labels: 0,
            vars: HashMap::new(),
            args: fn_type.args,
            external
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
pub struct Function {
    ret_type: Type,
    name: String,
    body: Vec<Instr>,
    values: Vec<Value>,
    vars: HashMap<String, Value>,
    max_labels: LabelIndex,
    args: Vec<(String, Type)>,
    pub external: bool
}

impl Function {
    pub fn get_type(&self) -> parser::FunctionType {
        parser::FunctionType {
            name: self.name.clone(),
            ret: self.ret_type.clone(),
            args: self.args.clone(),
        }
    }

    pub fn args(&self) -> &[(String, Type)] {
        &self.args
    }
    fn add_statement(
        &mut self,
        s: &Spanned<parser::Statement>,
        funcs: &HashMap<String, parser::FunctionType>,
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
                    if args_into[i].get_type() != &receiver.args[i].1 {
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
        funcs: &HashMap<String, parser::FunctionType>,
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
                    if send_load[i].get_type() != &receiver.args[i].1 {
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
        callee_type: parser::FunctionType,
        expect: usize,
        got: usize,
    },
    MismatchedArgumentTypes {
        callee_type: parser::FunctionType,
        expect: usize,
        got: usize,
    },
    UndefinedVariable,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UndefinedFunction => {
                logging::errorln!(f, "You tried to call an undefined function")?;
                logging::help!(f, "Maybe you have misspelled the name?")
            }
            Self::UndefinedVariable => {
                logging::errorln!(f, "You tried to use an undefined variable")?;
                logging::help!(f, "Maybe you have misspelled the name?")
            }
            Self::NotBooleanCondition => {
                logging::error!(
                    f,
                    "You tried to use a non-boolean condition in a `if` or `while` statement"
                )
            }
            Self::UndefinedVariableRedefinition => {
                logging::errorln!(f, "You tried to redefine an undefined variable",)?;
                logging::help!(f, "Maybe you have misspelled the name?",)
            }
            Self::MismatchedArgumentTypes { callee_type, .. } => {
                logging::errorln!(f, "Called a function with mismatched argument types",)?;
                logging::note!(f, "Callee type: {callee_type}")
            }
            Self::MismatchedArgumentCount { callee_type, .. } => {
                logging::errorln!(
                    f,
                    "Called a function with either not enough or too many arguments",
                )?;
                logging::note!(f, "Callee type: {callee_type}")
            }
            Self::UnexpectedType { got, expect } => {
                logging::errorln!(f, "You mismatched some types")?;
                logging::note!(f, "Expected: {expect:?}, got: {got:?}",)
            }
            Self::MismatchedReturnType { got, expect } => {
                logging::errorln!(
                    f,
                    "You tried to return a value that does not match the functions expected return type"
                )?;
                logging::note!(f, "Expected: {expect:?}, got: {got:?}")
            }
            Self::VariableRedefinition => {
                logging::error!(f, "You tried to redefine a variable")
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
        "void" => Type::Void,
        _ => todo!()
    }
}

pub type TempIndex = usize;
pub type LabelIndex = usize;
