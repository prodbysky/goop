use std::collections::HashMap;

use super::error::Error;
use crate::frontend::parser::parser;
use crate::location::{Span, Spanned};

#[derive(Debug, Clone)]
pub struct Module {
    functions: Vec<Function>,
}

impl Module {
    pub fn from_ast(ast_module: parser::Module) -> Result<Self, Spanned<Error>> {
        let mut s = Self { functions: vec![] };
        let mut func_types = HashMap::new();
        for f in ast_module.funcs() {
            let t = f.v.get_type();
            func_types.insert(t.name.to_string(), t);
        }

        for f in ast_module.funcs() {
            let f_type = f.v.get_type();
            match f.v.body() {
                None => {
                    s.add_function(f.v.name.clone(), f.v.get_type(), SymbolVisibility::External);
                }
                Some(b) => {
                    let func = s.add_function(
                        f.v.name.clone(),
                        f.v.get_type(),
                        SymbolVisibility::Exported,
                    );
                    for arg in &f_type.args {
                        let index = func.alloc_temp(arg.ty.clone());
                        func.put_var(
                            arg.name(),
                            Value::Temp {
                                t: arg.ty.clone(),
                                i: index,
                            },
                        );
                    }
                    for st in b {
                        func.add_statement(st, &func_types)?;
                    }
                }
            }
        }

        Ok(s)
    }

    fn add_function(
        &mut self,
        name: String,
        fn_type: parser::FunctionType,
        function_visibility: SymbolVisibility,
    ) -> &mut Function {
        self.functions.push(Function {
            name,
            body: vec![],
            ret_type: fn_type.ret,
            values: vec![],
            max_labels: 0,
            vars: vec![HashMap::new()],
            args: fn_type.args,
            visibility: function_visibility,
        });
        self.functions.last_mut().unwrap()
    }

    pub fn functions(&self) -> &[Function] {
        &self.functions
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Module:")?;
        for func in self.functions() {
            writeln!(f, "    Function: {}", func.name())?;
            writeln!(f, "        Arguments: {:?}", func.args())?;
            writeln!(f, "        Return type: {:?}", func.ret_type)?;
            if !func.is_external() {
                writeln!(f, "        Body:")?;
                for st in &func.body {
                    writeln!(f, "            {st}")?;
                }
            } else {
                writeln!(f, "            External")?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    ret_type: Type,
    name: String,
    body: Vec<Instr>,
    values: Vec<Value>,
    vars: Vec<HashMap<String, Value>>,
    max_labels: LabelIndex,
    args: Vec<FunctionArgument>,
    visibility: SymbolVisibility,
}

// TODO: Private visibility
#[derive(Debug, Clone, Copy)]
pub enum SymbolVisibility {
    /// Defined somewhere else not in this module
    External,
    /// Will be public in the final object file
    Exported,
}

impl SymbolVisibility {
    pub fn is_external(&self) -> bool {
        matches!(self, SymbolVisibility::External)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionArgument {
    name: String,
    ty: Type,
}

impl FunctionArgument {
    pub fn new(name: String, ty: Type) -> Self {
        Self { name, ty }
    }
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

impl Function {
    pub fn get_type(&self) -> parser::FunctionType {
        parser::FunctionType {
            name: self.name.clone(),
            ret: self.ret_type.clone(),
            args: self.args.clone(),
        }
    }

    pub fn args(&self) -> &[FunctionArgument] {
        &self.args
    }

    pub fn is_external(&self) -> bool {
        self.visibility.is_external()
    }

    pub fn visibility(&self) -> SymbolVisibility {
        self.visibility
    }

    fn add_statement(
        &mut self,
        s: &Spanned<parser::Statement>,
        funcs: &HashMap<String, parser::FunctionType>,
    ) -> Result<(), Spanned<Error>> {
        match &s.v {
            parser::Statement::Return(Some(v)) => {
                let v_ir = self.add_expr(v, funcs)?;
                if *v_ir.get_type() != self.ret_type {
                    return Err(Spanned::new(
                        Error::MismatchedReturnType {
                            got: v_ir.get_type().clone(),
                            expect: self.ret_type.clone(),
                        },
                        Span::new(v.begin(), v.end()),
                    ));
                }
                self.body.push(Instr::Return { v: Some(v_ir) });
            }
            parser::Statement::Return(None) => {
                if Type::Void != self.ret_type {
                    todo!();
                }
                self.body.push(Instr::Return { v: None });
            }
            parser::Statement::VarAssign { name, t, expr } => {
                if self.get_var(name).is_some() {
                    return Err(Spanned::new(
                        Error::VariableRedefinition,
                        Span::new(s.begin(), s.end()),
                    ));
                }
                let v = self.add_expr(expr, funcs)?;
                if t.is_some() {
                    let t = t.clone().unwrap();
                    if v.get_type() != &type_from_type_name(&t) {
                        return Err(Spanned::new(
                            Error::UnexpectedType {
                                got: v.get_type().clone(),
                                expect: type_from_type_name(&t),
                            },
                            Span::new(s.begin(), s.end()),
                        ));
                    }
                }
                self.put_var(name, v);
            }
            parser::Statement::VarReassign { name, expr } => {
                if self.get_var(name).is_none() {
                    return Err(Spanned::new(
                        Error::UndefinedVariableRedefinition,
                        Span::new(s.begin(), s.end()),
                    ));
                }
                let v = self.add_expr(expr, funcs)?;
                let prev = match self.get_var(name).unwrap() {
                    Value::Temp { t: _, i } => *i,
                    _ => unreachable!(),
                };
                self.body.push(Instr::Assign { index: prev, v });
            }
            parser::Statement::If { cond, body } => {
                self.push_scope();
                let cond = self.add_expr(cond, funcs)?;
                if cond.get_type() != &Type::Bool {
                    return Err(Spanned::new(
                        Error::NotBooleanCondition,
                        Span::new(s.begin(), s.end()),
                    ));
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
                    self.add_statement(s, funcs)?;
                }
                self.body.push(Instr::Label(over));
                self.pop_scope();
            }
            parser::Statement::While { cond, body } => {
                let header = self.alloc_label();
                let into = self.alloc_label();
                let over = self.alloc_label();
                self.push_scope();
                self.body.push(Instr::Label(header));
                let cond = self.add_expr(cond, funcs)?;
                self.body.push(Instr::Jnz {
                    cond,
                    to: into,
                    otherwise: over,
                });
                self.body.push(Instr::Label(into));
                for s in body {
                    self.add_statement(s, funcs)?;
                }
                self.body.push(Instr::Jump(header));
                self.body.push(Instr::Label(over));
                self.pop_scope();
            }
            parser::Statement::FuncCall { name, args } => {
                let mut args_into = vec![];

                let receiver = match funcs.get(name) {
                    None => {
                        return Err(Spanned::new(
                            Error::UndefinedFunction,
                            Span::new(s.begin(), s.end()),
                        ));
                    }
                    Some(r) => r,
                };

                if receiver.args.len() != args.len() {
                    return Err(Spanned::new(
                        Error::MismatchedArgumentCount {
                            callee_type: receiver.clone(),
                            expect: receiver.args.len(),
                            got: args.len(),
                        },
                        Span::new(s.begin(), s.end()),
                    ));
                }

                for arg in args {
                    args_into.push(self.add_expr(arg, funcs)?);
                }

                for i in 0..args.len() {
                    if args_into[i].get_type() != receiver.args[i].ty() {
                        return Err(Spanned::new(
                            Error::MismatchedArgumentTypes {
                                callee_type: receiver.clone(),
                                expect: receiver.args.len(),
                                got: args.len(),
                            },
                            Span::new(s.begin(), s.end()),
                        ));
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

    fn get_var(&self, name: &str) -> Option<&Value> {
        for scope in self.vars.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v);
            }
        }
        None
    }
    fn put_var(&mut self, name: &str, v: Value) {
        self.vars.last_mut().unwrap().insert(name.to_string(), v);
    }

    fn push_scope(&mut self) {
        self.vars.push(HashMap::new());
    }
    fn pop_scope(&mut self) {
        self.vars.pop();
    }

    fn add_expr(
        &mut self,
        e: &Spanned<parser::Expression>,
        funcs: &HashMap<String, parser::FunctionType>,
    ) -> Result<Value, Spanned<Error>> {
        match &e.v {
            parser::Expression::Integer(i) => {
                let place = self.alloc_temp(Type::U64);
                self.body.push(Instr::Assign {
                    index: place,
                    v: Value::Const {
                        t: Type::U64,
                        v: *i,
                    },
                });
                Ok(Value::Temp {
                    t: Type::U64,
                    i: place,
                })
            }
            parser::Expression::Char(c) => {
                let place = self.alloc_temp(Type::Char);
                self.body.push(Instr::Assign {
                    index: place,
                    v: Value::Const {
                        t: Type::Char,
                        v: *c as u64,
                    },
                });
                Ok(Value::Temp {
                    t: Type::Char,
                    i: place,
                })
            }
            parser::Expression::Bool(b) => {
                let place = self.alloc_temp(Type::Bool);
                self.body.push(Instr::Assign {
                    index: place,
                    v: Value::Const {
                        t: Type::Bool,
                        v: *b as u64,
                    },
                });
                Ok(Value::Temp {
                    t: Type::Bool,
                    i: place,
                })
            }
            parser::Expression::Identifier(id) => match self.get_var(id) {
                None => Err(Spanned::new(
                    Error::UndefinedVariable,
                    Span::new(e.begin(), e.end()),
                )),
                Some(v) => Ok(v.clone()),
            },
            parser::Expression::Binary { left, op, right } => {
                let l = self.add_expr(left, funcs)?;
                let r = self.add_expr(right, funcs)?;
                use crate::frontend::lexer::lexer::Operator as Op;
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
                let r = self.add_expr(right, funcs)?;
                let result_type = r.get_type().clone();
                let place = self.alloc_temp(result_type.clone());

                use crate::frontend::lexer::lexer::Operator as Op;
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
                        return Err(Spanned::new(
                            Error::UndefinedFunction,
                            Span::new(e.begin(), e.end()),
                        ));
                    }
                    Some(f_type) => f_type,
                };
                if args.len() != receiver.args.len() {
                    return Err(Spanned::new(
                        Error::MismatchedArgumentCount {
                            callee_type: receiver.clone(),
                            expect: receiver.args.len(),
                            got: args.len(),
                        },
                        Span::new(e.begin(), e.end()),
                    ));
                }
                let mut send_load = vec![];
                for arg in args {
                    send_load.push(self.add_expr(arg, funcs)?);
                }
                for i in 0..args.len() {
                    if send_load[i].get_type() != receiver.args[i].ty() {
                        return Err(Spanned::new(
                            Error::MismatchedArgumentTypes {
                                callee_type: receiver.clone(),
                                expect: receiver.args.len(),
                                got: args.len(),
                            },
                            Span::new(e.begin(), e.end()),
                        ));
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
            parser::Expression::Cast { value, to } => {
                let dest_type = type_from_type_name(to);
                let place = self.alloc_temp(dest_type.clone());
                let v = self.add_expr(value, funcs)?;
                self.body.push(Instr::Cast {
                    v,
                    into_type: dest_type.clone(),
                    into_index: place,
                });
                Ok(Value::Temp {
                    t: dest_type,
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
    Cast {
        v: Value,
        into_type: Type,
        into_index: TempIndex,
    },
}

impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Call { name, args, into } => {
                write!(f, "{name}({args:?}) -> {into:?}")
            }
            Self::Add { l, r, into } => {
                write!(f, "Temp[{into}] = {l:?} + {r:?}")
            }
            Self::Sub { l, r, into } => {
                write!(f, "Temp[{into}] = {l:?} - {r:?}")
            }
            Self::Mul { l, r, into } => {
                write!(f, "Temp[{into}] = {l:?} * {r:?}")
            }
            Self::Div { l, r, into } => {
                write!(f, "Temp[{into}] = {l:?} / {r:?}")
            }
            Self::Mod { l, r, into } => {
                write!(f, "Temp[{into}] = {l:?} % {r:?}")
            }
            Self::Less { l, r, into } => {
                write!(f, "Temp[{into}] = {l:?} < {r:?}")
            }
            Self::More { l, r, into } => {
                write!(f, "Temp[{into}] = {l:?} > {r:?}")
            }
            Self::Assign { index, v } => {
                write!(f, "Temp[{index}] = {v:?}")
            }
            Self::Jnz {
                cond,
                to,
                otherwise,
            } => {
                write!(f, "Jump if {cond:?} != 0 to {to}, otherwise {otherwise}")
            }
            Self::Jump(to) => {
                write!(f, "Jump to {to}")
            }
            Self::LogicalNot { r, into } => {
                write!(f, "Temp[{into}] = !{r:?}")
            }
            Self::Label(n) => {
                write!(f, "Label: {n}")
            }
            Self::Return { v } => {
                write!(f, "Return {v:?}")
            }
            Self::Cast {
                v,
                into_type,
                into_index,
            } => {
                write!(f, "Temp[{into_index}] = Cast({v:?}, into {into_type:?})")
            }
        }
    }
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
        _ => todo!(),
    }
}

pub type TempIndex = usize;
pub type LabelIndex = usize;
