use std::collections::HashMap;

use crate::{Spanned, parser};

#[derive(Debug, Clone)]
pub struct Module {
    functions: Vec<Function>
}


#[derive(Debug)]
pub enum Error {}

impl Module {
    pub fn from_ast(ast_module: parser::AstModule) -> Result<Self, Spanned<Error>> {
        let mut s = Self {functions: vec![]};

        let mut func_types = HashMap::new();
        for f in ast_module.funcs() {
            func_types.insert(f.v.name.clone(), f.v.get_type());
        }

        for f in ast_module.funcs() {
            let func = s.add_function(f.v.name.clone(), f.v.get_type());
            for st in f.v.body() {
                func.add_statement(&st.v, &func_types);
            }
        }


        Ok(s)
    }

    fn add_function(&mut self, name: String, fn_type: FunctionType) -> &mut Function {
        self.functions.push(Function { name, body: vec![], ret_type: fn_type.ret, values: vec![], max_labels: 0, vars: HashMap::new()});
        self.functions.last_mut().unwrap()
    }

    fn get_function_mut(&mut self, name: &str) -> Option<&mut Function> {
        self.functions.iter_mut().find(|v| v.name.as_str() == name)
    }
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub ret: Type,
    pub args: Vec<Type>,
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
    fn add_statement(&mut self, s: &parser::Statement, funcs: &HashMap<String, FunctionType>) {
        match s {
            parser::Statement::Return(v) => {
                let v = self.add_expr(&v.v, funcs);
                if *v.get_type() != self.ret_type {
                    todo!("report type mismatch of return type with actual return type")
                }
                self.body.push(Instr::Return { v: Some(v) });
            }
            parser::Statement::VarAssign { name, t, expr } => {
                if self.vars.get(name).is_some() {
                    todo!("report variable redefinition error")
                }
                let v = self.add_expr(&expr.v, funcs);
                if v.get_type() != &type_from_type_name(t) {
                    todo!("report mismatched type from the expected one")
                }
                self.vars.insert(name.to_string(), v);
            }
            parser::Statement::VarReassign { name, expr } => {
                if self.vars.get(name).is_none() {
                    todo!("report trying to redefine variable which is undefined")
                }
                let v = self.add_expr(&expr.v, funcs);
                let prev = match self.vars.get(name).unwrap() {
                    Value::Temp { t: _, i } => *i,
                    _ => unreachable!()
                };
                self.body.push(Instr::Assign { index: prev, v });
            }
            parser::Statement::If { cond, body } => {
                let cond = self.add_expr(&cond.v, funcs);
                if cond.get_type() != &Type::Bool {
                    todo!("report non-boolean condition in if statement")
                }
                let into = self.alloc_label();
                let over = self.alloc_label();
                self.body.push(Instr::Jnz { cond, to: into, otherwise: over });
                self.body.push(Instr::Label(into));
                for s in body {
                    self.add_statement(&s.v, funcs);
                }
                self.body.push(Instr::Label(over));
            }
            parser::Statement::While { cond, body } => {
                let header = self.alloc_label();
                let into = self.alloc_label();
                let over = self.alloc_label();
                self.body.push(Instr::Label(header));
                let cond = self.add_expr(&cond.v, funcs);
                self.body.push(Instr::Jnz { cond: cond, to: into, otherwise: over });
                self.body.push(Instr::Label(into));
                for s in body {
                    self.add_statement(&s.v, funcs);
                }
                self.body.push(Instr::Jump(header));
                self.body.push(Instr::Label(over));
            }
            parser::Statement::FuncCall { name, args } => {
                let mut args_into = vec![];

                let receiver = match funcs.get(name) {
                    None => todo!("report undefined function call"),
                    Some(r) => r
                };
                
                if receiver.args.len() != args.len() {
                    todo!("report mismatched count of arguments suplied to function")
                }

                for arg in args {
                    args_into.push(self.add_expr(&arg.v, funcs));
                }

                for i in 0..args.len() {
                    if args_into[i].get_type() != &receiver.args[i] {
                        todo!("report mismatched type in function arguments")
                    }
                }
                self.body.push(Instr::Call { name: name.to_string(), args: args_into, into: None });
            }
        }
    }

    fn add_expr(&mut self, e: &parser::Expression, funcs: &HashMap<String, FunctionType>) -> Value {
        match e {
            parser::Expression::Integer(i) => {
                Value::Const { t: Type::U64, v: *i }
            }
            parser::Expression::Char(c) => {
                Value::Const { t: Type::Char, v: *c as u64 }
            }
            parser::Expression::Bool(b) => {
                Value::Const { t: Type::Bool, v: *b as u64 }
            }
            parser::Expression::Identifier(id) => {
                match self.vars.get(id) {
                    None => todo!("report undefined variable"),
                    Some(v) => v.clone()
                }
            }
            parser::Expression::Binary { left, op, right } => {
                let l = self.add_expr(&left.v, funcs);
                let r = self.add_expr(&right.v, funcs);
                use crate::lexer::Operator as Op;
                let result_type = match (l.get_type(), op, r.get_type()) {
                    (Type::U64, Op::Plus | Op::Minus | Op::Star | Op::Slash, Type::U64 | Type::Char | Type::Bool) => Type::U64,
                    (_, Op::Less | Op::More, _) => Type::Bool,
                    (Type::Char, Op::Plus | Op::Minus | Op::Star | Op::Slash, Type::U64 | Type::Char) => Type::U64,
                    _ => todo!("report invalid values to binary operation")
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
                    _ => unreachable!()
                };
                Value::Temp { t: result_type, i: place }
            }
            parser::Expression::Unary { op, right } => {
                let r = self.add_expr(&right.v, funcs);
                let result_type = r.get_type().clone();
                let place = self.alloc_temp(result_type.clone());

                use crate::lexer::Operator as Op;
                match op {
                    Op::Not => self.body.push(Instr::LogicalNot { r, into: place }),
                    Op::Minus => self.body.push(Instr::Sub { l: Value::Const { t: Type::U64, v: 0 }, r, into: place }),
                    _ => unreachable!()
                }
                Value::Temp { t: result_type.clone(), i: place }
            }
            parser::Expression::FuncCall { name, args } => {
                let receiver = match funcs.get(name) {
                    None => todo!("report undefined function call"),
                    Some(f_type) => f_type
                };
                if args.len() != receiver.args.len() {
                    todo!("report unexpected amount of arguments")
                }
                let mut send_load = vec![];
                for arg in args {
                    send_load.push(self.add_expr(&arg.v, funcs));
                }
                for i in 0..args.len() {
                    if send_load[i].get_type() != &receiver.args[i] {
                        todo!("report mismatched types of arguments")
                    }
                }

                let place = self.alloc_temp(receiver.ret.clone());

                self.body.push(Instr::Call { name: name.to_string(), args: send_load, into: Some(place) });

                Value::Temp { t: receiver.ret.clone(), i: place }

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
}

#[derive(Debug, Clone)]
pub enum Instr {
    // Binary Operations
    Assign {
        index: TempIndex,
        v: Value
    },
    Add {
        l: Value,
        r: Value,
        into: TempIndex
    },
    Sub {
        l: Value,
        r: Value,
        into: TempIndex
    },
    Mul {
        l: Value,
        r: Value,
        into: TempIndex
    },
    Div {
        l: Value,
        r: Value,
        into: TempIndex
    },
    Mod {
        l: Value,
        r: Value,
        into: TempIndex
    },
    More {
        l: Value,
        r: Value,
        into: TempIndex
    },
    Less {
        l: Value,
        r: Value,
        into: TempIndex
    },
    LogicalNot {
        r: Value,
        into: TempIndex
    },
    Return {
        v: Option<Value>
    },
    Label(LabelIndex),
    Jump(LabelIndex),
    Jnz {
        cond: Value,
        to: LabelIndex,
        otherwise: LabelIndex
    },
    /// If `into` is Some(_) then it returns something and
    /// the result is used later
    Call {
        name: String,
        args: Vec<Value>,
        into: Option<TempIndex>
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Const {
        t: Type,
        v: u64
    },
    Temp {
        t: Type,
        i: TempIndex
    },
}

impl Value {
    pub fn get_type(&self) -> &Type {
        match self {
            Self::Temp { t, i: _} => t,
            Self::Const { t, v: _ } => t
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    U64,
    Char,
    Bool
}

fn type_from_type_name(n: &str) -> Type {
    match n {
        "i32" => Type::U64,
        "char" => Type::Char,
        "bool" => Type::Bool,
        _ => todo!()

    }
}

pub type TempIndex = usize;
pub type LabelIndex = usize;
