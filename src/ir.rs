use crate::{lexer, parser};
use std::collections::HashMap;

impl Module {
    pub fn new() -> Self {
        Self { functions: vec![] }
    }

    pub fn from_ast(ast: &parser::AstModule) -> Result<Self, Error> {
        let mut s = Self::new();

        for f in ast.funcs() {
            let func = s.add_function(f.v.name.clone());
            for statement in f.v.body() {
                func.add_statement(&statement.v)?;
            }
        }

        Ok(s)
    }

    pub fn functions(&self) -> &[Function] {
        &self.functions
    }

    fn add_function(&mut self, name: String) -> &mut Function {
        self.functions.push(Function {
            name,
            instructions: vec![],
            max_temps: 0,
            max_labels: 0,
            vars: vec![HashMap::new()],
        });
        self.functions.last_mut().unwrap()
    }
}

impl Function {
    pub fn add_statement(&mut self, s: &parser::Statement) -> Result<(), Error> {
        use parser::Statement as s;
        match &s {
            s::Return(i) => {
                let v = self.add_expr(&i.v)?;
                self.add_instr(Instr::Return {
                    value: Value::Temp(v),
                })?;
            }
            s::VarAssign { name, t: _, expr } => {
                let v = self.add_expr(&expr.v)?;
                self.put_var(name, v);
            }
            s::VarReassign { name, expr } => {
                let v = self.add_expr(&expr.v)?;
                let prev = self.get_var(name);
                self.add_instr(Instr::Assign {
                    index: prev,
                    v: Value::Temp(v),
                })?;
            }
            s::If { cond, body } => {
                let v = self.add_expr(&cond.v)?;
                let into = self.alloc_label();
                let over = self.alloc_label();
                self.add_instr(Instr::JumpNotZero {
                    cond: Value::Temp(v),
                    to: into,
                    otherwise: over,
                })?;
                self.add_instr(Instr::Label(into))?;
                self.push_scope();
                for s in body {
                    self.add_statement(&s.v)?;
                }
                self.pop_scope();
                self.add_instr(Instr::Label(over))?;
            }
            s::While { cond, body: b } => {
                let header = self.alloc_label();
                let body = self.alloc_label();
                let over = self.alloc_label();

                self.add_instr(Instr::Label(header))?;
                let v = self.add_expr(&cond.v)?;
                self.add_instr(Instr::JumpNotZero {
                    cond: Value::Temp(v),
                    to: body,
                    otherwise: over,
                })?;

                self.add_instr(Instr::Label(body))?;

                self.push_scope();
                for s in b {
                    self.add_statement(&s.v)?;
                }
                self.pop_scope();
                self.add_instr(Instr::Jump(header))?;
                self.add_instr(Instr::Label(over))?;
            }
            s::FuncCall { name, args } => {
                let mut values = vec![];
                for arg in args {
                    values.push(Value::Temp(self.add_expr(&arg.v)?));
                }
                self.add_instr(Instr::FuncCall {
                    name: name.to_string(),
                    args: values,
                    into: None,
                })?;
            }
            s::FuncDefinition {..} => unreachable!()
        }
        Ok(())
    }

    pub fn add_expr(&mut self, e: &parser::Expression) -> Result<ValueIndex, Error> {
        match e {
            parser::Expression::Integer(i) => {
                let place = self.alloc_temp();
                self.add_instr(Instr::Assign {
                    index: place,
                    v: Value::Const(*i),
                })?;
                Ok(place)
            }
            parser::Expression::Bool(i) => {
                let place = self.alloc_temp();
                self.add_instr(Instr::Assign {
                    index: place,
                    v: Value::Const(*i as u64),
                })?;
                Ok(place)
            }
            parser::Expression::Char(i) => {
                let place = self.alloc_temp();
                self.add_instr(Instr::Assign {
                    index: place,
                    v: Value::Const(*i as u64),
                })?;
                Ok(place)
            }
            parser::Expression::Identifier(i) => Ok(self.get_var(i)),
            parser::Expression::Binary { left, op, right } => {
                let l = self.add_expr(&left.v)?;
                let r = self.add_expr(&right.v)?;
                let place = self.alloc_temp();
                self.add_instr(Instr::BinaryOp {
                    op: *op,
                    l: Value::Temp(l),
                    r: Value::Temp(r),
                    into: place,
                })?;
                Ok(place)
            }
            parser::Expression::Unary { op, right } => {
                let r = self.add_expr(&right.v)?;
                let place = self.alloc_temp();
                self.add_instr(Instr::UnaryOp {
                    op: *op,
                    r: Value::Temp(r),
                    into: place,
                })?;
                Ok(place)
            }
            parser::Expression::FuncCall { name, args } => {
                let mut values = vec![];
                for arg in args {
                    values.push(Value::Temp(self.add_expr(&arg.v)?));
                }
                let place = self.alloc_temp();
                self.add_instr(Instr::FuncCall {
                    name: name.to_string(),
                    args: values,
                    into: Some(place),
                })?;
                Ok(place)
            }
        }
    }

    pub fn add_instr(&mut self, i: Instr) -> Result<(), Error> {
        match i {
            Instr::Assign { index: idx, .. } => {
                if idx >= self.max_temps {
                    return Err(Error::InvalidValueIndex {
                        max: self.max_temps,
                        got: idx,
                    });
                }
            }
            Instr::Return {
                value: Value::Temp(idx),
            } => {
                if idx >= self.max_temps {
                    return Err(Error::InvalidValueIndex {
                        max: self.max_temps,
                        got: idx,
                    });
                }
            }
            Instr::Label(idx) => {
                if idx >= self.max_labels {
                    return Err(Error::InvalidLabel {
                        max: self.max_labels,
                        got: idx,
                    });
                }
            }
            Instr::Jump(idx) => {
                if idx >= self.max_labels {
                    return Err(Error::InvalidLabel {
                        max: self.max_labels,
                        got: idx,
                    });
                }
            }
            Instr::UnaryOp {
                r: Value::Temp(idx),
                ..
            } => {
                if idx >= self.max_temps {
                    return Err(Error::InvalidValueIndex {
                        max: self.max_temps,
                        got: idx,
                    });
                }
            }
            Instr::BinaryOp {
                l: Value::Temp(idx_1),
                r: Value::Temp(idx_2),
                ..
            } => {
                if idx_1 >= self.max_temps {
                    return Err(Error::InvalidValueIndex {
                        max: self.max_temps,
                        got: idx_1,
                    });
                }
                if idx_2 >= self.max_temps {
                    return Err(Error::InvalidValueIndex {
                        max: self.max_temps,
                        got: idx_2,
                    });
                }
            }
            Instr::BinaryOp {
                l: Value::Temp(idx_1),
                ..
            } => {
                if idx_1 >= self.max_temps {
                    return Err(Error::InvalidValueIndex {
                        max: self.max_temps,
                        got: idx_1,
                    });
                }
            }
            Instr::BinaryOp {
                r: Value::Temp(idx_2),
                ..
            } => {
                if idx_2 >= self.max_temps {
                    return Err(Error::InvalidValueIndex {
                        max: self.max_temps,
                        got: idx_2,
                    });
                }
            }
            Instr::JumpNotZero {
                cond: Value::Temp(idx),
                to,
                otherwise,
            } => {
                if idx >= self.max_temps {
                    return Err(Error::InvalidValueIndex {
                        max: self.max_temps,
                        got: idx,
                    });
                }
                if to >= self.max_labels {
                    return Err(Error::InvalidLabel {
                        max: self.max_labels,
                        got: to,
                    });
                }
                if otherwise >= self.max_labels {
                    return Err(Error::InvalidLabel {
                        max: self.max_labels,
                        got: otherwise,
                    });
                }
            }
            Instr::FuncCall { into, .. } => {
                if into.is_some_and(|into| into >= *self.max_temps()) {
                    return Err(Error::InvalidValueIndex {
                        max: self.max_temps,
                        got: into.unwrap(),
                    });
                }
            }
            _ => unreachable!(),
        }
        self.instructions.push(i);
        Ok(())
    }

    pub fn push_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) {
        self.vars.pop().unwrap();
    }

    fn get_var(&self, name: &str) -> ValueIndex {
        for s in self.vars.iter().rev() {
            if let Some(v) = s.get(name) {
                return *v;
            }
        }
        unreachable!("Type checker should have caught this")
    }

    fn put_var(&mut self, name: &str, v: ValueIndex) {
        self.vars.last_mut().unwrap().insert(name.to_string(), v);
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn instructions(&self) -> &[Instr] {
        &self.instructions
    }

    pub fn max_labels(&self) -> &usize {
        &self.max_labels
    }

    pub fn max_temps(&self) -> &usize {
        &self.max_temps
    }

    fn alloc_temp(&mut self) -> ValueIndex {
        self.max_temps += 1;
        self.max_temps - 1
    }
    fn alloc_label(&mut self) -> LabelIndex {
        self.max_labels += 1;
        self.max_labels - 1
    }
}

#[derive(Debug)]
pub struct Module {
    functions: Vec<Function>,
}

#[derive(Debug, Clone, Default)]
pub struct Function {
    name: String,
    instructions: Vec<Instr>,
    vars: ScopeStack,
    max_labels: usize,
    max_temps: usize,
}

#[derive(Debug, Clone)]
pub enum Instr {
    Assign {
        index: ValueIndex,
        v: Value,
    },
    BinaryOp {
        op: lexer::Operator,
        l: Value,
        r: Value,
        into: ValueIndex,
    },
    UnaryOp {
        op: lexer::Operator,
        r: Value,
        into: ValueIndex,
    },
    Return {
        value: Value,
    },
    Label(LabelIndex),
    Jump(LabelIndex),
    JumpNotZero {
        cond: Value,
        to: LabelIndex,
        otherwise: LabelIndex,
    },
    FuncCall {
        name: String,
        args: Vec<Value>,
        into: Option<ValueIndex>,
    },
}

#[derive(Debug)]
pub enum Error {
    InvalidValueIndex { max: ValueIndex, got: ValueIndex },
    InvalidLabel { max: LabelIndex, got: LabelIndex },
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Const(u64),
    Temp(ValueIndex),
}

pub type ValueIndex = usize;
pub type LabelIndex = usize;
pub type ScopeStack = Vec<HashMap<String, ValueIndex>>;
