use crate::{lexer, parser, Spanned};
use std::collections::HashMap;


impl Module {
    pub fn new() -> Self {
        Self { functions: vec![] }
    }

    pub fn from_ast(mut self, ast: &[Spanned<parser::Statement>]) -> Result<Self, Error>{
        use parser::Statement as s;
        let main = self.add_function("main".to_string());
        for node in ast {
            match &node.v {
                s::Return(i) => {
                }
                _ => todo!()
            }
        }
        Ok(self)
    }

    fn gen_expr(&mut self, f: &mut Function, e: &parser::Expression) -> Result<ValueIndex, Error>{
        match e {
            parser::Expression::Integer(i) => {
                let place = f.alloc_temp();
                f.add_instr(Instr::Assign { index: place, v: Value::Const(*i) })?;
                Ok(place)
            }
            parser::Expression::Bool(i) => {
                let place = f.alloc_temp();
                f.add_instr(Instr::Assign { index: place, v: Value::Const(*i as u64) })?;
                Ok(place)
            }
            parser::Expression::Char(i) => {
                let place = f.alloc_temp();
                f.add_instr(Instr::Assign { index: place, v: Value::Const(*i as u64) })?;
                Ok(place)
            }
            parser::Expression::Identifier(i) => {
                Ok(*f.vars.get(i).unwrap())
            }
            parser::Expression::Binary { left, op, right } => {
                let l = self.gen_expr(f, &left.v)?;
                let r = self.gen_expr(f, &right.v)?;
                let place = f.alloc_temp();

                match op {
                    lexer::Operator::Plus => {
                        f.a

                    }
                }
            }
        }
    }

    fn add_function(&mut self, name: String) -> &mut Function {
        self.functions.push(Function { name, instructions: vec![], max_temps: 0, max_labels: 0, vars: HashMap::new() });
        self.functions.first_mut().unwrap()
    }
}

impl Function {
    pub fn alloc_temp(&mut self) -> ValueIndex {
        self.max_temps += 1;
        self.max_temps - 1
    }
    pub fn alloc_label(&mut self) -> LabelIndex {
        self.max_labels += 1;
        self.max_labels - 1
    }
    pub fn add_instr(&mut self, i: Instr) -> Result<(), Error> {
        match i {
            Instr::Assign { index: idx, .. } => {
                if idx >= self.max_temps {
                    return Err(Error::InvalidValueIndex { max: self.max_temps, got: idx });
                }
            }
            Instr::Return { value: Value::Temp(idx)} => {
                if idx >= self.max_temps {
                    return Err(Error::InvalidValueIndex { max: self.max_temps, got: idx });
                }
            }
            Instr::Label(idx) => {
                if idx >= self.max_labels {
                    return Err(Error::InvalidLabel { max: self.max_labels, got: idx });
                }
            }
            Instr::Jump(idx) => {
                if idx >= self.max_labels {
                    return Err(Error::InvalidLabel { max: self.max_labels, got: idx });
                }
            }
            Instr::UnaryOp { r: Value::Temp(idx), .. } => {
                if idx >= self.max_temps {
                    return Err(Error::InvalidValueIndex { max: self.max_temps, got: idx });
                }
            }
            Instr::BinaryOp { l: Value::Temp(idx_1), r: Value::Temp(idx_2), .. } => {
                if idx_1 >= self.max_temps {
                    return Err(Error::InvalidValueIndex { max: self.max_temps, got: idx_1 });
                }
                if idx_2 >= self.max_temps {
                    return Err(Error::InvalidValueIndex { max: self.max_temps, got: idx_2 });
                }
            }
            Instr::BinaryOp { l: Value::Temp(idx_1), .. } => {
                if idx_1 >= self.max_temps {
                    return Err(Error::InvalidValueIndex { max: self.max_temps, got: idx_1 });
                }
            }
            Instr::BinaryOp { r: Value::Temp(idx_2), .. } => {
                if idx_2 >= self.max_temps {
                    return Err(Error::InvalidValueIndex { max: self.max_temps, got: idx_2 });
                }
            }
            Instr::JumpNotZero { cond: Value::Temp(idx), to, otherwise } => {
                if idx >= self.max_temps {
                    return Err(Error::InvalidValueIndex { max: self.max_temps, got: idx });
                }
                if to >= self.max_labels {
                    return Err(Error::InvalidLabel { max: self.max_labels, got: to });
                }
                if otherwise >= self.max_labels {
                    return Err(Error::InvalidLabel { max: self.max_labels, got: otherwise });
                }
            }
            _ => unreachable!()
        }
        self.instructions.push(i);
        Ok(())
    }
}

pub enum Error {
    InvalidValueIndex {
        max: ValueIndex,
        got: ValueIndex,
    },
    InvalidLabel {
        max: LabelIndex,
        got: LabelIndex,
    }
}

#[derive(Debug, Clone, Default)]
pub struct Function {
    name: String,
    instructions: Vec<Instr>,
    vars: Vars,
    max_labels: usize,
    max_temps: usize
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Const(u64),
    Temp(ValueIndex)
}

pub type ValueIndex = usize;
pub type LabelIndex = usize;

#[derive(Debug, Clone)]
pub enum Instr {
    Assign {
        index: ValueIndex,
        v: Value
    },
    BinaryOp {
        op: lexer::Operator,
        l: Value,
        r: Value,
        into: ValueIndex
    },
    UnaryOp {
        op: lexer::Operator,
        r: Value,
        into: ValueIndex
    },
    Return {
        value: Value,
    },
    Label(LabelIndex),
    Jump(LabelIndex),
    JumpNotZero { 
        cond: Value, 
        to: LabelIndex,
        otherwise: LabelIndex
    },
}

pub type Vars = HashMap<String, ValueIndex>;

pub struct Module {
    functions: Vec<Function>
}
