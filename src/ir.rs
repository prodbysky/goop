use crate::{lexer, parser, Spanned};
use std::collections::HashMap;

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
        r: ValueIndex,
        into: ValueIndex
    },
    Return {
        value: Value,
    },
    Label(LabelIndex),
    Jump(LabelIndex),
    JumpNotZero { 
        index: ValueIndex, 
        to: LabelIndex,
        otherwise: LabelIndex
    },
}

pub type Vars = HashMap<String, ValueIndex>;


#[derive(Debug, Default)]
pub struct IREmmiter {
    ir: Vec<Instr>,
    vars: Vars,
    temp_count: usize,
    label_count: usize,
}

impl IREmmiter {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn generate_ir(mut self, module: &[Spanned<parser::Statement>]) -> Vec<Instr> {
        for s in module {
            self.generate_statement(&s.v);
        }

        self.ir
    }

    fn generate_statement(&mut self, s: &parser::Statement) {
        match s {
            parser::Statement::Return(v) => {
                let e = self.generate_expr(&v.v);
                self.ir.push(Instr::Return { value: Value::Temp(e) });
            }
            parser::Statement::VarAssign { name, t: _, expr } => {
                let e = self.generate_expr(&expr.v);
                self.vars.insert(name.to_string(), e);
            }
            parser::Statement::If { cond, body } => {
                let e = self.generate_expr(&cond.v);
                let body_idx = self.new_label();
                let over = self.new_label();
                self.ir.push(Instr::JumpNotZero {
                    to: body_idx,
                    otherwise: over,
                    index: e,
                });
                self.ir.push(Instr::Label(body_idx));
                for i in body {
                    self.generate_statement(&i.v);
                }
                self.ir.push(Instr::Label(over));
            }
            parser::Statement::VarReassign { name, expr } => {
                let new = self.generate_expr(&expr.v);
                self.vars.insert(name.to_string(), new);
            }
            parser::Statement::While { cond, body } => {
                let header_idx = self.new_label();
                let body_idx = self.new_label();
                let over_idx = self.new_label();
                self.ir.push(Instr::Label(header_idx));
                let e = self.generate_expr(&cond.v);
                self.ir.push(Instr::JumpNotZero { index: e, to: body_idx, otherwise: over_idx });
                self.ir.push(Instr::Label(body_idx));
                for b in body {
                    self.generate_statement(&b.v);
                }
                self.ir.push(Instr::Jump(header_idx));
                self.ir.push(Instr::Label(over_idx));
            }
        }
    }

    fn generate_expr(&mut self, e: &parser::Expression) -> ValueIndex {
        match e {
            parser::Expression::Integer(i) => {
                let place = self.new_temp();
                self.ir.push(Instr::Assign { index: place, v: Value::Const(*i) });
                place
            }
            parser::Expression::Char(i) => {
                let place = self.new_temp();
                self.ir.push(Instr::Assign { index: place, v: Value::Const(*i as u64) });
                place
            }
            parser::Expression::Bool(b) => {
                let place = self.new_temp();
                self.ir.push(Instr::Assign { index: place, v: Value::Const(*b as u64) });
                place
            }
            parser::Expression::Identifier(name) => {
                *self.vars.get(name).unwrap()
            }
            parser::Expression::Binary { left, op, right } => {
                let left = self.generate_expr(&left.v);
                let right = self.generate_expr(&right.v);
                let place = self.new_temp();
                self.ir.push(Instr::BinaryOp { op: *op, l: Value::Temp(left), r: Value::Temp(right), into: place });
                place
            }
            parser::Expression::Unary { op, right } => {
                let right = self.generate_expr(&right.v);
                let place = self.new_temp();
                self.ir.push(Instr::UnaryOp { op: *op, r: right, into: place } );
                place
            }
        }
    }

    fn new_label(&mut self) -> LabelIndex {
        self.label_count += 1;
        self.label_count - 1
    }
    fn new_temp(&mut self) -> ValueIndex {
        self.temp_count += 1;
        self.temp_count - 1
    }
}
