use crate::{ir::{self, LabelIndex, ValueIndex}, lexer};
use qbe::{Module, Function, Linkage, Type, Value};

pub fn generate_qbe_module(ir: &[ir::Instr]) -> Module {
    let mut module = Module::new();
    let main = module.add_function(Function::new(Linkage::public(), "main", vec![], Some(Type::Word)));
    main.add_block("entry");

    for i in ir {
        match i {
            ir::Instr::Assign { index, v } => {
                alloc_and_store_value(main, *v, *index);
            }
            ir::Instr::Return { value } => {
                let value = load_value(main, *value);
                main.add_instr(qbe::Instr::Ret(Some(value)));
            }
            ir::Instr::Label(idx) => {
                add_label(main, *idx);
            }
            ir::Instr::JumpNotZero { index, to, otherwise } => {
                let place = Value::Temporary(format!("value_{index}"));
                let from = Value::Temporary(format!("ptr_{index}"));
                main.assign_instr(place.clone(), qbe::Type::Word, qbe::Instr::Load(qbe::Type::Word, from));
                main.add_instr(qbe::Instr::Jnz(place, format!("l_{to}"), format!("l_{otherwise}")));
            }
            ir::Instr::BinaryOp { op, l, r, into } => {
                let l = load_value(main, *l);
                let r = load_value(main, *r);
                let val = Value::Temporary(format!("value_{into}"));
                let ptr = Value::Temporary(format!("ptr_{into}"));
                main.assign_instr(ptr.clone(), qbe::Type::Long, qbe::Instr::Alloc8(8));
                match op {
                    lexer::Operator::Plus => main.assign_instr(val.clone(), qbe::Type::Word, qbe::Instr::Add(l, r)),
                    lexer::Operator::Minus => main.assign_instr(val.clone(), qbe::Type::Word, qbe::Instr::Sub(l, r)),
                    lexer::Operator::Star => main.assign_instr(val.clone(), qbe::Type::Word, qbe::Instr::Mul(l, r)),
                    lexer::Operator::Slash => main.assign_instr(val.clone(), qbe::Type::Word, qbe::Instr::Div(l, r)),
                    lexer::Operator::Percent => main.assign_instr(val.clone(), qbe::Type::Word, qbe::Instr::Rem(l, r)),
                    lexer::Operator::Less => main.assign_instr(val.clone(), qbe::Type::Word, qbe::Instr::Cmp(qbe::Type::Word, qbe::Cmp::Slt, l, r)),
                    lexer::Operator::More => main.assign_instr(val.clone(), qbe::Type::Word, qbe::Instr::Cmp(qbe::Type::Word, qbe::Cmp::Sgt, l, r)),
                    lexer::Operator::Not => unreachable!()
                }
                main.add_instr(qbe::Instr::Store(qbe::Type::Word, ptr, val));
            }
            ir::Instr::UnaryOp { op, r, into } => {
                let place = Value::Temporary(format!("value_{r}"));
                let from = Value::Temporary(format!("ptr_{r}"));
                main.assign_instr(place.clone(), qbe::Type::Word, qbe::Instr::Load(qbe::Type::Word, from));
                match op {
                    lexer::Operator::Not => {
                        let place = alloc_value(main, *into);
                        let val = Value::Temporary(format!("value_{into}"));
                        main.assign_instr(val.clone(), qbe::Type::Word, qbe::Instr::Cmp(qbe::Type::Word, qbe::Cmp::Eq, Value::Const(0), place.clone()));
                        main.add_instr(qbe::Instr::Store(qbe::Type::Word, place, val));
                    },
                    lexer::Operator::Minus => {
                        let place = alloc_value(main, *into);
                        let val = Value::Temporary(format!("value_{into}"));
                        main.assign_instr(val.clone(), qbe::Type::Word, qbe::Instr::Sub(Value::Const(0), place.clone()));
                        main.add_instr(qbe::Instr::Store(qbe::Type::Word, place, val));
                    },
                    _ => unreachable!()
                }

            }
            ir::Instr::Jump(i) => {
                main.add_instr(qbe::Instr::Jmp(format!("l_{i}")));
            }
        }
    }

    module
}

fn add_label(func: &mut Function, i: LabelIndex) {
    func.add_block(format!("l_{i}"));
}

fn alloc_and_store_value(func: &mut Function, v: ir::Value, temp_index: ValueIndex) -> Value {
    let place = alloc_value(func, temp_index);
    match v {
        ir::Value::Const(i) => {func.add_instr(qbe::Instr::Store(qbe::Type::Word, place.clone(), Value::Const(i))); place},
        ir::Value::Temp(i) => {
            let from = Value::Temporary(format!("ptr_{i}"));
            func.assign_instr(place.clone(), qbe::Type::Word, qbe::Instr::Load(qbe::Type::Word, from));
            place
        }
    }
}

fn alloc_value(func: &mut Function, temp_index: ValueIndex) -> Value {
    let place = Value::Temporary(format!("ptr_{temp_index}"));
    func.assign_instr(place.clone(), qbe::Type::Long, qbe::Instr::Alloc8(8));
    place
}

fn load_value(func: &mut Function, value: ir::Value) -> Value {
    match value {
        ir::Value::Const(i) => Value::Const(i), 
        ir::Value::Temp(i) => {
            let place = Value::Temporary(format!("value_{i}"));
            let from = Value::Temporary(format!("ptr_{i}"));
            func.assign_instr(place.clone(), qbe::Type::Word, qbe::Instr::Load(qbe::Type::Word, from));
            place
        }
    }
}
