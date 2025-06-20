use crate::{ir, lexer};
use qbe::{Module, Function, Linkage, Type, Value, Instr, Cmp};

pub fn generate_qbe_module(ir_mod: ir::Module) -> Module<'static> {
    let mut module = Module::new();

    for f in ir_mod.functions() {
        let mut index_to_value = Vec::new();
        let qbe_func = module.add_function(Function::new(Linkage::public(), f.name(), vec![], Some(Type::Word)));
        qbe_func.add_block("entry");
        for i in 0..*f.max_temps() {
            let v = Value::Temporary(format!("t_{i}"));
            qbe_func.assign_instr(v.clone(), Type::Long, Instr::Alloc8(8));
            index_to_value.push(v);
        }

        for instr in f.instructions() {
            generate_statement(instr, qbe_func, &index_to_value);
        }
    }


    module
}

fn generate_statement(s: &ir::Instr, f: &mut Function, itv: &[Value])  {
    fn label(index: usize) -> String {
        format!("label_{index}")
    }

    match s {
        ir::Instr::Return { value } => {
            let loaded = load_value(f, value, itv);
            f.add_instr(Instr::Ret(Some(loaded)));
        }
        ir::Instr::Assign { index, v } => {
            let loaded = load_value(f, v, itv);
            f.add_instr(Instr::Store(Type::Word, itv[*index].clone(), loaded));
        }
        ir::Instr::Jump(label_index) => {
            f.add_instr(Instr::Jmp(label(*label_index)));
        }
        ir::Instr::JumpNotZero { cond, to, otherwise } => {
            let v = load_value(f, cond, itv);
            f.add_instr(Instr::Jnz(v, label(*to), label(*otherwise)));
        }
        ir::Instr::BinaryOp { op, l, r, into } => {
            let l = load_value(f, l, itv);
            let r = load_value(f, r, itv);
            let temp = qbe::Value::Temporary(format!("binary_{into}"));
            use lexer::Operator as Op;
            let instr = match op {
                Op::Plus => Instr::Add(l, r),
                Op::Minus => Instr::Sub(l, r),
                Op::Star => Instr::Mul(l, r),
                Op::Slash => Instr::Div(l, r),
                Op::Percent => Instr::Rem(l, r),
                Op::More => Instr::Cmp(Type::Word, Cmp::Sgt, l, r),
                Op::Less => Instr::Cmp(Type::Word, Cmp::Slt, l, r),
                other => unreachable!("{other:?}")
            };
            f.assign_instr(temp.clone(), qbe::Type::Word, instr);
            f.add_instr(Instr::Store(Type::Word, itv[*into].clone(), temp));
        }
        ir::Instr::UnaryOp { op, r, into } => {
            let r = load_value(f, r, itv);
            let temp = qbe::Value::Temporary(format!("unary_{into}"));
            use lexer::Operator as Op;
            let instr = match op {
                Op::Minus => Instr::Sub(Value::Const(0), r),
                Op::Not => Instr::Cmp(Type::Word, Cmp::Eq, r, Value::Const(0)),
                other => unreachable!("{other:?}")
            };
            f.assign_instr(temp.clone(), Type::Word, instr);
            f.add_instr(Instr::Store(Type::Word, itv[*into].clone(), temp));
        }
        ir::Instr::Label(i) => {
            f.add_block(label(*i));
        }
        other => todo!("{other:?}")
    }
}

fn load_value(f: &mut qbe::Function, v: &ir::Value, itv: &[Value]) -> Value {
    match v {
        ir::Value::Const(i) => {
            Value::Const(*i)
        }
        ir::Value::Temp(i) => {
            let v = qbe::Value::Temporary(format!("loaded_{i}"));
            f.assign_instr(v.clone(), qbe::Type::Word, Instr::Load(Type::Word, itv[*i].clone()));
            v
        }
    }
}
