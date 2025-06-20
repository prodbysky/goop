use crate::{ir::{self, LabelIndex, ValueIndex}, lexer};
use qbe::{Module, Function, Linkage, Type, Value};
use std::collections::HashSet;

pub fn generate_qbe_module(ir_mod: ir::Module) -> Module<'static> {
    let mut module = Module::new();

    for f in ir_mod.functions() {
        let mut index_to_value = Vec::new();
        let qbe_func = module.add_function(Function::new(Linkage::public(), f.name(), vec![], Some(Type::Word)));
        qbe_func.add_block("entry");
        for i in 0..*f.max_temps() {
            let v = Value::Temporary(format!("t_{i}"));
            qbe_func.assign_instr(v.clone(), Type::Long, qbe::Instr::Alloc8(8));
            index_to_value.push(v);
        }
    }


    module
}
