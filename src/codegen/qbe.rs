use crate::{ir::{self, LabelIndex, ValueIndex}, lexer};
use qbe::{Module, Function, Linkage, Type, Value};
use std::collections::HashSet;

pub fn generate_qbe_module(ir: &[ir::Instr]) -> Module {
    let mut module = Module::new();
    let main = module.add_function(Function::new(Linkage::public(), "main", vec![], Some(Type::Word)));
    main.add_block("entry");

    module
}
