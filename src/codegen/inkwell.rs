use crate::ir;
use crate::lexer;

pub fn generate_code(module: ir::Module, to: &std::path::Path) {
    inkwell::targets::Target::initialize_all(&inkwell::targets::InitializationConfig::default());
    let ctx = inkwell::context::Context::create();
    let builder = ctx.create_builder();
    let llvm_module = ctx.create_module("main");
    let triple = inkwell::targets::TargetMachine::get_default_triple();
    let target = inkwell::targets::Target::from_triple(&triple).unwrap();
    let machine = target.create_target_machine(
        &triple,
        "generic",
        "",
        inkwell::OptimizationLevel::Default,
        inkwell::targets::RelocMode::Default,
        inkwell::targets::CodeModel::Default,
    ).unwrap();
    let word = ctx.i64_type();
    
    for f in module.functions() {
        let fn_type = word.fn_type(&[], false);
        let func = llvm_module.add_function(&f.name(), fn_type, Some(inkwell::module::Linkage::External));
        let block = ctx.append_basic_block(func, "entry");
        builder.position_at_end(block);
        
        let mut locals = vec![];
        for _ in 0..*f.max_temps() {
            locals.push(builder.build_alloca(word, "t").unwrap());
        }
        
        let mut label_blocks = std::collections::HashMap::new();
        
        for (_idx, i) in f.instructions().iter().enumerate() {
            if let ir::Instr::Label(l) = i {
                let bb = ctx.append_basic_block(func, &format!("l_{}", l));
                label_blocks.insert(*l, bb);
            }
        }
        
        let get_value = |v: &ir::Value| -> inkwell::values::IntValue {
            match v {
                ir::Value::Const(i) => word.const_int(*i as u64, false),
                ir::Value::Temp(i) => {
                    builder.build_load(word, locals[*i], "loaded").unwrap().into_int_value()
                }
            }
        };
        
        let mut current_block = block;
        let mut has_terminator = false;
        
        for i in f.instructions() {
            match i {
                ir::Instr::Assign { index, v } => {
                    let val = get_value(v);
                    builder.build_store(locals[*index], val).unwrap();
                }
                ir::Instr::BinaryOp { op, l, r, into } => {
                    let left = get_value(l);
                    let right = get_value(r);
                    let result = match op {
                        lexer::Operator::Plus => builder.build_int_add(left, right, "add").unwrap(),
                        lexer::Operator::Minus => builder.build_int_sub(left, right, "sub").unwrap(),
                        lexer::Operator::Star => builder.build_int_mul(left, right, "mul").unwrap(),
                        lexer::Operator::Slash => builder.build_int_signed_div(left, right, "div").unwrap(),
                        lexer::Operator::Percent => builder.build_int_signed_rem(left, right, "rem").unwrap(),
                        lexer::Operator::Less => {
                            let cmp = builder.build_int_compare(inkwell::IntPredicate::SLT, left, right, "lt").unwrap();
                            builder.build_int_z_extend(cmp, word, "ext").unwrap()
                        },
                        lexer::Operator::More => {
                            let cmp = builder.build_int_compare(inkwell::IntPredicate::SGT, left, right, "gt").unwrap();
                            builder.build_int_z_extend(cmp, word, "ext").unwrap()
                        },
                        _ => panic!("Unsupported binary operator: {:?}", op),
                    };
                    builder.build_store(locals[*into], result).unwrap();
                }
                ir::Instr::UnaryOp { op, r, into } => {
                    let right = get_value(r);
                    let result = match op {
                        lexer::Operator::Minus => builder.build_int_neg(right, "neg").unwrap(),
                        lexer::Operator::Not => {
                            let cmp = builder.build_int_compare(inkwell::IntPredicate::EQ, right, word.const_int(0, false), "not").unwrap();
                            builder.build_int_z_extend(cmp, word, "ext").unwrap()
                        },
                        _ => panic!("Unsupported unary operator: {:?}", op),
                    };
                    builder.build_store(locals[*into], result).unwrap();
                }
                ir::Instr::Return { value } => {
                    let val = get_value(value);
                    builder.build_return(Some(&val)).unwrap();
                    has_terminator = true;
                }
                ir::Instr::Label(l) => {
                    if !has_terminator && current_block.get_terminator().is_none() {
                        if let Some(target_block) = label_blocks.get(l) {
                            builder.build_unconditional_branch(*target_block).unwrap();
                        }
                    }
                    
                    current_block = label_blocks[l];
                    builder.position_at_end(current_block);
                    has_terminator = false;
                }
                ir::Instr::Jump(l) => {
                    if let Some(target_block) = label_blocks.get(l) {
                        builder.build_unconditional_branch(*target_block).unwrap();
                        has_terminator = true;
                    }
                }
                ir::Instr::JumpNotZero { cond, to, otherwise } => {
                    let to_block = label_blocks.get(to).expect("Label not found");
                    let otherwise_block = label_blocks.get(otherwise).expect("Label not found");
                    let val = get_value(cond);
                    let cmp = builder.build_int_compare(inkwell::IntPredicate::NE, val, word.const_int(0, false), "cmp").unwrap();
                    builder.build_conditional_branch(cmp, *to_block, *otherwise_block).unwrap();
                    has_terminator = true;
                }
            }
        }
        
        if !has_terminator && current_block.get_terminator().is_none() {
            builder.build_return(Some(&word.const_int(0, false))).unwrap();
        }
    }
    
    llvm_module.verify().unwrap();
    machine.write_to_file(&llvm_module, inkwell::targets::FileType::Assembly, to).unwrap();
}
