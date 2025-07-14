use std::collections::HashMap;

use gccjit::ToRValue;

use crate::ir;

pub fn generate_module(ir_mod: ir::Module, root_name: &str) {
    let ctx = gccjit::Context::default();
    ctx.set_optimization_level(gccjit::OptimizationLevel::None);
    ctx.set_program_name(root_name);
    // // for dev
    // ctx.set_dump_code_on_compile(true);

    let mut functions = HashMap::new();

    for f in ir_mod.functions() {
        let t = f.get_type();  
        let ret = gcc_type(&ctx, &t.ret);
        let params: Vec<_> = t.args.iter().map(|(name, t)| {
            ctx.new_parameter(None, gcc_type(&ctx, t), name)
        }).collect();
        if f.external {
            functions.insert(f.name(), ctx.new_function(None, gccjit::FunctionType::Extern, ret, &params, f.name(), false));
        } else {
            functions.insert(f.name(), ctx.new_function(None, gccjit::FunctionType::Exported, ret, &params, f.name(), false));
        }
    }


    for f in ir_mod.functions() {
        if f.external {continue;}
        let current = functions.get(f.name()).unwrap();

        let mut temps = vec![];

        // prealloc temporaries
        for (i, t) in f.temps().iter().enumerate() {
            let ty = gcc_type(&ctx, t.get_type());
            temps.push(current.new_local(None, ty, format!("t_{i}")));
        }

        let mut blocks = vec![];
        blocks.push(current.new_block(format!("l_0")));
        let mut label = blocks.last().unwrap().clone();


        for st in f.body() {
            match st {
                ir::Instr::Return { v: Some(v) } => {
                    label.end_with_return(None, get_value(&ctx, current, &temps, v));
                }
                ir::Instr::Return { v: None } => {
                    label.end_with_void_return(None);
                }
                ir::Instr::Label(i) => {
                    let new = current.new_block(format!("l_{i}"));
                    blocks.push(new);
                    label = blocks.last().unwrap().clone();
                }
                ir::Instr::Assign { index, v } => {
                    let v = get_value(&ctx, current, &temps, v);
                    label.add_assignment(None, temps[*index], v);
                }
                ir::Instr::Call { name, args, into } => {
                    let callee = functions.get(name.as_str()).unwrap();
                    let args: Vec<_> = args.iter().map(|v| get_value(&ctx, current, &temps, v)).collect();
                    match into {
                        Some(i) => {
                            let value = ctx.new_call(None, *callee, &args);
                            label.add_assignment(None, temps[*i], value);
                        }
                        None => {
                            let value = ctx.new_call(None, *callee, &args);
                            label.add_eval(None, value);                        
                        }
                    };
                }
                ir::Instr::Add { l, r, into } => {
                    let l = get_value(&ctx, current, &temps, l);
                    let r = get_value(&ctx, current, &temps, r);
                    label.add_assignment(None, temps[*into], ctx.new_binary_op(None, gccjit::BinaryOp::Plus, temps[*into].to_rvalue().get_type(), l, r));
                }
                ir::Instr::Sub { l, r, into } => {
                    let l = get_value(&ctx, current, &temps, l);
                    let r = get_value(&ctx, current, &temps, r);
                    label.add_assignment(None, temps[*into], ctx.new_binary_op(None, gccjit::BinaryOp::Minus, temps[*into].to_rvalue().get_type(), l, r));
                }
                ir::Instr::Mul { l, r, into } => {
                    let l = get_value(&ctx, current, &temps, l);
                    let r = get_value(&ctx, current, &temps, r);
                    label.add_assignment(None, temps[*into], ctx.new_binary_op(None, gccjit::BinaryOp::Mult, temps[*into].to_rvalue().get_type(), l, r));
                }
                ir::Instr::Div { l, r, into } => {
                    let l = get_value(&ctx, current, &temps, l);
                    let r = get_value(&ctx, current, &temps, r);
                    label.add_assignment(None, temps[*into], ctx.new_binary_op(None, gccjit::BinaryOp::Divide, temps[*into].to_rvalue().get_type(), l, r));
                }
                ir::Instr::Mod { l, r, into } => {
                    let l = get_value(&ctx, current, &temps, l);
                    let r = get_value(&ctx, current, &temps, r);
                    label.add_assignment(None, temps[*into], ctx.new_binary_op(None, gccjit::BinaryOp::Modulo, temps[*into].to_rvalue().get_type(), l, r));
                }
                ir::Instr::Less { l, r, into } => {
                    let l = get_value(&ctx, current, &temps, l);
                    let r = get_value(&ctx, current, &temps, r);
                    label.add_assignment(None, temps[*into], ctx.new_comparison(None, gccjit::ComparisonOp::LessThan, l, r));
                }
                ir::Instr::More { l, r, into } => {
                    let l = get_value(&ctx, current, &temps, l);
                    let r = get_value(&ctx, current, &temps, r);
                    label.add_assignment(None, temps[*into], ctx.new_comparison(None, gccjit::ComparisonOp::GreaterThan, l, r));
                }
                ir::Instr::LogicalNot { r, into } => {
                    let r = get_value(&ctx, current, &temps, r);
                    label.add_assignment(None, temps[*into], ctx.new_unary_op(None, gccjit::UnaryOp::LogicalNegate, r.to_rvalue().get_type(), r));
                }
                ir::Instr::Jump(i) => {
                    label.end_with_jump(None, blocks[*i]);
                }
                ir::Instr::Jnz { cond, to, otherwise } => {
                    let v = get_value(&ctx, current, &temps, cond);
                    label.end_with_conditional(None, v, blocks[*to], blocks[*otherwise]);
                }
                ir::Instr::Cast { v, into_type, into_index } => {
                    let v = get_value(&ctx, current, &temps, v);
                    let new_type = gcc_type(&ctx, into_type);
                    label.add_assignment(None, temps[*into_index], ctx.new_cast(None, v, new_type));
                }
            }
        }
    }
    ctx.compile_to_file(gccjit::OutputKind::ObjectFile, format!("{root_name}.o"));
}

fn gcc_type<'a>(ctx: &'a gccjit::Context, t: &ir::Type) -> gccjit::Type<'a> {
    match t {
        ir::Type::U64 => ctx.new_int_type(8, false),
        ir::Type::Char => ctx.new_int_type(1, true),
        ir::Type::Void => ctx.new_type::<()>(),
        ir::Type::Bool => ctx.new_type::<bool>(),
    }
}

fn get_value<'a>(ctx: &'a gccjit::Context, f: &'a gccjit::Function, temps: &'a [gccjit::LValue], v: &ir::Value) -> gccjit::RValue<'a> {
    match v {
        ir::Value::Const { t, v } => {
            let t = gcc_type(ctx, t);
            ctx.new_rvalue_from_long(t, *v as i64)
        }
        ir::Value::Temp { t: _, i } => {
            if *i < f.get_param_count() {
                f.get_param(*i as i32).to_rvalue()
            } else {
                temps[*i].to_rvalue()
            }
        }
    }
}
