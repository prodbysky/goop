use crate::ir;

struct Codegen<'a> {
    ctx: &'a inkwell::context::Context,
    builder: inkwell::builder::Builder<'a>,
}

impl<'a> Codegen<'a> {
    fn new(ctx: &'a inkwell::context::Context) -> Self {
        let builder = ctx.create_builder();
        Self { ctx, builder }
    }

    fn gen_module(
        &mut self,
        module: ir::Module,
    ) -> Result<inkwell::module::Module, inkwell::builder::BuilderError> {
        let llvm_module = self.ctx.create_module("main");
        let u64_type = self.ctx.i64_type();
        let char_type = self.ctx.i8_type();
        let bool_type = self.ctx.bool_type();
        let void = self.ctx.void_type();


        for f in module.functions() {
            let fn_type_ir = f.get_type();
            let mut args = vec![];
            for arg in fn_type_ir.args {
                args.push(
                    match arg.1 {
                        ir::Type::U64 => u64_type,
                        ir::Type::Bool => bool_type,
                        ir::Type::Char => char_type,
                        ir::Type::Void => todo!("i dont know what to do here"),
                    }
                    .into(),
                );
            }
            let fn_type = match fn_type_ir.ret {
                ir::Type::U64 => u64_type.fn_type(&args, false),
                ir::Type::Bool => bool_type.fn_type(&args, false),
                ir::Type::Char => char_type.fn_type(&args, false),
                ir::Type::Void => void.fn_type(&args, false),
            };

            llvm_module.add_function(f.name(), fn_type, Some(inkwell::module::Linkage::External));
        }

        for f in module.functions() {
            if f.external {continue;}
            let func = llvm_module.get_function(&f.name()).unwrap();
            let block = self.ctx.append_basic_block(func, "entry");
            self.builder.position_at_end(block);

            let mut locals = vec![];
            for t in f.temps() {
                match t {
                    ir::Value::Temp { t, i: _ } => match t {
                        ir::Type::Char => {
                            locals.push(self.builder.build_alloca(char_type, "temp_char")?);
                        }
                        ir::Type::Void => unreachable!(),
                        ir::Type::Bool => {
                            locals.push(self.builder.build_alloca(bool_type, "temp_bool")?);
                        }
                        ir::Type::U64 => {
                            locals.push(self.builder.build_alloca(u64_type, "temp_int")?);
                        }
                    },
                    _ => todo!(),
                }
            }
            for (i, _) in f.args().iter().enumerate() {
                let llvm_arg = func.get_nth_param(i as u32).unwrap().into_int_value();
                self.builder.build_store(locals[i], llvm_arg)?;
            }

            let mut label_blocks = std::collections::HashMap::new();

            for i in f.body().iter() {
                if let ir::Instr::Label(l) = i {
                    let bb = self.ctx.append_basic_block(func, &format!("l_{}", l));
                    label_blocks.insert(*l, bb);
                }
            }

            let get_value = |v: &ir::Value| -> Result<
                inkwell::values::IntValue,
                inkwell::builder::BuilderError,
            > {
                match v {
                    ir::Value::Const {
                        t: ir::Type::U64,
                        v,
                    } => Ok(u64_type.const_int(*v, false)),
                    ir::Value::Const {
                        t: ir::Type::Char,
                        v,
                    } => Ok(char_type.const_int(*v, false)),
                    ir::Value::Const {
                        t: ir::Type::Bool,
                        v,
                    } => Ok(bool_type.const_int(*v, false)),
                    ir::Value::Const {
                        t: ir::Type::Void,
                        v: _,
                    } => {
                        unreachable!()
                    }
                    ir::Value::Temp {
                        t: ir::Type::U64,
                        i,
                    } => Ok(self
                        .builder
                        .build_load(u64_type, locals[*i], "loaded")?
                        .into_int_value()),
                    ir::Value::Temp {
                        t: ir::Type::Char,
                        i,
                    } => Ok(self
                        .builder
                        .build_load(char_type, locals[*i], "loaded")?
                        .into_int_value()),
                    ir::Value::Temp {
                        t: ir::Type::Bool,
                        i,
                    } => Ok(self
                        .builder
                        .build_load(bool_type, locals[*i], "loaded")?
                        .into_int_value()),
                    ir::Value::Temp {
                        t: ir::Type::Void,
                        i: _,
                    } => unreachable!(),
                }
            };

            let mut current_block = block;
            let mut has_terminator = false;

            for i in f.body() {
                match i {
                    ir::Instr::Assign { index, v } => {
                        let val = get_value(v)?;
                        self.builder.build_store(locals[*index], val)?;
                    }
                    ir::Instr::Add { l, r, into } => {
                        let left = get_value(l)?;
                        let right = get_value(r)?;
                        let result = self.builder.build_int_add(left, right, "add")?;
                        self.builder.build_store(locals[*into], result)?;
                    }
                    ir::Instr::Sub { l, r, into } => {
                        let left = get_value(l)?;
                        let right = get_value(r)?;
                        let result = self.builder.build_int_sub(left, right, "sub")?;
                        self.builder.build_store(locals[*into], result)?;
                    }
                    ir::Instr::Mul { l, r, into } => {
                        let left = get_value(l)?;
                        let right = get_value(r)?;
                        let result = self.builder.build_int_mul(left, right, "mul")?;
                        self.builder.build_store(locals[*into], result)?;
                    }
                    ir::Instr::Div { l, r, into } => {
                        let left = get_value(l)?;
                        let right = get_value(r)?;
                        let result = self.builder.build_int_signed_div(left, right, "div")?;
                        self.builder.build_store(locals[*into], result)?;
                    }
                    ir::Instr::Mod { l, r, into } => {
                        let left = get_value(l)?;
                        let right = get_value(r)?;
                        let result = self.builder.build_int_signed_rem(left, right, "mod")?;
                        self.builder.build_store(locals[*into], result)?;
                    }
                    ir::Instr::Less { l, r, into } => {
                        let left = get_value(l)?;
                        let right = get_value(r)?;
                        let cmp = self.builder.build_int_compare(
                            inkwell::IntPredicate::SLT,
                            left,
                            right,
                            "lt",
                        )?;
                        self.builder.build_store(locals[*into], cmp)?;
                    }
                    ir::Instr::More { l, r, into } => {
                        let left = get_value(l)?;
                        let right = get_value(r)?;
                        let cmp = self.builder.build_int_compare(
                            inkwell::IntPredicate::SGT,
                            left,
                            right,
                            "gt",
                        )?;
                        self.builder.build_store(locals[*into], cmp)?;
                    }
                    ir::Instr::LogicalNot { r, into } => {
                        let right = get_value(r)?;
                        let cmp = self.builder.build_int_compare(
                            inkwell::IntPredicate::EQ,
                            right,
                            u64_type.const_int(0, false),
                            "not",
                        )?;
                        self.builder.build_store(locals[*into], cmp)?;
                    }
                    ir::Instr::Return { v: value } => {
                        let v = value.clone().unwrap();
                        let val = get_value(&v)?;
                        self.builder.build_return(Some(&val))?;
                        has_terminator = true;
                    }
                    ir::Instr::Label(l) => {
                        if !has_terminator && current_block.get_terminator().is_none() {
                            if let Some(target_block) = label_blocks.get(l) {
                                self.builder.build_unconditional_branch(*target_block)?;
                            }
                        }

                        current_block = label_blocks[l];
                        self.builder.position_at_end(current_block);
                        has_terminator = false;
                    }
                    ir::Instr::Jump(l) => {
                        if let Some(target_block) = label_blocks.get(l) {
                            self.builder.build_unconditional_branch(*target_block)?;
                            has_terminator = true;
                        }
                    }
                    ir::Instr::Jnz {
                        cond,
                        to,
                        otherwise,
                    } => {
                        let to_block = label_blocks.get(to).expect("Label not found");
                        let otherwise_block = label_blocks.get(otherwise).expect("Label not found");
                        let val = get_value(cond)?;
                        let cmp = self.builder.build_int_compare(
                            inkwell::IntPredicate::NE,
                            val,
                            bool_type.const_int(0, false),
                            "cmp",
                        )?;
                        self.builder
                            .build_conditional_branch(cmp, *to_block, *otherwise_block)?;
                        has_terminator = true;
                    }
                    ir::Instr::Call { name, args, into } => {
                        let mut a = vec![];

                        for arg in args {
                            a.push(get_value(arg)?.into());
                        }
                        let func = llvm_module.get_function(name.as_str()).unwrap();
                        if into.is_some() {
                            let a = self.builder.build_call(func, &a, name)?;
                            self.builder.build_store(
                                locals[into.unwrap()],
                                a.try_as_basic_value().unwrap_left(),
                            )?;
                        } else {
                            self.builder.build_call(func, &a, name)?;
                        }
                    }
                }
            }
            let is_void_fn = matches!(f.get_type().ret, ir::Type::Void);
            if !has_terminator && current_block.get_terminator().is_none() {
                if is_void_fn {
                    self.builder.build_return(None)?;
                } else {
                    self.builder
                        .build_return(Some(&u64_type.const_int(0, false)))?;
                }
            }
        }
        Ok(llvm_module)
    }
}

pub fn generate_code(module: ir::Module, no_ext: &str, out_name: &str) {
    inkwell::targets::Target::initialize_all(&inkwell::targets::InitializationConfig::default());
    let ctx = inkwell::context::Context::create();
    let triple = inkwell::targets::TargetMachine::get_default_triple();
    let target = inkwell::targets::Target::from_triple(&triple).unwrap();
    let machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            inkwell::OptimizationLevel::None,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap();

    let mut cg = Codegen::new(&ctx);

    let module = cg.gen_module(module).unwrap();

    let assembly_name = format!("{no_ext}.s");
    module.verify().unwrap();
    machine
        .write_to_file(
            &module,
            inkwell::targets::FileType::Assembly,
            std::path::Path::new(assembly_name.as_str()),
        )
        .unwrap();
    std::process::Command::new("clang")
        .arg(&assembly_name)
        .arg("-o")
        .arg(out_name)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    std::process::Command::new("rm")
        .arg(assembly_name)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
}
