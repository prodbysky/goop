use crate::ir;
use crate::lexer;


struct Codegen<'a> {
    ctx: &'a inkwell::context::Context,
    builder: inkwell::builder::Builder<'a>,
}

impl<'a> Codegen<'a> {
    fn new(ctx: &'a inkwell::context::Context) -> Self {
        let builder = ctx.create_builder();
        Self { ctx, builder }
    }

    fn gen_module(&mut self, module: ir::Module) -> Result<inkwell::module::Module, inkwell::builder::BuilderError> {
        let llvm_module = self.ctx.create_module("main");
        let word = self.ctx.i64_type();

        for f in module.functions() {
            let fn_type = word.fn_type(&[], false);
            let func = llvm_module.add_function(&f.name(), fn_type, Some(inkwell::module::Linkage::External));
            let block = self.ctx.append_basic_block(func, "entry");
            self.builder.position_at_end(block);
            
            let mut locals = vec![];
            for _ in 0..*f.max_temps() {
                locals.push(self.builder.build_alloca(word, "t")?);
            }
            
            let mut label_blocks = std::collections::HashMap::new();
            
            for (_idx, i) in f.instructions().iter().enumerate() {
                if let ir::Instr::Label(l) = i {
                    let bb = self.ctx.append_basic_block(func, &format!("l_{}", l));
                    label_blocks.insert(*l, bb);
                }
            }
            
            let get_value = |v: &ir::Value| -> Result<inkwell::values::IntValue, inkwell::builder::BuilderError>{
                match v {
                    ir::Value::Const(i) => Ok(word.const_int(*i as u64, false)),
                    ir::Value::Temp(i) => {
                        Ok(self.builder.build_load(word, locals[*i], "loaded")?.into_int_value())
                    }
                }
            };
            
            let mut current_block = block;
            let mut has_terminator = false;
            
            for i in f.instructions() {
                match i {
                    ir::Instr::Assign { index, v } => {
                        let val = get_value(v)?;
                        self.builder.build_store(locals[*index], val)?;
                    }
                    ir::Instr::BinaryOp { op, l, r, into } => {
                        let left = get_value(l)?;
                        let right = get_value(r)?;
                        let result = match op {
                            lexer::Operator::Plus => self.builder.build_int_add(left, right, "add")?,
                            lexer::Operator::Minus => self.builder.build_int_sub(left, right, "sub")?,
                            lexer::Operator::Star => self.builder.build_int_mul(left, right, "mul")?,
                            lexer::Operator::Slash => self.builder.build_int_signed_div(left, right, "div")?,
                            lexer::Operator::Percent => self.builder.build_int_signed_rem(left, right, "rem")?,
                            lexer::Operator::Less => {
                                let cmp = self.builder.build_int_compare(inkwell::IntPredicate::SLT, left, right, "lt")?;
                                self.builder.build_int_z_extend(cmp, word, "ext")?
                            },
                            lexer::Operator::More => {
                                let cmp = self.builder.build_int_compare(inkwell::IntPredicate::SGT, left, right, "gt")?;
                                self.builder.build_int_z_extend(cmp, word, "ext")?
                            },
                            _ => panic!("Unsupported binary operator: {:?}", op),
                        };
                        self.builder.build_store(locals[*into], result)?;
                    }
                    ir::Instr::UnaryOp { op, r, into } => {
                        let right = get_value(r)?;
                        let result = match op {
                            lexer::Operator::Minus => self.builder.build_int_neg(right, "neg")?,
                            lexer::Operator::Not => {
                                let cmp = self.builder.build_int_compare(inkwell::IntPredicate::EQ, right, word.const_int(0, false), "not")?;
                                self.builder.build_int_z_extend(cmp, word, "ext")?
                            },
                            _ => panic!("Unsupported unary operator: {:?}", op),
                        };
                        self.builder.build_store(locals[*into], result)?;
                    }
                    ir::Instr::Return { value } => {
                        let val = get_value(value)?;
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
                    ir::Instr::JumpNotZero { cond, to, otherwise } => {
                        let to_block = label_blocks.get(to).expect("Label not found");
                        let otherwise_block = label_blocks.get(otherwise).expect("Label not found");
                        let val = get_value(cond)?;
                        let cmp = self.builder.build_int_compare(inkwell::IntPredicate::NE, val, word.const_int(0, false), "cmp")?;
                        self.builder.build_conditional_branch(cmp, *to_block, *otherwise_block)?;
                        has_terminator = true;
                    }
                }
            }
            
            if !has_terminator && current_block.get_terminator().is_none() {
                self.builder.build_return(Some(&word.const_int(0, false)))?;
            }
        }


        Ok(llvm_module)
    }
}

pub fn generate_code(module: ir::Module, to: &str) {
    inkwell::targets::Target::initialize_all(&inkwell::targets::InitializationConfig::default());
    let ctx = inkwell::context::Context::create();
    let triple = inkwell::targets::TargetMachine::get_default_triple();
    let target = inkwell::targets::Target::from_triple(&triple).unwrap();
    let machine = target.create_target_machine(
        &triple,
        "generic",
        "",
        inkwell::OptimizationLevel::Aggressive,
        inkwell::targets::RelocMode::Default,
        inkwell::targets::CodeModel::Default,
    ).unwrap();

    let mut cg = Codegen::new(&ctx);

    let module = cg.gen_module(module).unwrap();

    let assembly_name = format!("{to}.s");
    module.verify().unwrap();
    machine.write_to_file(&module, inkwell::targets::FileType::Assembly, std::path::Path::new(assembly_name.as_str())).unwrap();
    std::process::Command::new("clang").arg(assembly_name).arg("-o").arg(to).spawn().unwrap().wait().unwrap();
}
