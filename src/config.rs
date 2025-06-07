use colored::Colorize;

pub struct Config {
    pub program_name: String,
    pub input_name: String,
    pub backend: Backend,
}

#[derive(Debug)]
pub enum Backend {
    LLVM,
}

impl Config {
    pub fn from_args(mut args: std::env::Args) -> Option<Self> {
        let program_name = args.next().expect("No standard compliance?");
        let mut input_name = None;
        let mut backend = Backend::LLVM;
        let args: Vec<String> = args.collect();
        let mut args_slice = &args[..];
        while !args_slice.is_empty() {
            match args_slice[0].as_str() {
                "-b" => match args_slice.get(1) {
                    None => {
                        eprintln!("[{}]: Expected a backend name", "Error".red());
                        usage(&program_name);
                        return None;
                    }
                    Some(name) => {
                        if name.as_str() == "llvm" {
                            backend = Backend::LLVM;
                            args_slice = &args_slice[2..];
                        } else {
                            eprintln!("[{}]: Invalid backend name", "Error".red());
                            usage(&program_name);
                            return None;
                        }
                    }
                },
                name => {
                    if input_name.is_none() {
                        input_name = Some(name);
                        args_slice = &args_slice[1..];
                    } else {
                        eprintln!("[{}]: Unknown flag: {name}", "Error".red());
                        usage(&program_name);
                        return None;
                    }
                }
            }
        }

        if input_name.is_none() {
            eprintln!("[{}]: No input file provided", "Error".red());
            return None;
        }

        Some(Self {
            input_name: input_name.unwrap().to_string(),
            program_name,
            backend,
        })
    }
}

pub fn usage(prog_name: &str) {
    eprintln!("  [{}]: Usage:", "Info".blue());
    eprintln!("  [{}]: ./{prog_name} <input.gp> [OPTIONS]", "Info".blue());
    eprintln!("  [{}]: [OPTIONS]:", "Info".blue());
    eprintln!("  [{}]: -b (Backend): llvm", "Info".blue());
}
