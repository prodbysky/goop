use colored::Colorize;

pub struct Config {
    pub program_name: String,
    pub input_name: std::path::PathBuf,
}

impl Config {
    pub fn from_args(mut args: std::env::Args) -> Option<Self> {
        let program_name = args.next().expect("No standard compliance?");
        let mut input_name = None;
        let args: Vec<String> = args.collect();
        let mut args_slice = &args[..];
        while !args_slice.is_empty() {
            match args_slice[0].as_str() {
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
            input_name: input_name.unwrap().to_string().into(),
            program_name,
        })
    }
}

pub fn usage(prog_name: &str) {
    eprintln!("  [{}]: Usage:", "Info".blue());
    eprintln!("  [{}]: ./{prog_name} <input.gp> [OPTIONS]", "Info".blue());
    eprintln!("  [{}]: [OPTIONS]:", "Info".blue());
    eprintln!("  [{}]: -b (Backend): qbe", "Info".blue());
}
