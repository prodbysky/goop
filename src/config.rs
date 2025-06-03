use crate::usage;
use colored::Colorize;

pub struct Config {
    pub program_name: String,
    pub input_name: String,
}

impl Config {
    pub fn from_args(mut args: std::env::Args) -> Option<Self> {
        let program_name = args.next().expect("No standard compliance?");
        let input_name = match args.next() {
            Some(n) => n,
            None => {
                eprintln!("[{}]: No .gp file provided!", "Error".red());
                usage(&program_name);
                return None;
            }
        };
        Some(Self {
            input_name,
            program_name,
        })
    }
}
