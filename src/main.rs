use colored::Colorize;

fn main() {
    let config = match Config::from_args(std::env::args()) {
        Some(c) => c,
        None => return,
    };
    let input = match std::fs::read_to_string(&config.input_name) {
        Ok(i) => i,
        Err(e) => {
            eprintln!(
                "[{}]: Failed to read {input_name}: {e}",
                "Error".red(),
                input_name = config.input_name,
            );
            usage(&config.program_name);
            return;
        }
    };
    println!("{input}");
}

struct Config {
    program_name: String,
    input_name: String,
}

impl Config {
    fn from_args(mut args: std::env::Args) -> Option<Self> {
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

fn usage(prog_name: &str) {
    eprintln!("  [{}]: Usage:", "Info".blue());
    eprintln!("  [{}]: ./{prog_name} <input.gp>", "Info".blue());
}
