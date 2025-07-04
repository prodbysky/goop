use clap::Parser;

/// Goop language compiler
#[derive(Parser, Debug)]
#[command(version, about)]
pub struct Args {
    /// Input .gp files to compile
    #[arg(value_delimiter = ' ', num_args = 1..)]
    pub input: Vec<String>,

    /// Output executable name
    #[arg(short, default_value_t = String::from("a.out"))]
    pub output: String,

    /// Dump IR?
    #[arg(long)]
    pub dump_ir: bool,
}
