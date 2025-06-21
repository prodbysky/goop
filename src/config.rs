use clap::Parser;

/// Goop language compiler
#[derive(Parser, Debug)]
#[command(version, about)]
pub struct Args {
    /// Input .gp file to compile
    #[arg()]
    pub input: String,

    /// Output executable name
    #[arg(short, default_value_t = String::from("a.out"))]
    pub output: String,
}
