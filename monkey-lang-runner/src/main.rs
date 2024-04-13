mod repl;
mod runner;

use clap::{Parser, ValueEnum};
use std::path::PathBuf;

#[derive(Clone, ValueEnum)]
pub enum Mode {
    Interpreter,
    Compiler,
}

#[derive(Parser)]
#[command(author, version, about)]
struct Cli {
    #[arg(short, long, value_enum, default_value_t=Mode::Compiler)]
    mode: Mode,
    path: Option<PathBuf>,
}

fn main() {
    let cli = Cli::parse();

    match cli.path {
        None => repl::start(cli.mode).unwrap(),
        Some(path) => {
            let str = std::fs::read_to_string(path).unwrap();
            runner::execute(&str).unwrap();
        }
    }
}
