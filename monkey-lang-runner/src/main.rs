mod repl;
mod runner;

use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
#[command(author, version, about)]
struct Cli {
    path: Option<PathBuf>,
}

fn main() {
    let cli = Cli::parse();

    match cli.path {
        None => repl::start().unwrap(),
        Some(path) => {
            let str = std::fs::read_to_string(path).unwrap();
            runner::execute(&str).unwrap();
        }
    }
}
