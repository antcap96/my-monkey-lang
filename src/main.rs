mod lexer;
mod repl;
mod ast;
mod parser;
mod token_extensions;

fn main() {
    repl::start().unwrap();
}
