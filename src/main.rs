mod ast;
mod builtins;
mod environment;
mod evaluator;
mod lexer;
mod object;
mod parser;
mod repl;
mod token_extensions;

fn main() {
    repl::start().unwrap();
    // let tokenizer = lexer::Tokenizer::new("if true {1;} else {2;}");
    // let mut parser = parser::Parser::new(tokenizer);
    // let program = parser.parse_program().unwrap();
    // dbg!(program);
}
