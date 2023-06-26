mod lexer;
mod repl;
mod ast;
mod parser;
mod token_extensions;
mod object;
mod evaluator;
mod environment;
mod builtins;

fn main() {
    repl::start().unwrap();
    // let tokenizer = lexer::Tokenizer::new("if true {1;} else {2;}");
    // let mut parser = parser::Parser::new(tokenizer);
    // let program = parser.parse_program().unwrap();
    // dbg!(program);
}
