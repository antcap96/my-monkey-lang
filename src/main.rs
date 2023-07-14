mod ast;
mod builtins;
mod environment;
mod evaluator;
mod expression_parsing;
mod lexer;
mod object;
mod parser;
mod repl;

fn main() {
    repl::start().unwrap();
    // let tokenizer = lexer::Tokenizer::new("if true {1;} else {2;}");
    // let mut parser = parser::Parser::new(tokenizer);
    // let program = parser.parse_program().unwrap();
    // dbg!(program);
}
