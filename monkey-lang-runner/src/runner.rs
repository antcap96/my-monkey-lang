use monkey_lang_interpreter::environment;
use monkey_lang_interpreter::evaluator;
use monkey_lang_core::lexer;
use monkey_lang_core::parser;

pub fn execute(str: &str) -> Result<(), Vec<parser::ParseError>> {
    let tokenizer = lexer::Tokenizer::new(str);
    let mut parser = parser::Parser::new(tokenizer);
    let program = parser.parse_program()?;
    let mut env = environment::Environment::new();
    let evaluated = evaluator::eval_program(&program, &mut env);
    println!("{:?}", evaluated);
    Ok(())
}
