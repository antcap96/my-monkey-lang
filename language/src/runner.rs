pub fn execute(str: &str) -> Result<(), Vec<crate::parser::ParseError>> {
    let tokenizer = crate::lexer::Tokenizer::new(str);
    let mut parser = crate::parser::Parser::new(tokenizer);
    let program = parser.parse_program()?;
    let mut env = crate::environment::Environment::new();
    let evaluated = crate::evaluator::eval_program(&program, &mut env);
    println!("{:?}", evaluated);
    Ok(())
}
