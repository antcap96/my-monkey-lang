use std::io::Write;

use crate::environment;

const PROMPT: &str = ">> ";

pub fn start() -> Result<(), std::io::Error> {
    let mut lines = std::io::stdin().lines();
    let mut stdout = std::io::stdout().lock();
    let mut environment = environment::Environment::new();

    loop {

        print!("\n{}", PROMPT);
        stdout.flush()?;

        let content = match lines.next() {
            None => return Ok(()),
            Some(x) => x?,
        };

        let tokenizer = crate::lexer::Tokenizer::new(&content);
        tokenizer.clone().for_each(|token| println!("{:?}", token));
        let program = crate::parser::Parser::new(tokenizer).parse_program();

        match program {
            Ok(program) => {
                println!("{}", program);
                let object = crate::evaluator::eval_program(program, &mut environment);

                println!("result: {:?}", object)
            
            },
            Err(errors) => {
                println!("{:?}", errors);
            }
        }

    }
}
