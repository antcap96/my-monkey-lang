use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use monkey_lang_core::lexer;
use monkey_lang_core::parser;
use monkey_lang_interpreter::environment;
use monkey_lang_interpreter::evaluator;
const PROMPT: &str = ">> ";

pub fn start() -> Result<(), ReadlineError> {
    let mut environment = environment::Environment::new();

    let mut rl = DefaultEditor::new()?;
    let mut content: String;
    loop {
        let readline = rl.readline(PROMPT);

        match readline {
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                continue; // Clear line
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
            Ok(line) => {
                rl.add_history_entry(line.as_str())?;
                content = line;
            }
        }

        let tokenizer = lexer::Tokenizer::new(&content);
        // tokenizer.clone().for_each(|token| println!("{:?}", token));
        let program = parser::Parser::new(tokenizer).parse_program();

        match program {
            Ok(program) => {
                // println!("{}", program);
                let object = evaluator::eval_program(&program, &mut environment);

                println!("result: {:?}", object)
            }
            Err(errors) => {
                println!("{:?}", errors);
            }
        }
    }
    Ok(())
}
