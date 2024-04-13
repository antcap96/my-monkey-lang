use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use monkey_lang_compiler::compiler;
use monkey_lang_compiler::vm;
use monkey_lang_core::lexer;
use monkey_lang_core::parser;
use monkey_lang_interpreter::environment;
use monkey_lang_interpreter::evaluator;

use crate::Mode;

const PROMPT: &str = ">> ";

pub fn start(mode: Mode) -> Result<(), ReadlineError> {
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
                match mode {
                    Mode::Interpreter => {
                        let object = evaluator::eval_program(&program, &mut environment);

                        println!("result: {:?}", object)
                    }
                    Mode::Compiler => {
                        let comp = compiler::Compiler::new();
                        let Ok(bytecode) = comp.compile(program) else {
                            println!("Compilation failed");
                            continue;
                        };

                        let mut machine = vm::Vm::new(bytecode);
                        machine.run(); //TODO: should be able to fail

                        println!("result: {:?}", machine.stack_top());
                    }
                }
            }
            Err(errors) => {
                println!("{:?}", errors);
            }
        }
    }
    Ok(())
}
