use monkey_lang_core::ast::Program;
use rustyline::error::ReadlineError;
use rustyline::history::DefaultHistory;
use std::ops::ControlFlow;

use monkey_lang_core::lexer;
use monkey_lang_core::parser;
use rustyline::Editor;

const PROMPT: &str = ">> ";

pub enum ReadOutput {
    ControlFlow(ControlFlow<(), ()>),
    Value(Program),
}

pub struct Reader {
    rl: Editor<(), DefaultHistory>,
}

impl Reader {
    pub fn new(rl: Editor<(), DefaultHistory>) -> Self {
        Self { rl }
    }
    pub fn read(&mut self) -> ReadOutput {
        let readline = self.rl.readline(PROMPT);

        let line = match readline {
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                return ReadOutput::ControlFlow(ControlFlow::Continue(())); // Clear line
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                return ReadOutput::ControlFlow(ControlFlow::Break(()));
            }
            Err(err) => {
                println!("Error: {:?}", err);
                return ReadOutput::ControlFlow(ControlFlow::Break(()));
            }
            Ok(line) => {
                self.rl.add_history_entry(&line).unwrap(); // TODO: why can this fail?
                line
            }
        };

        let tokenizer = lexer::Tokenizer::new(&line);
        let program = parser::Parser::new(tokenizer).parse_program();

        match program {
            Ok(value) => ReadOutput::Value(value),
            Err(errors) => {
                println!("Parsing errors: {:?}", errors);
                ReadOutput::ControlFlow(ControlFlow::Continue(()))
            }
        }
    }
}
