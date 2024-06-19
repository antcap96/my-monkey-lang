use monkey_lang_core::ast::Program;
use rustyline::error::ReadlineError;
use rustyline::history::DefaultHistory;

use monkey_lang_core::lexer;
use monkey_lang_core::parser;
use rustyline::Editor;

const PROMPT: &str = ">> ";

pub enum ReadOutput {
    Exit,
    Clear,
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
                return ReadOutput::Clear; // Clear line
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                return ReadOutput::Exit;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                return ReadOutput::Exit;
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
                ReadOutput::Clear
            }
        }
    }
}
