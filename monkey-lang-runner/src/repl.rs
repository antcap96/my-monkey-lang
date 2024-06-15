use std::error::Error;

use monkey_lang_compiler::code::Instructions;
use monkey_lang_compiler::compiler::Bytecode;
use monkey_lang_compiler::symbol_table::SymbolTable;
use monkey_lang_core::ast::Program;
use monkey_lang_interpreter::object::Object;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use monkey_lang_compiler::compiler;
use monkey_lang_compiler::vm;
use monkey_lang_core::lexer;
use monkey_lang_core::parser;
use monkey_lang_interpreter::environment;
use monkey_lang_interpreter::evaluator;

use crate::Mode;

trait Evaluator {
    fn evaluate(&mut self, program: Program) -> Result<Option<Object>, Box<dyn Error>>;
}

struct InterpreterEvaluator {
    environment: environment::Environment,
}

impl InterpreterEvaluator {
    fn new() -> Self {
        Self {
            environment: environment::Environment::new(),
        }
    }
}

impl Evaluator for InterpreterEvaluator {
    fn evaluate(&mut self, program: Program) -> Result<Option<Object>, Box<dyn Error>> {
        let object = evaluator::eval_program(&program, &mut self.environment);

        object
            .map(|obj| Some(obj.as_ref().clone()))
            .map_err(|err| err.into())
    }
}

struct CompilerEvaluator {
    compiler: compiler::Compiler,
    machine: vm::Vm,
}

impl CompilerEvaluator {
    fn new() -> Self {
        Self {
            compiler: compiler::Compiler::new(),
            machine: vm::Vm::new(Bytecode {
                instructions: Instructions::new(),
                constants: vec![],
            }),
        }
    }
}

impl Evaluator for CompilerEvaluator {
    fn evaluate(&mut self, program: Program) -> Result<Option<Object>, Box<dyn Error>> {
        let constants = std::mem::take(&mut self.compiler.constants);
        let symbol_table = std::mem::replace(&mut self.compiler.symbol_table, SymbolTable::new());
        self.compiler = compiler::Compiler::new_with_state(constants, symbol_table);
        let compiled = self.compiler.compile(&program);

        match compiled {
            Ok(bytecode) => {
                let globals = std::mem::take(&mut self.machine.globals);
                self.machine = vm::Vm::new_with_global_store(bytecode, globals);

                let res = self.machine.run();
                res.map(|_| self.machine.last_popped_stack_element.clone())
                    .map_err(|err| err.into())
            }
            Err(errors) => Err(errors.into()),
        }
    }
}

const PROMPT: &str = ">> ";

pub fn start(mode: Mode) -> Result<(), ReadlineError> {
    let mut rl = DefaultEditor::new()?;
    let mut content: String;

    let mut evaluator: Box<dyn Evaluator> = match mode {
        Mode::Interpreter => Box::new(InterpreterEvaluator::new()),
        Mode::Compiler => Box::new(CompilerEvaluator::new()),
    };

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
        let program = parser::Parser::new(tokenizer).parse_program();

        match program {
            Ok(program) => match evaluator.evaluate(program) {
                Ok(Some(obj)) => {
                    println!("{:?}", obj);
                }
                Ok(None) => {}
                Err(err) => {
                    println!("{}", err);
                }
            },
            Err(errors) => {
                println!("{:?}", errors);
            }
        }
    }
    Ok(())
}
