use std::rc::Rc;

use monkey_lang_compiler::code::Instructions;
use monkey_lang_compiler::compiler::Bytecode;
use monkey_lang_compiler::compiler::CompilationError;
use monkey_lang_compiler::symbol_table::SymbolTable;
use monkey_lang_compiler::vm::VmError;
use monkey_lang_core::ast::Program;
use monkey_lang_interpreter::object::EvaluationError;
use rustyline::error::ReadlineError;
use rustyline::history::DefaultHistory;
use rustyline::DefaultEditor;
use std::ops::ControlFlow;

use monkey_lang_compiler::compiler;
use monkey_lang_compiler::vm;
use monkey_lang_core::lexer;
use monkey_lang_core::parser;
use monkey_lang_interpreter::environment;
use monkey_lang_interpreter::evaluator;
use rustyline::Editor;

use crate::Mode;

trait Evaluator {
    type Object;

    fn evaluate(&mut self, program: Program) -> Self::Object;
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
    type Object = Result<Rc<monkey_lang_interpreter::object::Object>, EvaluationError>;
    fn evaluate(&mut self, program: Program) -> Self::Object {
        evaluator::eval_program(&program, &mut self.environment)
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

enum CompilerError {
    CompilationError(CompilationError),
    VmError(VmError),
}

impl Evaluator for CompilerEvaluator {
    type Object = Result<Option<monkey_lang_compiler::object::Object>, CompilerError>;

    fn evaluate(&mut self, program: Program) -> Self::Object {
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
                    .map_err(CompilerError::VmError)
            }
            Err(error) => Err(CompilerError::CompilationError(error)),
        }
    }
}

enum ReadOutput {
    ControlFlow(ControlFlow<(), ()>),
    Value(Program),
}

struct Reader {
    rl: Editor<(), DefaultHistory>,
}

impl Reader {
    fn read(&mut self) -> ReadOutput {
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

trait Printer {
    type Object;

    fn print(&mut self, object: Self::Object);
}

struct InterpreterPrinter {}

impl Printer for InterpreterPrinter {
    type Object = Result<Rc<monkey_lang_interpreter::object::Object>, EvaluationError>;

    fn print(&mut self, object: Self::Object) {
        match object {
            Ok(obj) => println!("{:?}", obj),
            Err(err) => println!("Error evaluating:\n{}", err),
        }
    }
}

struct CompilerPrinter {}

impl Printer for CompilerPrinter {
    type Object = Result<Option<monkey_lang_compiler::object::Object>, CompilerError>;

    fn print(&mut self, object: Self::Object) {
        match object {
            Ok(obj) => println!("{:?}", obj),
            Err(CompilerError::CompilationError(err)) => println!("Error compiling:\n{}", err),
            Err(CompilerError::VmError(err)) => println!("Vm error:\n{}", err),
        }
    }
}

struct Repl<E: Evaluator, P: Printer> {
    reader: Reader,
    evaluator: E,
    printer: P,
}

impl<O, E: Evaluator<Object = O>, P: Printer<Object = O>> Repl<E, P> {
    fn run(mut self) {
        loop {
            let read_ouput = self.reader.read();
            match read_ouput {
                ReadOutput::ControlFlow(ControlFlow::Break(())) => break,
                ReadOutput::ControlFlow(ControlFlow::Continue(())) => continue,
                ReadOutput::Value(program) => {
                    let result = self.evaluator.evaluate(program);
                    self.printer.print(result)
                }
            }
        }
    }
}

const PROMPT: &str = ">> ";

pub fn start(mode: Mode) {
    let rl = DefaultEditor::new().unwrap();

    match mode {
        Mode::Interpreter => Repl {
            reader: Reader { rl },
            evaluator: InterpreterEvaluator::new(),
            printer: InterpreterPrinter {},
        }
        .run(),
        Mode::Compiler => Repl {
            reader: Reader { rl },
            evaluator: CompilerEvaluator::new(),
            printer: CompilerPrinter {},
        }
        .run(),
    };
}
