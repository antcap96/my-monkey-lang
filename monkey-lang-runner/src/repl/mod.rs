mod evaluator;
mod printer;
mod reader;

use rustyline::DefaultEditor;
use std::ops::ControlFlow;

use evaluator::{CompilerEvaluator, Evaluator, InterpreterEvaluator};
use printer::{CompilerPrinter, InterpreterPrinter, Printer};
use reader::{ReadOutput, Reader};

use crate::Mode;

struct Repl<E: Evaluator, P: Printer> {
    reader: Reader,
    evaluator: E,
    printer: P,
}

impl<O, E: Evaluator<Object = O>, P: Printer<Object = O>> Repl<E, P> {
    fn run(mut self) {
        loop {
            let input = self.reader.read();
            match input {
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

pub fn start(mode: Mode) {
    let rl = DefaultEditor::new().unwrap();

    match mode {
        Mode::Interpreter => Repl {
            reader: Reader::new(rl),
            evaluator: InterpreterEvaluator::new(),
            printer: InterpreterPrinter {},
        }
        .run(),
        Mode::Compiler => Repl {
            reader: Reader::new(rl),
            evaluator: CompilerEvaluator::new(),
            printer: CompilerPrinter {},
        }
        .run(),
    };
}
