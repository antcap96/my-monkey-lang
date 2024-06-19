use std::rc::Rc;

use monkey_lang_interpreter::object::EvaluationError;

use super::evaluator::CompilerError;

pub trait Printer {
    type Object;

    fn print(&mut self, object: Self::Object);
}

pub struct InterpreterPrinter {}

impl Printer for InterpreterPrinter {
    type Object = Result<Rc<monkey_lang_interpreter::object::Object>, EvaluationError>;

    fn print(&mut self, object: Self::Object) {
        match object {
            Ok(obj) => println!("{:?}", obj),
            Err(err) => println!("Error evaluating:\n{}", err),
        }
    }
}

pub struct CompilerPrinter {}

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
