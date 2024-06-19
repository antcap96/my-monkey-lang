use std::rc::Rc;

use monkey_lang_compiler::code::Instructions;
use monkey_lang_compiler::compiler::Bytecode;
use monkey_lang_compiler::compiler::CompilationError;
use monkey_lang_compiler::symbol_table::SymbolTable;
use monkey_lang_compiler::vm::VmError;
use monkey_lang_core::ast::Program;
use monkey_lang_interpreter::object::EvaluationError;

use monkey_lang_compiler::compiler;
use monkey_lang_compiler::vm;
use monkey_lang_interpreter::environment;
use monkey_lang_interpreter::evaluator;

pub trait Evaluator {
    type Object;

    fn evaluate(&mut self, program: Program) -> Self::Object;
}

pub struct InterpreterEvaluator {
    environment: environment::Environment,
}

impl InterpreterEvaluator {
    pub fn new() -> Self {
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

pub struct CompilerEvaluator {
    compiler: compiler::Compiler,
    machine: vm::Vm,
}

impl CompilerEvaluator {
    pub fn new() -> Self {
        Self {
            compiler: compiler::Compiler::new(),
            machine: vm::Vm::new(Bytecode {
                instructions: Instructions::new(),
                constants: vec![],
            }),
        }
    }
}

pub enum CompilerError {
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
