use crate::code::{instruction_iter_func, InstructionIterHelper, InstructionReadError, OpCode};

use super::code::Instructions;
use super::object::CompiledFunction;

// TODO: Should this be iterable instead of Instructions?
pub struct Frame {
    // TODO: Why does this have a `CompiledFunction` instead of `Instructions`?
    pub function: CompiledFunction,
    pub ip: usize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(function: CompiledFunction) -> Self {
        Self {
            function,
            ip: 0,
            base_pointer: 0,
        }
    }

    pub fn instructions(&self) -> &Instructions {
        &self.function.instructions
    }
}

impl InstructionIterHelper for Frame {
    fn read_u8(&mut self) -> Option<u8> {
        let offset = self.ip;
        self.ip += 1;
        self.instructions().bytes.get(offset).copied()
    }
}

impl<'a> Iterator for Frame {
    type Item = Result<OpCode, InstructionReadError>;

    fn next(&mut self) -> Option<Self::Item> {
        instruction_iter_func(self)
    }
}
