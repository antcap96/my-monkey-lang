use super::object::CompiledFunction;
use super::code::Instructions;

// TODO: Should this be iterable instead of Instructions?
pub struct Frame {
    // TODO: Why does this have a `CompiledFunction` instead of `Instructions`?
    pub function: CompiledFunction,
    pub ip: usize,
    pub base_pointer: usize,
}

impl Frame {
    pub fn new(function: CompiledFunction) -> Self {
        Self { function, ip: 0 , base_pointer: 0}
    }

    pub fn instructions(&self) -> &Instructions {
        &self.function.instructions
    }
}
