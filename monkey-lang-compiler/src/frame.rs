use super::object::CompiledFunction;
use super::code::Instructions;

// TODO: Should this be iterable instead of Instructions?
struct Frame {
    // TODO: Why does this have a `CompiledFunction` instead of `Instructions`?
    function: CompiledFunction,
    ip: usize,
}

impl Frame {
    fn new(function: CompiledFunction) -> Self {
        Self { function, ip: 0 }
    }

    fn instructions(&self) -> &Instructions {
        &self.function.instructions
    }
}
