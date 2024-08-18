use std::collections::HashMap;

use monkey_lang_core::ast;

use crate::code::Instructions;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
    Hash(HashMap<ast::HashKey, (Object, Object)>),
    CompiledFunction(CompiledFunction),
    Builtin(u8),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompiledFunction {
    pub instructions: Instructions,
    pub num_locals: usize,
    pub num_parameters: u8,
}

impl TryFrom<Object> for ast::HashKey {
    type Error = Object;

    fn try_from(value: Object) -> Result<Self, Self::Error> {
        match value {
            Object::Integer(int) => Ok(ast::HashKey::Integer(int)),
            Object::String(str) => Ok(ast::HashKey::String(str)),
            Object::Boolean(bool) => Ok(ast::HashKey::Boolean(bool)),
            _ => Err(value),
        }
    }
}
