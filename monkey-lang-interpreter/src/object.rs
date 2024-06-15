use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::environment::{Environment, EnvironmentCore};
use monkey_lang_core::ast;

use thiserror::Error;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Rc<Object>>),
    Hash(HashMap<ast::HashKey, (Rc<Object>, Rc<Object>)>),
    Function(Function),
    BuiltinFunction(BuiltinFunction),
    Null,
}

thread_local! {
    static NULL: Rc<Object> = Rc::new(Object::Null);
    static TRUE: Rc<Object> = Rc::new(Object::Boolean(true));
    static FALSE: Rc<Object> = Rc::new(Object::Boolean(false));
}

impl Object {
    pub fn null() -> Rc<Object> {
        NULL.with(|x| x.clone())
    }
    pub fn boolean(value: bool) -> Rc<Object> {
        if value {
            TRUE.with(|x| x.clone())
        } else {
            FALSE.with(|x| x.clone())
        }
    }
    pub fn integer(value: i64) -> Rc<Object> {
        Rc::new(Object::Integer(value))
    }
    pub fn string(value: String) -> Rc<Object> {
        Rc::new(Object::String(value))
    }
    pub fn array(array: Vec<Rc<Object>>) -> Rc<Object> {
        Rc::new(Object::Array(array))
    }
    pub fn hash(hash: HashMap<ast::HashKey, (Rc<Object>, Rc<Object>)>) -> Rc<Object> {
        Rc::new(Object::Hash(hash))
    }
    pub fn function(
        parameters: Vec<ast::Identifier>,
        body: ast::BlockStatement,
        env: Environment,
    ) -> Rc<Object> {
        Rc::new(Object::Function(Function {
            parameters,
            body,
            parent_env: Rc::downgrade(&env.environment),
            captured_environments: Vec::new(),
        }))
    }
    pub fn builtin_function(func: BuiltinFunction) -> Rc<Object> {
        Rc::new(Object::BuiltinFunction(func))
    }
}

impl TryFrom<&Object> for ast::HashKey {
    type Error = ();

    fn try_from(value: &Object) -> Result<Self, Self::Error> {
        match value {
            Object::Integer(int) => Ok(ast::HashKey::Integer(*int)),
            Object::String(str) => Ok(ast::HashKey::String(str.clone())),
            Object::Boolean(bool) => Ok(ast::HashKey::Boolean(*bool)),
            _ => Err(()),
        }
    }
}

#[derive(Clone)]
pub struct Function {
    pub parameters: Vec<ast::Identifier>,
    pub body: ast::BlockStatement,
    pub parent_env: std::rc::Weak<RefCell<EnvironmentCore>>,
    pub captured_environments: Vec<Environment>,
}

impl Function {
    pub fn clone_with_captured_environment(&self, env: Environment) -> Self {
        let mut captured_environments = self.captured_environments.clone();
        captured_environments.push(env);
        Function {
            captured_environments,
            ..self.clone()
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.parameters == other.parameters
            && self.body == other.body
            && self.parent_env.ptr_eq(&other.parent_env)
    }
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("ptr", &(self as *const Function as usize))
            .finish()
    }
}

#[derive(Clone)]
pub struct BuiltinFunction {
    #[allow(clippy::type_complexity)]
    pub func: fn(Vec<Rc<Object>>) -> Result<Rc<Object>, QuickReturn>,
}

impl PartialEq for BuiltinFunction {
    fn eq(&self, other: &Self) -> bool {
        self.func as usize == other.func as usize
    }
}

impl std::fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFunction")
            .field("ptr", &(self as *const BuiltinFunction))
            .finish()
    }
}

#[derive(Debug, PartialEq)]
pub enum QuickReturn {
    Return(Rc<Object>),
    Error(EvaluationError),
}

#[derive(Debug, PartialEq, Error)]
pub enum EvaluationError {
    #[error("Unknown infix operator: {operation:?} on {left:?} and {right:?}")]
    UnknownInfixOperator {
        left: Box<Rc<Object>>,
        right: Box<Rc<Object>>,
        operation: ast::InfixOperationKind,
    },
    #[error("Unknown prefix operator: {operation:?} on {right:?}")]
    UnknownPrefixOperator {
        right: Box<Rc<Object>>,
        operation: ast::PrefixOperationKind,
    },
    #[error("Unknown identifier: {0}")]
    UnknownIdentifier(Rc<str>),
    #[error("Non boolean used in condition: {0:?}")]
    NonBooleanCondition(Rc<Object>),
    #[error("Called an object that is not a function: {0:?}")]
    CallNonFunction(Rc<Object>),
    #[error(
        "Wrong number of arguments for function {function:?}. Expected {expected}, got {actual}"
    )]
    WrongArgumentCount {
        function: Function,
        expected: usize,
        actual: usize,
    },
    #[error("Builtin function error: {0}")]
    BuiltinFunctionError(Rc<str>),
    #[error("Index not supported for object: {0:?}")]
    IndexNotSupported(Rc<Object>),
    #[error("Object {0:?} cannot be used as index")]
    IndexingWithNonInteger(Rc<Object>),
    #[error("Object {0:?} cannot be used as hash key")]
    InvalidHashKey(Rc<Object>),
    #[error("No matching case for object: {0:?}")]
    NoMatchingCase(Rc<Object>),
}

pub fn object_to_key(object: &Rc<Object>) -> Result<ast::HashKey, EvaluationError> {
    match object.as_ref() {
        Object::Integer(value) => Ok(ast::HashKey::Integer(*value)),
        Object::Boolean(value) => Ok(ast::HashKey::Boolean(*value)),
        Object::String(value) => Ok(ast::HashKey::String(value.clone())),
        _ => Err(EvaluationError::InvalidHashKey(object.clone())),
    }
}
