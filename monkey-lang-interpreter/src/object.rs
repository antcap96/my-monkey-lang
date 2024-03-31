use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast;
use crate::environment::{Environment, EnvironmentCore};

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
        Rc::new(Object::Function(
            Function {
                parameters,
                body,
                parent_env: Rc::downgrade(&env.environment),
                captured_environments: Vec::new()
            },
        ))
    }
    pub fn builtin_function(func: BuiltinFunction) -> Rc<Object> {
        Rc::new(Object::BuiltinFunction(func))
    }
}

#[derive(Clone)]
pub struct Function {
    pub parameters: Vec<ast::Identifier>,
    pub body: ast::BlockStatement,
    pub parent_env: std::rc::Weak<RefCell<EnvironmentCore>>,
    pub captured_environments: Vec<Environment>
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

#[derive(Debug, PartialEq)]
pub enum EvaluationError {
    UnknownInfixOperator {
        left: Box<Rc<Object>>,
        right: Box<Rc<Object>>,
        operation: ast::InfixOperationKind,
    },
    UnknownPrefixOperator {
        right: Box<Rc<Object>>,
        operation: ast::PrefixOperationKind,
    },
    UnknownIdentifier(Rc<str>),
    NonBooleanCondition(Rc<Object>),
    CallNonFunction(Rc<Object>),
    WrongArgumentCount {
        function: Function,
        expected: usize,
        actual: usize,
    },
    BuiltinFunctionError(Rc<str>),
    IndexNotSupported(Rc<Object>),
    IndexingWithNonInteger(Rc<Object>),
    InvalidHashKey(Rc<Object>),
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
