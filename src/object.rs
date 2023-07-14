use gc::{Finalize, Gc, Trace};

use crate::environment::Environment;

#[derive(Debug, PartialEq, Clone, Trace, Finalize)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Gc<Object>>),
    Function(Function),
    BuiltinFunction(BuiltinFunction),
    Null,
}

thread_local! {
    static NULL: Gc<Object> = Gc::new(Object::Null);
    static TRUE: Gc<Object> = Gc::new(Object::Boolean(true));
    static FALSE: Gc<Object> = Gc::new(Object::Boolean(false));
}

impl Object {
    pub fn null() -> Gc<Object> {
        NULL.with(|x| x.clone())
    }
    pub fn boolean(value: bool) -> Gc<Object> {
        if value {
            TRUE.with(|x| x.clone())
        } else {
            FALSE.with(|x| x.clone())
        }
    }
    pub fn integer(value: i64) -> Gc<Object> {
        Gc::new(Object::Integer(value))
    }
    pub fn string(value: String) -> Gc<Object> {
        Gc::new(Object::String(value))
    }
    pub fn array(array: Vec<Gc<Object>>) -> Gc<Object> {
        Gc::new(Object::Array(array))
    }
    pub fn function(
        parameters: Vec<crate::ast::Identifier>,
        body: crate::ast::BlockStatement,
        env: Environment,
    ) -> Gc<Object> {
        Gc::new(Object::Function(Function {
            parameters,
            body,
            env,
        }))
    }
    pub fn builtin_function(func: BuiltinFunction) -> Gc<Object> {
        Gc::new(Object::BuiltinFunction(func))
    }
}

#[derive(PartialEq, Clone, Trace, Finalize)]
pub struct Function {
    #[unsafe_ignore_trace]
    pub parameters: Vec<crate::ast::Identifier>,
    #[unsafe_ignore_trace]
    pub body: crate::ast::BlockStatement,
    pub env: Environment,
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("ptr", &(self as *const Function as usize))
            .finish()
    }
}

#[derive(Clone, Trace, Finalize)]
pub struct BuiltinFunction {
    pub func: fn(Vec<Gc<Object>>) -> Result<Gc<Object>, QuickReturn>,
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

pub enum QuickReturn {
    Return(Gc<Object>),
    Error(EvaluationError),
}

#[derive(Debug, PartialEq)]
pub enum EvaluationError {
    UnknownInfixOperator {
        left: Box<Gc<Object>>,
        right: Box<Gc<Object>>,
        operation: crate::ast::InfixOperationKind,
    },
    UnknownPrefixOperator {
        right: Box<Gc<Object>>,
        operation: crate::ast::PrefixOperationKind,
    },
    UnknownIdentifier(String),
    NonBooleanCondition(Gc<Object>),
    CallNonFunction(Gc<Object>),
    WrongArgumentCount {
        function: Function,
        expected: usize,
        actual: usize,
    },
    BuiltinFunctionError(String),
    IndexNotSupported(Gc<Object>),
    IndexingWithNonInteger(Gc<Object>),
}
