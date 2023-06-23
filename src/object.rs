use gc::{Finalize, Trace};

use crate::environment::Environment;

#[derive(Debug, PartialEq, Clone, Trace, Finalize)]
pub enum ObjectCore {
    Integer(i64),
    Boolean(bool),
    String(String),
    Function(Function),
    Null,
}

#[derive(Debug, PartialEq, Clone, Trace, Finalize)]
pub struct Object {
    object: gc::Gc<ObjectCore>,
}

thread_local! {
    static NULL: Object = Object {
        object: gc::Gc::new(ObjectCore::Null),
    };
    static TRUE: Object = Object {
        object: gc::Gc::new(ObjectCore::Boolean(true)),
    };
    static FALSE: Object = Object {
        object: gc::Gc::new(ObjectCore::Boolean(false)),
    };
}

impl Object {
    pub fn null() -> Object {
        NULL.with(|x| x.clone())
    }
    pub fn boolean(value: bool) -> Object {
        if value {
            TRUE.with(|x| x.clone())
        } else {
            FALSE.with(|x| x.clone())
        }
    }
    pub fn integer(value: i64) -> Object {
        Object {
            object: gc::Gc::new(ObjectCore::Integer(value)),
        }
    }
    pub fn string(value: String) -> Object {
        Object {
            object: gc::Gc::new(ObjectCore::String(value)),
        }
    }
    pub fn function(
        parameters: Vec<crate::ast::Identifier>,
        body: crate::ast::BlockStatement,
        env: Environment,
    ) -> Object {
        Object {
            object: gc::Gc::new(ObjectCore::Function(Function {
                parameters,
                body,
                env,
            })),
        }
    }
    pub fn core_ref(&self) -> &ObjectCore {
        &self.object
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

pub enum QuickReturn {
    Return(Object),
    Error(EvaluationError),
}

#[derive(Debug, PartialEq)]
pub enum EvaluationError {
    UnknownInfixOperator {
        left: Box<Object>,
        right: Box<Object>,
        operation: crate::ast::InfixOperationKind,
    },
    UnknownPrefixOperator {
        right: Box<Object>,
        operation: crate::ast::PrefixOperationKind,
    },
    UnknownIdentifier(String),
    NonBooleanCondition(Object),
    CallNonFunction(Object),
    WrongArgumentCount {
        function: Function,
        expected: usize,
        actual: usize,
    },
}
