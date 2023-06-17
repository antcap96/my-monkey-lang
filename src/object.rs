use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Function(Function),
    Null,
}

#[derive(PartialEq, Clone)]
pub struct Function {
    pub parameters: Vec<crate::ast::Identifier>,
    pub body: crate::ast::BlockStatement,
    pub env: Rc<RefCell<crate::environment::Environment>>,
}

impl std::fmt::Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Function")
            .field("parameters", &self.parameters)
            .field("body", &self.body)
            .field("env", &self.env.as_ptr())
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
        operation: crate::ast::InfixOperationKind
    },
    UnknownPrefixOperator {
        right: Box<Object>,
        operation: crate::ast::PrefixOperationKind
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
