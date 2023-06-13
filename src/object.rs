#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
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
    // InvalidArgumentCount {
    //     expected: usize,
    //     actual: usize,
    // },
}
