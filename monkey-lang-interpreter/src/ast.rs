use std::fmt::Display;
use std::rc::Rc;

use gc::{Finalize, Trace};

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub identifier: Identifier,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(i64),
    StringLiteral(String),
    BooleanLiteral(bool),
    NullLiteral,
    ArrayLiteral(Vec<Expression>),
    HashLiteral(Vec<(Expression, Expression)>),
    PrefixOperation(PrefixOperationKind, Box<Expression>),
    InfixOperation(InfixOperationKind, Box<Expression>, Box<Expression>),
    IfExpression {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    FunctionLiteral {
        parameters: Vec<Identifier>,
        body: BlockStatement,
    },
    CallExpression {
        function: Box<Expression>,
        arguments: Vec<Expression>,
    },
    IndexExpression {
        left: Box<Expression>,
        index: Box<Expression>,
    },
    MatchExpression {
        expression: Box<Expression>,
        cases: Vec<MatchCase>,
    },
}

#[derive(Debug, PartialEq, Clone, Eq, Hash, Trace, Finalize)]
pub enum HashKey {
    Integer(i64),
    Boolean(bool),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub body: BlockStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Identifier(Identifier),
    IntegerLiteral(i64), // TODO: should there be 4 different types of literals?
    StringLiteral(String),
    BooleanLiteral(bool),
    NullLiteral,
    ArrayPattern(ArrayPattern),
    HashPattern(HashPattern),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayPattern {
    pub contents: Vec<Pattern>,
    pub remainder: Option<Identifier>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct HashPattern {
    pub contents: Vec<(HashKey, Pattern)>,
    pub remainder: Option<Identifier>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InfixOperationKind {
    Plus,
    Minus,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrefixOperationKind {
    Minus,
    Bang,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub name: Rc<str>,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.identifier.name, self.value)
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.value)
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for statement in &self.statements {
            writeln!(f, "  {}", statement)?;
        }
        write!(f, "}}")
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expression::*;
        match self {
            Identifier(ident) => write!(f, "{}", ident.name),
            IntegerLiteral(val) => write!(f, "{}", val),
            StringLiteral(val) => write!(f, "\"{}\"", val),
            BooleanLiteral(val) => write!(f, "{}", val),
            NullLiteral => write!(f, "null"),
            ArrayLiteral(arr) => {
                write!(f, "[")?;
                for (i, expr) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                write!(f, "]")
            }
            HashLiteral(hash) => {
                write!(f, "{{")?;
                for (i, (key, value)) in hash.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            PrefixOperation(kind, expr) => write!(f, "({}{})", kind.to_str(), expr),
            InfixOperation(kind, left, right) => {
                write!(f, "({} {} {})", left, kind.to_str(), right)
            }
            IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                write!(f, "if {} {}", condition, consequence)?;
                if let Some(alternative) = alternative {
                    write!(f, " else {}", alternative)?;
                }
                Ok(())
            }
            FunctionLiteral { parameters, body } => {
                write!(
                    f,
                    "fn({}) {}",
                    parameters
                        .iter()
                        .map(|id| id.name.as_ref())
                        .collect::<Box<[&str]>>()
                        .join(", "),
                    body
                )
            }
            CallExpression {
                function,
                arguments,
            } => {
                write!(
                    f,
                    "{}({})",
                    function,
                    arguments
                        .iter()
                        .map(|arg| arg.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
            IndexExpression { left, index } => write!(f, "({}[{}])", left, index),
            MatchExpression { expression, cases } => {
                write!(f, "match {} {{", expression)?;
                for case in cases {
                    write!(f, "{}", case)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Statement::*;
        match self {
            Let(statement) => write!(f, "{}", statement),
            Return(statement) => write!(f, "{}", statement),
            Expression(expression) => write!(f, "{};", expression),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{}", statement)?;
        }
        Ok(())
    }
}

impl Display for MatchCase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {}", self.pattern, self.body)
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Pattern::*;
        match self {
            Identifier(ident) => write!(f, "{}", ident.name),
            IntegerLiteral(val) => write!(f, "{}", val),
            StringLiteral(val) => write!(f, "\"{}\"", val),
            BooleanLiteral(val) => write!(f, "{}", val),
            NullLiteral => write!(f, "null"),
            ArrayPattern(arr) => {
                write!(f, "[")?;
                for (i, expr) in arr.contents.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                if let Some(remainder) = &arr.remainder {
                    write!(f, ", ...{}", remainder.name)?;
                }
                write!(f, "]")
            }
            HashPattern(hash) => {
                write!(f, "[")?;
                for (i, (key, value)) in hash.contents.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                if let Some(remainder) = &hash.remainder {
                    write!(f, ", ...{}", remainder.name)?;
                }
                write!(f, "]")
            }
        }
    }
}

impl Display for HashKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use HashKey::*;
        match self {
            &Integer(val) => write!(f, "{}", val),
            String(val) => write!(f, "\"{}\"", val),
            Boolean(val) => write!(f, "{}", val),
        }
    }
}

impl PrefixOperationKind {
    fn to_str(&self) -> &'static str {
        use PrefixOperationKind::*;
        match self {
            Minus => "-",
            Bang => "!",
        }
    }
}

impl InfixOperationKind {
    fn to_str(&self) -> &'static str {
        use InfixOperationKind::*;
        match self {
            Plus => "+",
            Minus => "-",
            LessThan => "<",
            GreaterThan => ">",
            Equal => "==",
            NotEqual => "!=",
            Multiply => "*",
            Divide => "/",
        }
    }
}
