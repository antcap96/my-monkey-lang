use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub name: Identifier,
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
    BooleanLiteral(bool),
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
    pub name: String,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    // TODO: Could this be an expression?
    pub statements: Vec<Statement>,
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name.name, self.value)
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
            BooleanLiteral(val) => write!(f, "{}", val),
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
                        .map(|id| id.name.as_str())
                        .collect::<Vec<&str>>()
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
