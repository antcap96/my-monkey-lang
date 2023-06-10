use std::fmt::Display;

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(Expression),
}

#[derive(Debug)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub value: Expression,
}

#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(i64),
    PrefixOperation(PrefixOperationKind, Box<Expression>),
    InfixOperation(InfixOperationKind, Box<Expression>, Box<Expression>),
    NotYetImplemented,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum PrefixOperationKind {
    Minus,
    Bang,
}

#[derive(Debug)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug)]
pub struct Program {
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

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expression::*;
        match self {
            Identifier(ident) => write!(f, "{}", ident.name),
            IntegerLiteral(val) => write!(f, "{}", val),
            PrefixOperation(kind, expr) => write!(f, "({}{})", kind.to_str(), expr),
            InfixOperation(kind, left, right) => {
                write!(f, "({} {} {})", left, kind.to_str(), right)
            }
            NotYetImplemented => write!(f, "NotYetImplemented"),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Statement::*;
        match self {
            Let(stmt) => write!(f, "{}", stmt),
            Return(stmt) => write!(f, "{}", stmt),
            Expression(expr) => write!(f, "{}", expr),
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for statement in &self.statements {
            writeln!(f, "{};", statement)?;
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
