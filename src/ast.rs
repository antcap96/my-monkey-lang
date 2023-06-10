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
