#[derive(Debug, PartialEq, Clone)]
pub enum OpCode {
    Constant(u16),
    Add,
    Subtract,
    Multiply,
    Divide,
    True,
    False,
    Equal,
    NotEqual,
    GreaterThan,
    Minus,
    Bang,
    Pop,
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_make() {}
}
