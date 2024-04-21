#[derive(Debug, PartialEq)]
pub enum OpCode {
    Constant(u16),
    Add,
    Subtract,
    Multiply,
    Divide,
    True,
    False,
    Pop,
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_make() {}
}
