#[derive(Debug, PartialEq)]
pub enum OpCode {
    OpConstant(u16),
    OpAdd,
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_make() {}
}
