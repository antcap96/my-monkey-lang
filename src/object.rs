#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}
