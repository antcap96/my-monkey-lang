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
    Jump(u16),
    JumpFalse(u16),
}

impl OpCode {
    pub fn bytes(&self) -> Box<[u8]> {
        match self {
            OpCode::Constant(constant) => {
                let mut out = Vec::with_capacity(3);
                out.push(OpCodeId::Constant as u8);
                out.extend_from_slice(&constant.to_be_bytes());
                out.into()
            }
            OpCode::Add => [OpCodeId::Add as u8].into(),
            OpCode::Subtract => [OpCodeId::Subtract as u8].into(),
            OpCode::Multiply => [OpCodeId::Multiply as u8].into(),
            OpCode::Divide => [OpCodeId::Divide as u8].into(),
            OpCode::True => [OpCodeId::True as u8].into(),
            OpCode::False => [OpCodeId::False as u8].into(),
            OpCode::Equal => [OpCodeId::Equal as u8].into(),
            OpCode::NotEqual => [OpCodeId::NotEqual as u8].into(),
            OpCode::GreaterThan => [OpCodeId::GreaterThan as u8].into(),
            OpCode::Minus => [OpCodeId::Minus as u8].into(),
            OpCode::Bang => [OpCodeId::Bang as u8].into(),
            OpCode::Pop => [OpCodeId::Pop as u8].into(),
            OpCode::Jump(offset) => {
                let mut out = Vec::with_capacity(3);
                out.push(OpCodeId::Jump as u8);
                out.extend_from_slice(&offset.to_be_bytes());
                out.into()
            }
            OpCode::JumpFalse(offset) => {
                let mut out = Vec::with_capacity(3);
                out.push(OpCodeId::JumpFalse as u8);
                out.extend_from_slice(&offset.to_be_bytes());
                out.into()
            }
        }
    }
}

#[repr(u8)]
pub enum OpCodeId {
    Constant,
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
    Jump,
    JumpFalse,
}

impl TryFrom<u8> for OpCodeId {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == OpCodeId::Constant as u8 => Ok(OpCodeId::Constant),
            x if x == OpCodeId::Add as u8 => Ok(OpCodeId::Add),
            x if x == OpCodeId::Subtract as u8 => Ok(OpCodeId::Subtract),
            x if x == OpCodeId::Multiply as u8 => Ok(OpCodeId::Multiply),
            x if x == OpCodeId::Divide as u8 => Ok(OpCodeId::Divide),
            x if x == OpCodeId::True as u8 => Ok(OpCodeId::True),
            x if x == OpCodeId::False as u8 => Ok(OpCodeId::False),
            x if x == OpCodeId::Equal as u8 => Ok(OpCodeId::Equal),
            x if x == OpCodeId::NotEqual as u8 => Ok(OpCodeId::NotEqual),
            x if x == OpCodeId::GreaterThan as u8 => Ok(OpCodeId::GreaterThan),
            x if x == OpCodeId::Minus as u8 => Ok(OpCodeId::Minus),
            x if x == OpCodeId::Bang as u8 => Ok(OpCodeId::Bang),
            x if x == OpCodeId::Pop as u8 => Ok(OpCodeId::Pop),
            x if x == OpCodeId::Jump as u8 => Ok(OpCodeId::Jump),
            x if x == OpCodeId::JumpFalse as u8 => Ok(OpCodeId::JumpFalse),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Instructions {
    pub bytes: Vec<u8>,
}

impl Instructions {
    pub fn new() -> Instructions {
        Instructions { bytes: Vec::new() }
    }

    pub fn iter_at(&self, offset: usize) -> InstructionsIter {
        InstructionsIter {
            instructions: self,
            offset,
        }
    }

    pub fn iter(&self) -> InstructionsIter {
        self.iter_at(0)
    }

    fn write_u8(&mut self, b: u8) {
        self.bytes.push(b);
    }

    fn write_u16(&mut self, s: u16) {
        self.bytes.extend(s.to_be_bytes());
    }

    pub fn push(&mut self, op: &OpCode) {
        self.bytes.extend_from_slice(&op.bytes());
    }

    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    pub fn pop_to(&mut self, to: usize) {
        self.bytes.truncate(to);
    }

    pub fn replace(&mut self, position: usize, new_instruction: OpCode) {
        let to_replace = new_instruction.bytes();

        self.bytes[position..][..to_replace.len()].copy_from_slice(&to_replace);
    }
}

pub struct InstructionsIter<'a> {
    instructions: &'a Instructions,
    offset: usize,
}
impl<'a> InstructionsIter<'a> {
    fn read_u8(&mut self) -> Option<u8> {
        let offset = self.offset;
        self.offset += 1;
        self.instructions.bytes.get(offset).copied()
    }

    fn read_u16(&mut self) -> Option<u16> {
        let b1 = self.read_u8();
        let b2 = self.read_u8();
        Some(u16::from_be_bytes([b1?, b2?]))
    }
}

#[derive(Debug, PartialEq)]
pub enum InstructionReadError {
    UnexpectedEndOfInstructions,
    InvalidOpCode(u8),
}

impl<'a> Iterator for InstructionsIter<'a> {
    type Item = Result<OpCode, InstructionReadError>;

    fn next(&mut self) -> Option<Self::Item> {
        // if self.offset >= self.instructions.bytes.len() {
        //     return None;
        // }

        let op = self.read_u8()?;

        let Ok(op): Result<OpCodeId, _> = op.try_into() else {
            return Some(Err(InstructionReadError::InvalidOpCode(op)));
        };

        match op {
            OpCodeId::Constant => {
                let constant = self.read_u16();
                match constant {
                    Some(c) => Some(Ok(OpCode::Constant(c))),
                    None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
                }
            }
            OpCodeId::Add => Some(Ok(OpCode::Add)),
            OpCodeId::Subtract => Some(Ok(OpCode::Subtract)),
            OpCodeId::Multiply => Some(Ok(OpCode::Multiply)),
            OpCodeId::Divide => Some(Ok(OpCode::Divide)),
            OpCodeId::True => Some(Ok(OpCode::True)),
            OpCodeId::False => Some(Ok(OpCode::False)),
            OpCodeId::Equal => Some(Ok(OpCode::Equal)),
            OpCodeId::NotEqual => Some(Ok(OpCode::NotEqual)),
            OpCodeId::GreaterThan => Some(Ok(OpCode::GreaterThan)),
            OpCodeId::Minus => Some(Ok(OpCode::Minus)),
            OpCodeId::Bang => Some(Ok(OpCode::Bang)),
            OpCodeId::Pop => Some(Ok(OpCode::Pop)),
            OpCodeId::Jump => {
                let offset = self.read_u16();
                match offset {
                    Some(o) => Some(Ok(OpCode::Jump(o))),
                    None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
                }
            }
            OpCodeId::JumpFalse => {
                let offset = self.read_u16();
                match offset {
                    Some(o) => Some(Ok(OpCode::JumpFalse(o))),
                    None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
                }
            }
        }
    }
}
