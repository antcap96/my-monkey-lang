use std::{fmt::Debug, iter::Scan};

use num_enum::TryFromPrimitive;
use thiserror::Error;

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
    Null,
    SetGlobal(u16),
    GetGlobal(u16),
    Array(u16),
    Hash(u16),
    Index,
    Call(u8),
    ReturnValue,
    Return,
    SetLocal(u8),
    GetLocal(u8),
    GetBuiltin(u8),
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
            OpCode::Null => [OpCodeId::Null as u8].into(),
            OpCode::SetGlobal(index) => {
                let mut out = Vec::with_capacity(3);
                out.push(OpCodeId::SetGlobal as u8);
                out.extend_from_slice(&index.to_be_bytes());
                out.into()
            }
            OpCode::GetGlobal(index) => {
                let mut out = Vec::with_capacity(3);
                out.push(OpCodeId::GetGlobal as u8);
                out.extend_from_slice(&index.to_be_bytes());
                out.into()
            }
            OpCode::Array(size) => {
                let mut out = Vec::with_capacity(3);
                out.push(OpCodeId::Array as u8);
                out.extend_from_slice(&size.to_be_bytes());
                out.into()
            }
            OpCode::Hash(size) => {
                let mut out = Vec::with_capacity(3);
                out.push(OpCodeId::Hash as u8);
                out.extend_from_slice(&size.to_be_bytes());
                out.into()
            }
            OpCode::Index => [OpCodeId::Index as u8].into(),
            OpCode::Call(n_args) => [OpCodeId::Call as u8, *n_args].into(),
            OpCode::ReturnValue => [OpCodeId::ReturnValue as u8].into(),
            OpCode::Return => [OpCodeId::Return as u8].into(),
            OpCode::SetLocal(index) => [OpCodeId::SetLocal as u8, *index].into(),
            OpCode::GetLocal(index) => [OpCodeId::GetLocal as u8, *index].into(),
            OpCode::GetBuiltin(index) => [OpCodeId::GetBuiltin as u8, *index].into(),
        }
    }
}

#[repr(u8)]
#[derive(Debug, PartialEq, TryFromPrimitive)]
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
    Null,
    SetGlobal,
    GetGlobal,
    Array,
    Hash,
    Index,
    Call,
    ReturnValue,
    Return,
    SetLocal,
    GetLocal,
    GetBuiltin,
}

#[derive(PartialEq, Clone)]
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
            ip: offset,
        }
    }

    pub fn iter(&self) -> InstructionsIter {
        self.iter_at(0)
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

fn take_while_inclusive<It, F>(
    iter: It,
    mut f: F,
) -> Scan<It, bool, impl FnMut(&mut bool, It::Item) -> Option<It::Item>>
where
    It: Iterator,
    F: FnMut(&It::Item) -> bool,
{
    iter.scan(false, move |terminate, e| {
        if *terminate {
            None
        } else {
            if f(&e) {
                Some(e)
            } else {
                *terminate = true;
                Some(e)
            }
        }
    })
}

impl Debug for Instructions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(take_while_inclusive(self.iter(), |el| el.is_ok()))
            .finish()
    }
}

impl Default for Instructions {
    fn default() -> Self {
        Self::new()
    }
}

pub struct InstructionsIter<'a> {
    instructions: &'a Instructions,
    pub ip: usize,
}

impl<'a> InstructionIterHelper for InstructionsIter<'a> {
    fn read_u8(&mut self) -> Option<u8> {
        let offset = self.ip;
        self.ip += 1;
        self.instructions.bytes.get(offset).copied()
    }
}

#[derive(Debug, PartialEq, Error)]
pub enum InstructionReadError {
    #[error("Unexpected end of instructions")]
    UnexpectedEndOfInstructions,
    #[error("Invalid opcode: {0}")]
    InvalidOpCode(u8),
}

pub trait InstructionIterHelper {
    fn read_u8(&mut self) -> Option<u8>;
    fn read_u16(&mut self) -> Option<u16> {
        let b1 = self.read_u8();
        let b2 = self.read_u8();
        Some(u16::from_be_bytes([b1?, b2?]))
    }
}

pub fn instruction_iter_func(
    obj: &mut impl InstructionIterHelper,
) -> Option<Result<OpCode, InstructionReadError>> {
    let op = obj.read_u8()?;

    let Ok(op): Result<OpCodeId, _> = op.try_into() else {
        return Some(Err(InstructionReadError::InvalidOpCode(op)));
    };

    match op {
        OpCodeId::Constant => {
            let constant = obj.read_u16();
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
            let offset = obj.read_u16();
            match offset {
                Some(o) => Some(Ok(OpCode::Jump(o))),
                None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
            }
        }
        OpCodeId::JumpFalse => {
            let offset = obj.read_u16();
            match offset {
                Some(o) => Some(Ok(OpCode::JumpFalse(o))),
                None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
            }
        }
        OpCodeId::Null => Some(Ok(OpCode::Null)),
        OpCodeId::SetGlobal => {
            let index = obj.read_u16();
            match index {
                Some(i) => Some(Ok(OpCode::SetGlobal(i))),
                None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
            }
        }
        OpCodeId::GetGlobal => {
            let index = obj.read_u16();
            match index {
                Some(i) => Some(Ok(OpCode::GetGlobal(i))),
                None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
            }
        }
        OpCodeId::Array => {
            let size = obj.read_u16();
            match size {
                Some(s) => Some(Ok(OpCode::Array(s))),
                None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
            }
        }
        OpCodeId::Hash => {
            let size = obj.read_u16();
            match size {
                Some(s) => Some(Ok(OpCode::Hash(s))),
                None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
            }
        }
        OpCodeId::Index => Some(Ok(OpCode::Index)),
        OpCodeId::Call => {
            let n_args = obj.read_u8();
            match n_args {
                Some(n) => Some(Ok(OpCode::Call(n))),
                None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
            }
        }
        OpCodeId::ReturnValue => Some(Ok(OpCode::ReturnValue)),
        OpCodeId::Return => Some(Ok(OpCode::Return)),
        OpCodeId::SetLocal => {
            let index = obj.read_u8();
            match index {
                Some(i) => Some(Ok(OpCode::SetLocal(i))),
                None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
            }
        }
        OpCodeId::GetLocal => {
            let index = obj.read_u8();
            match index {
                Some(i) => Some(Ok(OpCode::GetLocal(i))),
                None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
            }
        }
        OpCodeId::GetBuiltin => {
            let index = obj.read_u8();
            match index {
                Some(i) => Some(Ok(OpCode::GetBuiltin(i))),
                None => Some(Err(InstructionReadError::UnexpectedEndOfInstructions)),
            }
        }
    }
}

impl<'a> Iterator for InstructionsIter<'a> {
    type Item = Result<OpCode, InstructionReadError>;

    fn next(&mut self) -> Option<Self::Item> {
        instruction_iter_func(self)
    }
}

impl<T: IntoIterator<Item = OpCode>> From<T> for Instructions {
    fn from(value: T) -> Self {
        let mut output = Instructions::new();
        for element in value {
            output.push(&element)
        }
        output
    }
}
