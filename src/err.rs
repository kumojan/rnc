use crate::parse::ParseError;
use crate::tokenize::TokenizeError;
use std::borrow::Borrow;
// use std::fmt;

#[derive(Debug)]
pub enum EnumError {
    Tokenize { pos: usize },
    Parse { pos: usize, msg: String },
}
impl From<ParseError> for EnumError {
    fn from(err: ParseError) -> Self {
        Self::Parse {
            pos: err.pos,
            msg: err.msg,
        }
    }
}
impl From<TokenizeError> for EnumError {
    fn from(err: TokenizeError) -> Self {
        Self::Tokenize { pos: err.pos }
    }
}

#[derive(Debug)]
pub struct CompileError;
pub fn error_at<T: Borrow<str>>(code: &String, pos: usize, msg: T) -> Result<(), CompileError> {
    eprintln!("{}", code);
    eprintln!("{}^ {}", " ".repeat(pos), msg.borrow());
    Err(CompileError)
}

pub fn error<T: Borrow<str>>(msg: T) -> Result<(), CompileError> {
    eprintln!("{}", msg.borrow());
    Err(CompileError)
}
