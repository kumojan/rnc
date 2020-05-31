use crate::codegen::CodeGenError;
use crate::parse::ParseError;
use crate::tokenize::TokenizeError;
use std::borrow::Borrow;
// use std::fmt;

#[derive(Debug)]
#[allow(dead_code)]
pub enum CompileError {
    Tokenize { pos: usize, msg: String },
    Parse { pos: usize, msg: String },
    CodeGen { msg: String },
}
impl From<ParseError> for CompileError {
    fn from(err: ParseError) -> Self {
        Self::Parse {
            pos: err.pos,
            msg: err.msg,
        }
    }
}
impl From<TokenizeError> for CompileError {
    fn from(err: TokenizeError) -> Self {
        Self::Tokenize {
            pos: err.pos,
            msg: err.msg,
        }
    }
}
impl From<CodeGenError> for CompileError {
    fn from(err: CodeGenError) -> Self {
        Self::CodeGen { msg: err.msg }
    }
}

pub fn error_at<T: Borrow<str>>(code: &String, pos: usize, msg: T) -> Result<(), String> {
    eprintln!("{}", code);
    eprintln!("{}^ {}", " ".repeat(pos), msg.borrow());
    Err("compile failed".to_owned())
}

pub fn error<T: Borrow<str>>(msg: T) -> Result<(), String> {
    eprintln!("{}", msg.borrow());
    Err("compile failed".to_owned())
}
