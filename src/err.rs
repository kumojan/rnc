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
/// posが属する行番号(0始まり)と、その行内でのpos(0始まり)
fn get_line_number(code: &String, pos: usize) -> (usize, usize) {
    let (n, line_start) = code
        .chars()
        .take(pos)
        .enumerate()
        .filter(|c| c.1 == '\n')
        .enumerate()
        .last()
        .map(|(n, (m, _))| (n + 1, m + 1)) // countの代わりにenumerateを使っているので、1足す
        .unwrap_or((0, 0)); // posが1行目に含まれる場合
    (n, pos - line_start)
}

pub fn error_at<T: Borrow<str>>(
    code: &String,
    pos: usize,
    msg: T,
    filename: &String,
) -> Result<(), String> {
    let (n, pos) = get_line_number(&code, pos);
    let line = code.split('\n').nth(n).unwrap();
    let header = format!("{}:{}: ", filename, n);
    eprintln!("{}{}", header, line);
    eprintln!("{}^ {}", " ".repeat(pos + header.len()), msg.borrow()); // 前の行のヘッダの分ずらす
    Err("compile failed".to_owned())
}

pub fn error<T: Borrow<str>>(msg: T) -> Result<(), String> {
    eprintln!("{}", msg.borrow());
    Err("compile failed".to_owned())
}
