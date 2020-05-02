use std::fmt;

#[derive(Debug, Default)]
pub struct TokenizeError {
    pos: usize,
}
// このformaterを書き換えてcodeを挿入したら、自動でメッセージ出力できそうな気がする。
impl fmt::Display for TokenizeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "invalid token at {}", self.pos)
    }
}
impl From<usize> for TokenizeError {
    fn from(pos: usize) -> Self {
        Self { pos }
    }
}

#[derive(Debug, Default)]
pub struct ParseError {
    pub pos: usize,
    pub msg: String,
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "parse failed at {}", self.pos)
    }
}
// impl From<usize> for ParseError {
//     fn from(pos: usize) -> Self {
//         Self {
//             pos,
//             msg: "parse failed",
//         }
//     }
// }

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
pub fn error_at(code: &String, pos: usize, msg: String) -> Result<(), CompileError> {
    eprintln!("{}", code);
    eprintln!("{}^ {}", " ".repeat(pos), msg);
    Err(CompileError)
}

pub fn error(msg: String) -> Result<(), CompileError> {
    eprintln!("{}", msg);
    Err(CompileError)
}
