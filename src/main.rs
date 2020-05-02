use std::env;
mod err;
use crate::err::*;

const HEADER: &str = ".intel_syntax noprefix\n.global main\nmain:";
fn main() -> Result<(), CompileError> {
    // Result型を返すことで、, エラー終了時に終了ステータス1となる。
    //
    match env::args().nth(1) {
        Some(code) => match compile(&code) {
            Err(EnumError::Tokenize { pos }) => error_at(&code, pos, "uknown token".to_owned()),
            Err(EnumError::Parse { pos, msg }) => {
                error_at(&code, pos, format!("parse failed: {}", msg))
            }
            _ => Ok(()),
        },
        None => error("input needed".to_owned()),
    }
}

fn compile(code: &String) -> Result<(), EnumError> {
    let mut token_list = tokenize(code)?;
    println!("{}", HEADER);
    token_list.expr()?.code_gen();
    println!("  pop rax");
    println!("  ret");
    Ok(())
}

use crate::TokenKind::*;
///
/// トークナイザ
/// 字句解析
/// Lexical Analysis
/// トークナイザは、空白で区切る、記号は1文字ずつなどのルールで、入力文を
/// トークン列に分けていく
/// エラーメッセージとしては、予期せぬ記号のみ
///
#[derive(Copy, Clone, Debug, PartialEq)]
enum TokenKind {
    TkReserved(char),
    TkNum(u32),
    TkEOF,
}

/// 初期値EOF
impl Default for TokenKind {
    fn default() -> Self {
        Self::TkEOF
    }
}

/// 初期値EOF
#[derive(Default)]
struct Token {
    kind: TokenKind,
    pos: usize,
}
impl Token {
    fn new_num(val: u32, pos: usize) -> Self {
        Self {
            kind: TkNum(val),
            pos,
        }
    }
    fn new_res(c: char, pos: usize) -> Self {
        Self {
            kind: TkReserved(c),
            pos,
        }
    }
    fn eof(pos: usize) -> Self {
        Self {
            pos,
            ..Default::default()
        }
    }
}

fn tokenize(code: &String) -> Result<VecDeque<Token>, TokenizeError> {
    let mut numvec = vec![];
    let mut list: VecDeque<Token> = VecDeque::new();
    let code = format!("{} ", code); // numvecを回収させるために、末尾に空白追加(なんか嫌だけど)
    for (pos, c) in code.chars().enumerate() {
        if c.is_ascii_digit() {
            numvec.push(c.to_digit(10).unwrap());
            continue;
        } else if !numvec.is_empty() {
            // let tk = TkNum(numvec.iter().fold(0, |acc, x| acc * 10 + x));
            let tk = Token::new_num(numvec.iter().fold(0, |acc, x| acc * 10 + x), pos);
            list.push_back(tk);
            numvec = vec![];
        }
        if "+-*/()".find(c).is_some() {
            list.push_back(Token::new_res(c, pos));
            continue;
        }
        if c.is_whitespace() {
            continue;
        }
        return Err(pos)?;
    }
    list.push_back(Token::eof(code.len()));
    Ok(list)
}

trait TkList {
    fn peek(&mut self, c: char) -> bool;
    fn expect(&mut self, c: char) -> Result<(), ParseError>;
    fn expect_num(&mut self) -> Result<u32, ParseError>;
    fn mul(&mut self) -> Result<Node, ParseError>;
    fn expr(&mut self) -> Result<Node, ParseError>;
    fn primary(&mut self) -> Result<Node, ParseError>;
}

use std::collections::VecDeque;

impl TkList for VecDeque<Token> {
    /// 次がTkReserved(c) (cは指定)の場合は、1つずれてtrue, それ以外はずれずにfalse
    fn peek(&mut self, c: char) -> bool {
        if self.len() > 0 && self[0].kind == TkReserved(c) {
            self.pop_front();
            return true;
        }
        false
    }
    fn expect(&mut self, c: char) -> Result<(), ParseError> {
        if self.len() > 0 {
            if self[0].kind == TkReserved(c) {
                self.pop_front();
                Ok(())
            } else {
                Err(ParseError {
                    pos: self[0].pos,
                    msg: format!("expected \'{}\'", c),
                })
            }
        } else {
            panic!("EOF token not found!");
        }
    }
    /// 次がTkNum(val)の場合は1つずれてSome(val), それ以外はずれずにNone
    fn expect_num(&mut self) -> Result<u32, ParseError> {
        if self.len() > 0 {
            match self[0].kind {
                TkNum(val) => {
                    self.pop_front();
                    Ok(val)
                }
                _ => Err(ParseError {
                    pos: self[0].pos,
                    msg: "expected number".to_owned(),
                }),
            }
        } else {
            panic!("EOF token not found!");
        }
    }
    fn primary(&mut self) -> Result<Node, ParseError> {
        if self.peek('(') {
            let node = self.expr();
            self.expect(')').map(|_| node)?
        } else {
            self.expect_num().map(|val| Ok(Node::new_num(val)))?
        }
    }
    fn expr(&mut self) -> Result<Node, ParseError> {
        let mut node = self.mul()?;
        loop {
            if self.peek('+') {
                node = Node::new_bin(NdAdd, node, self.mul()?);
            } else if self.peek('-') {
                node = Node::new_bin(NdSub, node, self.mul()?);
            } else {
                return Ok(node);
            }
        }
    }
    fn mul(&mut self) -> Result<Node, ParseError> {
        let mut node = self.primary()?;
        loop {
            if self.peek('*') {
                node = Node::new_bin(NdMul, node, self.primary()?);
            } else if self.peek('/') {
                node = Node::new_bin(NdDiv, node, self.primary()?);
            } else {
                return Ok(node);
            }
        }
    }
}

///
/// パーサー
///
enum NodeKind {
    NdAdd,
    NdSub,
    NdMul,
    NdDiv,
    NdNum,
}
use crate::NodeKind::*;
impl Default for NodeKind {
    fn default() -> Self {
        Self::NdNum
    }
}

#[allow(dead_code)]
enum Node {
    Leaf {
        val: u32,
    },
    Bin {
        kind: NodeKind,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
}
impl Node {
    fn new_num(val: u32) -> Self {
        Self::Leaf { val }
    }
    fn new_bin(kind: NodeKind, lhs: Node, rhs: Node) -> Self {
        Self::Bin {
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    fn code_gen(&self) {
        if let Self::Leaf { val } = self {
            println!("  push {}", val);
        } else if let Self::Bin { kind, lhs, rhs } = self {
            lhs.code_gen();
            rhs.code_gen();
            println!("  pop rdi");
            println!("  pop rax");
            let code = match kind {
                NdAdd => "  add rax, rdi",
                NdSub => "  sub rax, rdi",
                NdMul => "  imul rax, rdi",
                NdDiv => "  cqo\n  idiv rdi",
                _ => panic!(),
            };
            println!("{}", code);
            println!("  push rax")
        }
        println!("");
    }
}
