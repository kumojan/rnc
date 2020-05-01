use std::env;

const HEADER: &str = ".intel_syntax noprefix\n.global main\nmain:";
fn main() -> Result<(), &'static str> {
    match env::args().nth(1) {
        Some(x) => match tokenize2(x) {
            None => Err("トークナイズできません。"),
            Some(mut tklist) => {
                println!("{}", HEADER);
                tklist.expr().code_gen();
                Ok(())
            }
        },
        None => Err("引数の数が正しくありません。"),
    }?;
    println!("  ret");
    Ok(())
}

fn consume_list(mut list: Vec<Token>) -> Result<(), &'static str> {
    println!("{}", HEADER);
    list.reverse();
    let fstnum = expect_num(list.pop().unwrap())?;
    println!("  mov rax, {}", fstnum);
    loop {
        match list.pop().unwrap() {
            TkReserved(c) => {
                let num = expect_num(list.pop().unwrap())?;
                match c {
                    '+' => println!("  add rax, {}", num),
                    '-' => println!("  sub rax, {}", num),
                    _ => return Err("unknown"),
                }
            }
            TkEOF => return Ok(()),
            _ => return Err("didn't expect a number"),
        }
    }
}

use crate::Token::*;

///
/// トークナイザ
///
#[derive(Copy, Clone, Debug, PartialEq)]
enum Token {
    TkReserved(char),
    TkNum(u32),
    TkEOF,
}

fn tokenize(mut code: String) -> Option<Vec<Token>> {
    let mut list = vec![];
    let mut numvec: Vec<u32> = vec![];
    code.push(' '); // numvecを回収させるために、末尾に空白追加(なんか嫌だけど)
    for c in code.chars() {
        if c.is_ascii_digit() {
            numvec.push(c.to_digit(10).unwrap());
            continue;
        } else if !numvec.is_empty() {
            let tk = TkNum(numvec.iter().fold(0, |acc, x| acc * 10 + x));
            list.push(tk);
            numvec = vec![];
        }
        if "+-*/()".find(c).is_some() {
            list.push(TkReserved(c));
            continue;
        }
        if c.is_whitespace() {
            continue;
        }
        return None;
    }
    list.push(TkEOF);
    Some(list)
}
fn tokenize2(mut code: String) -> Option<VecDeque<Token>> {
    tokenize(code).map(|v| VecDeque::from(v))
}

fn expect_num(tk: Token) -> Result<u32, &'static str> {
    match tk {
        TkNum(x) => Ok(x),
        _ => Err("expected number but got sth else"),
    }
}

trait TkList {
    fn expect(&mut self, c: char) -> bool;
    fn expect_num(&mut self) -> Option<u32>;
    fn mul(&mut self) -> Node_ {
        unimplemented!();
    }
    fn expr(&mut self) -> Node_ {
        unimplemented!();
    }
    fn primary(&mut self) -> Node_ {
        unimplemented!();
    }
}

use std::collections::VecDeque;

impl TkList for VecDeque<Token> {
    /// 次がTkReserved(c) (cは指定)の場合は、1つずれてtrue, それ以外はずれずにfalse
    fn expect(&mut self, c: char) -> bool {
        if self.len() > 0 && self[0] == TkReserved(c) {
            self.pop_front();
            return true;
        }
        false
    }
    /// 次がTkNum(val)の場合は1つずれてSome(val), それ以外はずれずにNone
    fn expect_num(&mut self) -> Option<u32> {
        if self.len() > 0 {
            match self[0] {
                TkNum(val) => {
                    self.pop_front();
                    Some(val)
                }
                _ => None,
            }
        } else {
            None
        }
    }
    fn primary(&mut self) -> Node_ {
        if self.expect('(') {
            let node = self.expr();
            if self.expect(')') {
                return node;
            } else {
                panic!();
            }
        };
        match self.expect_num() {
            Some(val) => Node_::new_num(val),
            _ => panic!(),
        }
    }
    fn expr(&mut self) -> Node_ {
        let mut node = self.mul();
        loop {
            if self.expect('+') {
                node = Node_::new_bin(NdAdd, node, self.mul());
            } else if self.expect('-') {
                node = Node_::new_bin(NdSub, node, self.mul());
            } else {
                return node;
            }
        }
    }
    fn mul(&mut self) -> Node_ {
        let mut node = self.primary();
        loop {
            if self.expect('*') {
                node = Node_::new_bin(NdMul, node, self.primary());
            } else if self.expect('/') {
                node = Node_::new_bin(NdDiv, node, self.primary());
            } else {
                return node;
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

struct Leaf {
    val: u32,
}
struct Bin {
    kind: NodeKind,
    lhs: Box<Node_>,
    rhs: Box<Node_>,
}
enum Node_ {
    leaf(Leaf),
    bin(Bin),
}
impl Node_ {
    fn new_num(val: u32) -> Self {
        Self::leaf(Leaf { val })
    }
    fn new_bin(kind: NodeKind, lhs: Node_, rhs: Node_) -> Self {
        Self::bin(Bin {
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        })
    }
    fn code_gen(&self) {
        if let Self::leaf(l) = self {
            println!("  push {}", l.val);
        } else if let Self::bin(b) = self {
            b.lhs.code_gen();
            b.rhs.code_gen();
            println!("  pop rdi");
            println!("  pop rax");
            let code = match b.kind {
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

#[derive(Default)]
struct Node {
    val: u32,
    kind: NodeKind,
    lhs: Option<Box<Node>>,
    rhs: Option<Box<Node>>,
}
impl Node {
    fn new_num(val: u32) -> Self {
        Self {
            val,
            ..Self::default()
        }
    }
    fn new_bin(kind: NodeKind, lhs: Node, rhs: Node) -> Self {
        Self {
            val: 0,
            kind,
            lhs: Some(Box::new(lhs)),
            rhs: Some(Box::new(rhs)),
        }
    }
    // fn code_gen()
}
