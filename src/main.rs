use std::env;
// use std::str;
mod err;
use crate::err::*;

const HEADER: &str = ".intel_syntax noprefix\n.global main\nmain:";
fn main() -> Result<(), CompileError> {
    // Result型を返すことで、, エラー終了時に終了ステータス1となる。
    //
    match env::args().nth(1) {
        Some(code) => match compile(&code) {
            Err(EnumError::Tokenize { pos }) => error_at(&code, pos, "uknown token"),
            Err(EnumError::Parse { pos, msg }) => {
                error_at(&code, pos, format!("parse failed: {}", msg))
            }
            _ => Ok(()),
        },
        None => error("input needed"),
    }
}

fn compile(code: &String) -> Result<(), EnumError> {
    let mut token_list = tokenize(code)?;
    // println!("{:?}", token_list);
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
#[derive(Clone, Debug, PartialEq)]
enum TokenKind {
    TkReserved(String),
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
#[derive(Default, Debug)]
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
    fn new_res(s: String, pos: usize) -> Self {
        Self {
            kind: TkReserved(s),
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
    let mut list: VecDeque<Token> = VecDeque::new();
    // let code = &format!("{} ", code)[..]; // numvecを回収させるために、末尾に空白追加(なんか嫌だけど)
    let mut pos = 0;
    while (pos < code.len()) {
        // 数字読み取り
        let n: String = code
            .chars()
            .skip(pos)
            .take_while(|c| c.is_ascii_digit())
            .collect();
        if n.len() > 0 {
            pos += n.len();
            list.push_back(Token::new_num(n.parse().unwrap(), pos));
            continue;
        }
        // 2文字読み取り
        if (pos < code.len() - 1) {
            // let cc = &code[pos..pos + 2];
            // let cc = &code.chars().skip(pos).take(2).collect::<String>()[..];  // こっちの方が安全?
            let x = &code.as_bytes()[pos..pos + 2]; // バイトでみた方が効率よさそう。
            if [b"==", b"!=", b"<=", b">="].iter().any(|b| b == &x) {
                let s = String::from_utf8(x.to_vec()).unwrap(); // "==", "!=", "<=", ">=" のどれかなので大丈夫
                let tk = Token::new_res(s, pos);
                list.push_back(tk);
                pos += 2;
                continue;
            }
        }
        // 1文字読み取り
        let c = code.chars().nth(pos).unwrap();
        if "+-*/()<>".find(c).is_some() {
            list.push_back(Token::new_res(c.to_string(), pos));
            pos += 1;
            continue;
        }
        if c.is_whitespace() {
            pos += 1;
            continue;
        }
        return Err(pos)?;
    }
    list.push_back(Token::eof(code.len()));
    Ok(list)
}

trait TkList {
    fn peek(&mut self, s: &str) -> bool;
    fn expect(&mut self, s: &str) -> Result<(), ParseError>;
    fn expect_num(&mut self) -> Result<u32, ParseError>;
    fn expect_eof(&mut self) -> Result<bool, ParseError>;
    fn expr(&mut self) -> Result<Node, ParseError>;
    fn equality(&mut self) -> Result<Node, ParseError>;
    fn relational(&mut self) -> Result<Node, ParseError>;
    fn add(&mut self) -> Result<Node, ParseError>;
    fn mul(&mut self) -> Result<Node, ParseError>;
    fn primary(&mut self) -> Result<Node, ParseError>;
    fn unary(&mut self) -> Result<Node, ParseError>;
}

use std::collections::VecDeque;

impl TkList for VecDeque<Token> {
    /// 次がTkReserved(c) (cは指定)の場合は、1つずれてtrue, それ以外はずれずにfalse
    fn peek(&mut self, s: &str) -> bool {
        if self.len() > 0 && self[0].kind == TkReserved(s.to_owned()) {
            self.pop_front();
            return true;
        }
        false
    }
    fn expect(&mut self, s: &str) -> Result<(), ParseError> {
        if self.len() > 0 {
            if self[0].kind == TkReserved(s.to_owned()) {
                self.pop_front();
                Ok(())
            } else {
                Err(ParseError {
                    pos: self[0].pos,
                    msg: format!("expected \'{}\'", s),
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
    fn expect_eof(&mut self) -> Result<bool, ParseError> {
        if self.len() > 0 {
            match self[0].kind {
                TkEOF => Ok(true),
                _ => Err(ParseError {
                    pos: self[0].pos,
                    msg: "unrecognized by parser".to_owned(),
                }),
            }
        } else {
            panic!("EOF token not found!")
        }
    }
    fn expr(&mut self) -> Result<Node, ParseError> {
        self.equality()
    }
    fn equality(&mut self) -> Result<Node, ParseError> {
        let mut node = self.relational()?;
        loop {
            if self.peek("==") {
                node = Node::new_bin(NdEq, node, self.relational()?);
            } else if self.peek("!=") {
                node = Node::new_bin(NdNeq, node, self.relational()?);
            // } else if self.expect_eof()? {
            } else {
                return Ok(node);
            }
        }
    }
    fn relational(&mut self) -> Result<Node, ParseError> {
        let mut node = self.add()?;
        loop {
            if self.peek("<") {
                node = Node::new_bin(NdLt, node, self.add()?);
            } else if self.peek("<=") {
                node = Node::new_bin(NdLe, node, self.add()?);
            } else if self.peek(">") {
                node = Node::new_bin(NdLt, self.add()?, node);
            } else if self.peek(">=") {
                node = Node::new_bin(NdLe, self.add()?, node);
            } else {
                return Ok(node);
            }
        }
    }
    // expr -> mul -> unary -> primaryの順に呼ばれる
    fn add(&mut self) -> Result<Node, ParseError> {
        let mut node = self.mul()?;
        loop {
            if self.peek("+") {
                node = Node::new_bin(NdAdd, node, self.mul()?);
            } else if self.peek("-") {
                node = Node::new_bin(NdSub, node, self.mul()?);
            } else {
                return Ok(node);
            }
        }
    }
    fn mul(&mut self) -> Result<Node, ParseError> {
        let mut node = self.unary()?;
        loop {
            if self.peek("*") {
                node = Node::new_bin(NdMul, node, self.unary()?);
            } else if self.peek("/") {
                node = Node::new_bin(NdDiv, node, self.unary()?);
            } else {
                return Ok(node);
            }
        }
    }
    fn unary(&mut self) -> Result<Node, ParseError> {
        if self.peek("+") {
            self.unary()
        } else if self.peek("-") {
            Ok(Node::new_bin(NdSub, Node::new_num(0), self.unary()?))
        } else {
            self.primary()
        }
    }
    fn primary(&mut self) -> Result<Node, ParseError> {
        if self.peek("(") {
            let node = self.expr();
            self.expect(")").map(|_| node)?
        } else {
            self.expect_num().map(|val| Ok(Node::new_num(val)))?
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
    NdEq,
    NdNeq,
    NdLt,
    NdLe,
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
                NdEq => "  cmp rax, rdi\nsete al\nmovzb rax, al", // 最後のmovzbは、raxの上位56を削除して、下位8ビットにalを入れるということだろう。
                NdNeq => "  cmp rax, rdi\nsetne al\nmovzb rax, al",
                NdLt => "  cmp rax, rdi\nsetl al\nmovzb rax, al",
                NdLe => "  cmp rax, rdi\nsetle al\nmovzb rax, al",
                _ => unimplemented!(),
            };
            println!("{}", code);
            println!("  push rax")
        }
        println!("");
    }
}
