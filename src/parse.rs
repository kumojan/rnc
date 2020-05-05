use crate::tokenize::TokenKind::*;
use crate::tokenize::*;
use std::collections::VecDeque;
use std::fmt;

///
/// パーサー
///
#[derive(Debug)]
pub enum NodeKind {
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
use NodeKind::*;
impl Default for NodeKind {
    fn default() -> Self {
        Self::NdNum
    }
}

#[allow(dead_code)]
pub enum Node {
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

pub trait TkList {
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

impl TkList for VecDeque<Token> {
    /// 次がTkReserved(c) (cは指定)の場合は、1つずれてtrue, それ以外はずれずにfalse
    fn peek(&mut self, s: &str) -> bool {
        // self.last().map_or(false, |l| {
        //     if l.kind == TkReserved(s.to_owned()) {
        //         self.pop_front();
        //         true
        //     } else {
        //         false
        //     }
        // })
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
                // 現状、ちゃんとコードが完結していなくても正常終了してしまう。
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
