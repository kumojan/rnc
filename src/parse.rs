use crate::tokenize::TokenKind::*;
use crate::tokenize::*;
use std::collections::VecDeque;
use std::fmt;

///
/// パーサー
///
// #[derive(Debug)]
pub enum NodeKind {
    NdAdd,
    NdSub,
    NdMul,
    NdDiv,
    NdEq,
    NdNeq,
    NdLt,
    NdLe,
    NdLvar,
    NdAssign,
    NdNum,
}
impl fmt::Debug for NodeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            NdAdd => write!(f, "+"),
            NdSub => write!(f, "-"),
            NdMul => write!(f, "*"),
            NdDiv => write!(f, "/"),
            NdEq => write!(f, "=="),
            NdNeq => write!(f, "!="),
            NdLt => write!(f, "<"),
            NdLe => write!(f, "<="),
            NdLvar => write!(f, "local var"),
            NdAssign => write!(f, "="),
            NdNum => write!(f, "number"),
        }
    }
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
        kind: NodeKind,
        val: u32,
        offset: char,
    },
    Bin {
        kind: NodeKind,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
}
impl Node {
    fn new_num(val: u32) -> Self {
        Self::Leaf {
            kind: NdNum,
            val,
            offset: ' ',
        }
    }
    fn new_lvar(c: char) -> Self {
        Self::Leaf {
            kind: NdLvar,
            val: 0,
            offset: c,
        }
    }
    pub fn if_num(&self) -> Option<u32> {
        if let Self::Leaf {
            kind: NdNum,
            val,
            offset,
        } = self
        {
            Some(*val)
        } else {
            None
        }
    }
    pub fn is_lvar(&self) -> bool {
        if let Self::Leaf {
            kind: NdLvar,
            val,
            offset,
        } = self
        {
            true
        } else {
            false
        }
    }
    pub fn offset(&self) -> u32 {
        const a_offset: u32 = b"a"[0] as u32;
        if let Self::Leaf {
            kind: NdLvar,
            val,
            offset,
        } = self
        {
            (offset.to_string().as_bytes()[0] as u32 - a_offset + 1) * 8
        } else {
            panic!();
        }
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
    // 見るだけ、エラーなし
    fn peek(&mut self, s: &str) -> bool;
    fn peek_num(&mut self) -> Option<u32>;
    fn peek_ident(&mut self) -> Option<char>;
    fn is_eof(&self) -> bool;
    // peekしてエラーを出す
    fn expect(&mut self, s: &str) -> Result<(), ParseError>;
    fn expect_num(&mut self) -> Result<u32, ParseError>;
    fn expect_ident(&mut self) -> Result<char, ParseError>;
    fn expect_eof(&mut self) -> Result<bool, ParseError>;
    fn program(&mut self) -> Result<Vec<Node>, ParseError>;
    // コード生成
    fn stmt(&mut self) -> Result<Node, ParseError>;
    fn assign(&mut self) -> Result<Node, ParseError>;
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
        if self[0].kind == TkReserved(s.to_owned()) {
            self.pop_front();
            return true;
        }
        false
    }
    fn peek_num(&mut self) -> Option<u32> {
        if let TkNum(val) = self[0].kind {
            self.pop_front();
            Some(val)
        } else {
            None
        }
    }
    fn peek_ident(&mut self) -> Option<char> {
        if let TkIdent(c) = self[0].kind {
            self.pop_front();
            Some(c)
        } else {
            None
        }
    }
    fn is_eof(&self) -> bool {
        self[0].kind == TkEOF
    }
    fn expect(&mut self, s: &str) -> Result<(), ParseError> {
        if !self.peek(s) {
            Err(ParseError {
                pos: self[0].pos,
                msg: format!("expected \'{}\'", s),
            })
        } else {
            Ok(())
        }
    }
    /// 次がTkNum(val)の場合は1つずれてOk(val),それ以外はErr
    fn expect_num(&mut self) -> Result<u32, ParseError> {
        self.peek_num().ok_or(ParseError {
            pos: self[0].pos,
            msg: "expected number".to_owned(),
        })
    }
    fn expect_ident(&mut self) -> Result<char, ParseError> {
        self.peek_ident().ok_or(ParseError {
            pos: self[0].pos,
            msg: "expected identifier".to_owned(),
        })
    }
    fn expect_eof(&mut self) -> Result<bool, ParseError> {
        // match self[0].kind {
        //     TkEOF => Ok(true),
        //     _ => Err(ParseError {
        //         pos: self[0].pos,
        //         msg: "unrecognized by parser".to_owned(),
        //     }),
        // }
        unimplemented!();
    }
    fn program(&mut self) -> Result<Vec<Node>, ParseError> {
        let mut code = vec![];
        while !self.is_eof() {
            code.push(self.stmt()?)
        }
        Ok(code)
    }
    fn stmt(&mut self) -> Result<Node, ParseError> {
        let node = self.expr()?;
        self.expect(";")?;
        Ok(node)
    }
    fn expr(&mut self) -> Result<Node, ParseError> {
        self.assign()
    }
    fn assign(&mut self) -> Result<Node, ParseError> {
        let mut node = self.equality()?;
        // "="が見えた場合は代入文にする。
        if self.peek("=") {
            node = Node::new_bin(NdAssign, node, self.equality()?);
        }
        Ok(node)
    }
    fn equality(&mut self) -> Result<Node, ParseError> {
        let mut node = self.relational()?;
        loop {
            if self.peek("==") {
                node = Node::new_bin(NdEq, node, self.relational()?);
            } else if self.peek("!=") {
                node = Node::new_bin(NdNeq, node, self.relational()?);
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
        } else if let Some(val) = self.peek_num() {
            Ok(Node::new_num(val))
        } else {
            Ok(Node::new_lvar(self.expect_ident()?))
        }
    }
}
