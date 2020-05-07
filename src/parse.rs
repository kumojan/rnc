use crate::tokenize::TokenKind::*;
use crate::tokenize::*;
use std::collections::{HashSet, VecDeque};
use std::fmt;

///
/// 変数リスト
///
pub fn local_variables(token_list: &VecDeque<Token>) -> Vec<String> {
    let mut lbars: Vec<_> = token_list
        .iter()
        .flat_map(|x| match &x.kind {
            TkIdent(s, _) => Some(s.clone()),
            _ => None,
        })
        .collect();
    lbars.dedup();
    lbars
}

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
            NdLvar => write!(f, "v"),
            NdAssign => write!(f, "="),
            NdNum => write!(f, "n"),
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
        name: String,
        offset: usize,
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
            name: val.to_string(),
            offset: 0,
        }
    }
    fn new_lvar(s: String, offset: usize) -> Self {
        Self::Leaf {
            kind: NdLvar,
            val: 0,
            name: s,
            offset,
        }
    }
    pub fn if_num(&self) -> Option<u32> {
        if let Self::Leaf {
            kind: NdNum, val, ..
        } = self
        {
            Some(*val)
        } else {
            None
        }
    }
    pub fn is_lvar(&self) -> bool {
        if let Self::Leaf { kind: NdLvar, .. } = self {
            true
        } else {
            false
        }
    }
    pub fn offset(&self) -> usize {
        if let Self::Leaf {
            kind: NdLvar,
            offset,
            ..
        } = self
        {
            *offset
        } else {
            0
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

pub trait Parser {
    // 見るだけ、エラーなし
    fn peek(&mut self, s: &str) -> bool;
    fn peek_num(&mut self) -> Option<u32>;
    fn peek_ident(&mut self) -> Option<String>;
    fn is_eof(&self) -> bool;
    // 結果をもらい、違ったらエラーを出す
    fn expect(&mut self, s: &str) -> Result<(), ParseError>;
    fn expect_num(&mut self) -> Result<u32, ParseError>;
    fn expect_ident(&mut self) -> Result<String, ParseError>;
    // コード生成
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
    fn primary(&mut self) -> Result<Node, ParseError>;
}

impl Parser for VecDeque<Token> {
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
    fn peek_ident(&mut self) -> Option<String> {
        if let TkIdent(s, _) = self[0].kind.clone() {
            self.pop_front();
            Some(s)
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
    fn expect_ident(&mut self) -> Result<String, ParseError> {
        self.peek_ident().ok_or(ParseError {
            pos: self[0].pos,
            msg: "expected identifier".to_owned(),
        })
    }
    fn primary(&mut self) -> Result<Node, ParseError> {
        unimplemented!();
    }
}

pub struct ParserStruct {
    tklist: VecDeque<Token>,
    lvars: Vec<String>,
    pub varoffset: usize,
}
impl ParserStruct {
    pub fn new(tklist: VecDeque<Token>) -> Self {
        let lvars = local_variables(&tklist);
        let varoffset = lvars.len() * 8;
        Self {
            tklist,
            lvars,
            varoffset,
        }
    }
}
impl Parser for ParserStruct {
    fn peek(&mut self, s: &str) -> bool {
        self.tklist.peek(s)
    }
    fn peek_num(&mut self) -> Option<u32> {
        self.tklist.peek_num()
    }
    fn peek_ident(&mut self) -> Option<String> {
        self.tklist.peek_ident()
    }
    fn is_eof(&self) -> bool {
        self.tklist.is_eof()
    }
    // peekしてエラーを出す
    fn expect(&mut self, s: &str) -> Result<(), ParseError> {
        self.tklist.expect(s)
    }
    fn expect_num(&mut self) -> Result<u32, ParseError> {
        self.tklist.expect_num()
    }
    fn expect_ident(&mut self) -> Result<String, ParseError> {
        self.tklist.expect_ident()
    }
    fn primary(&mut self) -> Result<Node, ParseError> {
        if self.peek("(") {
            let node = self.expr();
            self.expect(")").map(|_| node)?
        } else if let Some(val) = self.peek_num() {
            Ok(Node::new_num(val))
        } else {
            let name = self.expect_ident()?;
            let offset = self
                .lvars
                .iter()
                .enumerate()
                .flat_map(|(i, s)| if *s == name { Some((i + 1) * 8) } else { None })
                .next()
                .unwrap();
            Ok(Node::new_lvar(name, offset))
        }
    }
}
