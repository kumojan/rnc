use crate::tokenize::TokenKind::*;
use crate::tokenize::*;
use std::collections::VecDeque;
use std::fmt;
///
/// 変数リスト
///
pub fn local_variables(token_list: &VecDeque<Token>) -> Vec<String> {
    let mut lbars: Vec<_> = token_list
        .iter()
        .flat_map(|x| match &x.kind {
            TkIdent(s) => Some(s.clone()),
            _ => None,
        })
        .collect();
    lbars.sort();
    lbars.dedup();
    lbars
}

// TODO: 変数宣言の実装
pub struct Var {
    name: String,
    offset: usize,
}

// TODO: 型の実装
#[derive(Clone, Copy)]
pub enum TypeKind {
    TyInt,
}
#[derive(Clone, Copy)]
pub struct Type {
    ty: TypeKind,
    depth: u8, // ポインタなら1, ポインタのポインタなら2, ...
}
impl Type {
    fn new(ty: TypeKind) -> Self {
        Self { ty, depth: 0 }
    }
    fn is_ptr(&self) -> bool {
        self.depth > 0
    }
    fn to_ptr(&self) -> Self {
        Self {
            ty: self.ty,
            depth: self.depth + 1,
        }
    }
    fn deref(&self) -> Self {
        let mut d = self.clone();
        if d.depth == 0 {
            unimplemented!();
        } else {
            d.depth -= 1;
            d
        }
    }
}

// #[derive(Debug)]
#[derive(Clone, Copy)]
pub enum NodeKind {
    NdAdd,
    NdSub,
    NdMul,
    NdDiv,
    NdEq,
    NdNeq,
    NdLt,
    NdLe,
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
        }
    }
}
use NodeKind::*;

#[allow(dead_code)]
pub enum Node {
    // 式(expression)
    Num {
        val: u32,
    },
    Lvar {
        ty: Type,
        name: String,
        offset: usize,
    },
    Addr {
        node: Box<Node>,
    },
    Deref {
        node: Box<Node>,
    },
    Assign {
        lvar: Box<Node>,
        rhs: Box<Node>,
    },
    Bin {
        kind: NodeKind,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    FunCall {
        name: String,
        args: Box<Vec<Node>>,
    },
    // 文(statement)
    ExprStmt {
        expr: Box<Node>,
    },
    Return {
        returns: Box<Node>,
    },
    Block {
        stmts: Box<Vec<Node>>,
    },
    If {
        condi: Box<Node>,
        then_: Box<Node>,
        else_: Option<Box<Node>>,
    },
    While {
        condi: Box<Node>,
        then_: Box<Node>,
    },
    For {
        start: Option<Box<Node>>,
        condi: Option<Box<Node>>,
        end: Option<Box<Node>>,
        loop_: Box<Node>,
    },
}
impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Num { val } => write!(f, "Num {}", val),
            Node::Lvar { name, offset, .. } => write!(f, "Val {} at {}", name.clone(), offset),
            Node::Return { .. } => write!(f, "return"),
            Node::ExprStmt { .. } => write!(f, "expr stmt"),
            Node::Bin { kind, .. } => write!(f, "Bin {:?}", kind),
            Node::Assign { .. } => write!(f, "assign"),
            Node::If { .. } => write!(f, "If"),
            Node::While { .. } => write!(f, "while"),
            Node::For { .. } => write!(f, "for"),
            Node::Block { stmts } => stmts.iter().map(|s| write!(f, "{:?} ", s)).collect(),
            Node::FunCall { name, .. } => write!(f, "function {}", name),
            Node::Addr { .. } => write!(f, "address"),
            Node::Deref { .. } => write!(f, "deref"),
        }
    }
}
impl Node {
    pub fn get_type(&self) -> Type {
        match self {
            Self::Num { .. } => Type::new(TypeKind::TyInt),
            Self::Lvar { ty, .. } => *ty,
            Self::Addr { node } => node.get_type().to_ptr(),
            Self::Deref { node } => node.get_type().deref(),
            Self::Assign { lvar, .. } => lvar.get_type(),
            Self::Bin { kind, lhs, rhs } => match kind {
                NodeKind::NdSub if lhs.get_type().is_ptr() && rhs.get_type().is_ptr() => {
                    Type::new(TypeKind::TyInt)
                } // ポインタ同士の引き算はint
                _ => lhs.get_type(),
            },
            Self::FunCall { .. } => Type::new(TypeKind::TyInt), // 関数の戻り値は全部int
            _ => unimplemented!(),
        }
    }
    fn new_num(val: u32) -> Self {
        Self::Num { val }
    }
    fn new_lvar(s: String, offset: usize) -> Self {
        Self::Lvar {
            name: s,
            offset,
            ty: Type::new(TypeKind::TyInt),
        }
    }
    fn new_if(condi: Node, then_: Node, else_: Option<Node>) -> Self {
        Self::If {
            condi: Box::new(condi),
            then_: Box::new(then_),
            else_: else_.map(Box::new),
        }
    }
    fn new_return(returns: Node) -> Self {
        Self::Return {
            returns: Box::new(returns),
        }
    }
    fn new_unary(node: Node, t: &str) -> Self {
        match t {
            "return" => Self::Return {
                returns: Box::new(node),
            },
            "addr" => Self::Addr {
                node: Box::new(node),
            },
            "deref" => Self::Deref {
                node: Box::new(node),
            },
            _ => unimplemented!(),
        }
    }
    fn new_assign(lvar: Node, rhs: Node) -> Self {
        Self::Assign {
            lvar: Box::new(lvar),
            rhs: Box::new(rhs),
        }
    }
    fn new_bin(kind: NodeKind, lhs: Node, rhs: Node) -> Self {
        Self::Bin {
            kind,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    fn new_add(lhs: Node, rhs: Node) -> Self {
        match (lhs.get_type().is_ptr(), rhs.get_type().is_ptr()) {
            // 値 + 値
            (false, false) => Self::new_bin(NdAdd, lhs, rhs),
            // 値+ポインタ => ポインタ+値と見なす
            (false, true) => Self::new_add(rhs, lhs),
            // ポインタ + 値 は値だけずれたポインタを返す(型はlhsなのでポインタになる)
            (true, false) => {
                Self::new_bin(NdSub, lhs, Self::new_bin(NdMul, Node::Num { val: 8 }, rhs))
            }
            (true, true) => unimplemented!(), // ポインタ同士の足し算は意味をなさない
        }
    }
    fn new_sub(lhs: Node, rhs: Node) -> Self {
        match (lhs.get_type().is_ptr(), rhs.get_type().is_ptr()) {
            // 値 - 値
            (false, false) => Self::new_bin(NdSub, lhs, rhs),
            // ポインタ - 値
            (true, false) => {
                Self::new_bin(NdAdd, lhs, Self::new_bin(NdMul, Node::Num { val: 8 }, rhs))
            }
            // ポインタ - ポインタ (ずれを返す int)
            // 現状ではスタックはマイナス方向に伸びるので、-8で割る
            // マイナスする代わりにlhs, rhsを入れ替えた
            (true, true) => {
                Node::new_bin(NdDiv, Node::new_bin(NdSub, rhs, lhs), Node::Num { val: 8 })
            }
            // 値 - ポインタは意味をなさない
            (false, true) => unimplemented!(),
        }
    }
    fn new_while(condi: Node, then_: Node) -> Self {
        Self::While {
            condi: Box::new(condi),
            then_: Box::new(then_),
        }
    }
    fn new_for(start: Option<Node>, condi: Option<Node>, end: Option<Node>, loop_: Node) -> Self {
        Self::For {
            start: start.map(Box::new),
            condi: condi.map(Box::new),
            end: end.map(Box::new),
            loop_: Box::new(loop_),
        }
    }
}

#[derive(Default)]
pub struct Function {
    pub name: String,
    pub body: Vec<Node>,
    pub params: Vec<Var>,
    pub locals: Vec<Var>,
    pub stack_size: usize,
}
impl Function {
    fn new(name: String, body: Vec<Node>, stack_size: usize) -> Self {
        Self {
            name,
            body,
            stack_size,
            ..Self::default()
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

pub trait TokenReader {
    fn shift(&mut self) -> &mut Self;
    fn head_str(&self) -> Option<String>;
    // 見るだけ、エラーなし
    fn peek(&mut self, s: &str) -> bool;
    // 見て、存在したら消費してtrue、存在しなければfalse
    fn consume(&mut self, s: &str) -> bool;
    fn consume_return(&mut self) -> bool;
    fn consume_num(&mut self) -> Option<u32>;
    fn consume_ident(&mut self) -> Option<String>;
    fn is_eof(&self) -> bool;
    // 結果をもらい、違ったらエラーを出す
    fn expect(&mut self, s: &str) -> Result<(), ParseError>;
    fn expect_num(&mut self) -> Result<u32, ParseError>;
    fn expect_ident(&mut self) -> Result<String, ParseError>;
}
///
/// program = function*
/// function = ident "(" ")" "{" stmt* "}"
/// stmt_vec = (stmt)*
/// stmt = expr ";"  // expression statement (値を残さない)
///     | "return" expr ";"  
///     | "{" stmt* "}"
///     | "if" "(" expr ")" stmt ( "else" stmt )?
///     | "while" "(" expr ")" stmt
///     | "for" "(" expr? ";" expr? ";" expr? ")" stmt
/// expr = assign
/// assign = equality ("=" assign)?
/// equality = relational (("==" | "!=") relational)*
/// relational = add (("<" | "<=" | ">" | ">=") add)*
/// add = mul (("+" | "-") mul)*
/// mul = primary (("*" | "/") primary)*
/// primary = num | ident ("(" ")")? | "(" expr ")"
///
pub trait CodeGen {
    fn program(&mut self) -> Result<Vec<Function>, ParseError>;
    fn function(&mut self) -> Result<Function, ParseError>;
    fn compound_stmt(&mut self) -> Result<Vec<Node>, ParseError>;
    fn stmt(&mut self) -> Result<Node, ParseError>;
    fn expr(&mut self) -> Result<Node, ParseError>;
    fn assign(&mut self) -> Result<Node, ParseError>;
    fn equality(&mut self) -> Result<Node, ParseError>;
    fn relational(&mut self) -> Result<Node, ParseError>;
    fn add(&mut self) -> Result<Node, ParseError>;
    fn mul(&mut self) -> Result<Node, ParseError>;
    fn unary(&mut self) -> Result<Node, ParseError>;
    fn primary(&mut self) -> Result<Node, ParseError>;
}

impl TokenReader for VecDeque<Token> {
    fn shift(&mut self) -> &mut Self {
        self.pop_front();
        self
    }
    fn head_str(&self) -> Option<String> {
        match self[0].kind.clone() {
            TkPunct(s) => Some(s),
            _ => None,
        }
    }
    fn peek(&mut self, s: &str) -> bool {
        match &self[0].kind {
            TkPunct(_s) | TkResWord(_s) if _s == s => true,
            _ => false,
        }
    }
    /// 次がTkReserved(c) (cは指定)の場合は、1つずれてtrue, それ以外はずれずにfalse
    fn consume(&mut self, s: &str) -> bool {
        match &self[0].kind {
            TkPunct(_s) | TkResWord(_s) if _s == s => {
                self.pop_front();
                true
            }
            _ => false,
        }
    }
    fn consume_return(&mut self) -> bool {
        if self[0].kind == TkReturn {
            self.pop_front();
            return true;
        }
        false
    }
    fn consume_num(&mut self) -> Option<u32> {
        if let TkNum(val) = self[0].kind {
            self.pop_front();
            Some(val)
        } else {
            None
        }
    }
    fn consume_ident(&mut self) -> Option<String> {
        if let TkIdent(s) = self[0].kind.clone() {
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
        if !self.consume(s) {
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
        self.consume_num().ok_or(ParseError {
            pos: self[0].pos,
            msg: "expected number".to_owned(),
        })
    }
    fn expect_ident(&mut self) -> Result<String, ParseError> {
        self.consume_ident().ok_or(ParseError {
            pos: self[0].pos,
            msg: "expected identifier".to_owned(),
        })
    }
}

pub struct Parser {
    tklist: VecDeque<Token>,
    lvars: Vec<String>,
    pub varoffset: usize,
}
impl Parser {
    pub fn new(tklist: VecDeque<Token>) -> Self {
        // let lvars = local_variables(&tklist);
        // let varoffset = lvars.len() * 8;
        Self {
            tklist,
            lvars: Vec::new(),
            varoffset: 0,
        }
    }
    fn find_var(&mut self, varname: &String) -> usize {
        let offset = self
            .lvars
            .iter()
            .enumerate()
            .flat_map(|(i, s)| Some((i + 1) * 8).filter(|_| s == varname)) // s==varnameの時以外はNoneにしてしまう。
            .next();
        if let Some(offset) = offset {
            return offset;
        } else {
            self.lvars.push(varname.clone());
            return self.lvars.len() * 8;
        }
    }
}
impl TokenReader for Parser {
    fn shift(&mut self) -> &mut Self {
        self.tklist.shift();
        self
    }
    fn head_str(&self) -> Option<String> {
        self.tklist.head_str()
    }
    fn peek(&mut self, s: &str) -> bool {
        self.tklist.peek(s)
    }
    fn consume(&mut self, s: &str) -> bool {
        self.tklist.consume(s)
    }
    fn consume_return(&mut self) -> bool {
        self.tklist.consume_return()
    }
    fn consume_num(&mut self) -> Option<u32> {
        self.tklist.consume_num()
    }
    fn consume_ident(&mut self) -> Option<String> {
        self.tklist.consume_ident()
    }
    fn is_eof(&self) -> bool {
        self.tklist.is_eof()
    }
    fn expect(&mut self, s: &str) -> Result<(), ParseError> {
        self.tklist.expect(s)
    }
    fn expect_num(&mut self) -> Result<u32, ParseError> {
        self.tklist.expect_num()
    }
    fn expect_ident(&mut self) -> Result<String, ParseError> {
        self.tklist.expect_ident()
    }
}

impl CodeGen for Parser {
    // コード生成
    fn program(&mut self) -> Result<Vec<Function>, ParseError> {
        let mut code = vec![];
        while !self.is_eof() {
            code.push(self.function()?)
        }
        Ok(code)
    }
    fn function(&mut self) -> Result<Function, ParseError> {
        match self.consume_ident() {
            Some(name) => {
                self.expect("(")?;
                self.expect(")")?;
                let stmts = self.compound_stmt()?;
                let stack_size = self.lvars.len() * 8;
                self.lvars.clear();
                Ok(Function::new(name, stmts, stack_size))
            }
            _ => Err(ParseError {
                pos: 0,
                msg: "evrything has to be inside a function!".to_owned(),
            }),
        }
    }
    fn compound_stmt(&mut self) -> Result<Vec<Node>, ParseError> {
        self.expect("{")?;
        let mut stmts = vec![];
        while let Ok(stmt) = self.stmt() {
            stmts.push(stmt);
        }
        self.expect("}")?;
        Ok(stmts)
    }
    fn stmt(&mut self) -> Result<Node, ParseError> {
        let read_until = |self_: &mut Self, s: &str| {
            Ok({
                let n = self_.expr()?;
                self_.expect(s)?;
                n
            })
        };
        let node = if self.peek("{") {
            Node::Block {
                stmts: Box::new(self.compound_stmt()?),
            }
        } else if self.consume("if") {
            self.expect("(")?;
            let condi = read_until(self, ")")?;
            let then_ = self.stmt()?; // <- if trueのやつ
            let else_ = if self.consume("else") {
                Some(self.stmt()?)
            } else {
                None
            };
            Node::new_if(condi, then_, else_)
        } else if self.consume("while") {
            self.expect("(")?;
            Node::new_while(read_until(self, ")")?, self.stmt()?)
        } else if self.consume("for") {
            self.expect("(")?;
            let mut maybe_null_expr = |s: &str| {
                Ok(if self.consume(s) {
                    None
                } else {
                    let n = Some(self.expr()?);
                    self.expect(s)?;
                    n
                })
            };
            let start = maybe_null_expr(";")?;
            let condi = maybe_null_expr(";")?;
            let end = maybe_null_expr(")")?;
            let loop_ = self.stmt()?;
            Node::new_for(start, condi, end, loop_)
        } else if self.consume("return") {
            Node::new_return(read_until(self, ";")?)
        } else {
            Node::ExprStmt {
                expr: Box::new(read_until(self, ";")?),
            }
        };
        Ok(node)
    }
    fn expr(&mut self) -> Result<Node, ParseError> {
        self.assign()
    }
    fn assign(&mut self) -> Result<Node, ParseError> {
        let mut node = self.equality()?;
        // "="が見えた場合は代入文にする。
        if self.consume("=") {
            node = Node::new_assign(node, self.equality()?);
        }
        Ok(node)
    }
    fn equality(&mut self) -> Result<Node, ParseError> {
        let mut node = self.relational()?;
        loop {
            match self.head_str().as_deref() {
                Some("==") => node = Node::new_bin(NdEq, node, self.shift().relational()?),
                Some("!=") => node = Node::new_bin(NdNeq, node, self.shift().relational()?),
                _ => return Ok(node),
            }
        }
    }
    fn relational(&mut self) -> Result<Node, ParseError> {
        let mut node = self.add()?;
        loop {
            match self.head_str().as_deref() {
                Some("<") => node = Node::new_bin(NdLt, node, self.shift().add()?),
                Some("<=") => node = Node::new_bin(NdLe, node, self.shift().add()?),
                Some(">") => node = Node::new_bin(NdLt, self.shift().add()?, node),
                Some(">=") => node = Node::new_bin(NdLe, self.shift().add()?, node),
                _ => return Ok(node),
            }
        }
    }
    fn add(&mut self) -> Result<Node, ParseError> {
        let mut node = self.mul()?;
        loop {
            match self.head_str().as_deref() {
                Some("+") => node = Node::new_add(node, self.shift().mul()?),
                Some("-") => node = Node::new_sub(node, self.shift().mul()?),
                _ => return Ok(node),
            }
        }
    }
    fn mul(&mut self) -> Result<Node, ParseError> {
        let mut node = self.unary()?;
        loop {
            match self.head_str().as_deref() {
                Some("*") => node = Node::new_bin(NdMul, node, self.shift().unary()?),
                Some("/") => node = Node::new_bin(NdDiv, node, self.shift().unary()?),
                _ => return Ok(node),
            }
        }
    }
    fn unary(&mut self) -> Result<Node, ParseError> {
        match self.head_str().as_deref() {
            Some("+") => self.shift().unary(),
            Some("-") => Ok(Node::new_bin(
                NdSub,
                Node::new_num(0),
                self.shift().unary()?,
            )),
            Some("&") => Ok(Node::new_unary(self.shift().unary()?, "addr")),
            Some("*") => Ok(Node::new_unary(self.shift().unary()?, "deref")),
            _ => self.primary(),
        }
    }
    fn primary(&mut self) -> Result<Node, ParseError> {
        if self.consume("(") {
            let node = self.expr();
            self.expect(")").map(|_| node)?
        } else if let Some(val) = self.consume_num() {
            Ok(Node::new_num(val))
        } else {
            let name = self.expect_ident()?;
            // 関数呼び出し
            if self.consume("(") {
                let mut args = Vec::new();
                while !self.consume(")") {
                    args.push(self.expr()?);
                    if !self.consume(",") {
                        self.expect(")")?;
                        break;
                    }
                }
                Ok(Node::FunCall {
                    name,
                    args: Box::new(args),
                })
            } else {
                // ただの変数
                let offset = self.find_var(&name); // 探して、存在しなければpushした上でoffsetを返す。
                Ok(Node::new_lvar(name, offset))
            }
        }
    }
}
