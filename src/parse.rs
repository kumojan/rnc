use crate::r#type::*;
use crate::tokenize::*;
use std::collections::VecDeque;
use std::fmt;
use std::rc::Rc;

const FUNC_STACK_SIZE: usize = 32;

pub struct Var {
    name: String,
    ty: Type,
    pub offset: usize,
}

// impl Var {
//     fn new(name: String, ty: Type) {}
// }
impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {}", self.ty, self.name)
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
        var: Rc<Var>,
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
            Node::Lvar { var } => write!(f, "{:?}", var),
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
            Self::Lvar { var } => var.ty,
            Self::Addr { node } => node.get_type().to_type(),
            Self::Deref { node } => {
                if node.get_type().is_ptr() {
                    node.get_type().deref()
                } else {
                    panic!("invalid dereference!")
                }
            }
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
    fn new_lvar(var: Rc<Var>) -> Self {
        Self::Lvar { var }
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

pub struct Function {
    pub ty: Type,
    pub name: String,
    pub body: Vec<Node>,
    pub params: Vec<Rc<Var>>,
    pub locals: Vec<Rc<Var>>,
    pub stack_size: usize,
}
impl Function {
    fn new(
        ty: Type,
        name: String,
        body: Vec<Node>,
        stack_size: usize,
        params: Vec<Rc<Var>>,
        locals: Vec<Rc<Var>>,
    ) -> Self {
        Self {
            ty,
            name,
            body,
            stack_size,
            params,
            locals,
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
    // 型を取得する
    fn typespec(&mut self) -> Option<TypeKind>;
}
///
/// program = function*
/// function = ident "(" ")" "{" compound_stmt "}"
/// compound_stmt = (declaration | stmt)*
/// declarator = "*"* ident
/// declaration = typespec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
/// stmt = expr ";"  // expression statement (値を残さない)
///     | "return" expr ";"  
///     | "{" compound_stmt "}"
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
    fn decleration(&mut self) -> Result<Vec<Node>, ParseError>;
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
            TokenKind::TkReserved(s) => Some(s),
            _ => None,
        }
    }
    fn peek(&mut self, s: &str) -> bool {
        match &self[0].kind {
            TokenKind::TkReserved(_s) if _s == s => true,
            _ => false,
        }
    }
    /// 次がTkReserved(c) (cは指定)の場合は、1つずれてtrue, それ以外はずれずにfalse
    fn consume(&mut self, s: &str) -> bool {
        match &self[0].kind {
            TokenKind::TkReserved(_s) if _s == s => {
                self.pop_front();
                true
            }
            _ => false,
        }
    }
    fn consume_return(&mut self) -> bool {
        if self[0].kind == TokenKind::TkReturn {
            self.pop_front();
            return true;
        }
        false
    }
    fn consume_num(&mut self) -> Option<u32> {
        if let TokenKind::TkNum(val) = self[0].kind {
            self.pop_front();
            Some(val)
        } else {
            None
        }
    }
    fn consume_ident(&mut self) -> Option<String> {
        if let TokenKind::TkIdent(s) = self[0].kind.clone() {
            self.pop_front();
            Some(s)
        } else {
            None
        }
    }
    fn is_eof(&self) -> bool {
        self[0].kind == TokenKind::TkEOF
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
    fn typespec(&mut self) -> Option<TypeKind> {
        if self.consume("int") {
            Some(TypeKind::TyInt)
        } else {
            None
        }
    }
}

pub struct Parser {
    tklist: VecDeque<Token>,
    lvars: Vec<Rc<Var>>, // Node::Lvarと共有する。
}
impl Parser {
    fn pos(&self) -> usize {
        self.tklist[0].pos
    }
    fn offset(&self) -> usize {
        self.lvars
            .last()
            .map(|v| v.offset)
            .unwrap_or(FUNC_STACK_SIZE) // 非揮発性レジスタのためにまず32ビット確保する
    }
    pub fn new(tklist: VecDeque<Token>) -> Self {
        Self {
            tklist,
            lvars: Vec::new(),
        }
    }
    fn find_var(&self, name: &String) -> Option<Rc<Var>> {
        self.lvars.iter().find(|v| v.name == *name).cloned()
    }
    fn add_var(&mut self, name: &String, ty: Type) -> Result<Rc<Var>, ParseError> {
        if let Some(v) = self.lvars.iter().find(|v| v.name == *name) {
            if v.ty.kind != ty.kind {
                unimplemented!("type change of vars");
            }
            return Ok(v.clone());
        } else {
            let offset = self.offset() + ty.size();
            let var = Rc::new(Var {
                name: name.clone(),
                ty,
                offset,
            });
            self.lvars.push(var.clone());
            return Ok(var);
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
    fn typespec(&mut self) -> Option<TypeKind> {
        self.tklist.typespec()
    }
    // fn declarator(&mut self) -> Option<
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
        let return_type = match self.typespec() {
            Some(ty) => {
                let mut depth = 0;
                while self.consume("*") {
                    depth += 1;
                }
                ty.to_type(depth)
            }
            _ => {
                return Err(ParseError {
                    pos: self.pos(),
                    msg: "return type for function needed!".to_owned(),
                })
            }
        };
        match self.consume_ident() {
            Some(name) => {
                self.expect("(")?;
                // let mut args = vec![];
                // 関数の引数
                while let Some(ty) = self.typespec() {
                    let mut depth = 0;
                    while self.consume("*") {
                        depth += 1;
                    }
                    let name = self.expect_ident()?;
                    self.add_var(&name, ty.to_type(depth))?;
                    if !self.consume(",") {
                        break; // 宣言終了
                    }
                }
                self.expect(")")?;
                // この時点でのローカル変数が引数である
                let params: Vec<_> = self.lvars.iter().cloned().collect();
                let stmts = self.compound_stmt()?;
                Ok(Function::new(
                    return_type,
                    name,
                    stmts,
                    self.offset(),
                    params,
                    std::mem::replace(&mut self.lvars, vec![]),
                ))
            }
            _ => Err(ParseError {
                pos: self.pos(),
                msg: "evrything has to be inside a function!".to_owned(),
            }),
        }
    }
    fn decleration(&mut self) -> Result<Vec<Node>, ParseError> {
        if let Some(ty) = self.typespec() {
            let mut stmts = vec![];
            loop {
                let mut depth = 0;
                while self.consume("*") {
                    depth += 1;
                }
                let name = self.expect_ident()?;
                // 初期化がなければ、コードには現れないので捨てられる
                let lvar = Node::new_lvar(self.add_var(&name, ty.to_type(depth)).unwrap());
                if self.consume("=") {
                    stmts.push(Node::new_assign(lvar, self.expr()?));
                }
                if !self.consume(",") {
                    self.expect(";")?; // コンマを見なかったら、セミコロンがあるはず
                    break; // そして宣言終了
                }
            }
            Ok(stmts)
        } else {
            Ok(vec![])
        }
    }

    fn compound_stmt(&mut self) -> Result<Vec<Node>, ParseError> {
        self.expect("{")?;
        let mut block = vec![];
        while !self.consume("}") {
            match self.head_str().as_deref() {
                Some("int") => block.extend(self.decleration()?),
                _ => block.push(self.stmt()?), // 途中に中括弧閉じがあっても、ここで消費されるはず
            };
        }
        Ok(block)
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
            node = Node::new_assign(node, self.assign()?);
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
                match self.find_var(&name) {
                    Some(var) => Ok(Node::new_lvar(var)),
                    _ => Err(ParseError {
                        pos: 0,
                        msg: "variable not found!".to_owned(),
                    }),
                }
            }
        }
    }
}
