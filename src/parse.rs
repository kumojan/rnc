use crate::r#type::*;
use crate::tokenize::*;
use std::collections::VecDeque;
use std::fmt;
use std::rc::Rc;

pub struct Var {
    pub name: String,
    pub ty: Type,
    pub id: usize,
    pub is_local: bool,
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_local {
            write!(f, "{:?} {}: {}", self.ty, self.name, self.id)
        } else {
            write!(f, "gvar {:?} {}", self.ty, self.name)
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
    Var {
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
            Node::Var { var } => write!(f, "{:?}", var),
            Node::Return { .. } => write!(f, "return"),
            Node::ExprStmt { .. } => write!(f, "expr stmt"),
            Node::Bin { kind, .. } => write!(f, "Bin {:?}", kind),
            Node::Assign { .. } => write!(f, "assign"),
            Node::If { .. } => write!(f, "If"),
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
            Self::Num { .. } => Type::TyInt,
            Self::Var { var } => var.ty.clone(),
            Self::Addr { node } => node.get_type().to_ptr(),
            Self::Deref { node } => {
                let _ty = node.get_type();
                if _ty.is_ptr_like() {
                    _ty.get_base().unwrap()
                } else {
                    panic!("invalid dereference!")
                }
            }
            Self::Assign { lvar, .. } => lvar.get_type(),
            Self::Bin { kind, lhs, rhs } => match kind {
                NodeKind::NdSub if lhs.get_type().is_ptr() && rhs.get_type().is_ptr() => {
                    Type::TyInt
                } // ポインタ同士の引き算はint
                _ => lhs.get_type(),
            },
            Self::FunCall { .. } => Type::TyInt, // 関数の戻り値は全部int
            _ => unimplemented!(),
        }
    }
    fn new_num(val: u32) -> Self {
        Self::Num { val }
    }
    fn new_lvar(var: Rc<Var>) -> Self {
        Self::Var { var }
    }
    fn new_if(condi: Node, then_: Node, else_: Option<Node>) -> Self {
        Self::If {
            condi: Box::new(condi),
            then_: Box::new(then_),
            else_: else_.map(Box::new),
        }
    }
    fn new_unary(t: &str, node: Node) -> Self {
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
            "expr_stmt" => Self::ExprStmt {
                expr: Box::new(node),
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
    fn new_expr_stmt(expr: Node) -> Self {
        Self::ExprStmt {
            expr: Box::new(expr),
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
        match (lhs.get_type().is_ptr_like(), rhs.get_type().is_ptr_like()) {
            // 値 + 値
            (false, false) => Self::new_bin(NdAdd, lhs, rhs),
            // 値+ポインタ => ポインタ+値と見なす
            (false, true) => Self::new_add(rhs, lhs),
            // ポインタ + 値 は値だけずれたポインタを返す(型はlhsなのでポインタになる)
            (true, false) => {
                let ty_size = lhs.get_type().get_base().unwrap().size() as u32;
                Self::new_bin(
                    NdAdd,
                    lhs,
                    Self::new_bin(NdMul, Node::new_num(ty_size), rhs),
                )
            }
            (true, true) => unimplemented!(), // ポインタ同士の足し算は意味をなさない
        }
    }
    fn new_sub(lhs: Node, rhs: Node) -> Self {
        match (lhs.get_type().is_ptr_like(), rhs.get_type().is_ptr_like()) {
            // 値 - 値
            (false, false) => Self::new_bin(NdSub, lhs, rhs),
            // ポインタ - 値
            (true, false) => {
                let ty_size = lhs.get_type().get_base().unwrap().size() as u32;
                Self::new_bin(
                    NdSub,
                    lhs,
                    Self::new_bin(NdMul, rhs, Node::new_num(ty_size)),
                )
            }
            // ポインタ - ポインタ (ずれを返す int)
            (true, true) => {
                let ty_size = lhs.get_type().get_base().unwrap().size() as u32;
                Node::new_bin(
                    NdDiv,
                    Node::new_bin(NdSub, lhs, rhs),
                    Node::new_num(ty_size),
                )
            }
            // 値 - ポインタは意味をなさない
            (false, true) => unimplemented!(),
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
}
impl Function {
    fn new(
        ty: Type,
        name: String,
        body: Vec<Node>,
        params: Vec<Rc<Var>>,
        locals: Vec<Rc<Var>>,
    ) -> Self {
        Self {
            ty,
            name,
            body,
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
    fn typespec(&mut self) -> Result<Type, ParseError>;
    fn ptr(&mut self, ty: Type) -> Type;
    // raise error
    fn raise_err(&self, msg: &str) -> ParseError;
}
///
/// program = (funcdef | global-var)*
/// funcdef = typespec ptr ident funcargs "{" compound_stmt "}"
/// typespec = "int"
/// ptr = "*"*
/// funcargs = "(" typespec ptr ident ( "," typespec ptr ident)? ")"
/// compound_stmt = (vardef | stmt)*
/// vardef = typespec declarator ("," declarator)*;
/// declarator = ptr ident ("[" num "]")* initializer
/// initializer = "=" (expr | array_init)
/// array_init = "{" num ("," num)?"}"
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
/// unary = ("+" | "-" | "*" | "&") unary
///       | postfix
/// postfix = primary ("[" expr "]")*
/// primary = num | ident ("(" (expr ("," expr )*)? ")")? | "(" expr ")" | "sizeof" unary
///
pub trait CodeGen {
    fn program(&mut self) -> Result<(Vec<Function>, Vec<Rc<Var>>), ParseError>;
    fn funcargs(&mut self) -> Result<(), ParseError>;
    fn funcdef(&mut self) -> Result<Function, ParseError>;
    fn funcdef2(&mut self) -> Result<Option<Function>, ParseError>;
    fn type_suffix(&mut self, ty: Type) -> Result<Type, ParseError>;
    fn vardef(&mut self) -> Result<Vec<Node>, ParseError>;
    fn initializer(&mut self) -> Result<Option<Node>, ParseError>;
    fn compound_stmt(&mut self) -> Result<Vec<Node>, ParseError>;
    fn stmt(&mut self) -> Result<Node, ParseError>;
    fn expr(&mut self) -> Result<Node, ParseError>;
    fn assign(&mut self) -> Result<Node, ParseError>;
    fn equality(&mut self) -> Result<Node, ParseError>;
    fn relational(&mut self) -> Result<Node, ParseError>;
    fn add(&mut self) -> Result<Node, ParseError>;
    fn mul(&mut self) -> Result<Node, ParseError>;
    fn unary(&mut self) -> Result<Node, ParseError>;
    fn postfix(&mut self) -> Result<Node, ParseError>;
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
    fn typespec(&mut self) -> Result<Type, ParseError> {
        if self.consume("int") {
            Ok(Type::TyInt)
        } else {
            Err(self.raise_err("typespec expected"))
        }
    }
    fn ptr(&mut self, ty: Type) -> Type {
        let mut depth = 0;
        while self.consume("*") {
            depth += 1;
        }
        ty.to_ptr_recursive(depth)
    }
    fn raise_err(&self, msg: &str) -> ParseError {
        ParseError {
            pos: self[0].pos,
            msg: msg.to_owned(),
        }
    }
}

pub struct Parser {
    tklist: VecDeque<Token>,
    locals: Vec<Rc<Var>>, // Node::Varと共有する。
    globals: Vec<Rc<Var>>,
}
impl Parser {
    fn pos(&self) -> usize {
        self.tklist[0].pos
    }
    pub fn new(tklist: VecDeque<Token>) -> Self {
        Self {
            tklist,
            locals: Vec::new(),
            globals: Vec::new(),
        }
    }
    fn find_var(&self, name: &String) -> Option<Rc<Var>> {
        self.locals
            .iter()
            .find(|v| v.name == *name)
            .or_else(|| self.globals.iter().find(|v| v.name == *name))
            .cloned()
    }
    fn add_var(&mut self, name: &String, ty: Type) -> Result<Rc<Var>, ParseError> {
        if let Some(v) = self.locals.iter().find(|v| v.name == *name) {
            if v.ty != ty {
                unimplemented!("type change of vars");
            }
            return Ok(v.clone());
        } else {
            let var = Rc::new(Var {
                name: name.clone(),
                ty,
                id: self.locals.len(),
                is_local: true,
            });
            self.locals.push(var.clone());
            return Ok(var);
        }
    }
    fn add_global(&mut self, name: &String, ty: Type) -> Result<Rc<Var>, ParseError> {
        if let Some(v) = self.globals.iter().find(|v| v.name == *name) {
            if v.ty != ty {
                unimplemented!("type change of vars");
            }
            return Ok(v.clone());
        } else {
            let var = Rc::new(Var {
                name: name.clone(),
                ty,
                id: self.globals.len(),
                is_local: false,
            });
            self.globals.push(var.clone());
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
    fn typespec(&mut self) -> Result<Type, ParseError> {
        self.tklist.typespec()
    }
    fn ptr(&mut self, ty: Type) -> Type {
        self.tklist.ptr(ty)
    }
    fn raise_err(&self, msg: &str) -> ParseError {
        self.tklist.raise_err(msg)
    }
}

impl CodeGen for Parser {
    // コード生成
    fn program(&mut self) -> Result<(Vec<Function>, Vec<Rc<Var>>), ParseError> {
        let mut code = vec![];
        while !self.is_eof() {
            if let Some(func) = self.funcdef2()? {
                code.push(func);
            }
        }
        Ok((code, std::mem::replace(&mut self.globals, vec![])))
    }
    fn funcargs(&mut self) -> Result<(), ParseError> {
        self.expect("(")?;
        if !self.consume(")") {
            loop {
                let ty = self.typespec()?;
                let ty = self.ptr(ty);
                let name = self.expect_ident()?;
                self.add_var(&name, ty)?;
                if !self.consume(",") {
                    self.expect(")")?;
                    break; // 宣言終了
                }
            }
        }
        Ok(())
    }
    fn funcdef2(&mut self) -> Result<Option<Function>, ParseError> {
        // まず int *x まで見る
        let ty = self.typespec()?;
        let mut _ty = self.ptr(ty.clone());
        let name = self.expect_ident()?;
        // 次に関数の有無を見る
        if self.peek("(") {
            // 関数の宣言
            self.funcargs()?;
            let params: Vec<_> = self.locals.iter().cloned().collect();
            let stmts = self.compound_stmt()?;
            return Ok(Some(Function::new(
                _ty,
                name,
                stmts,
                params,
                std::mem::replace(&mut self.locals, vec![]),
            )));
        }
        // 関数でないとしたら、グローバル変数が続いている
        // TODO: グローバル変数の初期化
        _ty = self.type_suffix(_ty)?;
        self.add_global(&name, _ty)?;
        if !self.consume(";") {
            loop {
                self.expect(",")?;
                let mut _ty = self.ptr(ty.clone());
                let name = self.expect_ident()?;
                _ty = self.type_suffix(_ty)?;
                self.add_global(&name, _ty)?;
                if self.consume(";") {
                    break;
                }
            }
        }
        Ok(None)
    }
    fn funcdef(&mut self) -> Result<Function, ParseError> {
        let ty = self.typespec()?;
        let return_type = self.ptr(ty);
        match self.consume_ident() {
            Some(name) => {
                self.funcargs()?;
                // この時点でのローカル変数が引数である
                let params: Vec<_> = self.locals.iter().cloned().collect();
                let stmts = self.compound_stmt()?;
                Ok(Function::new(
                    return_type,
                    name,
                    stmts,
                    params,
                    std::mem::replace(&mut self.locals, vec![]),
                ))
            }
            _ => Err(ParseError {
                pos: self.pos(),
                msg: "evrything has to be inside a function!".to_owned(),
            }),
        }
    }
    fn type_suffix(&mut self, mut ty: Type) -> Result<Type, ParseError> {
        let mut array_dims = vec![];
        while self.consume("[") {
            array_dims.push(self.expect_num()? as usize);
            self.expect("]")?;
        }
        while let Some(len) = array_dims.pop() {
            ty = ty.to_array(len);
        }
        return Ok(ty);
    }
    fn vardef(&mut self) -> Result<Vec<Node>, ParseError> {
        let ty = self.typespec()?;
        let mut stmts = vec![];
        loop {
            let mut _ty = self.ptr(ty.clone()); // "*"を数える
            let name = self.expect_ident()?;
            let mut array_dims = vec![];
            while self.consume("[") {
                array_dims.push(self.expect_num()? as usize);
                self.expect("]")?;
            }
            while let Some(len) = array_dims.pop() {
                _ty = _ty.to_array(len);
            }
            // 初期化がなければ、コードには現れないので捨てられる
            let lvar = self.add_var(&name, _ty).unwrap();
            if let Some(init) = self.initializer()? {
                stmts.push(Node::new_expr_stmt(Node::new_assign(
                    Node::new_lvar(lvar),
                    init,
                )));
            }
            if !self.consume(",") {
                self.expect(";")?; // コンマを見なかったら、セミコロンがあるはず
                break; // そして宣言終了
            }
        }
        Ok(stmts)
    }
    fn initializer(&mut self) -> Result<Option<Node>, ParseError> {
        let node = if self.consume("=") {
            if self.peek("{") {
                unimplemented!();
            } else {
                Some(self.expr()?)
            }
        } else {
            None
        };
        Ok(node)
    }

    fn compound_stmt(&mut self) -> Result<Vec<Node>, ParseError> {
        self.expect("{")?;
        let mut block = vec![];
        while !self.consume("}") {
            match self.head_str().as_deref() {
                Some("int") => block.extend(self.vardef()?),
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
            Node::new_for(None, Some(read_until(self, ")")?), None, self.stmt()?)
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
            let start = maybe_null_expr(";")?.map(Node::new_expr_stmt);
            let condi = maybe_null_expr(";")?;
            let end = maybe_null_expr(")")?.map(Node::new_expr_stmt);
            let loop_ = self.stmt()?;
            Node::new_for(start, condi, end, loop_)
        } else if self.consume("return") {
            Node::new_unary("return", read_until(self, ";")?)
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
            Some("&") => Ok(Node::new_unary("addr", self.shift().unary()?)),
            Some("*") => Ok(Node::new_unary("deref", self.shift().unary()?)),
            _ => self.postfix(),
        }
    }
    fn postfix(&mut self) -> Result<Node, ParseError> {
        let mut node = self.primary()?;
        // x[y] は *(x+y)に同じ
        while self.consume("[") {
            node = Node::new_unary("deref", Node::new_add(node, self.expr()?));
            self.expect("]")?;
        }
        Ok(node)
    }
    fn primary(&mut self) -> Result<Node, ParseError> {
        if self.consume("(") {
            let node = self.expr();
            self.expect(")").map(|_| node)?
        } else if let Some(val) = self.consume_num() {
            Ok(Node::new_num(val))
        } else if self.consume("sizeof") {
            // このnodeは型のサイズを取得するためのみに使われ、
            // 実際には評価されない
            let node = self.unary()?;
            Ok(Node::new_num(node.get_type().size() as u32))
        } else {
            let name = self.expect_ident()?;
            // 関数呼び出し
            // TODO: 呼び出しがわで関数の型を把握すべし
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
