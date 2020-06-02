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

// #[derive(Default)]
// struct Scope {
//     vars: Vec<Rc<Var>>,
// }

// #[derive(Debug)]
#[derive(Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    _Eq,
    Neq,
    Lt,
    Le,
}
impl fmt::Debug for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::_Eq => write!(f, "=="),
            BinOp::Neq => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Le => write!(f, "<="),
        }
    }
}

#[allow(dead_code)]
pub enum NodeKind {
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
        kind: BinOp,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    FunCall {
        name: String,
        args: Box<Vec<Node>>,
    },
    Literal {
        ty: Type,
        id: usize,
    },
    StmtExpr {
        stmts: Box<Node>, // blockを持たせる
        expr: Box<Node>,
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
impl fmt::Debug for NodeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeKind::Num { val } => write!(f, "Num {}", val),
            NodeKind::Var { var } => write!(f, "{:?}", var),
            NodeKind::Return { .. } => write!(f, "return"),
            NodeKind::ExprStmt { .. } => write!(f, "expr stmt"),
            NodeKind::Bin { kind, .. } => write!(f, "Bin {:?}", kind),
            NodeKind::Assign { .. } => write!(f, "assign"),
            NodeKind::If { .. } => write!(f, "If"),
            NodeKind::For { .. } => write!(f, "for"),
            // NodeKind::Block { stmts } => stmts.iter().map(|s| write!(f, " {:?} ", s)).collect(),
            NodeKind::Block { .. } => write!(f, "block"),
            NodeKind::FunCall { name, .. } => write!(f, "function {}", name),
            NodeKind::Addr { .. } => write!(f, "address"),
            NodeKind::Deref { .. } => write!(f, "deref"),
            NodeKind::Literal { ty, .. } => write!(f, "{:?} literal", ty),
            NodeKind::StmtExpr { .. } => write!(f, "stmt expr"),
        }
    }
}
pub struct Node {
    pub kind: NodeKind,
    ty: Option<Type>,
}
impl Node {
    pub fn get_type(&self) -> Type {
        match &self.ty {
            Some(ty) => ty.clone(),
            _ => unimplemented!("called get_type for statements!"),
        }
    }
    fn new_num(val: u32) -> Self {
        Self {
            kind: NodeKind::Num { val },
            ty: Some(Type::TyInt),
        }
    }
    fn new_lvar(var: Rc<Var>) -> Self {
        Self {
            ty: Some(var.ty.clone()),
            kind: NodeKind::Var { var },
        }
    }
    fn new_if(condi: Node, then_: Node, else_: Option<Node>) -> Self {
        Self {
            kind: NodeKind::If {
                condi: Box::new(condi),
                then_: Box::new(then_),
                else_: else_.map(Box::new),
            },
            ty: None,
        }
    }
    fn new_unary(t: &str, node: Node) -> Self {
        Self {
            ty: match t {
                "return" | "expr_stmt" => None,
                "addr" => node.ty.clone().map(|t| t.to_ptr()),
                "deref" => node.ty.clone().map(|t| t.get_base().unwrap()),
                _ => unimplemented!(),
            },
            kind: match t {
                "return" => NodeKind::Return {
                    returns: Box::new(node),
                },
                "addr" => NodeKind::Addr {
                    node: Box::new(node),
                },
                "deref" => NodeKind::Deref {
                    node: Box::new(node),
                },
                "expr_stmt" => NodeKind::ExprStmt {
                    expr: Box::new(node),
                },
                _ => unimplemented!(),
            },
        }
    }
    fn new_assign(lvar: Node, rhs: Node) -> Self {
        Self {
            ty: rhs.ty.clone(),
            kind: NodeKind::Assign {
                lvar: Box::new(lvar),
                rhs: Box::new(rhs),
            },
        }
    }
    fn new_expr_stmt(expr: Node) -> Self {
        Self {
            kind: NodeKind::ExprStmt {
                expr: Box::new(expr),
            },
            ty: None,
        }
    }
    fn new_bin(kind: BinOp, lhs: Node, rhs: Node) -> Self {
        Self {
            ty: match kind {
                BinOp::Sub if lhs.get_type().is_ptr() && rhs.get_type().is_ptr() => {
                    Some(Type::TyInt)
                } // ポインタ同士の引き算はint
                _ => Some(lhs.get_type()),
            },
            kind: NodeKind::Bin {
                kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        }
    }
    fn new_add(lhs: Node, rhs: Node) -> Self {
        match (lhs.get_type().is_ptr_like(), rhs.get_type().is_ptr_like()) {
            // 値 + 値
            (false, false) => Node::new_bin(BinOp::Add, lhs, rhs),
            // 値+ポインタ => ポインタ+値と見なす
            (false, true) => Node::new_add(rhs, lhs),
            // ポインタ + 値 は値だけずれたポインタを返す(型はlhsなのでポインタになる)
            (true, false) => {
                let ty_size = lhs.get_type().get_base().unwrap().size() as u32;
                Node::new_bin(
                    BinOp::Add,
                    lhs,
                    Node::new_bin(BinOp::Mul, Node::new_num(ty_size), rhs),
                )
            }
            (true, true) => unimplemented!(), // ポインタ同士の足し算は意味をなさない
        }
    }
    fn new_sub(lhs: Node, rhs: Node) -> Self {
        match (lhs.get_type().is_ptr_like(), rhs.get_type().is_ptr_like()) {
            // 値 - 値
            (false, false) => Node::new_bin(BinOp::Sub, lhs, rhs),
            // ポインタ - 値
            (true, false) => {
                let ty_size = lhs.get_type().get_base().unwrap().size() as u32;
                Node::new_bin(
                    BinOp::Sub,
                    lhs,
                    Node::new_bin(BinOp::Mul, rhs, Node::new_num(ty_size)),
                )
            }
            // ポインタ - ポインタ (ずれを返す int)
            (true, true) => {
                let ty_size = lhs.get_type().get_base().unwrap().size() as u32;
                Node::new_bin(
                    BinOp::Div,
                    Node::new_bin(BinOp::Sub, lhs, rhs),
                    Node::new_num(ty_size),
                )
            }
            // 値 - ポインタは意味をなさない
            (false, true) => unimplemented!(),
        }
    }
    fn new_for(start: Option<Node>, condi: Option<Node>, end: Option<Node>, loop_: Node) -> Self {
        Node {
            kind: NodeKind::For {
                start: start.map(Box::new),
                condi: condi.map(Box::new),
                end: end.map(Box::new),
                loop_: Box::new(loop_),
            },
            ty: None,
        }
    }
    fn new_block(stmts: Vec<Node>) -> Self {
        Self {
            kind: NodeKind::Block {
                stmts: Box::new(stmts),
            },
            ty: None,
        }
    }
    fn new_stmt_expr(stmts: Vec<Node>, expr: Node) -> Self {
        Node {
            ty: Some(expr.get_type()),
            kind: NodeKind::StmtExpr {
                stmts: Box::new(Node::new_block(stmts)),
                expr: Box::new(expr),
            },
        }
    }
    fn new_funcall(name: String, args: Vec<Node>) -> Self {
        Self {
            kind: NodeKind::FunCall {
                name,
                args: Box::new(args),
            },
            ty: Some(Type::TyInt),
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
    // 見るだけ、進まず、エラーなし
    fn peek(&mut self, s: &str) -> bool;
    fn peek_reserved(&self) -> Option<String>;
    // 見て、存在したら消費してtrue、存在しなければfalse
    fn consume(&mut self, s: &str) -> bool;
    fn consume_num(&mut self) -> Option<u32>;
    fn consume_string(&mut self) -> Option<Vec<u8>>;
    fn consume_ident(&mut self) -> Option<String>;
    fn consume_reserved(&mut self) -> Option<String>;
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
/// primary = num
///     | str
///     | ident ("(" (expr ("," expr )*)? ")")?
///     | "(" expr ")"
///     | "sizeof" unary
///     | "(" "{" stmt* expr ";" "}" ")"
///
pub trait CodeGen {
    fn program(self) -> Result<(Vec<Function>, Vec<Rc<Var>>, Vec<Vec<u8>>), ParseError>;
    fn funcargs(&mut self) -> Result<(), ParseError>;
    fn funcdef(&mut self) -> Result<Option<Function>, ParseError>;
    fn type_suffix(&mut self, ty: Type) -> Result<Type, ParseError>;
    fn vardef(&mut self) -> Result<Vec<Node>, ParseError>;
    fn initializer(&mut self) -> Result<Option<Node>, ParseError>;
    fn compound_stmt(&mut self, new_scope: bool) -> Result<Vec<Node>, ParseError>;
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
    fn peek(&mut self, s: &str) -> bool {
        match &self[0].kind {
            TokenKind::TkReserved(_s) if _s == s => true,
            _ => false,
        }
    }
    fn peek_reserved(&self) -> Option<String> {
        match self[0].kind.clone() {
            TokenKind::TkReserved(s) => Some(s),
            _ => None,
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
    fn consume_num(&mut self) -> Option<u32> {
        if let TokenKind::TkNum(val) = self[0].kind {
            self.pop_front();
            Some(val)
        } else {
            None
        }
    }
    fn consume_string(&mut self) -> Option<Vec<u8>> {
        if let TokenKind::TkString(ref s) = self[0].kind {
            let s = s.clone();
            self.pop_front();
            Some(s)
        } else {
            None
        }
    }
    fn consume_ident(&mut self) -> Option<String> {
        if let TokenKind::TkIdent(ref s) = self[0].kind {
            let s = s.clone();
            self.pop_front();
            Some(s)
        } else {
            None
        }
    }
    fn consume_reserved(&mut self) -> Option<String> {
        if let TokenKind::TkReserved(ref s) = self[0].kind {
            let s = s.clone();
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
        match self.consume_reserved().as_deref() {
            Some("int") => Ok(Type::TyInt),
            Some("char") => Ok(Type::TyChar),
            _ => Err(self.raise_err("expected type")),
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
    string_literals: Vec<Vec<u8>>,
    scope_stack: VecDeque<VecDeque<Rc<Var>>>, // 前に内側のスコープがある
}
impl Parser {
    pub fn new(tklist: VecDeque<Token>) -> Self {
        Self {
            tklist,
            locals: Vec::new(),
            globals: Vec::new(),
            string_literals: Vec::new(),
            scope_stack: Default::default(),
        }
    }
    /// 必ずleave_scopeと対にして使うこと
    fn enter_scope(&mut self) {
        self.scope_stack.push_front(Default::default());
    }
    /// 必ずenter_scopeと対にして使うこと
    fn leave_scope(&mut self) {
        self.scope_stack.pop_front();
    }
    fn find_var(&self, name: &String) -> Option<Rc<Var>> {
        // println!("searching {} in {:?}", name, self.scope_stack);
        self.scope_stack
            .iter()
            .flat_map(|scope| scope.iter().find(|v| &v.name == name))
            .next()
            .or_else(|| self.globals.iter().find(|v| v.name == *name))
            .cloned()
    }
    fn add_var(&mut self, name: &String, ty: Type) -> Result<Rc<Var>, ParseError> {
        let var = Rc::new(Var {
            name: name.clone(),
            ty,
            id: self.locals.len(),
            is_local: true,
        });
        self.locals.push(var.clone());
        self.scope_stack[0].push_front(var.clone()); // scopeにも追加
        return Ok(var);
    }
    // グローバル変数は重複して宣言できない
    fn add_global(&mut self, name: &String, ty: Type) -> Result<Rc<Var>, ParseError> {
        if self.globals.iter().find(|v| v.name == *name).is_some() {
            Err(self.raise_err("global var redefined!"))
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
    fn add_string_literal(&mut self, data: Vec<u8>) -> Node {
        let n = Node {
            kind: NodeKind::Literal {
                ty: Type::TyChar.to_array(data.len() + 1), // string末尾の'\0'も大きさに含める
                id: self.string_literals.len(),
            },
            ty: Some(Type::TyChar.to_array(data.len() + 1)),
        };
        self.string_literals.push(data);
        n
    }
}
impl TokenReader for Parser {
    fn shift(&mut self) -> &mut Self {
        self.tklist.shift();
        self
    }
    fn peek_reserved(&self) -> Option<String> {
        self.tklist.peek_reserved()
    }
    fn peek(&mut self, s: &str) -> bool {
        self.tklist.peek(s)
    }
    fn consume(&mut self, s: &str) -> bool {
        self.tklist.consume(s)
    }
    fn consume_num(&mut self) -> Option<u32> {
        self.tklist.consume_num()
    }
    fn consume_ident(&mut self) -> Option<String> {
        self.tklist.consume_ident()
    }
    fn consume_string(&mut self) -> Option<Vec<u8>> {
        self.tklist.consume_string()
    }
    fn consume_reserved(&mut self) -> Option<String> {
        self.tklist.consume_reserved()
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
    fn program(mut self) -> Result<(Vec<Function>, Vec<Rc<Var>>, Vec<Vec<u8>>), ParseError> {
        let mut code = vec![];
        while !self.is_eof() {
            if let Some(func) = self.funcdef()? {
                code.push(func);
            }
        }
        Ok((code, self.globals, self.string_literals))
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
    fn funcdef(&mut self) -> Result<Option<Function>, ParseError> {
        // まず int *x まで見る
        let ty = self.typespec()?;
        let mut _ty = self.ptr(ty.clone());
        let name = self.expect_ident()?;
        // 次に関数の有無を見る
        if self.peek("(") {
            // 関数の宣言
            self.enter_scope();
            self.funcargs()?;
            let params: Vec<_> = self.locals.iter().cloned().collect();
            let stmts = self.compound_stmt(false)?;
            self.leave_scope();
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
                unimplemented!("array initiation not implemented");
            } else {
                Some(self.expr()?)
            }
        } else {
            None
        };
        Ok(node)
    }

    fn compound_stmt(&mut self, enter_scope: bool) -> Result<Vec<Node>, ParseError> {
        self.expect("{")?;
        if enter_scope {
            self.enter_scope();
        }
        let mut block = vec![];
        while !self.consume("}") {
            match self.peek_reserved().as_deref() {
                Some("int") | Some("char") => block.extend(self.vardef()?),
                _ => block.push(self.stmt()?),
            };
        }
        if enter_scope {
            self.leave_scope();
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
            Node::new_block(self.compound_stmt(true)?)
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
            Node::new_expr_stmt(read_until(self, ";")?)
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
            match self.peek_reserved().as_deref() {
                Some("==") => node = Node::new_bin(BinOp::_Eq, node, self.shift().relational()?),
                Some("!=") => node = Node::new_bin(BinOp::Neq, node, self.shift().relational()?),
                _ => return Ok(node),
            }
        }
    }
    fn relational(&mut self) -> Result<Node, ParseError> {
        let mut node = self.add()?;
        loop {
            match self.peek_reserved().as_deref() {
                Some("<") => node = Node::new_bin(BinOp::Lt, node, self.shift().add()?),
                Some("<=") => node = Node::new_bin(BinOp::Le, node, self.shift().add()?),
                Some(">") => node = Node::new_bin(BinOp::Lt, self.shift().add()?, node),
                Some(">=") => node = Node::new_bin(BinOp::Le, self.shift().add()?, node),
                _ => return Ok(node),
            }
        }
    }
    fn add(&mut self) -> Result<Node, ParseError> {
        let mut node = self.mul()?;
        loop {
            match self.peek_reserved().as_deref() {
                Some("+") => node = Node::new_add(node, self.shift().mul()?),
                Some("-") => node = Node::new_sub(node, self.shift().mul()?),
                _ => return Ok(node),
            }
        }
    }
    fn mul(&mut self) -> Result<Node, ParseError> {
        let mut node = self.unary()?;
        loop {
            match self.peek_reserved().as_deref() {
                Some("*") => node = Node::new_bin(BinOp::Mul, node, self.shift().unary()?),
                Some("/") => node = Node::new_bin(BinOp::Div, node, self.shift().unary()?),
                _ => return Ok(node),
            }
        }
    }
    fn unary(&mut self) -> Result<Node, ParseError> {
        match self.peek_reserved().as_deref() {
            Some("+") => self.shift().unary(),
            Some("-") => Ok(Node::new_bin(
                BinOp::Sub,
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
            if self.peek("{") {
                let mut stmts = self.compound_stmt(true)?;
                self.expect(")")?;
                // 最後はexpression statementでないといけない
                if let Some(NodeKind::ExprStmt { expr }) = stmts.pop().map(|n| n.kind) {
                    Ok(Node::new_stmt_expr(stmts, *expr))
                } else {
                    Err(self.raise_err("statement expression returning void is not supported"))
                }
            } else {
                let node = self.expr();
                self.expect(")").map(|_| node)?
            }
        } else if let Some(val) = self.consume_num() {
            Ok(Node::new_num(val))
        } else if let Some(s) = self.consume_string() {
            Ok(self.add_string_literal(s))
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
                Ok(Node::new_funcall(name, args))
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
