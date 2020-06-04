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
    Comma {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Member {
        obj: Box<Node>,
        mem: Member,
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
            NodeKind::Var { var } => write!(f, "Var {:?}", var),
            NodeKind::Return { .. } => write!(f, "return"),
            NodeKind::ExprStmt { .. } => write!(f, "expr stmt"),
            NodeKind::Bin { kind, .. } => write!(f, "Bin {:?}", kind),
            NodeKind::Assign { .. } => write!(f, "assign"),
            NodeKind::If { .. } => write!(f, "If"),
            NodeKind::For { .. } => write!(f, "for"),
            NodeKind::Block { .. } => write!(f, "block"),
            NodeKind::FunCall { name, .. } => write!(f, "function {}", name),
            NodeKind::Addr { .. } => write!(f, "address"),
            NodeKind::Deref { .. } => write!(f, "deref"),
            NodeKind::Literal { ty, .. } => write!(f, "{:?} literal", ty),
            NodeKind::StmtExpr { .. } => write!(f, "stmt expr"),
            NodeKind::Member { obj, .. } => write!(f, "mem of {:?}", obj.kind),
            NodeKind::Comma { .. } => write!(f, "comma"),
        }
    }
}
pub struct Node {
    pub kind: NodeKind,
    ty: Option<Type>,
    pub tok: Option<Token>,
}
impl Node {
    pub fn get_type(&self) -> Type {
        match &self.ty {
            Some(ty) => ty.clone(),
            _ => unimplemented!("called get_type for statements!"),
        }
    }
    fn new_num(val: u32, tok: Option<Token>) -> Self {
        Self {
            kind: NodeKind::Num { val },
            ty: Some(Type::TyInt),
            tok,
        }
    }
    fn new_lvar(var: Rc<Var>, tok: Option<Token>) -> Self {
        Self {
            ty: Some(var.ty.clone()),
            kind: NodeKind::Var { var },
            tok,
        }
    }
    fn new_if(condi: Node, then_: Node, else_: Option<Node>, tok: Option<Token>) -> Self {
        Self {
            kind: NodeKind::If {
                condi: Box::new(condi),
                then_: Box::new(then_),
                else_: else_.map(Box::new),
            },
            ty: None,
            tok,
        }
    }
    fn new_unary(t: &str, node: Node, tok: Option<Token>) -> Self {
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
            tok,
        }
    }
    fn new_member_access(obj: Node, member: Member, tok: Option<Token>) -> Self {
        Self {
            ty: Some(member.ty.clone()),
            kind: NodeKind::Member {
                obj: Box::new(obj),
                mem: member,
            },
            tok,
        }
    }
    fn new_assign(lvar: Node, rhs: Node, tok: Option<Token>) -> Self {
        Self {
            ty: rhs.ty.clone(),
            kind: NodeKind::Assign {
                lvar: Box::new(lvar),
                rhs: Box::new(rhs),
            },
            tok,
        }
    }
    fn new_comma(lhs: Node, rhs: Node, tok: Option<Token>) -> Self {
        Self {
            ty: rhs.ty.clone(),
            kind: NodeKind::Comma {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            tok,
        }
    }
    fn new_expr_stmt(expr: Node, tok: Option<Token>) -> Self {
        Self {
            kind: NodeKind::ExprStmt {
                expr: Box::new(expr),
            },
            ty: None,
            tok,
        }
    }
    fn new_bin(kind: BinOp, lhs: Node, rhs: Node, tok: Option<Token>) -> Self {
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
            tok,
        }
    }
    fn new_add(lhs: Node, rhs: Node, tok: Option<Token>) -> Self {
        match (lhs.get_type().is_ptr_like(), rhs.get_type().is_ptr_like()) {
            // 値 + 値
            (false, false) => Node::new_bin(BinOp::Add, lhs, rhs, tok),
            // 値+ポインタ => ポインタ+値と見なす
            (false, true) => Node::new_add(rhs, lhs, tok),
            // ポインタ + 値 は値だけずれたポインタを返す(型はlhsなのでポインタになる)
            (true, false) => {
                let ty_size = lhs.get_type().get_base().unwrap().size() as u32;
                Node::new_bin(
                    BinOp::Add,
                    lhs,
                    Node::new_bin(BinOp::Mul, Node::new_num(ty_size, None), rhs, None),
                    tok,
                )
            }
            (true, true) => unimplemented!(), // ポインタ同士の足し算は意味をなさない
        }
    }
    fn new_sub(lhs: Node, rhs: Node, tok: Option<Token>) -> Self {
        match (lhs.get_type().is_ptr_like(), rhs.get_type().is_ptr_like()) {
            // 値 - 値
            (false, false) => Node::new_bin(BinOp::Sub, lhs, rhs, tok),
            // ポインタ - 値
            (true, false) => {
                let ty_size = lhs.get_type().get_base().unwrap().size() as u32;
                Node::new_bin(
                    BinOp::Sub,
                    lhs,
                    Node::new_bin(BinOp::Mul, rhs, Node::new_num(ty_size, None), None),
                    tok,
                )
            }
            // ポインタ - ポインタ (ずれを返す int)
            (true, true) => {
                let ty_size = lhs.get_type().get_base().unwrap().size() as u32;
                Node::new_bin(
                    BinOp::Div,
                    Node::new_bin(BinOp::Sub, lhs, rhs, None),
                    Node::new_num(ty_size, None),
                    tok,
                )
            }
            // 値 - ポインタは意味をなさない
            (false, true) => unimplemented!(),
        }
    }
    fn new_for(
        start: Option<Node>,
        condi: Option<Node>,
        end: Option<Node>,
        loop_: Node,
        tok: Option<Token>,
    ) -> Self {
        Node {
            kind: NodeKind::For {
                start: start.map(Box::new),
                condi: condi.map(Box::new),
                end: end.map(Box::new),
                loop_: Box::new(loop_),
            },
            ty: None,
            tok,
        }
    }
    fn new_block(stmts: Vec<Node>, tok: Option<Token>) -> Self {
        Self {
            kind: NodeKind::Block {
                stmts: Box::new(stmts),
            },
            ty: None,
            tok,
        }
    }
    fn new_stmt_expr(stmts: Vec<Node>, expr: Node, tok: Option<Token>) -> Self {
        Node {
            ty: Some(expr.get_type()),
            kind: NodeKind::StmtExpr {
                stmts: Box::new(Node::new_block(stmts, None)),
                expr: Box::new(expr),
            },
            tok,
        }
    }
    fn new_funcall(name: String, args: Vec<Node>, tok: Option<Token>) -> Self {
        Self {
            kind: NodeKind::FunCall {
                name,
                args: Box::new(args),
            },
            ty: Some(Type::TyInt),
            tok,
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
/// typespec = "int" | "char" | struct_decl
/// ptr = "*"*
/// funcargs = "(" typespec ptr ident ( "," typespec ptr ident)? ")"
/// compound_stmt = (vardef | stmt)*
/// vardef = typespec declarator initializer ("," declarator initializer)*;
/// declarator = ptr ident ("[" num "]")*
/// initializer = "=" (assign | array_init)
/// array_init = "{" num ("," num)?"}"
/// stmt = expr ";"  // expression statement (値を残さない)
///     | "return" expr ";"  
///     | "{" compound_stmt "}"
///     | "if" "(" expr ")" stmt ( "else" stmt )?
///     | "while" "(" expr ")" stmt
///     | "for" "(" expr? ";" expr? ";" expr? ")" stmt
/// expr = assign ("," expr )?   // exprはassignをコンマで連結している
/// assign = equality ("=" assign)?  // assignでは括弧()の中以外ではコンマは出てこない
/// equality = relational (("==" | "!=") relational)*
/// relational = add (("<" | "<=" | ">" | ">=") add)*
/// add = mul (("+" | "-") mul)*
/// mul = primary (("*" | "/") primary)*
/// unary = ("+" | "-" | "*" | "&") unary
///       | postfix
/// postfix = primary ("[" expr "]" | "." ident )*
/// primary = num
///     | str
///     | ident ("(" (assign ("," assign )*)? ")")?  // 関数の引数はassignにした。(コンマ演算子との兼ね合い)
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
    fn declarator(&mut self, ty: Type) -> Result<(String, Type), ParseError>;
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
pub struct Parser {
    tklist: VecDeque<Token>,
    locals: Vec<Rc<Var>>, // Node::Varと共有する。
    globals: Vec<Rc<Var>>,
    string_literals: Vec<Vec<u8>>,
    scope_stack: VecDeque<VecDeque<Rc<Var>>>, // 前に内側のスコープがある
    cur_tok: Option<Token>,
}

impl TokenReader for Parser {
    /// トークンを一つ読んで、すすむ
    /// 読んだトークンはcur_tokにいれる
    fn shift(&mut self) -> &mut Self {
        self.cur_tok = self.tklist.pop_front();
        self
    }
    fn peek(&mut self, s: &str) -> bool {
        match self.head_kind() {
            TokenKind::TkReserved(_s) if _s == s => true,
            _ => false,
        }
    }
    fn peek_reserved(&self) -> Option<String> {
        match self.head_kind() {
            TokenKind::TkReserved(s) => Some(s.clone()),
            _ => None,
        }
    }
    /// 次がTkReserved(c) (cは指定)の場合は、1つずれてtrue, それ以外はずれずにfalse
    fn consume(&mut self, s: &str) -> bool {
        match self.head_kind() {
            TokenKind::TkReserved(_s) if _s == s => {
                self.shift();
                true
            }
            _ => false,
        }
    }
    fn consume_num(&mut self) -> Option<u32> {
        if let TokenKind::TkNum(ref val) = self.head_kind() {
            let val = *val;
            self.shift();
            Some(val)
        } else {
            None
        }
    }
    fn consume_string(&mut self) -> Option<Vec<u8>> {
        if let TokenKind::TkString(ref s) = self.head_kind() {
            let s = s.clone();
            self.shift();
            Some(s)
        } else {
            None
        }
    }
    fn consume_ident(&mut self) -> Option<String> {
        if let TokenKind::TkIdent(ref s) = self.head_kind() {
            let s = s.clone();
            self.shift();
            Some(s)
        } else {
            None
        }
    }
    fn consume_reserved(&mut self) -> Option<String> {
        if let TokenKind::TkReserved(ref s) = self.head_kind() {
            let s = s.clone();
            self.shift();
            Some(s)
        } else {
            None
        }
    }
    fn is_eof(&self) -> bool {
        self.head_kind() == &TokenKind::TkEOF
    }
    fn expect(&mut self, s: &str) -> Result<(), ParseError> {
        if !self.consume(s) {
            Err(self.raise_err(&format!("expected \'{}\'", s)))
        } else {
            Ok(())
        }
    }
    fn expect_num(&mut self) -> Result<u32, ParseError> {
        self.consume_num().ok_or(self.raise_err("expected number"))
    }
    fn expect_ident(&mut self) -> Result<String, ParseError> {
        self.consume_ident()
            .ok_or(self.raise_err("expected identifier"))
    }
    fn typespec(&mut self) -> Result<Type, ParseError> {
        match self.consume_reserved().as_deref() {
            Some("int") => Ok(Type::TyInt),
            Some("char") => Ok(Type::TyChar),
            Some("struct") => {
                self.expect("{")?;
                let mut mems = Vec::new();
                let mut offset = 0;
                loop {
                    let ty = self.typespec()?;
                    loop {
                        let (name, ty) = self.declarator(ty.clone())?;
                        let size = ty.size();
                        mems.push(Member { ty, name, offset });
                        offset += size;
                        if !self.consume(",") {
                            break;
                        }
                    }
                    self.expect(";")?;
                    if self.consume("}") {
                        break;
                    }
                }
                Ok(Type::TyStruct {
                    name: "no name".to_owned(),
                    mem: Box::new(mems),
                })
            }
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
            pos: self.tklist[0].pos,
            msg: msg.to_owned(),
        }
    }
}
impl Parser {
    pub fn new(tklist: VecDeque<Token>) -> Self {
        Self {
            tklist,
            locals: Vec::new(),
            globals: Vec::new(),
            string_literals: Vec::new(),
            scope_stack: Default::default(),
            cur_tok: None,
        }
    }
    fn head_kind(&self) -> &TokenKind {
        &self.tklist[0].kind
    }
    /// shiftによってtokenが格納されているときは
    /// それを出して、Noneに置き換える
    /// あるいはそのままNoneが取り出される
    /// tokenは一つのNodeにしか渡らない
    fn tok(&mut self) -> Option<Token> {
        std::mem::replace(&mut self.cur_tok, None)
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
    fn add_var(&mut self, name: &String, ty: Type) -> Rc<Var> {
        let var = Rc::new(Var {
            name: name.clone(),
            ty,
            id: self.locals.len(),
            is_local: true,
        });
        self.locals.push(var.clone());
        self.scope_stack[0].push_front(var.clone()); // scopeにも追加
        var
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
            tok: self.tok(),
        };
        self.string_literals.push(data);
        n
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
                self.add_var(&name, ty);
                if !self.consume(",") {
                    break; // 宣言終了
                }
            }
            self.expect(")")?;
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
                let (name, _ty) = self.declarator(ty.clone())?;
                self.add_global(&name, _ty)?;
                if self.consume(";") {
                    break;
                }
            }
        }
        Ok(None)
    }
    fn declarator(&mut self, mut ty: Type) -> Result<(String, Type), ParseError> {
        ty = self.ptr(ty);
        let name = self.expect_ident()?;
        ty = self.type_suffix(ty)?;
        Ok((name, ty))
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
            let (name, _ty) = self.declarator(ty.clone())?;
            // 初期化がなければ、コードには現れないので捨てられる
            let var = self.add_var(&name, _ty);
            if let Some(init) = self.initializer()? {
                stmts.push(Node::new_expr_stmt(
                    Node::new_assign(Node::new_lvar(var, self.tok()), init, self.tok()),
                    self.tok(),
                ));
            }
            if !self.consume(",") {
                break; // 宣言終了
            }
        }
        self.expect(";")?;
        Ok(stmts)
    }
    fn initializer(&mut self) -> Result<Option<Node>, ParseError> {
        let node = if self.consume("=") {
            if self.peek("{") {
                unimplemented!("array initiation not implemented");
            } else {
                Some(self.assign()?)
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
                Some("int") | Some("char") | Some("struct") => block.extend(self.vardef()?),
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
            Node::new_block(self.compound_stmt(true)?, self.tok())
        } else if self.consume("if") {
            self.expect("(")?;
            let condi = read_until(self, ")")?;
            let then_ = self.stmt()?; // <- if trueのやつ
            let else_ = if self.consume("else") {
                Some(self.stmt()?)
            } else {
                None
            };
            Node::new_if(condi, then_, else_, self.tok())
        } else if self.consume("while") {
            self.expect("(")?;
            Node::new_for(
                None,
                Some(read_until(self, ")")?),
                None,
                self.stmt()?,
                self.tok(),
            )
        } else if self.consume("for") {
            self.expect("(")?;
            let mut maybe_null_expr = |s: &str| {
                Ok(if self.consume(s) {
                    None
                } else {
                    let n = Some((self.expr()?, self.tok()));
                    self.expect(s)?;
                    n
                })
            };
            let start = maybe_null_expr(";")?.map(|(e, t)| Node::new_expr_stmt(e, t));
            let condi = maybe_null_expr(";")?.map(|(e, _)| e);
            let end = maybe_null_expr(")")?.map(|(e, t)| Node::new_expr_stmt(e, t));
            let loop_ = self.stmt()?;
            Node::new_for(start, condi, end, loop_, self.tok())
        } else if self.consume("return") {
            Node::new_unary("return", read_until(self, ";")?, self.tok())
        } else {
            Node::new_expr_stmt(read_until(self, ";")?, self.tok())
        };
        Ok(node)
    }
    fn expr(&mut self) -> Result<Node, ParseError> {
        let mut node = self.assign()?;
        if self.consume(",") {
            node = Node::new_comma(node, self.expr()?, self.tok());
        }
        Ok(node)
    }
    fn assign(&mut self) -> Result<Node, ParseError> {
        let mut node = self.equality()?;
        if self.consume("=") {
            node = Node::new_assign(node, self.assign()?, self.tok());
        }
        Ok(node)
    }
    fn equality(&mut self) -> Result<Node, ParseError> {
        let mut node = self.relational()?;
        loop {
            match self.peek_reserved().as_deref() {
                Some("==") => {
                    node = Node::new_bin(BinOp::_Eq, node, self.shift().relational()?, self.tok())
                }
                Some("!=") => {
                    node = Node::new_bin(BinOp::Neq, node, self.shift().relational()?, self.tok())
                }
                _ => return Ok(node),
            }
        }
    }
    fn relational(&mut self) -> Result<Node, ParseError> {
        let mut node = self.add()?;
        loop {
            match self.peek_reserved().as_deref() {
                Some("<") => node = Node::new_bin(BinOp::Lt, node, self.shift().add()?, self.tok()),
                Some("<=") => {
                    node = Node::new_bin(BinOp::Le, node, self.shift().add()?, self.tok())
                }
                Some(">") => node = Node::new_bin(BinOp::Lt, self.shift().add()?, node, self.tok()),
                Some(">=") => {
                    node = Node::new_bin(BinOp::Le, self.shift().add()?, node, self.tok())
                }
                _ => return Ok(node),
            }
        }
    }
    fn add(&mut self) -> Result<Node, ParseError> {
        let mut node = self.mul()?;
        loop {
            match self.peek_reserved().as_deref() {
                Some("+") => node = Node::new_add(node, self.shift().mul()?, self.tok()),
                Some("-") => node = Node::new_sub(node, self.shift().mul()?, self.tok()),
                _ => return Ok(node),
            }
        }
    }
    fn mul(&mut self) -> Result<Node, ParseError> {
        let mut node = self.unary()?;
        loop {
            match self.peek_reserved().as_deref() {
                Some("*") => {
                    node = Node::new_bin(BinOp::Mul, node, self.shift().unary()?, self.tok())
                }
                Some("/") => {
                    node = Node::new_bin(BinOp::Div, node, self.shift().unary()?, self.tok())
                }
                _ => return Ok(node),
            }
        }
    }
    fn unary(&mut self) -> Result<Node, ParseError> {
        match self.peek_reserved().as_deref() {
            Some("+") => self.shift().unary(),
            Some("-") => Ok(Node::new_bin(
                BinOp::Sub,
                Node::new_num(0, self.tok()),
                self.shift().unary()?,
                self.tok(),
            )),
            Some("&") => Ok(Node::new_unary("addr", self.shift().unary()?, self.tok())),
            Some("*") => Ok(Node::new_unary("deref", self.shift().unary()?, self.tok())),
            _ => self.postfix(),
        }
    }
    fn postfix(&mut self) -> Result<Node, ParseError> {
        let mut node = self.primary()?;
        // x[y] は *(x+y)に同じ
        loop {
            if self.consume("[") {
                node = Node::new_unary(
                    "deref",
                    Node::new_add(node, self.expr()?, self.tok()),
                    self.tok(),
                );
                self.expect("]")?;
            } else if self.consume(".") {
                let name = self.expect_ident()?;
                let mem = match match node.get_type() {
                    Type::TyStruct { mem, .. } => mem.into_iter().filter(|m| m.name == name).next(),
                    _ => Err(self.raise_err("not a struct!"))?,
                } {
                    Some(mem) => mem,
                    _ => Err(self.raise_err(&format!("unknown member {}", name)))?,
                };
                node = Node::new_member_access(node, mem, self.tok());
            } else {
                return Ok(node);
            }
        }
    }
    fn primary(&mut self) -> Result<Node, ParseError> {
        if self.consume("(") {
            if self.peek("{") {
                let mut stmts = self.compound_stmt(true)?;
                self.expect(")")?;
                // 最後はexpression statementでないといけない
                if let Some(NodeKind::ExprStmt { expr }) = stmts.pop().map(|n| n.kind) {
                    Ok(Node::new_stmt_expr(stmts, *expr, self.tok()))
                } else {
                    Err(self.raise_err("statement expression returning void is not supported"))
                }
            } else {
                let node = self.expr();
                self.expect(")").map(|_| node)?
            }
        } else if let Some(val) = self.consume_num() {
            Ok(Node::new_num(val, self.tok()))
        } else if let Some(s) = self.consume_string() {
            Ok(self.add_string_literal(s))
        } else if self.consume("sizeof") {
            // このnodeは型のサイズを取得するためのみに使われ、
            // 実際には評価されない
            let node = self.unary()?;
            Ok(Node::new_num(node.get_type().size() as u32, self.tok()))
        } else {
            let name = self.expect_ident()?;
            // 関数呼び出し
            // TODO: 呼び出しがわで関数の型を把握すべし
            if self.consume("(") {
                let mut args = Vec::new();
                while !self.consume(")") {
                    args.push(self.assign()?);
                    if !self.consume(",") {
                        self.expect(")")?;
                        break;
                    }
                }
                Ok(Node::new_funcall(name, args, self.tok()))
            } else {
                // ただの変数
                match self.find_var(&name) {
                    Some(var) => Ok(Node::new_lvar(var, self.tok())),
                    _ => Err(ParseError {
                        pos: 0,
                        msg: "variable not found!".to_owned(),
                    }),
                }
            }
        }
    }
}
