use crate::r#type::*;
use crate::tokenize::*;
use crate::util::*;
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
        val: usize,
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
    Cast(Box<Node>),
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
            NodeKind::Cast(..) => write!(f, "cast"),
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
    pub ty: Option<Type>,
    pub tok: Option<Token>,
}
impl Node {
    pub fn get_type(&self) -> &Type {
        match &self.ty {
            Some(ty) => ty,
            _ => unimplemented!("called get_type for statements!"),
        }
    }
    fn new_num(val: usize, tok: Option<Token>) -> Self {
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
    fn new_cast(ty: Type, expr: Node, tok: Option<Token>) -> Self {
        Self {
            kind: NodeKind::Cast(Box::new(expr)),
            ty: Some(ty),
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
    fn new_assign(lvar: Node, mut rhs: Node, tok: Option<Token>) -> Self {
        if lvar.ty != rhs.ty {
            rhs = Node::new_cast(lvar.get_type().clone(), rhs, None);
        }
        Self {
            ty: lvar.ty.clone(),
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
                _ => Some(lhs.get_type().clone()),
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
                let ty_size = lhs.get_type().get_base().unwrap().size();
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
                let ty_size = lhs.get_type().get_base().unwrap().size();
                Node::new_bin(
                    BinOp::Sub,
                    lhs,
                    Node::new_bin(BinOp::Mul, rhs, Node::new_num(ty_size, None), None),
                    tok,
                )
            }
            // ポインタ - ポインタ (ずれを返す int)
            (true, true) => {
                let ty_size = lhs.get_type().get_base().unwrap().size();
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
            ty: Some(expr.get_type().clone()),
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
    pub is_warning: bool,
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "parse failed at {}", self.pos)
    }
}
#[derive(Default)]
pub struct Parser {
    tklist: VecDeque<Token>,
    locals: Vec<Rc<Var>>, // Node::Varと共有する。
    globals: Vec<VarScope>,
    string_literals: Vec<Vec<u8>>,
    var_scopes: Vec<Vec<VarScope>>, // 後方に内側のスコープがある(読むときはrevする)
    struct_tag_scopes: VecDeque<VecDeque<Type>>, // 前方に内側のスコープがある
    cur_tok: Option<Token>,
    line_no: usize,
    // attr: VarAttr,
}

enum PtrDim {
    Ptr(usize),
    Dim(usize),
}
enum VarScope {
    Var(Rc<Var>),
    Type(String, Type),
}

/// typedef や externなど
// struct VarAttr {
//     is_typedef: bool,
// }
// impl Default for VarAttr {
//     fn default() -> Self {
//         Self { is_typedef: false }
//     }
// }
impl VarScope {
    fn name(&self) -> &String {
        match self {
            Self::Type(name, ..) => name,
            Self::Var(var) => &var.name,
        }
    }
    fn is_type(&self, name: &str) -> bool {
        match self {
            Self::Type(_name, ..) => _name == name,
            _ => false,
        }
    }
    fn get_type(&self, name: &str) -> Option<Type> {
        match self {
            Self::Type(_name, ty) if _name == name => Some(ty.clone()),
            _ => None,
        }
    }
    fn get_var(&self, name: &str) -> Option<Rc<Var>> {
        match self {
            Self::Var(var) if var.name == name => Some(var.clone()),
            _ => None,
        }
    }
}

///
/// program = (funcdef | global-var)*
/// funcdef = typespec declarator funcargs "{" compound_stmt "}"
/// typespec = typename+
/// typename = "void" | "char" | "short" | "int" | "long" | "struct" struct_decl | "union" union_decl | typedef_name
/// struct_decl = ident? "{" struct_members "}"?
/// union_decl = ident? "{" struct_members "}"?
/// struct_members = (typespec declarator (","  declarator)* ";")*
/// ptr = "*"*
/// funcargs = "(" typespec declarator ( "," typespec declarator)? ")"
/// compound_stmt = (vardef | typedef | stmt)*
/// typedef = "typedef" typespec declarator ("," declarator)*
/// vardef = typespec declarator initializer ("," declarator initializer)*;
/// declarator = ptr ("(" declarator ")" | ident) ("[" num "]")*
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
/// mul = cast ("*" cast | "/" cast)*
/// cast = "(" type-name ")" cast | unary
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
impl Parser {
    pub fn new(tklist: VecDeque<Token>) -> Self {
        Self {
            tklist,
            ..Self::default()
        }
    }
    /// トークンを一つ読んで、すすむ
    /// 読んだトークンはcur_tokにいれる
    fn shift(&mut self) -> &mut Self {
        self.cur_tok = self.tklist.pop_front();
        if let Some(tok) = &self.cur_tok {
            self.line_no = tok.line_no;
        }
        self
    }
    fn unshift(&mut self) -> &mut Self {
        if let Some(tok) = std::mem::replace(&mut self.cur_tok, None) {
            self.tklist.push_front(tok);
        } else {
            panic!("unshift called but token was none");
        }
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
    fn peek_ident(&self) -> Option<String> {
        match self.head_kind() {
            TokenKind::TkIdent(s) => Some(s.clone()),
            _ => None,
        }
    }
    fn peek_base_type(&self) -> Option<String> {
        self.peek_reserved()
            .as_deref()
            .filter(|s| {
                [
                    "void", "char", "short", "int", "long", "struct", "union", "_Bool",
                ]
                .contains(s)
            })
            .map(str::to_owned)
    }
    /// 型名や"struct", "union"を見たときにtrue
    fn peek_type(&self) -> bool {
        let is_basic = self.peek_base_type().is_some();
        let is_other = self // typedefされた型であるかを調べる
            .peek_ident()
            .map_or(false, |s| {
                self.var_scopes
                    .iter()
                    .flat_map(|scope| scope.iter())
                    .chain(self.globals.iter())
                    .any(|v| v.is_type(&s))
            });
        is_basic || is_other
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
    fn consume_num(&mut self) -> Option<usize> {
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
    // fn consume_reserved(&mut self) -> Option<String> {
    //     if let TokenKind::TkReserved(ref s) = self.head_kind() {
    //         let s = s.clone();
    //         self.shift();
    //         Some(s)
    //     } else {
    //         None
    //     }
    // }
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
    fn expect_num(&mut self) -> Result<usize, ParseError> {
        self.consume_num().ok_or(self.raise_err("expected number"))
    }
    fn expect_ident(&mut self) -> Result<String, ParseError> {
        self.consume_ident()
            .ok_or(self.raise_err("expected identifier"))
    }
    fn typespec(&mut self) -> Result<Type, ParseError> {
        match self.peek_reserved().as_deref() {
            Some("struct") => return self.shift().struct_decl(),
            Some("union") => return self.shift().union_decl(),
            _ => (),
        };
        if let Some(ty) = self.peek_typedef() {
            self.shift();
            return Ok(ty);
        }
        const VOID: usize = 1 << 0;
        const BOOL: usize = 1 << 2;
        const CHAR: usize = 1 << 4;
        const SHORT: usize = 1 << 6;
        const INT: usize = 1 << 8;
        const LONG: usize = 1 << 10;
        let mut counter = 0;
        while let Some(ty) = self.peek_base_type().as_deref() {
            self.shift();
            counter += match ty {
                "void" => VOID,
                "_Bool" => BOOL,
                "int" => INT,
                "short" => SHORT,
                "long" => LONG,
                "char" => CHAR,
                _ => Err(self.raise_err("invalid type!"))?, // structやunion
            };
        }
        Ok(match counter {
            VOID => Type::TyVoid,
            BOOL => Type::TyBool,
            CHAR => Type::TyChar,
            0 | INT => Type::TyInt,
            ref s if [SHORT, SHORT + INT].contains(s) => Type::TyShort,
            ref l if [LONG, LONG + INT, LONG + LONG, LONG + LONG + INT].contains(l) => Type::TyLong,
            _ => Err(self.raise_err("invalid type!"))?,
        })
    }
    fn struct_decl(&mut self) -> Result<Type, ParseError> {
        let name = self.consume_ident();
        if self.consume("{") {
            let mut mems = Vec::new();
            loop {
                let ty = self.typespec()?;
                loop {
                    let (name, ty) = self.declarator(ty.clone())?; // int *x[4]; のような型が確定する
                    mems.push(Member {
                        ty,
                        name,
                        offset: 0,
                    });
                    if !self.consume(",") {
                        break;
                    }
                }
                self.expect(";")?;
                if self.consume("}") {
                    break;
                }
            }
            let mut offset = 0;
            for m in mems.iter_mut() {
                offset = align_to(offset, m.ty.align()); // 新しく追加される型のアライメントにoffsetを合わせる
                m.offset = offset; // メンバのoffsetを設定
                offset += m.ty.size(); // メンバのサイズだけoffsetをずらす
            }
            let align = mems.iter().map(|m| m.ty.align()).max().unwrap_or(1);
            let ty = Type::TyStruct {
                name,
                mem: Box::new(mems),
                align,
                size: align_to(offset, align),
            };
            self.struct_tag_scopes[0].push_front(ty.clone());
            Ok(ty)
        } else if let Some(name) = name {
            match self.find_struct(&name) {
                Some(ty) => Ok(ty),
                None => Err(self.raise_err("unknown struct tag")),
            }
        } else {
            Err(self.raise_err("expected identifier"))
        }
    }
    fn union_decl(&mut self) -> Result<Type, ParseError> {
        let name = self.consume_ident();
        if self.consume("{") {
            let mut mems = Vec::new();
            loop {
                let ty = self.typespec()?;
                loop {
                    let (name, ty) = self.declarator(ty.clone())?; // int *x[4]; のような型が確定する
                    mems.push(Member {
                        ty,
                        name,
                        offset: 0,
                    });
                    if !self.consume(",") {
                        break;
                    }
                }
                self.expect(";")?;
                if self.consume("}") {
                    break;
                }
            }
            let align = mems.iter().map(|m| m.ty.align()).max().unwrap_or(1);
            let size = mems.iter().map(|m| m.ty.size()).max().unwrap_or(1);
            let ty = Type::TyStruct {
                name,
                mem: Box::new(mems),
                align,
                size: align_to(size, align),
            };
            self.struct_tag_scopes[0].push_front(ty.clone());
            Ok(ty)
        } else if let Some(name) = name {
            match self.find_struct(&name) {
                Some(ty) => Ok(ty),
                None => Err(self.raise_err("unknown union tag")),
            }
        } else {
            Err(self.raise_err("expected identifier"))
        }
    }
    fn raise_err(&self, msg: &str) -> ParseError {
        // panic!();
        ParseError {
            pos: self.tklist[0].pos,
            msg: msg.to_owned(),
            is_warning: false,
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
        self.struct_tag_scopes.push_front(Default::default());
        self.var_scopes.push(Default::default());
    }
    /// 必ずenter_scopeと対にして使うこと
    fn leave_scope(&mut self) {
        self.struct_tag_scopes.pop_front();
        self.var_scopes.pop();
    }
    fn find_var(&self, name: String) -> Option<Rc<Var>> {
        // println!("searching {} in {:?}", name, self.var_scopes);
        self.var_scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev()) // 後ろから順に走査する
            .chain(self.globals.iter())
            .flat_map(|v| v.get_var(&name))
            .next()
    }
    fn find_struct(&self, name: &String) -> Option<Type> {
        // println!("searching {} in {:?}", name, self.var_scopes);
        self.struct_tag_scopes
            .iter()
            .flat_map(|scope| scope.iter())
            .find(|t| match &t {
                Type::TyStruct {
                    name: Some(_name), ..
                } => _name == name,
                _ => false,
            })
            .cloned()
    }
    fn find_func(&self, name: &String) -> Option<Type> {
        self.globals
            .iter()
            .flat_map(|v| v.get_var(&name))
            .flat_map(|v| match v.ty {
                Type::TyFunc { .. } => Some(v.ty.clone()),
                _ => None,
            })
            .next()
    }
    fn peek_typedef(&mut self) -> Option<Type> {
        self.peek_ident().map_or(None, |name| {
            self.var_scopes
                .iter()
                .rev()
                .flat_map(|scope| scope.iter().rev()) // 後ろから順に走査する
                .chain(self.globals.iter())
                .flat_map(|v| v.get_type(&name))
                .next()
        })
    }
    fn add_var(&mut self, name: &String, ty: Type) -> Rc<Var> {
        let var = Rc::new(Var {
            name: name.clone(),
            ty,
            id: self.locals.len(),
            is_local: true,
        });
        self.locals.push(var.clone());
        let len = self.var_scopes.len();
        self.var_scopes[len - 1].push(VarScope::Var(var.clone())); // scopeにも追加
        var
    }
    fn add_typdef(&mut self, name: String, ty: Type) {
        let len = self.var_scopes.len();
        self.var_scopes[len - 1].push(VarScope::Type(name, ty));
    }
    // グローバル変数は重複して宣言できない
    fn add_global(&mut self, name: String, ty: Type) -> Result<Rc<Var>, ParseError> {
        if self.globals.iter().find(|v| v.name() == &name).is_some() {
            Err(self.raise_err("global var/type or func redefined!"))
        } else {
            let var = Rc::new(Var {
                name: name.clone(),
                ty,
                id: 0,
                is_local: false,
            });
            self.globals.push(VarScope::Var(var.clone()));
            Ok(var)
        }
    }
    fn add_global_typedef(&mut self, name: String, ty: Type) -> Result<(), ParseError> {
        if self.globals.iter().find(|v| v.name() == &name).is_some() {
            Err(self.raise_err("global var/type or func redefined!"))
        } else {
            self.globals.push(VarScope::Type(name, ty));
            Ok(())
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
    // コード生成
    pub fn program(mut self) -> Result<(Vec<Function>, Vec<Rc<Var>>, Vec<Vec<u8>>), ParseError> {
        let mut code = vec![];
        while !self.is_eof() {
            if self.consume("typedef") {
                self.typedef(true)?; // is_global=true
            }
            if let Some(func) = self.funcdef()? {
                code.push(func); // グローバル変数か関数
            }
        }
        let globals = self
            .globals
            .into_iter()
            .flat_map(|v| match v {
                VarScope::Var(var) => Some(var),
                _ => None,
            })
            .collect();
        Ok((code, globals, self.string_literals))
    }
    fn funcargs(&mut self) -> Result<(), ParseError> {
        self.expect("(")?;
        if !self.consume(")") {
            loop {
                // TODO: 関数の引数では配列は本当には配列にならない
                let ty = self.typespec()?;
                let (name, ty) = self.declarator(ty)?;
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
        let basety = self.typespec()?;
        let (name, ty) = self.declarator(basety.clone())?;
        // 次に関数の有無を見る
        if self.peek("(") {
            // 関数の宣言
            self.enter_scope();
            self.funcargs()?;
            let params: Vec<_> = self.locals.iter().cloned().collect();
            let arg = params.iter().map(|p| p.ty.clone()).collect();
            let ty = Type::TyFunc {
                arg: Box::new(arg),
                ret: Box::new(ty),
            };
            self.add_global(name.clone(), ty.clone())?;
            if self.consume(";") {
                // int func();のよに関数の宣言のみの場合
                self.leave_scope(); // スコープは全く使わずに捨てる
                return Ok(None);
            }
            let stmts = self.compound_stmt(false)?;
            self.leave_scope();
            return Ok(Some(Function::new(
                ty,
                name,
                stmts,
                params,
                std::mem::replace(&mut self.locals, vec![]),
            )));
        }
        // 関数でないとしたら、グローバル変数が続いている
        // TODO: グローバル変数の初期化
        self.add_global(name, ty)?;
        if !self.consume(";") {
            loop {
                self.expect(",")?;
                let (name, ty) = self.declarator(basety.clone())?;
                self.add_global(name, ty)?;
                if self.consume(";") {
                    break;
                }
            }
        }
        Ok(None)
    }
    /// declarator = ptr ("(" declarator ")" | ident) ("[" num "]")*
    fn declarator(&mut self, mut ty: Type) -> Result<(String, Type), ParseError> {
        let (name, v) = self.get_name_ptr_dim()?;
        for i in v {
            ty = match i {
                PtrDim::Ptr(depth) => ty.to_ptr_recursive(depth as u8),
                PtrDim::Dim(dim) => ty.to_array(dim),
            }
        }
        let name = if let Some(name) = name {
            name
        } else {
            return Err(self.raise_err("expected ident"));
        };
        Ok((name, ty))
    }
    fn abstruct_declarator(&mut self, mut ty: Type) -> Result<Type, ParseError> {
        let (name, v) = self.get_name_ptr_dim()?;
        for i in v {
            ty = match i {
                PtrDim::Ptr(depth) => ty.to_ptr_recursive(depth as u8),
                PtrDim::Dim(dim) => ty.to_array(dim),
            }
        }
        if name.is_some() {
            return Err(self.raise_err("this has to be abstruct declarator"));
        };
        Ok(ty)
    }
    fn get_name_ptr_dim(&mut self) -> Result<(Option<String>, Vec<PtrDim>), ParseError> {
        let mut v = Vec::new();
        // read pointer depth
        let mut depth = 0;
        while self.consume("*") {
            depth += 1;
        }
        if depth > 0 {
            v.push(PtrDim::Ptr(depth));
        }
        // recursive
        let (name, inner) = if self.consume("(") {
            let name_ty = self.get_name_ptr_dim()?;
            self.expect(")")?;
            name_ty
        } else {
            (self.consume_ident(), Vec::new())
        };
        // read array dimension
        let mut dims = Vec::new();
        while self.consume("[") {
            dims.push(PtrDim::Dim(self.expect_num()?));
            self.expect("]")?;
        }
        v.extend(dims.into_iter().rev());

        // push inner result to stack
        v.extend(inner);
        Ok((name, v))
    }
    fn typedef(&mut self, is_global: bool) -> Result<(), ParseError> {
        let basety = self.typespec()?;
        if !self.consume(";") {
            loop {
                let (name, ty) = self.declarator(basety.clone())?;
                if is_global {
                    self.add_global_typedef(name, ty)?;
                } else {
                    self.add_typdef(name, ty);
                }
                if !self.consume(",") {
                    break; // 宣言終了
                }
            }
            self.expect(";")?;
        };
        Ok(())
    }
    fn vardef(&mut self) -> Result<Vec<Node>, ParseError> {
        let basety = self.typespec()?;
        let mut stmts = vec![];
        // 構造体宣言の後にすぐにセミコロンの場合、変数は設定せず、初期化もないのでstmts=vec![]のまま
        if !self.consume(";") {
            loop {
                let (name, ty) = self.declarator(basety.clone())?;
                if ty == Type::TyVoid {
                    Err(self.raise_err("variable decleared void"))?
                }
                // 初期化がなければ、コードには現れないので捨てられる
                let var = self.add_var(&name, ty);
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
        }
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
            if self.peek_type() {
                block.extend(self.vardef()?);
            } else if self.consume("typedef") {
                self.typedef(false)?;
            } else {
                block.push(self.stmt()?);
            }
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
    /// mul = cast ("*" cast | "/" cast)*
    fn mul(&mut self) -> Result<Node, ParseError> {
        let mut node = self.cast()?;
        loop {
            match self.peek_reserved().as_deref() {
                Some("*") => {
                    node = Node::new_bin(BinOp::Mul, node, self.shift().cast()?, self.tok())
                }
                Some("/") => {
                    node = Node::new_bin(BinOp::Div, node, self.shift().cast()?, self.tok())
                }
                _ => return Ok(node),
            }
        }
    }
    /// cast = "(" type-name ")" cast | unary
    fn cast(&mut self) -> Result<Node, ParseError> {
        if self.consume("(") {
            if self.peek_type() {
                let mut ty = self.typespec()?;
                ty = self.abstruct_declarator(ty)?;
                self.expect(")")?;
                return Ok(Node::new_cast(ty, self.cast()?, self.tok()));
            } else {
                self.unshift();
            }
        }
        self.unary()
    }
    /// unary = ("+" | "-" | "*" | "&") cast | postfix
    fn unary(&mut self) -> Result<Node, ParseError> {
        match self.peek_reserved().as_deref() {
            Some("+") => self.shift().cast(),
            Some("-") => Ok(Node::new_bin(
                BinOp::Sub,
                Node::new_num(0, self.tok()),
                self.shift().cast()?,
                self.tok(),
            )),
            Some("*") => {
                let addr = self.shift().cast()?;
                self.check_deref(addr)
            }
            Some("&") => Ok(Node::new_unary("addr", self.shift().cast()?, self.tok())),
            _ => self.postfix(),
        }
    }
    fn check_deref(&self, addr: Node) -> Result<Node, ParseError> {
        let ty = if let Ok(ty) = addr.get_type().get_base() {
            ty
        } else {
            println!("kind:{:?} type:{:?}", addr.kind, addr.get_type());
            return Err(self.raise_err("cannot dereference"));
        };
        Ok(Node {
            kind: NodeKind::Deref {
                node: Box::new(addr),
            },
            ty: Some(ty),
            tok: None,
        })
    }
    /// identを受けて構造体のメンバにアクセスする
    fn struct_ref(&mut self, node: Node) -> Result<Node, ParseError> {
        let name = self.expect_ident()?;
        let mem = match match node.get_type() {
            Type::TyStruct { mem, .. } => mem.clone().into_iter().filter(|m| m.name == name).next(),
            _ => Err(self.raise_err("not a struct!"))?,
        } {
            Some(mem) => mem,
            _ => Err(self.raise_err(&format!("unknown member {}", name)))?,
        };
        Ok(Node::new_member_access(node, mem, self.tok()))
    }
    /// postfix = primary ("[" expr "]" | "." ident  | "->" ident)*
    fn postfix(&mut self) -> Result<Node, ParseError> {
        let mut node = self.primary()?;
        loop {
            // x[y] は *(x+y)に同じ
            if self.consume("[") {
                node = Node::new_unary(
                    "deref",
                    Node::new_add(node, self.expr()?, self.tok()),
                    self.tok(),
                );
                self.expect("]")?;
            } else if self.consume(".") {
                node = self.struct_ref(node)?;
            // x->yは(*x).yに同じ
            } else if self.consume("->") {
                node = Node::new_unary("deref", node, self.tok());
                node = self.struct_ref(node)?;
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
            if self.consume("(") {
                if self.peek_type() {
                    let mut ty = self.typespec()?;
                    ty = self.abstruct_declarator(ty)?;
                    self.expect(")")?;
                    return Ok(Node::new_num(ty.size(), self.tok()));
                } else {
                    self.unshift(); // "("を取り戻す 途中でNodeが生成されていなければ、取り戻せるはず
                }
            }
            // このnodeは型のサイズを取得するためのみに使われ、
            // 実際には評価されない
            let node = self.unary()?;
            Ok(Node::new_num(node.get_type().size(), self.tok()))
        } else {
            let name = self.expect_ident()?;
            // 関数呼び出し
            if self.consume("(") {
                // 関数呼び出しとわかったので、チェック
                let (ret, mut arg_types) = match self.find_func(&name) {
                    Some(Type::TyFunc { ret, arg }) => (*ret, *arg),
                    _ => Err(self.raise_err("function not defined"))?,
                };
                let mut args = Vec::new();
                while !self.consume(")") {
                    let mut arg = self.assign()?;
                    if let Some(ty) = arg_types.pop() {
                        // 足りない型は無視する(可変長引数未対応)
                        if &ty != arg.get_type() {
                            arg = Node::new_cast(ty, arg, self.tok());
                        }
                    }
                    args.push(arg);
                    if !self.consume(",") {
                        self.expect(")")?;
                        break;
                    }
                }
                Ok(Node::new_cast(
                    ret,
                    Node::new_funcall(name, args, self.tok()),
                    None,
                ))
            } else {
                // ただの変数
                match self.find_var(name) {
                    Some(var) => Ok(Node::new_lvar(var, self.tok())),
                    _ => Err(self.raise_err("variable not found!")),
                }
            }
        }
    }
}
