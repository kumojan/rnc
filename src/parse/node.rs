use crate::ctype::*;
use std::fmt;
use std::rc::Rc;

pub struct Var {
    pub name: String,
    pub ty: TypeRef,
    pub id: usize,
    pub is_local: bool,
    pub is_static: bool,
    pub is_extern: bool,
    pub init_data: Option<Vec<Data>>,
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
impl Var {
    pub fn global_name(&self) -> String {
        if self.is_static && self.is_local {
            // localなstatic変数はアセンブリにおいて関数の外に記述されるが、その時の名前をラベルから生成する。
            format!(".L.static.{}", self.id)
        } else {
            format!("{}", self.name)
        }
    }
}
#[derive(Clone)]
pub struct Quad {
    pub label: String,
    pub is_positive: bool,
    pub addend: usize,
}
#[derive(Clone)]
pub enum Data {
    Byte(u8),        // バイト直書き
    Quad(Box<Quad>), // 指定する場所から8バイト読み取ってくる
}
impl fmt::Debug for Data {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Data::Byte(b) => write!(f, "{}", b),
            Data::Quad(q) => write!(f, "{}", q.label),
        }
    }
}
#[derive(Debug)]
pub enum InitKind {
    Leaf(Box<Node>),
    List(Vec<Initializer>),
    Zero,
}
#[derive(Debug)]
pub struct Initializer {
    pub kind: InitKind,
    pub tok_no: usize,
}
impl Initializer {
    pub(super) fn new_leaf(leaf: Node, tok_no: usize) -> Self {
        Initializer {
            kind: InitKind::Leaf(Box::new(leaf)),
            tok_no,
        }
    }
    pub(super) fn new_list(list: Vec<Initializer>, tok_no: usize) -> Self {
        Initializer {
            kind: InitKind::List(list),
            tok_no,
        }
    }
    pub(super) fn new_zero(tok_no: usize) -> Self {
        Initializer {
            kind: InitKind::Zero,
            tok_no,
        }
    }
    pub(super) fn eval(
        self,
        ty: TypeRef,
        types: &TypeList,
    ) -> Result<Vec<Data>, (usize, &'static str)> {
        Ok(match self.kind {
            InitKind::Zero => vec![Data::Byte(0); types.get(ty).size],
            InitKind::Leaf(node) => {
                let (var, init) = node.eval2(types)?;
                if let Some(var) = var {
                    let mut v = vec![Data::Byte(0); 8]; // quadは要するにポインタなので、8バイト確保する
                    v[0] = Data::Quad(Box::new(Quad {
                        label: var.name.clone(),
                        addend: init.abs() as usize,
                        is_positive: init.is_positive(),
                    }));
                    v
                } else {
                    let mut init = init.to_le_bytes().to_vec();
                    init.truncate(types.get(ty).size);
                    init.into_iter().map(Data::Byte).collect()
                }
            }
            InitKind::List(list) => match &types.get(ty).kind {
                TypeKind::TyArray(Some(len), base) => {
                    if *len < list.len() {
                        Err((self.tok_no, "initializer too long!"))?;
                    }
                    let mut v = vec![Data::Byte(0); types.get(ty).size];
                    let mut head = 0;
                    for init in list {
                        let next = head + types.get(*base).size;
                        v[head..next].clone_from_slice(&init.eval(*base, types)?);
                        head = next;
                    }
                    v
                }
                TypeKind::TyStruct {
                    mems,
                    is_union: false,
                    ..
                } => {
                    if mems.len() < list.len() {
                        Err((self.tok_no, "initializer too long!"))?;
                    }
                    let mut v = vec![Data::Byte(0); types.get(ty).size];
                    for (init, mem) in list.into_iter().zip(mems.iter()) {
                        let init = init.eval(mem.ty, types)?;
                        v[mem.offset..mem.offset + init.len()].clone_from_slice(&init);
                    }
                    v
                }
                _ => Err((self.tok_no, "invalid initializer!"))?,
            },
        })
    }
}

pub struct Function {
    pub ty: TypeRef,
    pub name: String,
    pub body: Vec<Node>,
    pub params: Vec<Rc<Var>>,
    pub locals: Vec<Rc<Var>>,
    pub is_static: bool,
}
impl Function {
    pub(super) fn new(
        ty: TypeRef,
        name: String,
        body: Vec<Node>,
        params: Vec<Rc<Var>>,
        locals: Vec<Rc<Var>>,
        is_static: bool,
    ) -> Self {
        Self {
            ty,
            name,
            body,
            params,
            locals,
            is_static,
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    _Eq,
    Neq,
    Lt,
    Le,
    Or,
    And,
    Xor,
    Shl,
    Shr,
}
impl fmt::Debug for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::_Eq => write!(f, "=="),
            BinOp::Neq => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Le => write!(f, "<="),
            BinOp::Or => write!(f, "|"),
            BinOp::And => write!(f, "&"),
            BinOp::Xor => write!(f, "^"),
            BinOp::Shl => write!(f, "<<"),
            BinOp::Shr => write!(f, ">>"),
        }
    }
}

#[allow(dead_code)]
pub enum NodeKind {
    // 式(expression)
    Num(usize),
    INum(i64),
    Var(Rc<Var>),
    Addr(Box<Node>),
    Deref(Box<Node>),
    Cast(Box<Node>),
    BitNot(Box<Node>),
    LogOr(Box<Node>, Box<Node>),
    LogAnd(Box<Node>, Box<Node>),
    Assign(Box<Node>, Box<Node>),
    Bin {
        op: BinOp,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    FunCall {
        name: String,
        args: Vec<Node>,
    },
    Literal {
        id: usize,
    },
    StmtExpr {
        stmts: Box<Node>, // blockを持たせる
        expr: Box<Node>,
    },
    Comma(Box<Node>, Box<Node>),
    Member {
        obj: Box<Node>,
        mem: Member2,
    },
    Conditional {
        condi: Box<Node>,
        then_: Box<Node>,
        else_: Box<Node>,
    },
    // 文(statement)
    Break,
    Continue,
    Goto(String),
    Label(String, Box<Node>),
    ExprStmt(Box<Node>),
    Return(Option<Box<Node>>),
    Do {
        stmt: Box<Node>,
        condi: Box<Node>,
    },
    Block {
        stmts: Vec<Node>,
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
    Switch {
        condi: Box<Node>,
        stmt: Box<Node>,
        cases: Vec<i64>,
        has_default: bool,
    },
    Case {
        stmt: Box<Node>,
        id: usize,
    },
    Default_(Box<Node>),
}
impl fmt::Debug for NodeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeKind::Num(val) => write!(f, "Num {}", val),
            NodeKind::Var(var) => write!(f, "Var {:?}", var),
            NodeKind::Cast(..) => write!(f, "cast"),
            NodeKind::BitNot(node) => write!(f, "bitnot of {:?}", node),
            NodeKind::LogAnd(lhs, rhs) => write!(f, "[{:?} && {:?}]", lhs, rhs),
            NodeKind::LogOr(lhs, rhs) => write!(f, "[{:?} || {:?}]", lhs, rhs),
            NodeKind::Return { .. } => write!(f, "return"),
            NodeKind::ExprStmt { .. } => write!(f, "expr stmt"),
            NodeKind::Bin { op, lhs, rhs } => write!(f, "{:?} {:?} {:?}", lhs, op, rhs),
            NodeKind::Assign { .. } => write!(f, "assign"),
            NodeKind::If { .. } => write!(f, "If"),
            NodeKind::For { .. } => write!(f, "for"),
            NodeKind::Block { .. } => write!(f, "block"),
            NodeKind::FunCall { name, .. } => write!(f, "function {}", name),
            NodeKind::Addr { .. } => write!(f, "address"),
            NodeKind::Deref { .. } => write!(f, "deref"),
            NodeKind::Literal { .. } => write!(f, "literal"),
            NodeKind::StmtExpr { .. } => write!(f, "stmt expr"),
            NodeKind::Member { obj, .. } => write!(f, "mem of {:?}", obj.kind),
            NodeKind::Comma { .. } => write!(f, "comma"),
            NodeKind::Break => write!(f, "break"),
            NodeKind::Continue => write!(f, "continue"),
            NodeKind::Goto(..) => write!(f, "goto"),
            NodeKind::Label(..) => write!(f, "label"),
            NodeKind::Do { .. } => write!(f, "do"),
            _ => write!(f, "unimplemented"),
        }
    }
}
pub struct Node {
    pub kind: NodeKind,
    pub ty: TypeRef,
    pub tok_no: usize,
}
impl fmt::Debug for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "kind:({:?}) type:({:?})", self.kind, self.ty)
    }
}
impl Node {
    pub fn get_type(&self) -> TypeRef {
        if self.ty.0 > 0 {
            self.ty
        } else {
            unimplemented!("called get_type for statements!")
        }
    }
    #[allow(overflowing_literals)] // 数値のキャストでオーバーフローを許す
    pub(super) fn eval(&self, types: &TypeList) -> Result<i64, (usize, &'static str)> {
        let cast_int = |b: bool| if b { 1 } else { 0 };
        let val = match &self.kind {
            NodeKind::Bin { op, lhs, rhs } => {
                let l = lhs.eval(types)?;
                let r = rhs.eval(types)?;
                match op {
                    BinOp::Add => l + r,
                    BinOp::Sub => l - r,
                    BinOp::Mul => l * r,
                    BinOp::Or => l | r,
                    BinOp::And => l & r,
                    BinOp::Xor => l ^ r,
                    BinOp::Div => l / r,
                    BinOp::Mod => l % r,
                    BinOp::_Eq => cast_int(l == r),
                    BinOp::Neq => cast_int(l != r),
                    BinOp::Lt => cast_int(l < r),
                    BinOp::Le => cast_int(l <= r),
                    BinOp::Shl => l << r,
                    BinOp::Shr => l >> r,
                }
            }
            NodeKind::Comma(_, rhs) => rhs.eval(types)?,
            NodeKind::Conditional {
                condi,
                then_,
                else_,
            } => {
                if condi.eval(types)? != 0 {
                    then_.eval(types)?
                } else {
                    else_.eval(types)?
                }
            }
            NodeKind::BitNot(node) => !node.eval(types)?,
            NodeKind::LogOr(lhs, rhs) => cast_int(lhs.eval(types)? != 0 || rhs.eval(types)? != 0),
            NodeKind::LogAnd(lhs, rhs) => cast_int(lhs.eval(types)? != 0 && rhs.eval(types)? != 0),
            NodeKind::Cast(node) => {
                let ty = self.ty;
                let val = node.eval(types)?;
                if types.is_integer(ty) {
                    match types.get(ty).size {
                        1 => val as i8 as i64,
                        2 => val as i16 as i64,
                        4 => val as i32 as i64,
                        _ => val,
                    }
                } else {
                    val
                }
            }
            NodeKind::Num(val) => *val as i64,
            _ => Err((self.tok_no, "invalid constant expression"))?,
        };
        Ok(val)
    }
    pub(super) fn eval2(
        &self,
        types: &TypeList,
    ) -> Result<(Option<Rc<Var>>, i64), (usize, &'static str)> {
        Ok(match &self.kind {
            NodeKind::Bin { op, lhs, rhs } if op == &BinOp::Add || op == &BinOp::Sub => {
                let (var, l) = lhs.eval2(types)?;
                let r = rhs.eval(types)?;
                (var, if op == &BinOp::Add { l + r } else { l - r })
            }
            NodeKind::Var(var) if types.get(self.ty).is_array() => {
                (Some(var.clone()), 0) // 配列のポインタ扱い
            }
            NodeKind::Addr(node) => match &node.kind {
                NodeKind::Var(var) => (Some(var.clone()), 0),
                _ => Err((self.tok_no, "invalid constant expression"))?,
            },
            _ => (None, self.eval(types)?),
        })
    }
    pub(super) fn new_num(val: usize, tok_no: usize) -> Self {
        let ty = if val <= i32::MAX as usize {
            TypeRef::INT
        } else {
            TypeRef::LONG
        };
        Self {
            kind: NodeKind::Num(val),
            ty,
            tok_no,
        }
    }
    pub(super) fn new_inum(val: i64, tok_no: usize) -> Self {
        let ty = if val <= i32::MAX as i64 {
            TypeRef::INT
        } else {
            TypeRef::LONG
        };
        Self {
            kind: NodeKind::INum(val),
            ty,
            tok_no,
        }
    }
    pub(super) fn new_var(var: Rc<Var>, tok_no: usize) -> Self {
        Self {
            ty: var.ty,
            kind: NodeKind::Var(var),
            tok_no,
        }
    }
    pub(super) fn new_cast(ty: TypeRef, expr: Node, tok_no: usize) -> Self {
        if ty == expr.ty {
            expr
        } else {
            Self {
                kind: NodeKind::Cast(Box::new(expr)),
                ty,
                tok_no,
            }
        }
    }
    pub(super) fn new_if(condi: Node, then_: Node, else_: Option<Node>, tok_no: usize) -> Self {
        Self {
            kind: NodeKind::If {
                condi: Box::new(condi),
                then_: Box::new(then_),
                else_: else_.map(Box::new),
            },
            ty: TypeRef::STMT,
            tok_no,
        }
    }
    pub(super) fn new_conditional(
        condi: Node,
        then_: Node,
        else_: Node,
        tok_no: usize,
        types: &TypeList,
    ) -> Self {
        let ty = types.common_ty(then_.ty, else_.ty);
        Self {
            kind: NodeKind::Conditional {
                condi: Box::new(condi),
                then_: Box::new(Node::new_cast(ty, then_, tok_no)),
                else_: Box::new(Node::new_cast(ty, else_, tok_no)),
            },
            ty,
            tok_no,
        }
    }
    pub(super) fn new_do(stmt: Node, condi: Node, tok_no: usize) -> Self {
        Self {
            ty: TypeRef::STMT,
            kind: NodeKind::Do {
                stmt: Box::new(stmt),
                condi: Box::new(condi),
            },
            tok_no,
        }
    }
    pub(super) fn new_return(node: Option<Node>, tok_no: usize) -> Self {
        Self {
            ty: TypeRef::STMT,
            kind: NodeKind::Return(node.map(Box::new)),
            tok_no,
        }
    }
    pub(super) fn new_unary(t: &str, node: Node, tok_no: usize, types: &TypeList) -> Self {
        Self {
            ty: match t {
                "expr_stmt" => TypeRef::STMT,
                "deref" => types.get(node.ty).base.unwrap(),
                "bitnot" => TypeRef::INT,
                _ => unimplemented!(),
            },
            kind: match t {
                "deref" => NodeKind::Deref(Box::new(node)),
                "expr_stmt" => NodeKind::ExprStmt(Box::new(node)),
                "bitnot" => NodeKind::BitNot(Box::new(node)),
                _ => unimplemented!(),
            },
            tok_no,
        }
    }
    /// アドレスnodeを作る前に、ポインタ型を作成して渡すこと
    pub(super) fn new_addr(node: Node, tok_no: usize, ptr_ty: TypeRef) -> Self {
        Self {
            ty: ptr_ty,
            kind: NodeKind::Addr(Box::new(node)),
            tok_no,
        }
    }
    pub(super) fn new_member_access(obj: Node, member: &Member2, tok_no: usize) -> Self {
        Self {
            ty: member.ty,
            kind: NodeKind::Member {
                obj: Box::new(obj),
                mem: member.clone(),
            },
            tok_no,
        }
    }
    pub(super) fn new_assign(var: Node, mut rhs: Node, tok_no: usize) -> Self {
        if var.ty != rhs.ty {
            rhs = Node::new_cast(var.ty.clone(), rhs, tok_no);
        }
        Self {
            ty: var.ty.clone(),
            kind: NodeKind::Assign(Box::new(var), Box::new(rhs)),
            tok_no,
        }
    }
    pub(super) fn new_comma(lhs: Node, rhs: Node, tok_no: usize) -> Self {
        Self {
            ty: rhs.ty.clone(),
            kind: NodeKind::Comma(Box::new(lhs), Box::new(rhs)),
            tok_no,
        }
    }
    pub(super) fn new_expr_stmt(expr: Node, tok_no: usize) -> Self {
        Self {
            kind: NodeKind::ExprStmt(Box::new(expr)),
            ty: TypeRef::STMT,
            tok_no,
        }
    }
    pub(super) fn new_bin(
        op: BinOp,
        lhs: Node,
        rhs: Node,
        tok_no: usize,
        types: &TypeList,
    ) -> Self {
        Self {
            ty: match op {
                BinOp::Sub if types.is_ptr(lhs.ty) && types.is_ptr(rhs.ty) => TypeRef::INT, // ポインタ同士の引き算はint
                BinOp::_Eq | BinOp::Neq | BinOp::Le | BinOp::Lt => TypeRef::INT, // 論理演算の結果はint?
                _ => lhs.ty,
            },
            kind: NodeKind::Bin {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            tok_no,
        }
    }
    pub(super) fn new_add(lhs: Node, rhs: Node, tok_no: usize, types: &TypeList) -> Self {
        match (
            types.get(lhs.ty).base.is_some(),
            types.get(rhs.ty).base.is_some(),
        ) {
            // 値 + 値
            (false, false) => Node::new_bin(BinOp::Add, lhs, rhs, tok_no, types),
            // 値+ポインタ => ポインタ+値と見なす
            (false, true) => Node::new_add(rhs, lhs, tok_no, types),
            // ポインタ + 値 は値だけずれたポインタを返す(型はlhsなのでポインタになる)
            (true, false) => {
                let ty_size = types.get_base_kind(lhs.ty).unwrap().size;
                Node::new_bin(
                    BinOp::Add,
                    lhs,
                    Node::new_bin(
                        BinOp::Mul,
                        Node::new_num(ty_size, tok_no),
                        rhs,
                        tok_no,
                        types,
                    ),
                    tok_no,
                    types,
                )
            }
            (true, true) => unimplemented!(), // ポインタ同士の足し算は意味をなさない
        }
    }
    pub(super) fn new_sub(
        lhs: Node,
        rhs: Node,
        tok_no: usize,
        types: &TypeList,
    ) -> Result<Self, (usize, &'static str)> {
        // TODO: 適当
        let node = match (
            types.get(lhs.ty).base.is_some(),
            types.get(rhs.ty).base.is_some(),
        ) {
            // 値 - 値
            (false, false) => Node::new_bin(BinOp::Sub, lhs, rhs, tok_no, types),
            // ポインタ - 値
            (true, false) => {
                let ty_size = types.get_base_kind(lhs.ty).unwrap().size;
                Node::new_bin(
                    BinOp::Sub,
                    lhs,
                    Node::new_bin(
                        BinOp::Mul,
                        rhs,
                        Node::new_num(ty_size, tok_no),
                        tok_no,
                        types,
                    ),
                    tok_no,
                    types,
                )
            }
            // ポインタ - ポインタ (ずれを返す int)
            (true, true) => {
                let ty_size = types.get_base_kind(lhs.ty).unwrap().size;
                Node::new_bin(
                    BinOp::Div,
                    Node::new_bin(BinOp::Sub, lhs, rhs, tok_no, types),
                    Node::new_num(ty_size, tok_no),
                    tok_no,
                    types,
                )
            }
            // 値 - ポインタは意味をなさない
            (false, true) => return Err((tok_no, "tried to subtract pointer from value")),
        };
        Ok(node)
    }
    pub(super) fn new_and(lhs: Node, rhs: Node, tok_no: usize) -> Self {
        Self {
            ty: TypeRef::BOOL,
            kind: NodeKind::LogAnd(Box::new(lhs), Box::new(rhs)),
            tok_no,
        }
    }
    pub(super) fn new_or(lhs: Node, rhs: Node, tok_no: usize) -> Self {
        Self {
            ty: TypeRef::BOOL,
            kind: NodeKind::LogOr(Box::new(lhs), Box::new(rhs)),
            tok_no,
        }
    }
    pub(super) fn new_for(
        start: Option<Node>,
        condi: Option<Node>,
        end: Option<Node>,
        loop_: Node,
        tok_no: usize,
    ) -> Self {
        Node {
            kind: NodeKind::For {
                start: start.map(Box::new),
                condi: condi.map(Box::new),
                end: end.map(Box::new),
                loop_: Box::new(loop_),
            },
            ty: TypeRef::STMT,
            tok_no,
        }
    }
    pub(super) fn new_block(stmts: Vec<Node>, tok_no: usize) -> Self {
        Self {
            kind: NodeKind::Block { stmts },
            ty: TypeRef::STMT,
            tok_no,
        }
    }
    pub(super) fn new_stmt_expr(stmts: Vec<Node>, expr: Node, tok_no: usize) -> Self {
        Node {
            ty: expr.ty,
            kind: NodeKind::StmtExpr {
                stmts: Box::new(Node::new_block(stmts, tok_no)),
                expr: Box::new(expr),
            },
            tok_no,
        }
    }
    pub(super) fn new_funcall(name: String, args: Vec<Node>, tok_no: usize) -> Self {
        Self {
            kind: NodeKind::FunCall { name, args },
            ty: TypeRef::INT,
            tok_no,
        }
    }
}
