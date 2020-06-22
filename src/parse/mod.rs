use crate::r#type::*;
use crate::tokenize::*;
use crate::util::*;
use node::*;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::fmt;
use std::rc::Rc;
pub mod node;

#[derive(Debug, Default)]
pub struct ParseError {
    pub pos: usize,
    pub msg: String,
}
impl ParseError {
    fn new(pos: usize, msg: &str) -> ParseError {
        ParseError {
            pos,
            msg: msg.to_owned(),
        }
    }
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "parse failed at {}", self.pos)
    }
}
#[derive(Default)]
pub struct Parser<'a> {
    cur_line: (usize, &'a str),
    cur_pos: &'a str, // 現在のトークンからのコード
    cur_tok: &'a str, // 現在のトークン
    head: usize,      // 読んでいるtokenのtklist上の位置
    tklist: Vec<Token>,
    locals: Vec<Rc<Var>>,  // Node::Varと共有する。
    statics: Vec<Rc<Var>>, // static variables
    globals: Vec<VarScope>,
    string_literals: Vec<CString>,
    var_scopes: Vec<Vec<VarScope>>, // 後方に内側のスコープがある(読むときはrevする)
    tag_scopes: VecDeque<VecDeque<(String, Type)>>, // 前方に内側のスコープがある
    cases: Vec<Vec<i64>>,
    switch_has_default: Vec<bool>,
    // var_attr: VarAttr,
    pub code: &'a str,            // debug用
    pub code_lines: Vec<&'a str>, // debug用
}

enum PtrDim {
    Ptr(usize),
    Dim(Option<usize>),
}
#[derive(Debug)]
enum VarScope {
    Var(Rc<Var>),
    Type(String, Type),
    Enum(String, i64),
}

// typedef, static, externなど
// #[derive(Default)]
// struct VarAttr {
//     is_typedef: bool,
//     is_static: bool,
// }

impl VarScope {
    fn name(&self) -> &String {
        match self {
            Self::Type(name, ..) => name,
            Self::Enum(name, ..) => name,
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
    fn get_union(&self, name: &str) -> Option<i64> {
        match self {
            Self::Enum(_name, val) if _name == name => Some(*val),
            _ => None,
        }
    }
}

enum Init {
    Index(usize),
    Member(String),
}

///
/// program = (funcdef | global-var)*
/// funcdef = "static"? typespec declarator funcargs "{" compound_stmt "}"
/// typespec = typename+
/// typename = "void" | "char" | "short" | "int" | "long" | "struct" struct_decl | "union" union_decl | "enum" enum_decl | typedef_name
/// struct_decl = ident? "{" struct_members "}"?
/// union_decl = ident? "{" struct_members "}"?
/// enum_decl = ident? "{" enum_list "}" | ident ("{" enum_list "}")?
/// enum_list = ident ("=" num)? ("," ident ("=" num)?)*
/// struct_members = (typespec declarator (","  declarator)* ";")*
/// ptr = "*"*
/// funcargs = "(" typespec declarator ( "," typespec declarator)? ")" | "(" "void" ")"
/// compound_stmt = (vardef | typedef | stmt)*
/// typedef = "typedef" typespec declarator ("," declarator)*
/// vardef = typespec declarator ("=" initializer)? ("," declarator ("=" initializer)?)* ";"
/// declarator = ptr ("(" declarator ")" | ident) ("[" num "]")*
/// initializer = "{" initializer ("," initializer )*"}" | assign
/// stmt = expr ";"  // expression statement (値を残さない)
///     | "return" expr ";"  
///     | "break" ";"
///     | "{" compound_stmt "}"
///     | "goto" ident ";"
///     | ident ":" stmt
///     | "if" "(" expr ")" stmt ( "else" stmt )?
///     | "while" "(" expr ")" stmt
///     | "for" "(" expr? ";" | vardef )  expr? ";" expr? ")" stmt  // vardefはセミコロンまで含む
///     | "switch" "(" expr ")" stmt
///     | "case" num ":" stmt
///     | "default" ":" stmt
/// expr = assign ("," expr )?   // exprはassignをコンマで連結している
/// assign = bitor (assign-op assign)?  // assignでは括弧()の中以外ではコンマは出てこない    
/// bitor = bitxor ("|" bitxor)*
/// bitxor = bitand ("^" bitand)*
/// bitand = equality ("&" equality)*
/// equality = relational (("==" | "!=") relational)*
/// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
/// shift = add ("<<" add | ">>" add)*
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
///     | "sizeof" "(" type ")"
///     | "alignof" "(" type ")"
///     | "(" "{" stmt* expr ";" "}" ")"
///
impl Parser<'_> {
    pub fn new(tklist: Vec<Token>) -> Self {
        Self {
            tklist,
            ..Self::default()
        }
    }
    fn set_tok(&mut self) {
        if self.head < self.tklist.len() {
            let head = &self.tklist[self.head];
            self.cur_line = (head.line_no + 1, self.code_lines[head.line_no]);
            self.cur_pos = &self.code[head.byte_len..];
            self.cur_tok = &self.code[head.byte_len..head.byte_len + head.len];
        }
    }
    /// トークンを一つ読んで、すすむ
    /// 読んだトークンはcur_tokにいれる
    fn skip(&mut self) -> &mut Self {
        self.head += 1;
        self.set_tok();
        self
    }
    fn unshift(&mut self) -> &mut Self {
        self.head -= 1;
        self.set_tok();
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
                    "void", "char", "short", "int", "long", "_Bool", "struct", "union", "enum",
                ]
                .contains(s)
            })
            .map(str::to_owned)
    }
    /// 型名や"struct", "union"を見たときにtrue
    fn peek_type(&self) -> bool {
        return self.peek_base_type().is_some()
            || self // typedefされた型であるかを調べる
                .peek_ident()
                .map_or(false, |s| {
                    self.var_scopes
                        .iter()
                        .flat_map(|scope| scope.iter())
                        .chain(self.globals.iter())
                        .any(|v| v.is_type(&s))
                });
    }
    /// 次がTkReserved(c) (cは指定)の場合は、1つずれてtrue, それ以外はずれずにfalse
    fn consume(&mut self, s: &str) -> bool {
        match self.head_kind() {
            TokenKind::TkReserved(_s) if _s == s => {
                self.skip();
                true
            }
            _ => false,
        }
    }
    fn consume_num(&mut self) -> Option<usize> {
        if let TokenKind::TkNum(ref val) = self.head_kind() {
            let val = *val;
            self.skip();
            Some(val)
        } else if let TokenKind::TkChar(ref c) = self.head_kind() {
            let c = *c as usize;
            self.skip();
            Some(c)
        } else {
            None
        }
    }
    fn consume_string(&mut self) -> Option<CString> {
        if let TokenKind::TkString(ref s) = self.head_kind() {
            let s = s.clone();
            self.skip();
            Some(s)
        } else {
            None
        }
    }
    fn consume_ident(&mut self) -> Option<String> {
        if let TokenKind::TkIdent(ref s) = self.head_kind() {
            let s = s.clone();
            self.skip();
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
    fn expect_ident(&mut self) -> Result<String, ParseError> {
        self.consume_ident()
            .ok_or(self.raise_err("expected identifier"))
    }
    fn typespec(&mut self) -> Result<Type, ParseError> {
        match self.peek_reserved().as_deref() {
            Some("struct") => return self.skip().struct_decl(),
            Some("union") => return self.skip().union_decl(),
            Some("enum") => return self.skip().enum_decl(),
            _ => (),
        };
        if let Some(ty) = self.peek_typedef() {
            self.skip();
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
            self.skip();
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
    fn struct_list(&mut self) -> Result<Vec<Member>, ParseError> {
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
        Ok(mems)
    }
    fn struct_decl(&mut self) -> Result<Type, ParseError> {
        let tag = self.consume_ident();
        let ty = if self.consume("{") {
            let mut mems = self.struct_list()?;
            let mut offset = 0;
            for m in mems.iter_mut() {
                offset = align_to(offset, m.ty.align()); // 新しく追加される型のアライメントにoffsetを合わせる
                m.offset = offset; // メンバのoffsetを設定
                offset += m.ty.size(); // メンバのサイズだけoffsetをずらす
            }
            let align = mems.iter().map(|m| m.ty.align()).max().unwrap_or(1);
            Type::TyStruct {
                mems: Box::new(mems),
                align,
                size: align_to(offset, align),
                is_union: false,
            }
        } else {
            if let Some(tag) = &tag {
                // 宣言がない時、タグがあればそこから探す
                if let Some(ty) = self.find_struct_tag(tag) {
                    return Ok(ty);
                }
            }
            // タグがないか、探しても見つからない時
            Type::new_incomplete_struct(tag.clone(), Rc::new(RefCell::new(None)))
        };
        if let Some(tag) = tag {
            if !self.redefine_tag(&tag, &ty) {
                self.add_tag(tag, ty.clone())
            }
        }
        Ok(ty)
    }
    fn union_decl(&mut self) -> Result<Type, ParseError> {
        let tag = self.consume_ident();
        if self.consume("{") {
            let mems = self.struct_list()?;
            let align = mems.iter().map(|m| m.ty.align()).max().unwrap_or(1);
            let size = mems.iter().map(|m| m.ty.size()).max().unwrap_or(1);
            let ty = Type::TyStruct {
                mems: Box::new(mems),
                align,
                size: align_to(size, align),
                is_union: true,
            };
            if let Some(tag) = tag {
                self.add_tag(tag, ty.clone());
            }
            Ok(ty)
        } else if let Some(tag) = tag {
            self.find_union_tag(&tag)
                .ok_or(self.raise_err("unknown union tag"))
        } else {
            Err(self.raise_err("expected identifier"))
        }
    }
    fn enum_decl(&mut self) -> Result<Type, ParseError> {
        let tag = self.consume_ident();
        if self.consume("{") {
            let mut val = 0;
            let mut mems = Vec::new();
            loop {
                let name = self.expect_ident()?;
                if self.consume("=") {
                    val = self.const_expr()?; // 実際は負の数とかも定義できるっぽいが...
                }
                mems.push(EnumMem { name, val });
                val += 1;
                if !self.consume(",") {
                    break;
                }
            }
            self.expect("}")?;
            for EnumMem { name, val } in &mems {
                self.add_enum(name.clone(), *val);
            }
            let ty = Type::new_enum(mems);
            if let Some(tag) = tag {
                self.add_tag(tag, ty.clone());
            }
            Ok(ty)
        } else if let Some(tag) = tag {
            match self.find_enum_tag(&tag) {
                Some(ty) => Ok(ty),
                None => Err(self.raise_err("unknown enum tag")),
            }
        } else {
            Err(self.raise_err("expected identifier"))
        }
    }
    fn raise_err(&self, msg: &str) -> ParseError {
        // panic!();
        ParseError {
            pos: self.tklist[self.head].pos,
            msg: msg.to_owned(),
        }
    }
    fn head_kind(&self) -> &TokenKind {
        &self.tklist[self.head].kind
    }
    /// shiftによってtokenが格納されているときは
    /// それを出して、Noneに置き換える
    /// あるいはそのままNoneが取り出される
    /// tokenは一つのNodeにしか渡らない
    fn tok(&mut self) -> usize {
        0
    }
    /// 必ずleave_scopeと対にして使うこと
    fn enter_scope(&mut self) {
        self.tag_scopes.push_front(Default::default());
        self.var_scopes.push(Default::default());
    }
    /// 必ずenter_scopeと対にして使うこと
    fn leave_scope(&mut self) {
        self.tag_scopes.pop_front();
        self.var_scopes.pop();
    }
    fn find_var(&self, name: &String) -> Option<Rc<Var>> {
        // println!("searching {} in {:?}", name, self.var_scopes);
        self.var_scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev()) // 後ろから順に走査する
            .chain(self.globals.iter())
            .flat_map(|v| v.get_var(name))
            .next()
    }
    fn find_enum(&self, name: &String) -> Option<i64> {
        self.var_scopes
            .iter()
            .rev()
            .flat_map(|scope| scope.iter().rev()) // 後ろから順に走査する
            .chain(self.globals.iter())
            .flat_map(|v| v.get_union(name))
            .next()
    }
    fn add_tag(&mut self, tag: String, ty: Type) {
        self.tag_scopes[0].push_front((tag, ty));
    }
    //
    fn redefine_tag(&mut self, name: &String, ty: &Type) -> bool {
        if ty.is_incomplete() {
            return false;
        }
        // var_scopeにあるtypedefのやつも書き換えたいが、
        // 現状ではIncempleteStructがresolveされた形になっていて、一応動くのでいいか。
        for (_name, _ty) in &mut self.tag_scopes[0] {
            if _name == name {
                if let Type::IncompleteStruct { resolved_type, .. } = _ty {
                    *resolved_type.borrow_mut() = Some(ty.clone()); // _tyを捨てる前に、その中の不完全型を解決しておく
                }
                *_ty = ty.clone();
                return true;
            }
        }
        false
    }
    fn find_tag(&self, name: &String) -> Option<Type> {
        self.tag_scopes
            .iter()
            .flat_map(|scope| scope.iter())
            .find(|(_name, _)| name == _name)
            .cloned()
            .map(|(_, ty)| ty)
    }
    fn find_struct_tag(&self, name: &String) -> Option<Type> {
        let ty = self.find_tag(name);
        if let Some(Type::TyStruct {
            is_union: false, ..
        }) = ty
        {
            ty
        } else {
            None
        }
    }
    fn find_union_tag(&self, name: &String) -> Option<Type> {
        let ty = self.find_tag(name);
        if let Some(Type::TyStruct { is_union: true, .. }) = ty {
            ty
        } else {
            None
        }
    }
    fn find_enum_tag(&self, name: &String) -> Option<Type> {
        let ty = self.find_tag(name);
        if let Some(Type::TyEnum { .. }) = ty {
            ty
        } else {
            None
        }
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
    fn peek_typedef(&self) -> Option<Type> {
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
    fn add_var(&mut self, name: &str, ty: Type) -> Rc<Var> {
        let var = Rc::new(Var {
            name: name.to_owned(),
            ty,
            id: self.locals.len(),
            is_local: true,
            is_static: false,
            init_data: None,
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
    fn add_enum(&mut self, name: String, num: i64) {
        let len = self.var_scopes.len();
        self.var_scopes[len - 1].push(VarScope::Enum(name, num));
    }
    // グローバル変数は重複して宣言できない
    fn add_global(
        &mut self,
        name: String,
        ty: Type,
        init_data: Option<Vec<Data>>,
        is_static: bool,
    ) -> Result<Rc<Var>, ParseError> {
        if self.globals.iter().find(|v| v.name() == &name).is_some() {
            Err(self.raise_err("global var/type or func redefined!"))
        } else {
            let var = Rc::new(Var {
                name: name.clone(),
                ty,
                id: 0,
                is_local: false,
                is_static,
                init_data,
            });
            self.globals.push(VarScope::Var(var.clone()));
            Ok(var)
        }
    }
    fn add_static(
        &mut self,
        name: String,
        ty: Type,
        init_data: Option<Vec<Data>>,
    ) -> Result<Rc<Var>, ParseError> {
        let var = Rc::new(Var {
            name: name.clone(),
            ty,
            id: self.statics.len(), // static変数の通し番号
            is_local: true,
            is_static: true,
            init_data,
        });
        let len = self.var_scopes.len();
        self.var_scopes[len - 1].push(VarScope::Var(var.clone())); // var_scopeに追加
        self.statics.push(var.clone()); // 初期化はglobal
        Ok(var)
    }
    fn add_global_typedef(&mut self, name: String, ty: Type) -> Result<(), ParseError> {
        if self.globals.iter().find(|v| v.name() == &name).is_some() {
            Err(self.raise_err("global var/type or func redefined!"))
        } else {
            self.globals.push(VarScope::Type(name, ty));
            Ok(())
        }
    }
    fn add_string_literal(&mut self, data: CString) -> Node {
        let n = Node {
            kind: NodeKind::Literal {
                ty: Type::TyChar.to_complete_array(data.0.len()),
                id: self.string_literals.len(),
            },
            ty: Some(Type::TyChar.to_complete_array(data.0.len())),
            tok_no: self.head,
        };
        self.string_literals.push(data);
        n
    }
    // コード生成
    pub fn program(
        mut self,
    ) -> Result<(Vec<Function>, Vec<Rc<Var>>, Vec<CString>, Vec<Token>), ParseError> {
        let mut code = vec![];
        while !self.is_eof() {
            if self.consume("typedef") {
                self.typedef(true)?; // is_global=true
            }
            if let Some(func) = self.funcdef()? {
                code.push(func); // グローバル変数か関数
            }
        }
        if self.var_scopes.len() > 0 {
            return Err(self.raise_err("varscope remains!"));
        }
        if self.tag_scopes.len() > 0 {
            return Err(self.raise_err("tagscopes remains!"));
        }
        let globals = self
            .globals
            .into_iter()
            .flat_map(|v| match v {
                VarScope::Var(var) => Some(var),
                _ => None,
            })
            .chain(self.statics.into_iter()) // static変数も追加
            .collect();
        Ok((code, globals, self.string_literals, self.tklist))
    }
    fn funcargs(&mut self) -> Result<(), ParseError> {
        self.expect("(")?;
        if self.consume("void") {
            self.expect(")")?;
            return Ok(()); // voidはパラメタがないことを意味する。
        }
        if !self.consume(")") {
            loop {
                // TODO: 関数の引数では配列は本当には配列にならない
                let ty = self.typespec()?;
                let (name, mut ty) = self.declarator(ty)?;
                if let Type::TyArray { base, .. } = ty {
                    ty = base.to_ptr();
                }
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
        let is_static = self.consume("static");
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
            self.add_global(name.clone(), ty.clone(), None, is_static)?;
            if self.consume(";") {
                // int func();のように関数の宣言のみの場合
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
                is_static,
            )));
        }
        // 関数でないとしたら、グローバル変数が続いている
        self.global_vardef(name, ty, is_static)?;
        if !self.consume(";") {
            loop {
                self.expect(",")?;
                let (name, ty) = self.declarator(basety.clone())?;
                self.global_vardef(name, ty, is_static)?;
                if self.consume(";") {
                    break;
                }
            }
        }
        Ok(None)
    }
    fn global_vardef(
        &mut self,
        name: String,
        mut ty: Type,
        is_static: bool,
    ) -> Result<(), ParseError> {
        let init = if self.consume("=") {
            let init = self.initializer()?;
            if let Type::TyArray { mut len, base } = ty {
                if let InitKind::List(v) = &init.kind {
                    if len.is_none() {
                        len = Some(v.len());
                    }
                }
                ty = Type::TyArray { len, base };
            }
            init.eval(&ty)
                .map(Some)
                .map_err(|(tok_no, msg)| ParseError::new(self.tklist[tok_no].pos, msg))?
        } else {
            None
        };
        self.add_global(name, ty, init, is_static)?;
        Ok(())
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
            let dim = if self.peek("]") {
                None
            } else {
                use std::convert::TryFrom;
                usize::try_from(self.const_expr()?)
                    .map(Some)
                    .map_err(|_| self.raise_err("array dimension must be non-negative!"))?
            };
            dims.push(PtrDim::Dim(dim));
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
    fn static_vardef(&mut self) -> Result<(), ParseError> {
        let basety = self.typespec()?;
        loop {
            let (name, mut ty) = self.declarator(basety.clone())?;
            let init = if self.consume("=") {
                let init = self.initializer()?;
                if let Type::TyArray { mut len, base } = ty {
                    if let InitKind::List(v) = &init.kind {
                        if len.is_none() {
                            len = Some(v.len());
                        }
                    }
                    ty = Type::TyArray { len, base };
                }
                init.eval(&ty)
                    .map(Some)
                    .map_err(|(tok_no, msg)| ParseError::new(self.tklist[tok_no].pos, msg))?
            } else {
                None
            };
            self.add_static(name, ty, init)?;
            if self.consume(";") {
                break;
            }
            self.expect(",")?;
        }
        Ok(())
    }
    fn vardef(&mut self) -> Result<Vec<Node>, ParseError> {
        let basety = self.typespec()?;
        let mut stmts = vec![];
        if !self.consume(";") {
            loop {
                let (name, mut ty) = self.declarator(basety.clone())?;
                if ty == Type::TyVoid {
                    Err(self.raise_err("variable decleared void"))?
                }
                if self.consume("=") {
                    let init = self.initializer()?;
                    if let Type::TyArray { mut len, base } = ty {
                        if let InitKind::List(v) = &init.kind {
                            if len.is_none() {
                                len = Some(v.len());
                            }
                        }
                        ty = Type::TyArray { len, base };
                    }
                    let var = self.add_var(&name, ty);
                    let mut nodes = vec![];
                    self.init_stmt(&var, init, vec![], &mut nodes, &var.ty)?;
                    stmts.extend(nodes);
                } else {
                    self.add_var(&name, ty);
                }
                if !self.consume(",") {
                    break; // 宣言終了
                }
            }
            self.expect(";")?;
        }
        Ok(stmts)
    }
    /// initializer = "{" initializer ("," initializer )*"}" | assign | string
    fn initializer(&mut self) -> Result<Initializer, ParseError> {
        let pos = self.head;
        if self.consume("{") {
            if self.consume("}") {
                Ok(Initializer::new_zero(pos))
            } else {
                let mut v = vec![];
                loop {
                    v.push(self.initializer()?);
                    if !self.consume(",") {
                        break;
                    }
                }
                self.expect("}")?;
                Ok(Initializer::new_list(v, pos))
            }
        } else if let Some(s) = self.consume_string() {
            let s =
                s.0.into_iter()
                    .map(|c| {
                        let n = Node::new_num(c as usize, pos);
                        Initializer::new_leaf(n, pos)
                    })
                    .collect();
            Ok(Initializer::new_list(s, pos))
        } else {
            Ok(Initializer::new_leaf(self.assign()?, pos))
        }
    }
    /// x[1][2] = a; というような代入文を生成する。
    fn init_stmt(
        &mut self,
        var: &Rc<Var>,
        i: Initializer,
        mut v: Vec<Init>,
        nodes: &mut Vec<Node>,
        ty: &Type,
    ) -> Result<Vec<Init>, ParseError> {
        let Initializer { kind, tok_no } = i;
        // 配列または構造体に代入している時は分ける
        // TODO: union
        match ty {
            Type::TyArray {
                len: Some(len),
                base,
            } => match kind {
                InitKind::List(list) => {
                    if *len < list.len() {
                        Err(self.raise_err("initializer too long"))?;
                    }
                    for d in list.len()..*len {
                        v.push(Init::Index(d));
                        v = self.init_stmt(var, Initializer::new_zero(tok_no), v, nodes, base)?;
                    }
                    for (d, i) in list.into_iter().enumerate() {
                        v.push(Init::Index(d));
                        v = self.init_stmt(var, i, v, nodes, base)?;
                    }
                }
                InitKind::Zero => {
                    for d in 0..*len {
                        v.push(Init::Index(d));
                        v = self.init_stmt(var, Initializer::new_zero(tok_no), v, nodes, base)?;
                    }
                }
                _ => Err(self.raise_err("array must be initialized with braces"))?,
            },
            Type::TyStruct {
                mems,
                is_union: false,
                ..
            } => match kind {
                InitKind::List(list) => {
                    if mems.len() < list.len() {
                        Err(self.raise_err("initializer too long"))?;
                    }
                    for m in mems.iter().skip(list.len()) {
                        v.push(Init::Member(m.name.clone()));
                        v = self.init_stmt(var, Initializer::new_zero(tok_no), v, nodes, &m.ty)?;
                    }
                    for (i, m) in list.into_iter().zip(mems.iter()) {
                        v.push(Init::Member(m.name.clone()));
                        v = self.init_stmt(var, i, v, nodes, &m.ty)?;
                    }
                }
                InitKind::Zero => {
                    for m in mems.iter() {
                        v.push(Init::Member(m.name.clone()));
                        v = self.init_stmt(var, Initializer::new_zero(tok_no), v, nodes, &m.ty)?;
                    }
                }
                InitKind::Leaf(val) => {
                    self.init_leaf(var.clone(), *val, nodes, &v, tok_no)?;
                }
            },
            ty => {
                let val = match kind {
                    InitKind::Leaf(val) => *val,
                    InitKind::Zero => {
                        if ty.is_integer() || ty.is_ptr() {
                            Node::new_num(0, tok_no) // ptrの場合、nullポインタ
                        } else {
                            Err(self.raise_err("invalid initializer"))?
                        }
                    }
                    InitKind::List(..) => Err(self.raise_err("too much dimension"))?, // 配列でも構造体でもないものに、配列風の初期化をしようとしている
                };
                self.init_leaf(var.clone(), val, nodes, &v, tok_no)?;
            }
        }
        v.pop(); // 自分の階層のインデックスを捨てる
        Ok(v)
    }
    fn init_leaf(
        &mut self,
        var: Rc<Var>,
        val: Node,
        nodes: &mut Vec<Node>,
        v: &Vec<Init>,
        tok_no: usize,
    ) -> Result<(), ParseError> {
        let mut node = Node::new_var(var.clone(), tok_no);
        for d in v {
            node = match d {
                Init::Index(idx) => {
                    let idx = Node::new_num(*idx, tok_no);
                    Node::new_unary("deref", Node::new_add(node, idx, tok_no), tok_no)
                }
                Init::Member(name) => self.struct_ref(node, name)?,
            };
        }
        node = Node::new_assign(node, val, tok_no);
        nodes.push(Node::new_expr_stmt(node, tok_no));
        Ok(())
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
            } else if self.consume("static") {
                self.static_vardef()?;
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
        let maybe_null_expr = |self_: &mut Self, s: &str| {
            Ok(if self_.consume(s) {
                None
            } else {
                let tok = self_.tok();
                let n = Some((self_.expr()?, tok));
                self_.expect(s)?;
                n
            })
        };
        let node = if self.peek("{") {
            Node::new_block(self.compound_stmt(true)?, self.head)
        } else if self.consume("if") {
            self.expect("(")?;
            let condi = read_until(self, ")")?;
            let then_ = self.stmt()?; // <- if trueのやつ
            let else_ = if self.consume("else") {
                Some(self.stmt()?)
            } else {
                None
            };
            Node::new_if(condi, then_, else_, self.head)
        } else if self.consume("while") {
            self.expect("(")?;
            Node::new_for(
                None,
                Some(read_until(self, ")")?),
                None,
                self.stmt()?,
                self.head,
            )
        } else if self.consume("for") {
            self.enter_scope();
            self.expect("(")?;
            let start = if self.peek_type() {
                Some(Node::new_block(self.vardef()?, self.head))
            } else {
                maybe_null_expr(self, ";")?.map(|(e, t)| Node::new_expr_stmt(e, t))
            };
            let condi = maybe_null_expr(self, ";")?.map(|(e, _)| e);
            let end = maybe_null_expr(self, ")")?.map(|(e, t)| Node::new_expr_stmt(e, t));
            let loop_ = self.stmt()?;
            self.leave_scope();
            Node::new_for(start, condi, end, loop_, self.head)
        } else if self.consume("return") {
            let node = if self.peek(";") {
                None
            } else {
                Some(self.expr()?)
            };
            self.expect(";")?;
            Node::new_return(node, self.head)
        } else if self.consume("break") {
            self.expect(";")?;
            Node {
                kind: NodeKind::Break,
                ty: None,
                tok_no: self.head,
            }
        } else if self.consume("continue") {
            self.expect(";")?;
            Node {
                kind: NodeKind::Continue,
                ty: None,
                tok_no: self.head,
            }
        } else if self.consume("goto") {
            let label = self.expect_ident()?;
            self.expect(";")?;
            Node {
                kind: NodeKind::Goto(label),
                ty: None,
                tok_no: self.head,
            }
        } else if self.peek_ident().is_some()
            && self.tklist[self.head + 1].kind == TokenKind::TkReserved(":".to_owned())
        {
            let label = self.expect_ident()?;
            let tok_no = self.head;
            self.expect(":")?;
            Node {
                kind: NodeKind::Label(label, Box::new(self.stmt()?)),
                ty: None,
                tok_no,
            }
        } else if self.consume("switch") {
            self.expect("(")?;
            let condi = Box::new(self.expr()?); // コンマもあり(expr, expr)みたいな
            self.expect(")")?;
            self.cases.push(vec![]);
            self.switch_has_default.push(false);
            Node {
                tok_no: self.head,
                kind: NodeKind::Switch {
                    condi,
                    stmt: Box::new(self.stmt()?),
                    cases: self.cases.pop().unwrap(),
                    has_default: self.switch_has_default.pop().unwrap(),
                },
                ty: None,
            }
        } else if self.consume("case") {
            let val = self.const_expr()?;
            self.cases
                .last_mut()
                .map(|v| v.push(val))
                .ok_or(self.raise_err("stray case"))?;
            self.expect(":")?;
            let id = self.cases[self.cases.len() - 1].len() - 1; // self.stmt()の前に呼ばないとラベルがずれるので注意
            Node {
                tok_no: self.head,
                kind: NodeKind::Case {
                    id,
                    stmt: Box::new(self.stmt()?),
                },
                ty: None,
            }
        } else if self.consume("default") {
            self.expect(":")?;
            if let Some(b) = self.switch_has_default.last_mut() {
                if *b {
                    Err(self.raise_err("duplicate default label"))?;
                } else {
                    *b = true;
                }
            } else {
                Err(self.raise_err("stray default"))?;
            }
            Node {
                tok_no: self.head,
                kind: NodeKind::Default_(Box::new(self.stmt()?)),
                ty: None,
            }
        } else {
            Node::new_expr_stmt(read_until(self, ";")?, self.head)
        };
        Ok(node)
    }
    /// expr = assign ("," expr )?   
    /// exprはassignをコンマで連結している
    fn expr(&mut self) -> Result<Node, ParseError> {
        let mut node = self.assign()?;
        if self.consume(",") {
            node = Node::new_comma(node, self.expr()?, self.head);
        }
        Ok(node)
    }
    /// assign = conditional (assign-op assign)?
    /// assignでは丸括弧の中以外ではコンマは出てこない
    /// assgin-op = "=", "+=", "-=", "*=", "/=", "%=", "|=", "&=", "^="
    fn assign(&mut self) -> Result<Node, ParseError> {
        let node = self.conditional()?;
        let binop: Node;
        macro_rules! to_assign {
            ($binop:expr) => {{
                binop = $binop;
                self.to_assign(binop)
            }};
        }
        macro_rules! to_assign_op {
            ($op:expr) => {{
                to_assign!(Node::new_bin($op, node, self.skip().assign()?, self.head))
            }};
        }
        Ok(match self.peek_reserved().as_deref() {
            Some("=") => Node::new_assign(node, self.skip().assign()?, self.head),
            Some("+=") => to_assign!(Node::new_add(node, self.skip().assign()?, self.head)),
            Some("-=") => to_assign!(Node::new_sub(node, self.skip().assign()?, self.head)),
            Some("*=") => to_assign_op!(BinOp::Mul),
            Some("/=") => to_assign_op!(BinOp::Div),
            Some("%=") => to_assign_op!(BinOp::Mod),
            Some("|=") => to_assign_op!(BinOp::Or),
            Some("&=") => to_assign_op!(BinOp::And),
            Some("^=") => to_assign_op!(BinOp::Xor),
            Some("<<=") => to_assign_op!(BinOp::Shl),
            Some(">>=") => to_assign_op!(BinOp::Shr),
            _ => node,
        })
    }
    /// A op= B を tmp = &A; *tmp = *tmp op B; にコンパイルする
    /// A = A op B で本当にうまくいかないのかはよくわからん。
    /// A = A op Bのように、Aを表すノードを作るには、
    /// Aが単なる変数であったら特に問題はないが、実際には
    /// a[x+f()]みたいにかなりややこしいものになっている可能性もある
    /// なので、tmpによってここをさすようにするのである。
    fn to_assign(&mut self, binop: Node) -> Node {
        if let Node {
            kind: NodeKind::Bin { op, lhs, rhs },
            ..
        } = binop
        {
            let ty = lhs.get_type().clone().to_ptr();
            let tmp = self.add_var("", ty);
            let tmp_node = || Node::new_var(tmp.clone(), self.head);
            let tmp_deref = || Node::new_unary("deref", tmp_node(), self.head);

            // tmp = &A
            let line1 = Node::new_assign(
                tmp_node(),
                Node::new_unary("addr", *lhs, self.head),
                self.head,
            );
            // *tmp = *tmp op B
            let line2 = Node::new_assign(
                tmp_deref(),
                Node::new_bin(op, tmp_deref(), *rhs, self.head),
                self.head,
            );
            Node::new_comma(line1, line2, self.head)
        } else {
            let msg = format!("line:{:?} at:{}", self.cur_line, self.cur_pos);
            unimplemented!("expected binop! {}", msg)
        }
    }
    // conditional = logor ("?" expr ":" conditional)?
    fn conditional(&mut self) -> Result<Node, ParseError> {
        let mut node = self.logor()?;
        if self.consume("?") {
            let tok = self.head;
            let then_ = self.expr()?;
            self.expect(":")?;
            let else_ = self.conditional()?;
            node = Node::new_conditional(node, then_, else_, tok);
        }
        Ok(node)
    }
    fn const_expr(&mut self) -> Result<i64, ParseError> {
        self.conditional()?
            .eval()
            .map_err(|(tok_no, msg)| ParseError::new(self.tklist[tok_no].pos, msg))
    }
    /// logor = logand ("&&" logand)*
    fn logor(&mut self) -> Result<Node, ParseError> {
        let mut node = self.logand()?;
        while self.consume("||") {
            node = Node::new_or(node, self.logand()?, self.head)
        }
        Ok(node)
    }
    /// logand = bitor ("&&" bitor)*
    fn logand(&mut self) -> Result<Node, ParseError> {
        let mut node = self.bitor()?;
        while self.consume("&&") {
            node = Node::new_and(node, self.logand()?, self.head)
        }
        Ok(node)
    }
    /// bitor = bitxor ("|" bitxor)*
    fn bitor(&mut self) -> Result<Node, ParseError> {
        let mut node = self.bitxor()?;
        while self.consume("|") {
            node = Node::new_bin(BinOp::Or, node, self.bitxor()?, self.head)
        }
        Ok(node)
    }
    /// bitxor = bitand ("^" bitand)*
    fn bitxor(&mut self) -> Result<Node, ParseError> {
        let mut node = self.bitand()?;
        while self.consume("^") {
            node = Node::new_bin(BinOp::Xor, node, self.bitand()?, self.head)
        }
        Ok(node)
    }
    /// bitand = equality ("&" equality)*
    fn bitand(&mut self) -> Result<Node, ParseError> {
        let mut node = self.equality()?;
        while self.consume("&") {
            node = Node::new_bin(BinOp::And, node, self.equality()?, self.head)
        }
        Ok(node)
    }
    fn equality(&mut self) -> Result<Node, ParseError> {
        let mut node = self.relational()?;
        loop {
            node = match self.peek_reserved().as_deref() {
                Some("==") => Node::new_bin(BinOp::_Eq, node, self.skip().relational()?, self.head),
                Some("!=") => Node::new_bin(BinOp::Neq, node, self.skip().relational()?, self.head),
                _ => return Ok(node),
            }
        }
    }
    /// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
    fn relational(&mut self) -> Result<Node, ParseError> {
        let mut node = self.shift()?;
        loop {
            node = match self.peek_reserved().as_deref() {
                Some("<") => Node::new_bin(BinOp::Lt, node, self.skip().shift()?, self.head),
                Some("<=") => Node::new_bin(BinOp::Le, node, self.skip().shift()?, self.head),
                Some(">") => Node::new_bin(BinOp::Lt, self.skip().shift()?, node, self.head),
                Some(">=") => Node::new_bin(BinOp::Le, self.skip().shift()?, node, self.head),
                _ => return Ok(node),
            }
        }
    }
    /// shift = add ("<<" add | ">>" add)*
    fn shift(&mut self) -> Result<Node, ParseError> {
        let mut node = self.add()?;
        loop {
            node = match self.peek_reserved().as_deref() {
                Some("<<") => Node::new_bin(BinOp::Shl, node, self.skip().add()?, self.head),
                Some(">>") => Node::new_bin(BinOp::Shr, node, self.skip().add()?, self.head),
                _ => return Ok(node),
            }
        }
    }
    /// add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> Result<Node, ParseError> {
        let mut node = self.mul()?;
        loop {
            node = match self.peek_reserved().as_deref() {
                Some("+") => Node::new_add(node, self.skip().mul()?, self.head),
                Some("-") => Node::new_sub(node, self.skip().mul()?, self.head),
                _ => return Ok(node),
            }
        }
    }
    /// mul = cast ("*" cast | "/" cast | "%" cast)*
    fn mul(&mut self) -> Result<Node, ParseError> {
        let mut node = self.cast()?;
        loop {
            node = match self.peek_reserved().as_deref() {
                Some("*") => Node::new_bin(BinOp::Mul, node, self.skip().cast()?, self.head),
                Some("/") => Node::new_bin(BinOp::Div, node, self.skip().cast()?, self.head),
                Some("%") => Node::new_bin(BinOp::Mod, node, self.skip().cast()?, self.head),
                _ => return Ok(node),
            };
        }
    }
    /// cast = "(" type-name ")" cast | unary
    fn cast(&mut self) -> Result<Node, ParseError> {
        if self.consume("(") {
            if self.peek_type() {
                let mut ty = self.typespec()?;
                ty = self.abstruct_declarator(ty)?;
                self.expect(")")?;
                return Ok(Node::new_cast(ty, self.cast()?, self.head));
            } else {
                self.unshift();
            }
        }
        self.unary()
    }
    /// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
    ///         | ("++" | "--") unary
    ///         | postfix
    fn unary(&mut self) -> Result<Node, ParseError> {
        match self.peek_reserved().as_deref() {
            Some("+") => self.skip().cast(),
            Some("-") => Ok(Node::new_bin(
                BinOp::Sub,
                Node::new_num(0, self.head),
                self.skip().cast()?,
                self.head,
            )),
            Some("*") => {
                let addr = self.skip().cast()?;
                self.check_deref(addr)
            }
            Some("&") => Ok(Node::new_unary("addr", self.skip().cast()?, self.head)),
            Some("~") => Ok(Node::new_unary("bitnot", self.skip().cast()?, self.head)),
            Some("!") => Ok(Node::new_bin(
                BinOp::_Eq,
                self.skip().cast()?,
                Node::new_num(0, self.head),
                self.head,
            )),
            Some("++") => {
                // ++i => i+=1
                let cast = self.skip().cast()?;
                let tok = self.head;
                Ok(self.to_assign(Node::new_add(cast, Node::new_num(1, self.head), tok)))
            }
            Some("--") => {
                // --i => i-=1
                let cast = self.skip().cast()?;
                let tok = self.head;
                Ok(self.to_assign(Node::new_sub(cast, Node::new_num(1, self.head), tok)))
            }
            _ => self.postfix(),
        }
    }
    fn check_deref(&self, addr: Node) -> Result<Node, ParseError> {
        let ty = if let Ok(ty) = addr.get_type().get_base() {
            if ty == &Type::TyVoid {
                return Err(self.raise_err("dereferencing void pointer!"));
            }
            Some(ty.clone())
        } else {
            println!("kind:{:?} type:{:?}", addr.kind, addr.get_type());
            return Err(self.raise_err("cannot dereference"));
        };
        Ok(Node {
            kind: NodeKind::Deref(Box::new(addr)),
            ty,
            tok_no: self.head,
        })
    }
    /// identを受けて構造体のメンバにアクセスする
    fn struct_ref(&mut self, node: Node, name: &String) -> Result<Node, ParseError> {
        let mem = match node.get_type().get_struct_mem(name) {
            Some(mem) => mem,
            _ => Err(self.raise_err(&format!("unknown member {}", name)))?,
        };
        Ok(Node::new_member_access(node, mem, self.head))
    }
    /// postfix = primary ("[" expr "]" | "." ident  | "->" ident | "++" | "--")*
    fn postfix(&mut self) -> Result<Node, ParseError> {
        let mut node = self.primary()?;
        loop {
            // x[y] は *(x+y)に同じ
            if self.consume("[") {
                node = Node::new_unary(
                    "deref",
                    Node::new_add(node, self.expr()?, self.head),
                    self.head,
                );
                self.expect("]")?;
            } else if self.consume(".") {
                let name = &self.expect_ident()?;
                node = self.struct_ref(node, name)?;
            // x->yは(*x).yに同じ
            } else if self.consume("->") {
                let name = &self.expect_ident()?;
                node = Node::new_unary("deref", node, self.head);
                node = self.struct_ref(node, name)?;
            } else if self.consume("++") {
                node = self.new_inc_dec(node, "+");
            } else if self.consume("--") {
                node = self.new_inc_dec(node, "-");
            } else {
                return Ok(node);
            }
        }
    }
    /// A++ は (tmp = &A, *tmp = *tmp + 1, *tmp - 1) になる
    /// ++Aと違って、A+=1としたあと、A-1という単なる値が返ることに注意
    fn new_inc_dec(&mut self, node: Node, op: &str) -> Node {
        let tok_no = self.head;
        let tmp = self.add_var("", node.get_type().clone().to_ptr());
        let tmp_node = || Node::new_var(tmp.clone(), tok_no);
        let tmp_deref = || Node::new_unary("deref", tmp_node(), tok_no);
        let n1 = Node::new_assign(
            tmp_node(),
            Node::new_unary("addr", node, self.head),
            self.head,
        );
        let n2: Node;
        let n3: Node;
        if op == "+" {
            n2 = self.to_assign(Node::new_add(tmp_deref(), Node::new_num(1, tok_no), tok_no));
            n3 = Node::new_sub(tmp_deref(), Node::new_num(1, tok_no), tok_no);
        } else {
            n2 = self.to_assign(Node::new_sub(tmp_deref(), Node::new_num(1, tok_no), tok_no));
            n3 = Node::new_add(tmp_deref(), Node::new_num(1, self.head), self.head);
        }
        Node::new_comma(n1, Node::new_comma(n2, n3, self.head), self.head)
    }
    fn primary(&mut self) -> Result<Node, ParseError> {
        if self.consume("(") {
            if self.peek("{") {
                let mut stmts = self.compound_stmt(true)?;
                self.expect(")")?;
                // 最後はexpression statementでないといけない
                if let Some(NodeKind::ExprStmt(expr)) = stmts.pop().map(|n| n.kind) {
                    Ok(Node::new_stmt_expr(stmts, *expr, self.head))
                } else {
                    Err(self.raise_err("statement expression returning void is not supported"))
                }
            } else {
                let node = self.expr();
                self.expect(")").map(|_| node)?
            }
        } else if let Some(val) = self.consume_num() {
            Ok(Node::new_num(val, self.head))
        } else if let Some(s) = self.consume_string() {
            Ok(self.add_string_literal(s))
        } else if self.consume("sizeof") {
            if self.consume("(") {
                if self.peek_type() {
                    let mut ty = self.typespec()?;
                    ty = self.abstruct_declarator(ty)?;
                    self.expect(")")?;
                    return Ok(Node::new_num(ty.size(), self.head));
                } else {
                    self.unshift(); // 型名ではなかったので、戻す
                }
            }
            // このnodeは型のサイズを取得するためのみに使われ、
            // 実際には評価されない
            let node = self.unary()?;
            Ok(Node::new_num(node.get_type().size(), self.head))
        } else if self.consume("alignof") {
            self.expect("(")?;
            let mut ty = self.typespec()?;
            ty = self.abstruct_declarator(ty)?;
            self.expect(")")?;
            Ok(Node::new_num(ty.align(), self.head))
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
                            arg = Node::new_cast(ty, arg, self.head);
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
                    Node::new_funcall(name, args, self.head),
                    self.head,
                ))
            } else {
                // 変数、またはenum定数
                if let Some(var) = self.find_var(&name) {
                    Ok(Node::new_var(var, self.head))
                } else if let Some(val) = self.find_enum(&name) {
                    Ok(Node::new_inum(val, self.head))
                } else {
                    Err(self.raise_err("variable not found!"))
                }
            }
        }
    }
}
