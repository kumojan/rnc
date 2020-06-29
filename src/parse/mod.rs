use crate::ctype::*;
use crate::tokenize::*;
use node::*;
use std::fmt;
use std::rc::Rc;
pub mod node;

#[derive(Debug, Default)]
pub struct ParseError {
    pub pos: usize,
    pub msg: String,
}
impl ParseError {
    fn new<S: Into<String>>(pos: usize, msg: S) -> ParseError {
        ParseError {
            pos,
            msg: msg.into(),
        }
    }
}
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "parse failed at {}", self.pos)
    }
}
#[derive(PartialEq, Debug)]
enum TagKind {
    Struct,
    Union,
    Enum,
}
#[derive(Debug)]
struct Tag {
    name: String,
    kind: TagKind,
    ty: TypeRef,
}
impl Tag {
    fn new_struct(name: String, ty: TypeRef) -> Self {
        Self {
            name,
            ty,
            kind: TagKind::Struct,
        }
    }
    fn new_union(name: String, ty: TypeRef) -> Self {
        Self {
            name,
            ty,
            kind: TagKind::Union,
        }
    }
    fn new_enum(name: String, ty: TypeRef) -> Self {
        Self {
            name,
            ty,
            kind: TagKind::Enum,
        }
    }
    fn new_struct_union(name: String, ty: TypeRef, is_union: bool) -> Self {
        if is_union {
            Self::new_union(name, ty)
        } else {
            Self::new_struct(name, ty)
        }
    }
}

#[derive(Default)]
pub struct Parser<'a> {
    cur_line: (usize, &'a str),     // 現在の行とその内容(デバッグ用)
    cur_tok: (&'a str, &'a str),    // 現在のトークンと、それ以降のコード(デバッグ用)
    head: usize,                    // 読んでいるtokenのtklist上の位置
    tklist: &'a [Token<'a>],        // トークン列
    locals: Vec<Rc<Var>>,           // 現在定義中の関数のローカル変数
    static_label: usize,            // ローカルなstatic変数をアセンブリで宣言するためのラベル
    tmp_gvar_label: usize,          // 一時的なグローバル変数に対する通し番号
    globals: Vec<Rc<Var>>,          // グローバル変数と、ローカルなstatic変数
    string_literals: Vec<CString>,  // 文字列リテラル
    var_scopes: Vec<Vec<VarScope>>, // 前方に内側のスコープがある
    tag_scopes: Vec<Vec<Tag>>,      // 前方に内側のスコープがある
    cases: Vec<Vec<i64>>,           // 現在のswitch文が持っているcase
    switch_has_default: Vec<bool>,  // 現在のswitchがdfaultを持つか
    types: TypeList,                // プログラムに出現した型のリスト
    code_lines: Vec<&'a str>,       // プログラムの各行への参照(デバッグ用)
}

enum PtrDim {
    Ptr(usize),
    Dim(Option<usize>),
}
#[derive(Debug)]
enum VarScope {
    Var(Rc<Var>),
    Type(String, TypeRef),
    Enum(String, i64),
}

impl VarScope {
    fn get_type(&self, name: &str) -> Option<TypeRef> {
        match self {
            Self::Type(_name, ty) if _name == name => Some(*ty),
            _ => None,
        }
    }
    fn get_var(&self, name: &str) -> Option<Rc<Var>> {
        match self {
            Self::Var(var) if var.name == name => Some(var.clone()),
            _ => None,
        }
    }
    fn get_enum(&self, name: &str) -> Option<i64> {
        match self {
            Self::Enum(_name, val) if _name == name => Some(*val),
            _ => None,
        }
    }
}

enum Init<'a> {
    Index(usize),    // 配列のインデックス
    Member(&'a str), // 構造体のメンバ
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
///     | "do" stmt "while" "(" expr ")" ";"
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
/// cast = "(" type-name ")" initializer | "(" type-name ")" cast | unary
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
impl<'a> Parser<'a> {
    pub fn new(tklist: &'a [Token<'a>], code_lines: Vec<&'a str>) -> Self {
        Self {
            tklist,
            code_lines,
            ..Self::default()
        }
    }
    fn var_scope(&mut self) -> &mut Vec<VarScope> {
        self.var_scopes.last_mut().unwrap()
    }
    fn tag_scope(&mut self) -> &mut Vec<Tag> {
        self.tag_scopes.last_mut().unwrap()
    }
    fn new_tmp_gvar_label(&mut self) -> String {
        let label = self.tmp_gvar_label;
        self.tmp_gvar_label += 1;
        format!(".L.tmp.{}", label)
    }
    fn set_tok(&mut self) {
        if self.head < self.tklist.len() {
            let head = &self.tklist[self.head];
            self.cur_line = (head.line_no + 1, self.code_lines[head.line_no]);
            self.cur_tok = (&head.tok[..head.len], head.tok);
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
    fn peek_reserved(&self) -> Option<&str> {
        match self.head_kind() {
            TokenKind::TkReserved(s) => Some(s),
            _ => None,
        }
    }
    fn peek_ident(&self) -> Option<String> {
        match self.head_kind() {
            TokenKind::TkIdent(s) => Some(s.clone()),
            _ => None,
        }
    }
    fn peek_base_type(&self) -> Option<&str> {
        self.peek_reserved().filter(|s| {
            [
                "void", "char", "short", "int", "long", "_Bool", "struct", "union", "enum",
            ]
            .contains(s)
        })
    }
    /// 型名や"struct", "union"を見たときにtrue
    fn peek_type(&self) -> bool {
        return self.peek_base_type().is_some()
            || self // typedefされた型であるかを調べる
                .peek_ident()
                .map_or(false, |name| {
                    self.var_scopes
                        .iter()
                        .flatten()
                        .rev()
                        .any(|v| v.get_type(&name).is_some())
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
    fn typespec(&mut self) -> Result<TypeRef, ParseError> {
        match self.peek_reserved() {
            Some("struct") => return self.skip().struct_union_decl(false),
            Some("union") => return self.skip().struct_union_decl(true),
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
        while let Some(ty) = self.peek_base_type() {
            counter += match ty {
                "void" => VOID,
                "_Bool" => BOOL,
                "int" => INT,
                "short" => SHORT,
                "long" => LONG,
                "char" => CHAR,
                _ => Err(self.raise_err("invalid type!"))?, // structやunion
            };
            self.skip();
        }
        Ok(match counter {
            VOID => TypeRef::VOID,
            BOOL => TypeRef::BOOL,
            CHAR => TypeRef::CHAR,
            0 | INT => TypeRef::INT,
            ref s if [SHORT, SHORT + INT].contains(s) => TypeRef::SHORT,
            ref l if [LONG, LONG + INT, LONG + LONG, LONG + LONG + INT].contains(l) => {
                TypeRef::LONG
            }
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
    fn struct_union_decl(&mut self, is_union: bool) -> Result<TypeRef, ParseError> {
        let tag = self.consume_ident();
        let ty = if self.consume("{") {
            let mems = self.struct_list()?;
            if let Some(name) = tag {
                // 同じスコープ内で同じ種類、かつ不完全に定義されていたら、おきかえる
                // 同じスコープ内で別の種類(enum, struct, union)として定義されていたらerr
                // 同じスコープ内に同名が存在しない時、置き換えずに新たに宣言して、追加
                let replace_with = self.find_tag_to_replace(&name, is_union)?;
                let ty = self.types.new_struct_union(mems, is_union, replace_with); // すでにタグが存在している場合、置き換える
                if replace_with.is_none() {
                    // 置き換えなかった時は、新規タグとして追加
                    self.add_tag(Tag::new_struct_union(name, ty, is_union));
                }
                ty
            } else {
                self.types.new_struct_union(mems, is_union, None)
            }
        } else {
            // 定義がついてない場合
            if let Some(name) = tag {
                if let Some(ty) = self.find_struct_union_tag(&name, is_union) {
                    ty
                } else {
                    let ty = self.types.new_placeholder();
                    self.add_tag(Tag::new_struct_union(name, ty, is_union));
                    ty
                }
            } else {
                return Err(self.raise_err("expected struct tag!"));
            }
        };
        Ok(ty)
    }
    fn enum_decl(&mut self) -> Result<TypeRef, ParseError> {
        let tag = self.consume_ident();
        if self.consume("{") {
            let mut val = 0;
            let mut mems = Vec::new();
            loop {
                let name = self.expect_ident()?;
                if self.consume("=") {
                    val = self.const_expr()?;
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
            let ty = self.types.add_enum(mems);
            if let Some(name) = tag {
                self.add_tag(Tag::new_enum(name, ty));
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
        // println!("{:?}", self.types);
        // panic!("{}", msg);
        ParseError::new(self.tklist[self.head].pos, msg)
    }
    fn cast_err<S: Into<String>>(&self, (tok_no, msg): (usize, S)) -> ParseError {
        println!("{:?}", self.types);
        println!("{:?}", self.locals);
        ParseError::new(self.tklist[tok_no].pos, msg)
    }
    fn head_kind(&self) -> &TokenKind {
        &self.tklist[self.head].kind
    }
    /// 必ずleave_scopeと対にして使うこと
    fn enter_scope(&mut self) {
        self.tag_scopes.push(Default::default());
        self.var_scopes.push(Default::default());
    }
    /// 必ずenter_scopeと対にして使うこと
    fn leave_scope(&mut self) {
        self.tag_scopes.pop();
        self.var_scopes.pop();
    }
    fn find_var(&self, name: &str) -> Option<Rc<Var>> {
        self.var_scopes // 後ろから順に走査する
            .iter()
            .flatten()
            .rev()
            .flat_map(|v| v.get_var(name))
            .next()
    }
    fn find_enum(&self, name: &str) -> Option<i64> {
        self.var_scopes
            .iter()
            .flatten()
            .rev()
            .flat_map(|v| v.get_enum(name))
            .next()
    }
    fn add_tag(&mut self, tag: Tag) {
        self.tag_scope().push(tag);
    }
    fn find_tag(&self, name: &str) -> Option<&Tag> {
        self.tag_scopes
            .iter()
            .flatten()
            .rev()
            .filter(|tag| tag.name == name)
            .next()
    }
    fn find_tag_to_replace(
        &self,
        name: &str,
        is_union: bool,
    ) -> Result<Option<TypeRef>, ParseError> {
        let kind = if is_union {
            TagKind::Union
        } else {
            TagKind::Struct
        };
        match self
            .tag_scopes
            .last()
            .unwrap()
            .iter()
            .rev()
            .filter(|tag| tag.name == name)
            .next()
        {
            Some(tag) if tag.kind != kind => {
                return Err(
                    self.raise_err(&format!("tag {:} was declared as {:?}", tag.name, tag.kind))
                );
            }
            Some(tag) if self.types.get(tag.ty).kind == TypeKind::Placeholder => Ok(Some(tag.ty)),
            _ => Ok(None),
        }
    }
    fn find_struct_union_tag(&self, name: &str, is_union: bool) -> Option<TypeRef> {
        if let Some(Tag { kind, ty, .. }) = self.find_tag(name) {
            if is_union && kind == &TagKind::Union || !is_union && kind == &TagKind::Struct {
                return Some(*ty);
            }
        }
        None
    }
    fn find_enum_tag(&self, name: &str) -> Option<TypeRef> {
        if let Some(Tag { kind, ty, .. }) = self.find_tag(name) {
            if kind == &TagKind::Enum {
                return Some(*ty);
            }
        }
        None
    }
    fn find_func(&self, name: &str) -> Option<(Vec<TypeRef>, TypeRef)> {
        self.globals
            .iter()
            .filter(|v| v.name == name)
            .flat_map(|v| match &self.types.get(v.ty).kind {
                TypeKind::TyFunc { args, ret } => Some((args.clone(), *ret)),
                _ => None,
            })
            .next()
    }
    fn peek_typedef(&self) -> Option<TypeRef> {
        self.peek_ident().map_or(None, |name| {
            self.var_scopes
                .iter()
                .flatten()
                .rev()
                .flat_map(|v| v.get_type(&name))
                .next()
        })
    }
    fn add_var<S: Into<String>>(&mut self, name: S, ty: TypeRef) -> Rc<Var> {
        let var = Rc::new(Var {
            name: name.into(),
            ty,
            id: self.locals.len(),
            is_local: true,
            is_static: false,
            is_extern: false,
            init_data: None,
        });
        self.locals.push(var.clone());
        self.var_scope().push(VarScope::Var(var.clone())); // scopeにも追加
        var
    }
    fn add_typdef(&mut self, name: String, ty: TypeRef) {
        self.var_scope().push(VarScope::Type(name, ty));
    }
    fn add_enum(&mut self, name: String, num: i64) {
        self.var_scope().push(VarScope::Enum(name, num));
    }
    fn check_global_name(&self, name: &str) -> Result<(), ParseError> {
        match self.globals.iter().any(|v| !v.is_local && v.name == name) {
            true => Err(self.raise_err("global var or func redefined!")),
            false => Ok(()),
        }
    }
    /// グローバル変数とstaticローカル変数
    fn add_global(
        &mut self,
        name: String,
        ty: TypeRef,
        init_data: Option<Vec<Data>>,
        is_static: bool,
        is_extern: bool,
        is_local: bool,
    ) -> Result<Rc<Var>, ParseError> {
        let id = if is_local { self.static_label } else { 0 };
        let var = Rc::new(Var {
            name: name.clone(),
            ty,
            id,
            is_local,
            is_static,
            is_extern,
            init_data,
        });
        if is_local {
            self.static_label += 1;
        }
        self.globals.push(var.clone());
        self.var_scope().push(VarScope::Var(var.clone()));
        Ok(var)
    }
    fn add_string_literal(&mut self, data: CString) -> Node {
        let n = Node {
            kind: NodeKind::Literal {
                id: self.string_literals.len(),
            },
            ty: self.types.array_of(TypeRef::CHAR, Some(data.0.len())),
            tok_no: self.head,
        };
        self.string_literals.push(data);
        n
    }
    // コード生成
    pub fn program(
        mut self,
    ) -> Result<(Vec<Function>, Vec<Rc<Var>>, Vec<CString>, TypeList), ParseError> {
        self.enter_scope(); // ファイルスコープ
        let mut code = vec![];
        while !self.is_eof() {
            if self.consume("typedef") {
                self.typedef()?; //
            }
            if let Some(func) = self.funcdef()? {
                code.push(func); // グローバル変数か関数
            }
        }
        self.leave_scope();
        if self.var_scopes.len() > 0 {
            return Err(self.raise_err("varscope remains!"));
        }
        if self.tag_scopes.len() > 0 {
            return Err(self.raise_err("tagscope remains!"));
        }
        Ok((code, self.globals, self.string_literals, self.types))
    }
    fn funcargs(&mut self) -> Result<(), ParseError> {
        self.expect("(")?;
        if self.consume("void") {
            self.expect(")")?;
            return Ok(()); // voidはパラメタがないことを意味する。
        }
        if !self.consume(")") {
            loop {
                let ty = self.typespec()?;
                let (name, mut ty) = self.declarator(ty)?;
                ty = self.types.array_to_ptr(ty);
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
        // TODO: 順番入れ替わっても対応
        let is_static = self.consume("static");
        let is_extern = self.consume("extern");
        if is_extern && is_static {
            return Err(self.raise_err("cannot be extern and static at the same time!"));
        }
        let basety = self.typespec()?;
        if self.consume(";") {
            // struct x {};のように構造体タグ宣言のみの場合。
            return Ok(None);
        }
        let (name, ty) = self.declarator(basety.clone())?;
        self.check_global_name(&name)?;
        // 次に関数の有無を見る
        if self.peek("(") {
            // 関数の宣言
            self.enter_scope();
            self.funcargs()?;
            let params: Vec<_> = self.locals.iter().cloned().collect();
            let arg = params.iter().map(|p| p.ty).collect();
            let ty = self.types.function_of(arg, ty);
            self.add_global(name.clone(), ty.clone(), None, is_static, is_extern, false)?;
            if self.consume(";") {
                // int func();のように関数の宣言のみの場合
                self.leave_scope(); // スコープは全く使わずに捨てる
                return Ok(None);
            }
            let stmts = self.compound_stmt(false)?;
            self.leave_scope();
            // dbg!(&self.types);
            // dbg!(&self.locals);
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
        self.global_vardef(name, ty, is_static, is_extern, false)?;
        if !self.consume(";") {
            loop {
                self.expect(",")?;
                let (name, ty) = self.declarator(basety.clone())?;
                self.check_global_name(&name)?;
                self.global_vardef(name, ty, is_static, is_extern, false)?;
                if self.consume(";") {
                    break;
                }
            }
        }
        Ok(None)
    }
    /// declarator = ptr ("(" declarator ")" | ident) ("[" num "]")*
    fn declarator(&mut self, mut ty: TypeRef) -> Result<(String, TypeRef), ParseError> {
        let (name, v) = self.get_name_ptr_dim()?;
        for i in v {
            ty = match i {
                PtrDim::Ptr(depth) => self.types.ptr_recursive(ty, depth),
                PtrDim::Dim(dim) => self.types.array_of(ty, dim),
            }
        }
        let name = if let Some(name) = name {
            name
        } else {
            return Err(self.raise_err("expected ident"));
        };
        Ok((name, ty))
    }
    fn abstruct_declarator(&mut self, mut ty: TypeRef) -> Result<TypeRef, ParseError> {
        let (name, v) = self.get_name_ptr_dim()?;
        for i in v {
            ty = match i {
                PtrDim::Ptr(depth) => self.types.ptr_recursive(ty, depth),
                PtrDim::Dim(dim) => self.types.array_of(ty, dim),
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
    fn typedef(&mut self) -> Result<(), ParseError> {
        let basety = self.typespec()?;
        if !self.consume(";") {
            loop {
                let (name, ty) = self.declarator(basety.clone())?;
                self.add_typdef(name, ty);
                if !self.consume(",") {
                    break; // 宣言終了
                }
            }
            self.expect(";")?;
        };
        Ok(())
    }
    fn global_vardef(
        &mut self,
        name: String,
        ty: TypeRef,
        is_static: bool,
        is_extern: bool,
        is_local: bool,
    ) -> Result<(), ParseError> {
        let init = if self.consume("=") {
            let init = self.initializer()?;
            if let InitKind::List(l) = &init.kind {
                self.types.update_array_length(ty, l.len());
            }
            init.eval(ty, &self.types)
                .map(Some)
                .map_err(|e| self.cast_err(e))?
        } else {
            None
        };
        self.add_global(name, ty, init, is_static, is_extern, is_local)?;
        Ok(())
    }
    fn static_vardef(&mut self) -> Result<(), ParseError> {
        let basety = self.typespec()?;
        loop {
            let (name, ty) = self.declarator(basety.clone())?;
            self.global_vardef(name, ty, true, false, true)?; // static, extern, local
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
                let (name, ty) = self.declarator(basety.clone())?;
                if TypeKind::TyVoid == self.types.get(ty).kind {
                    Err(self.raise_err("variable decleared void"))?
                }
                if self.consume("=") {
                    let init = self.initializer()?;
                    if let InitKind::List(l) = &init.kind {
                        self.types.update_array_length(ty, l.len());
                    }
                    let var = self.add_var(&name, ty);
                    let mut nodes = vec![];
                    self.init_stmt(&var, init, &mut vec![], &mut nodes, var.ty)?;
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
    /// x[1].m[2] = a; というような代入文を生成する。
    /// vには[1, "m", 2]というのが入っていく
    /// 代入先が配列か構造体以外に到達したら、init_leafにvを渡して初期化式を生成し
    /// nodesに渡す。
    fn init_stmt(
        &'a self,
        var: &Rc<Var>,
        i: Initializer,
        v: &mut Vec<Init<'a>>, // 配列や構造体のどの要素を初期化するか記録している
        nodes: &mut Vec<Node>,
        ty: TypeRef,
    ) -> Result<(), ParseError> {
        let Initializer { kind, tok_no } = i;
        // 配列または構造体に代入している時は分ける
        // TODO: union
        match &self.types.get(ty).kind {
            TypeKind::TyArray(Some(len), base) => match kind {
                InitKind::List(list) => {
                    let mut list = list.into_iter(); // 先頭から取り出していく
                    for d in 0..*len {
                        v.push(Init::Index(d));
                        let i = list.next().unwrap_or(Initializer::new_zero(tok_no));
                        self.init_stmt(var, i, v, nodes, *base)?;
                    }
                    if list.next().is_some() {
                        Err(self.raise_err("initializer too long"))?;
                    }
                }
                InitKind::Zero => {
                    for d in 0..*len {
                        v.push(Init::Index(d));
                        self.init_stmt(var, Initializer::new_zero(tok_no), v, nodes, *base)?;
                    }
                }
                _ => Err(self.raise_err("invalid initializer for an array!"))?,
            },
            TypeKind::TyStruct {
                mems,
                is_union: false,
                ..
            } => match kind {
                InitKind::List(list) => {
                    let mut list = list.into_iter(); // 先頭から取り出していく
                    for m in mems {
                        v.push(Init::Member(&m.name));
                        let i = list.next().unwrap_or(Initializer::new_zero(tok_no));
                        self.init_stmt(var, i, v, nodes, m.ty)?;
                    }
                    if list.next().is_some() {
                        Err(self.raise_err("initializer too long"))?;
                    }
                }
                InitKind::Zero => {
                    for m in mems {
                        v.push(Init::Member(&m.name));
                        self.init_stmt(var, Initializer::new_zero(tok_no), v, nodes, m.ty)?;
                    }
                }
                InitKind::Leaf(val) => {
                    nodes.push(self.init_leaf(var.clone(), *val, &v, tok_no)?);
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
                nodes.push(self.init_leaf(var.clone(), val, &v, tok_no)?);
            }
        }
        v.pop(); // 自分の階層のインデックスを捨てる
        Ok(())
    }
    fn init_leaf(
        &self,
        var: Rc<Var>,
        val: Node,
        v: &Vec<Init>,
        tok_no: usize,
    ) -> Result<Node, ParseError> {
        let mut node = Node::new_var(var.clone(), tok_no);
        for d in v {
            node = match d {
                Init::Index(idx) => {
                    let idx = Node::new_num(*idx, tok_no);
                    let node = Node::new_add(node, idx, tok_no, &self.types)
                        .map_err(|e| self.cast_err(e))?;
                    Node::new_deref(node, self.head, &self.types).map_err(|e| self.cast_err(e))?
                }
                Init::Member(name) => self.struct_ref(node, name)?,
            };
        }
        Ok(Node::new_expr_stmt(
            Node::new_assign(node, val, tok_no),
            tok_no,
        ))
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
                self.typedef()?;
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
                let tok = self_.head;
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
        } else if self.consume("do") {
            let stmt = self.stmt()?;
            self.expect("while")?;
            self.expect("(")?;
            let condi = self.expr()?;
            self.expect(")")?;
            self.expect(";")?;
            Node::new_do(stmt, condi, self.head)
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
                ty: TypeRef::STMT,
                tok_no: self.head,
            }
        } else if self.consume("continue") {
            self.expect(";")?;
            Node {
                kind: NodeKind::Continue,
                ty: TypeRef::STMT,
                tok_no: self.head,
            }
        } else if self.consume("goto") {
            let label = self.expect_ident()?;
            self.expect(";")?;
            Node {
                kind: NodeKind::Goto(label),
                ty: TypeRef::STMT,
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
                ty: TypeRef::STMT,
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
                ty: TypeRef::STMT,
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
                ty: TypeRef::STMT,
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
                ty: TypeRef::STMT,
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
                let node2 = self.skip().assign()?;
                to_assign!(Node::new_bin($op, node, node2, self.head, &self.types))
            }};
        }
        Ok(match self.peek_reserved() {
            Some("=") => Node::new_assign(node, self.skip().assign()?, self.head),
            Some("+=") => {
                to_assign!(
                    Node::new_add(node, self.skip().assign()?, self.head, &self.types)
                        .map_err(|e| self.cast_err(e))?
                )
            }
            Some("-=") => {
                to_assign!(
                    Node::new_sub(node, self.skip().assign()?, self.head, &self.types)
                        .map_err(|e| self.cast_err(e))?
                )
            }
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
            let tok_no = self.head;
            let ptr_ty = self.types.ptr_of(lhs.ty);
            let tmp = self.add_var("", ptr_ty);
            let tmp_node = || Node::new_var(tmp.clone(), tok_no);
            let tmp_deref = || Node::new_deref(tmp_node(), tok_no, &self.types).unwrap();

            // tmp = &A
            let line1 = Node::new_assign(tmp_node(), Node::new_addr(*lhs, tok_no, ptr_ty), tok_no);
            // *tmp = *tmp op B
            let line2 = Node::new_assign(
                tmp_deref(),
                Node::new_bin(op, tmp_deref(), *rhs, tok_no, &self.types),
                tok_no,
            );
            Node::new_comma(line1, line2, tok_no)
        } else {
            let msg = format!("line:{:?} at:{}", self.cur_line, 0);
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
            node = Node::new_conditional(node, then_, else_, tok, &self.types);
        }
        Ok(node)
    }
    fn const_expr(&mut self) -> Result<i64, ParseError> {
        self.conditional()?
            .eval(&self.types)
            .map_err(|e| self.cast_err(e))
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
            node = Node::new_bin(BinOp::Or, node, self.bitxor()?, self.head, &self.types)
        }
        Ok(node)
    }
    /// bitxor = bitand ("^" bitand)*
    fn bitxor(&mut self) -> Result<Node, ParseError> {
        let mut node = self.bitand()?;
        while self.consume("^") {
            node = Node::new_bin(BinOp::Xor, node, self.bitand()?, self.head, &self.types)
        }
        Ok(node)
    }
    /// bitand = equality ("&" equality)*
    fn bitand(&mut self) -> Result<Node, ParseError> {
        let mut node = self.equality()?;
        while self.consume("&") {
            node = Node::new_bin(BinOp::And, node, self.equality()?, self.head, &self.types)
        }
        Ok(node)
    }
    fn equality(&mut self) -> Result<Node, ParseError> {
        let mut node = self.relational()?;
        use BinOp::*;
        loop {
            node = match self.peek_reserved() {
                Some("==") => {
                    Node::new_bin(_Eq, node, self.skip().relational()?, self.head, &self.types)
                }
                Some("!=") => {
                    Node::new_bin(Neq, node, self.skip().relational()?, self.head, &self.types)
                }
                _ => return Ok(node),
            }
        }
    }
    /// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
    fn relational(&mut self) -> Result<Node, ParseError> {
        let mut node = self.shift()?;
        use BinOp::*;
        loop {
            node = match self.peek_reserved() {
                Some("<") => Node::new_bin(Lt, node, self.skip().shift()?, self.head, &self.types),
                Some("<=") => Node::new_bin(Le, node, self.skip().shift()?, self.head, &self.types),
                Some(">") => Node::new_bin(Lt, self.skip().shift()?, node, self.head, &self.types),
                Some(">=") => Node::new_bin(Le, self.skip().shift()?, node, self.head, &self.types),
                _ => return Ok(node),
            }
        }
    }
    /// shift = add ("<<" add | ">>" add)*
    fn shift(&mut self) -> Result<Node, ParseError> {
        let mut node = self.add()?;
        use BinOp::*;
        loop {
            node = match self.peek_reserved() {
                Some("<<") => Node::new_bin(Shl, node, self.skip().add()?, self.head, &self.types),
                Some(">>") => Node::new_bin(Shr, node, self.skip().add()?, self.head, &self.types),
                _ => return Ok(node),
            }
        }
    }
    /// add = mul ("+" mul | "-" mul)*
    fn add(&mut self) -> Result<Node, ParseError> {
        let mut node = self.mul()?;
        loop {
            node = match self.peek_reserved() {
                Some("+") => Node::new_add(node, self.skip().mul()?, self.head, &self.types)
                    .map_err(|e| self.cast_err(e))?,
                Some("-") => Node::new_sub(node, self.skip().mul()?, self.head, &self.types)
                    .map_err(|e| self.cast_err(e))?,
                _ => return Ok(node),
            }
        }
    }
    /// mul = cast ("*" cast | "/" cast | "%" cast)*
    fn mul(&mut self) -> Result<Node, ParseError> {
        let mut node = self.cast()?;
        use BinOp::*;
        loop {
            node = match self.peek_reserved() {
                Some("*") => Node::new_bin(Mul, node, self.skip().cast()?, self.head, &self.types),
                Some("/") => Node::new_bin(Div, node, self.skip().cast()?, self.head, &self.types),
                Some("%") => Node::new_bin(Mod, node, self.skip().cast()?, self.head, &self.types),
                _ => return Ok(node),
            };
        }
    }
    /// cast = "(" type-name ")" cast | "(" type-name ")" initializer | unary
    fn cast(&mut self) -> Result<Node, ParseError> {
        if self.consume("(") {
            if self.peek_type() {
                let mut ty = self.typespec()?;
                ty = self.abstruct_declarator(ty)?;
                self.expect(")")?;
                return if self.peek("{") {
                    let init = self.initializer()?;
                    if let InitKind::List(l) = &init.kind {
                        self.types.update_array_length(ty, l.len());
                    }
                    // ローカル変数を生成して一旦そこに代入する
                    // (int[]){11,22,33}
                    // のようになっていたら、
                    // int tmp[] = {11,22,33};
                    // tmp　　に変換する。正確には
                    // ({tmpへの代入コードたち, tmp})に変換する
                    // statement_expressionを使うと良さそう
                    if self.var_scopes.len() == 1 {
                        // グローバル変数の場合
                        let data = init.eval(ty, &self.types).map_err(|e| self.cast_err(e))?;
                        let name = self.new_tmp_gvar_label();
                        let tmp = self.add_global(name, ty, Some(data), false, false, false)?;
                        Ok(Node::new_var(tmp, self.head)) // これはグローバル変数の初期化に渡される。
                    } else {
                        // ローカル変数の場合
                        let tmp = self.add_var("", ty); // int tmp[3]; に対応
                        let mut nodes = vec![];
                        self.init_stmt(&tmp, init, &mut vec![], &mut nodes, ty)?;
                        // 初期化文たちをコンマに入れるために、ダミーのnode_num(0)を挿入
                        let stmt_expr =
                            Node::new_stmt_expr(nodes, Node::new_num(0, self.head), self.head);
                        Ok(Node::new_comma(
                            stmt_expr,
                            Node::new_var(tmp, self.head),
                            self.head,
                        ))
                    }
                } else {
                    Ok(Node::new_cast(ty, self.cast()?, self.head))
                };
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
        match self.peek_reserved() {
            Some("+") => self.skip().cast(),
            Some("-") => Ok(Node::new_bin(
                BinOp::Sub,
                Node::new_num(0, self.head),
                self.skip().cast()?,
                self.head,
                &self.types,
            )),
            Some("*") => {
                let addr = self.skip().cast()?;
                Node::new_deref(addr, self.head, &self.types).map_err(|e| self.cast_err(e))
            }
            Some("&") => {
                let node = self.skip().cast()?;
                Ok(self.new_addr(node))
            }
            Some("~") => Ok(Node::new_unary("bitnot", self.skip().cast()?, self.head)),
            Some("!") => Ok(Node::new_bin(
                BinOp::_Eq,
                self.skip().cast()?,
                Node::new_num(0, self.head),
                self.head,
                &self.types,
            )),
            Some("++") => {
                // ++i => i+=1
                let cast = self.skip().cast()?;
                let tok = self.head;
                Ok(self.to_assign(
                    Node::new_add(cast, Node::new_num(1, self.head), tok, &self.types)
                        .map_err(|e| self.cast_err(e))?,
                ))
            }
            Some("--") => {
                // --i => i-=1
                let cast = self.skip().cast()?;
                let tok = self.head;
                Ok(self.to_assign(
                    Node::new_sub(cast, Node::new_num(1, self.head), tok, &self.types)
                        .map_err(|e| self.cast_err(e))?,
                ))
            }
            _ => self.postfix(),
        }
    }
    fn new_addr(&mut self, node: Node) -> Node {
        let ty = self.types.ptr_of(node.ty);
        Node {
            ty,
            kind: NodeKind::Addr(Box::new(node)),
            tok_no: self.head,
        }
    }
    /// identを受けて構造体のメンバにアクセスする
    fn struct_ref(&self, node: Node, name: &str) -> Result<Node, ParseError> {
        let mem = match self.types.get_struct_mem(node.ty, name) {
            Some(mem) => mem,
            _ => {
                println!("{:?}", self.types);
                Err(self.raise_err(&format!("unknown member {}", name)))?
            }
        };
        Ok(Node::new_member_access(node, mem, self.head))
    }
    /// postfix = primary ("[" expr "]" | "." ident  | "->" ident | "++" | "--")*
    fn postfix(&mut self) -> Result<Node, ParseError> {
        let mut node = self.primary()?;
        loop {
            match self.peek_reserved() {
                Some("[") => {
                    // x[y] は *(x+y)に同じ
                    node = Node::new_add(node, self.skip().expr()?, self.head, &self.types)
                        .map_err(|e| self.cast_err(e))?;
                    node = Node::new_deref(node, self.head, &self.types)
                        .map_err(|e| self.cast_err(e))?;
                    self.expect("]")?;
                }
                Some(".") => {
                    let name = &self.skip().expect_ident()?;
                    node = self.struct_ref(node, name)?;
                }
                Some("->") => {
                    // x->yは(*x).yに同じ
                    let name = &self.skip().expect_ident()?;
                    node = Node::new_deref(node, self.head, &self.types)
                        .map_err(|e| self.cast_err(e))?;
                    node = self.struct_ref(node, name)?;
                }
                Some("++") => node = self.skip().new_inc_dec(node, true),
                Some("--") => node = self.skip().new_inc_dec(node, false),
                _ => return Ok(node),
            }
        }
    }
    /// A++ は (tmp = &A, *tmp = *tmp + 1, *tmp - 1) になる
    /// ++Aと違って、A+=1としたあと、A-1という単なる値が返ることに注意
    fn new_inc_dec(&mut self, node: Node, inc: bool) -> Node {
        let tok_no = self.head;
        let one = || Node::new_num(1, tok_no); // use to inc or dec
        let tmp_ty = self.types.ptr_of(node.ty);
        let tmp = self.add_var("", tmp_ty);
        let tmp_node = || Node::new_var(tmp.clone(), tok_no);
        let n1 = Node::new_assign(tmp_node(), self.new_addr(node), tok_no);
        macro_rules! tmp_deref {
            () => {
                Node::new_deref(tmp_node(), tok_no, &self.types).unwrap()
            };
        }
        let n2: Node;
        let n3: Node;
        if inc {
            let node = Node::new_add(tmp_deref!(), one(), tok_no, &self.types).unwrap();
            n2 = self.to_assign(node);
            n3 = Node::new_sub(tmp_deref!(), one(), tok_no, &self.types).unwrap();
        } else {
            let node = Node::new_sub(tmp_deref!(), one(), tok_no, &self.types).unwrap();
            n2 = self.to_assign(node);
            n3 = Node::new_add(tmp_deref!(), one(), tok_no, &self.types).unwrap();
        }
        Node::new_comma(n1, Node::new_comma(n2, n3, tok_no), tok_no)
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
                    return Ok(Node::new_num(self.types.get(ty).size, self.head));
                } else {
                    self.unshift(); // 型名ではなかったので、戻す
                }
            }
            // このnodeは型のサイズを取得するためのみに使われ、
            // 実際には評価されない
            let node = self.unary()?;
            Ok(Node::new_num(self.types.get(node.ty).size, self.head))
        } else if self.consume("alignof") {
            self.expect("(")?;
            let mut ty = self.typespec()?;
            ty = self.abstruct_declarator(ty)?;
            self.expect(")")?;
            Ok(Node::new_num(self.types.get(ty).align, self.head))
        } else {
            let name = self.expect_ident()?;
            // 関数呼び出し
            if self.consume("(") {
                // 関数呼び出しとわかったので、チェック
                let (mut arg_types, ret) = self
                    .find_func(&name)
                    .ok_or(self.raise_err("function not defined"))?;
                let mut args = vec![];
                while !self.consume(")") {
                    let mut arg = self.assign()?;
                    if let Some(ty) = arg_types.pop() {
                        // 足りない型は無視する(可変長引数未対応)
                        if ty != arg.ty {
                            arg = Node::new_cast(ty, arg, self.head);
                        }
                    }
                    args.push(arg);
                    if !self.consume(",") {
                        self.expect(")")?;
                        break;
                    }
                }
                Ok(Node::new_funcall(ret, name, args, self.head))
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
