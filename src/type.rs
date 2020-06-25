// use crate::parse::ParseError;
use crate::util::align_to;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Default)]
pub struct TypeError {
    pub msg: String,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Member {
    pub name: String,
    pub ty: Type,
    pub offset: usize,
}

#[derive(Clone, PartialEq, Debug)]
pub struct EnumMem {
    pub name: String,
    pub val: i64,
}

#[allow(dead_code)]
#[derive(Clone, PartialEq)]
pub enum Type {
    TyVoid,
    TyBool,
    TyShort,
    TyInt,
    TyLong,
    TyChar,
    TyPtr(Box<Type>),
    TyArray {
        base: Box<Type>,
        len: Option<usize>,
    },
    TyStruct {
        mems: Box<Vec<Member>>,
        size: usize,
        align: usize,
        is_union: bool,
    },
    TyEnum {
        mems: Rc<Vec<EnumMem>>,
    },
    TyFunc {
        arg: Box<Vec<Type>>,
        ret: Box<Type>,
    },
    IncompleteStruct {
        tag: Option<String>,
        resolved_type: Rc<RefCell<Option<Type>>>,
    },
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::TyVoid => write!(f, "void"),
            Type::TyBool => write!(f, "bool"),
            Type::TyChar => write!(f, "char"),
            Type::TyShort => write!(f, "short"),
            Type::TyInt => write!(f, "int"),
            Type::TyLong => write!(f, "long"),
            Type::TyPtr(ty) => write!(f, "*{:?}", ty),
            Type::TyArray { base, len } => write!(
                f,
                "[{}]{:?}",
                len.map(|n| n.to_string()).unwrap_or_default(),
                base
            ),
            Type::TyStruct { mems, .. } => write!(f, "struct {{{:?}}}", mems),
            Type::TyEnum { .. } => write!(f, "enum"),
            Type::TyFunc { .. } => write!(f, "func"),
            t @ Type::IncompleteStruct { .. } => {
                write!(f, "incomplete resolved {:?}", !t.is_incomplete())
            }
        }
    }
}
impl Type {
    // pub fn new_short() -> Self {
    //     Self::TyShort
    // }
    // pub fn new_int() -> Self {
    //     Self::TyInt
    // }
    // pub fn new_long() -> Self {
    //     Self::TyLong
    // }
    // pub fn new_char() -> Self {
    //     Self::TyChar
    // }
    pub fn is_ptr(&self) -> bool {
        match self {
            Type::TyPtr(..) => true,
            _ => false,
        }
    }
    pub fn is_ptr_like(&self) -> bool {
        self.is_ptr() || self.is_array()
    }
    pub fn is_array(&self) -> bool {
        match self {
            Type::TyArray { .. } => true,
            _ => false,
        }
    }
    pub fn is_func(&self) -> bool {
        match self {
            Type::TyFunc { .. } => true,
            _ => false,
        }
    }
    pub fn to_ptr(self) -> Self {
        Type::TyPtr(Box::new(self))
    }
    pub fn to_complete_array(self, len: usize) -> Self {
        Type::TyArray {
            len: Some(len),
            base: Box::new(self),
        }
    }
    pub fn to_array(self, len: Option<usize>) -> Self {
        Type::TyArray {
            len,
            base: Box::new(self),
        }
    }
    pub fn to_ptr_recursive(mut self, depth: u8) -> Self {
        for _ in 0..depth {
            self = self.to_ptr();
        }
        self
    }
    pub fn new_incomplete_struct(
        tag: Option<String>,
        to_resolve: Rc<RefCell<Option<Type>>>,
    ) -> Self {
        Self::IncompleteStruct {
            tag,
            resolved_type: to_resolve,
        }
    }
    pub fn new_enum(mems: Vec<EnumMem>) -> Self {
        Self::TyEnum {
            mems: Rc::new(mems),
        }
    }
    pub fn get_base(&self) -> Result<&Self, TypeError> {
        let base = match self {
            Type::TyPtr(base) => base,
            Type::TyArray { base, .. } => base,
            _ => Err(TypeError {
                msg: "cannot dereference".to_owned(),
            })?,
        };
        Ok(base)
    }
    pub fn size(&self) -> usize {
        match self {
            Type::TyVoid => unimplemented!(),
            Type::TyBool => 1,
            Type::TyChar => 1,
            Type::TyShort => 2,
            Type::TyInt => 4,
            Type::TyEnum { .. } => 4,
            Type::TyLong | Type::TyPtr(..) => 8,
            Type::TyArray { base, len } => base.size() * len.unwrap(),
            Type::TyStruct { size, .. } => *size,
            Type::IncompleteStruct { resolved_type, .. } => match &*resolved_type.borrow() {
                Some(Type::TyStruct { size, .. }) => *size,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
    pub fn align(&self) -> usize {
        match self {
            Type::TyVoid => unimplemented!(),
            Type::TyBool => 1,
            Type::TyChar => 1,
            Type::TyShort => 2,
            Type::TyInt => 4,
            Type::TyEnum { .. } => 4,
            Type::TyLong | Type::TyPtr(..) => 8,
            Type::TyArray { base, .. } => base.align(),
            Type::TyStruct { align, .. } => *align,
            Type::IncompleteStruct { resolved_type, .. } => match &*resolved_type.borrow() {
                Some(Type::TyStruct { align, .. }) => *align,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }
    pub fn cast_int(&self) -> Self {
        match self {
            Type::TyBool | Type::TyChar | Type::TyShort | Type::TyInt | Type::TyEnum { .. } => {
                Type::TyInt
            }
            Type::TyLong => Type::TyLong,
            _ => unimplemented!(),
        }
    }
    pub fn is_incomplete(&self) -> bool {
        match self {
            Type::IncompleteStruct { resolved_type, .. } => match &*resolved_type.borrow() {
                Some(Type::TyStruct { .. }) => false,
                _ => true,
            },
            _ => false,
        }
    }
    pub fn get_struct_mem(&self, name: &str) -> Option<Member> {
        match self {
            Type::TyStruct { mems, .. } => mems.iter().filter(|m| m.name == name).next().cloned(),
            Type::IncompleteStruct { resolved_type, .. } => match &*resolved_type.borrow() {
                Some(Type::TyStruct { mems, .. }) => {
                    mems.iter().filter(|m| m.name == name).next().cloned()
                }
                _ => None,
            },
            _ => None,
        }
    }
    pub fn is_integer(&self) -> bool {
        match self {
            Type::TyBool
            | Type::TyChar
            | Type::TyShort
            | Type::TyInt
            | Type::TyLong
            | Type::TyEnum { .. } => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct TypeRef(pub usize);
impl TypeRef {
    pub const Stmt: Self = TypeRef(0);
    pub const Void: Self = TypeRef(1);
    pub const Bool: Self = TypeRef(2);
    pub const Char: Self = TypeRef(3);
    pub const Short: Self = TypeRef(4);
    pub const Int: Self = TypeRef(5);
    pub const Long: Self = TypeRef(6);
    fn to_ptr(self) -> Type2 {
        Type2 {
            kind: TypeKind::TyPtr(self),
            size: 8,
            align: 8,
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Member2 {
    pub name: String,
    pub ty: TypeRef,
    pub offset: usize,
}

#[derive(PartialEq, Debug)]
pub enum TypeKind {
    Stmt, // 式ではなく文
    TyVoid,
    TyBool,
    TyShort,
    TyInt,
    TyLong,
    TyChar,
    TyPtr(TypeRef),
    TyArray(Option<usize>, TypeRef),
    TyStruct { mems: Vec<Member2>, is_union: bool },
    TyEnum(Vec<EnumMem>),
    TyFunc { args: Vec<TypeRef>, ret: TypeRef },
    TyIncomplete, // 不完全型
}
impl TypeKind {
    pub fn is_integer(&self) -> bool {
        use TypeKind::*;
        match self {
            TyVoid | TyBool | TyShort | TyInt | TyLong | TyChar | TyEnum(..) => true,
            _ => false,
        }
    }
    pub fn is_ptr(&self) -> bool {
        match self {
            TypeKind::TyPtr(..) => true,
            _ => false,
        }
    }
    pub fn is_array(&self) -> bool {
        match self {
            TypeKind::TyArray(..) => true,
            _ => false,
        }
    }
    pub fn is_ptr_like(&self) -> bool {
        self.is_ptr() || self.is_array()
    }
    pub fn is_func(&self) -> bool {
        match self {
            TypeKind::TyFunc { .. } => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Type2 {
    pub kind: TypeKind,
    pub size: usize,
    pub align: usize,
}
impl Type2 {
    fn new(kind: TypeKind, size: usize, align: usize) -> Self {
        Self { kind, size, align }
    }
    pub fn is_integer(&self) -> bool {
        use TypeKind::*;
        match self.kind {
            TyVoid | TyBool | TyShort | TyInt | TyLong | TyChar | TyEnum(..) => true,
            _ => false,
        }
    }
    pub fn is_ptr(&self) -> bool {
        match self.kind {
            TypeKind::TyPtr(..) => true,
            _ => false,
        }
    }
    pub fn is_array(&self) -> bool {
        match self.kind {
            TypeKind::TyArray(..) => true,
            _ => false,
        }
    }
    pub fn is_ptr_like(&self) -> bool {
        self.is_ptr() || self.is_array()
    }
    pub fn is_struct(&self) -> bool {
        match self.kind {
            TypeKind::TyStruct { .. } => true,
            _ => false,
        }
    }
    pub fn is_func(&self) -> bool {
        match self.kind {
            TypeKind::TyFunc { .. } => true,
            _ => false,
        }
    }
}

/// あらゆる型を保持し、参照&Typeを貸し出す構造体
/// これ以外のオブジェクトはこのリスト上のインデックス
/// として型を保持する
pub struct TypeList {
    list: Vec<Type2>,
}
impl Default for TypeList {
    fn default() -> Self {
        use TypeKind::*;
        let list = vec![
            Type2::new(Stmt, 0, 0),
            Type2::new(TyVoid, 0, 0),
            Type2::new(TyBool, 1, 1),
            Type2::new(TyChar, 1, 1),
            Type2::new(TyShort, 2, 2),
            Type2::new(TyInt, 4, 4),
            Type2::new(TyLong, 8, 8),
        ];
        Self { list }
    }
}
impl fmt::Debug for TypeList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, ty) in self.list.iter().enumerate() {
            writeln!(f, "{}: {:?}", i, ty)?;
        }
        Ok(())
    }
}
impl TypeList {
    fn last(&self) -> TypeRef {
        TypeRef(self.list.len() - 1)
    }
    pub fn add_new(&mut self, ty: Type2) -> TypeRef {
        self.list.push(ty);
        self.last()
    }
    pub fn get(&self, i: TypeRef) -> &Type2 {
        &self.list[i.0]
    }
    pub fn is_integer(&self, ty: TypeRef) -> bool {
        self.get(ty).is_integer()
    }
    pub fn is_ptr(&self, ty: TypeRef) -> bool {
        self.get(ty).is_ptr()
    }
    pub fn is_array(&self, ty: TypeRef) -> bool {
        self.get(ty).is_array()
    }
    pub fn is_ptr_like(&self, ty: TypeRef) -> bool {
        self.get(ty).is_ptr_like()
    }
    pub fn is_func(&self, ty: TypeRef) -> bool {
        self.get(ty).is_func()
    }
    pub fn is_struct(&self, ty: TypeRef) -> bool {
        self.get(ty).is_struct()
    }
    pub fn function_of(&mut self, args: Vec<TypeRef>, ret: TypeRef) -> TypeRef {
        let kind = TypeKind::TyFunc { args, ret };
        self.add_new(Type2 {
            kind,
            size: 0,
            align: 0,
        })
    }
    pub fn ptr_of(&mut self, ty: TypeRef) -> TypeRef {
        let index = self.list.iter().position(|t| match t.kind {
            TypeKind::TyPtr(_ty) => ty == _ty,
            _ => false,
        });
        if let Some(index) = index {
            TypeRef(index)
        } else {
            self.add_new(ty.to_ptr())
        }
    }
    pub fn ptr_recursive(&mut self, mut ty: TypeRef, depth: usize) -> TypeRef {
        for _ in 0..depth {
            ty = self.ptr_of(ty);
        }
        ty
    }
    pub fn array_of(&mut self, ty: TypeRef, len: Option<usize>) -> TypeRef {
        let index = self.list.iter().position(|t| match t.kind {
            TypeKind::TyArray(_len, base) => base == ty && _len == len,
            _ => false,
        });
        if let Some(index) = index {
            TypeRef(index)
        } else {
            let ty = Type2 {
                kind: TypeKind::TyArray(len, ty),
                size: self.get(ty).size * len.unwrap_or(0), // 長さのない配列のサイズはゼロとする
                align: self.get(ty).align,
            };
            self.add_new(ty)
        }
    }
    pub fn add_incomplete(&mut self) -> TypeRef {
        self.add_new(Type2 {
            kind: TypeKind::TyIncomplete,
            size: 0,
            align: 0,
        })
    }
    pub fn new_struct(&self, mut mems: Vec<Member2>) -> Type2 {
        let mut offset = 0;
        for m in mems.iter_mut() {
            offset = align_to(offset, self.get(m.ty).align); // 新しく追加される型のアライメントにoffsetを合わせる
            m.offset = offset; // メンバのoffsetを設定
            offset += self.get(m.ty).size; // メンバのサイズだけoffsetをずらす
        }
        let align = mems.iter().map(|m| self.get(m.ty).align).max().unwrap_or(1);
        let kind = TypeKind::TyStruct {
            mems,
            is_union: false,
        };
        Type2 {
            kind,
            align,
            size: align_to(offset, align),
        }
    }
    pub fn update_array_length(&mut self, ty: TypeRef, len: usize) {
        if let TypeKind::TyArray(ref mut _len, ..) = self.list[ty.0].kind {
            *_len = Some(_len.unwrap_or(len));
        }
    }
    pub fn array_to_ptr(&mut self, ty: TypeRef) -> TypeRef {
        match self.get(ty).kind {
            TypeKind::TyArray(.., base) => self.ptr_of(base),
            _ => ty,
        }
    }
    pub fn get_base(&self, ty: TypeRef) -> Result<TypeRef, TypeError> {
        match self.get(ty).kind {
            TypeKind::TyArray(.., base) => Ok(base),
            TypeKind::TyPtr(base) => Ok(base),
            _ => Err(TypeError {
                msg: "cannot dereference".to_owned(),
            }),
        }
    }
    pub fn get_base_kind(&self, ty: TypeRef) -> Result<&Type2, TypeError> {
        let base = match self.get(ty).kind {
            TypeKind::TyArray(.., base) => base,
            TypeKind::TyPtr(base) => base,
            _ => Err(TypeError {
                msg: "cannot dereference".to_owned(),
            })?,
        };
        Ok(self.get(base))
    }
    pub fn get_struct_mem(&self, ty: TypeRef, name: &str) -> Option<&Member2> {
        match &self.get(ty).kind {
            TypeKind::TyStruct { mems, .. } => mems.iter().filter(|m| m.name == name).next(),
            _ => None,
        }
    }
    pub fn replace(&mut self, ty: TypeRef, ty_: Type2) {
        self.list[ty.0] = ty_;
    }
}
