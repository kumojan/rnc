// use crate::parse::ParseError;
use crate::util::align_to;
use std::fmt;

#[derive(Debug, Default)]
pub struct TypeError {
    pub msg: String,
}

#[derive(Clone, PartialEq, Debug)]
pub struct EnumMem {
    pub name: String,
    pub val: i64,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct TypeRef(pub usize);
impl TypeRef {
    pub const STMT: Self = TypeRef(0);
    pub const VOID: Self = TypeRef(1);
    pub const BOOL: Self = TypeRef(2);
    pub const CHAR: Self = TypeRef(3);
    pub const SHORT: Self = TypeRef(4);
    pub const INT: Self = TypeRef(5);
    pub const LONG: Self = TypeRef(6);
}
impl fmt::Debug for TypeRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Self::STMT => write!(f, "stmt"),
            Self::VOID => write!(f, "void"),
            Self::BOOL => write!(f, "bool"),
            Self::CHAR => write!(f, "char"),
            Self::SHORT => write!(f, "short"),
            Self::INT => write!(f, "int"),
            Self::LONG => write!(f, "long"),
            TypeRef(i) => write!(f, "ref({})", i),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Member {
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
    TyStruct { mems: Vec<Member>, is_union: bool },
    TyEnum(Vec<EnumMem>),
    TyFunc { args: Vec<TypeRef>, ret: TypeRef },
    Placeholder, // 不完全型
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
}

#[derive(PartialEq, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub size: usize,
    pub align: usize,
    pub base: Option<TypeRef>,
}
impl Type {
    fn new(kind: TypeKind, size: usize) -> Self {
        Self {
            kind,
            size,
            align: size,
            base: None,
        }
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
    list: Vec<Type>,
}
impl Default for TypeList {
    fn default() -> Self {
        use TypeKind::*;
        let list = vec![
            Type::new(Stmt, 0),
            Type::new(TyVoid, 0),
            Type::new(TyBool, 1),
            Type::new(TyChar, 1),
            Type::new(TyShort, 2),
            Type::new(TyInt, 4),
            Type::new(TyLong, 8),
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
    pub fn add_new(&mut self, ty: Type) -> TypeRef {
        self.list.push(ty);
        self.last()
    }
    pub fn get(&self, i: TypeRef) -> &Type {
        if i == TypeRef::STMT {
            panic!() // stmtを参照することはあってはいけない
        }
        &self.list[i.0]
    }
    pub fn is_integer(&self, ty: TypeRef) -> bool {
        self.get(ty).is_integer()
    }
    pub fn is_ptr(&self, ty: TypeRef) -> bool {
        self.get(ty).is_ptr()
    }
    pub fn is_func(&self, ty: TypeRef) -> bool {
        self.get(ty).is_func()
    }
    pub fn function_of(&mut self, args: Vec<TypeRef>, ret: TypeRef) -> TypeRef {
        let kind = TypeKind::TyFunc { args, ret };
        self.add_new(Type::new(kind, 0))
    }
    pub fn ptr_of(&mut self, ty: TypeRef) -> TypeRef {
        let index = self.list.iter().position(|t| match t.kind {
            TypeKind::TyPtr(_ty) => ty == _ty,
            _ => false,
        });
        if let Some(index) = index {
            TypeRef(index)
        } else {
            self.add_new(Type {
                kind: TypeKind::TyPtr(ty),
                size: 8,
                align: 8,
                base: Some(ty),
            })
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
            let ty = Type {
                kind: TypeKind::TyArray(len, ty),
                size: self.get(ty).size * len.unwrap_or(0), // 長さのない配列のサイズはゼロとする
                align: self.get(ty).align,
                base: Some(ty),
            };
            self.add_new(ty)
        }
    }
    pub fn new_placeholder(&mut self) -> TypeRef {
        self.add_new(Type::new(TypeKind::Placeholder, 0))
    }
    pub fn new_struct_union(
        &mut self,
        mut mems: Vec<Member>,
        is_union: bool,
        replace_with: Option<TypeRef>,
    ) -> TypeRef {
        let align = mems.iter().map(|m| self.get(m.ty).align).max().unwrap_or(1);
        let size = if is_union {
            align_to(
                mems.iter().map(|m| self.get(m.ty).size).max().unwrap_or(1),
                align,
            )
        } else {
            let mut offset = 0;
            for m in mems.iter_mut() {
                offset = align_to(offset, self.get(m.ty).align); // 新しく追加される型のアライメントにoffsetを合わせる
                m.offset = offset; // メンバのoffsetを設定
                offset += self.get(m.ty).size; // メンバのサイズだけoffsetをずらす
            }
            align_to(offset, align)
        };
        let kind = TypeKind::TyStruct { mems, is_union };
        let ty = Type {
            kind,
            size,
            align,
            base: None,
        };
        if let Some(ty_) = replace_with {
            self.list[ty_.0] = ty;
            return ty_;
        } else {
            self.add_new(ty)
        }
    }
    pub fn add_enum(&mut self, mems: Vec<EnumMem>) -> TypeRef {
        self.add_new(Type {
            kind: TypeKind::TyEnum(mems),
            size: 4,
            align: 4,
            base: None,
        })
    }
    pub fn update_array_length(&mut self, ty: TypeRef, len: usize) {
        let base_size = self.get(ty).base.map(|b| self.get(b).size).unwrap_or(0);
        if let Type {
            kind: TypeKind::TyArray(ref mut _len, ..),
            ref mut size,
            ..
        } = self.list[ty.0]
        {
            if _len.is_none() {
                *_len = Some(len);
                *size = len * base_size;
            }
        }
    }
    pub fn array_to_ptr(&mut self, ty: TypeRef) -> TypeRef {
        match self.get(ty).kind {
            TypeKind::TyArray(.., base) => self.ptr_of(base),
            _ => ty,
        }
    }
    pub fn get_base_kind(&self, ty: TypeRef) -> Result<&Type, TypeError> {
        let base = match self.get(ty).kind {
            TypeKind::TyArray(.., base) => base,
            TypeKind::TyPtr(base) => base,
            _ => Err(TypeError {
                msg: "cannot dereference".to_owned(),
            })?,
        };
        Ok(self.get(base))
    }
    pub fn get_struct_mem(&self, ty: TypeRef, name: &str) -> Option<&Member> {
        match &self.get(ty).kind {
            TypeKind::TyStruct { mems, .. } => mems.iter().filter(|m| m.name == name).next(),
            _ => None,
        }
    }
    pub fn common_ty(&self, ty1: TypeRef, ty2: TypeRef) -> TypeRef {
        // TODO: いろいろ不十分だと思う。
        if ty1 == TypeRef::VOID || ty2 == TypeRef::VOID {
            TypeRef::VOID
        } else if self.get(ty1).size == 8 || self.get(ty2).size == 8 {
            TypeRef::LONG
        } else {
            TypeRef::INT
        }
    }
}
