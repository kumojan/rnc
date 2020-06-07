// use crate::parse::ParseError;
use std::fmt;

#[derive(Debug, Default)]
pub struct TypeError {
    pub msg: String,
}

#[derive(Clone, PartialEq)]
pub struct Member {
    pub name: String,
    pub ty: Type,
    pub offset: usize,
}

#[allow(dead_code)]
#[derive(Clone, PartialEq)]
pub enum Type {
    TyVoid,
    TyShort,
    TyInt,
    TyLong,
    TyChar,
    TyPtr(Box<Type>),
    TyArray {
        base: Box<Type>,
        len: usize,
    },
    TyStruct {
        name: Option<String>,
        mem: Box<Vec<Member>>,
        size: usize,
        align: usize,
    },
    TyFunction {
        arg: Box<Vec<Type>>,
        ret: Box<Type>,
    },
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::TyVoid => write!(f, "void"),
            Type::TyChar => write!(f, "char"),
            Type::TyShort => write!(f, "short"),
            Type::TyInt => write!(f, "int"),
            Type::TyLong => write!(f, "long"),
            Type::TyPtr(ty) => write!(f, "*{:?}", ty),
            Type::TyArray { base, len } => write!(f, "[{}]{:?}", len, base),
            Type::TyStruct { .. } => write!(f, "struct"),
            Type::TyFunction { .. } => write!(f, "function"),
        }
    }
}
impl Type {
    pub fn new_short() -> Self {
        Self::TyShort
    }
    pub fn new_int() -> Self {
        Self::TyInt
    }
    pub fn new_long() -> Self {
        Self::TyLong
    }
    pub fn new_char() -> Self {
        Self::TyChar
    }
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
    pub fn to_ptr(self) -> Self {
        Type::TyPtr(Box::new(self))
    }
    pub fn to_array(self, len: usize) -> Self {
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
    pub fn get_base(&self) -> Result<Self, TypeError> {
        let base = match self {
            Type::TyPtr(base) => *base.clone(),
            Type::TyArray { base, .. } => *base.clone(),
            _ => Err(TypeError {
                msg: "cannot dereference".to_owned(),
            })?,
        };
        if base == Type::TyVoid {
            Err(TypeError {
                msg: "dereferencing void pointer".to_owned(),
            })?;
        }
        Ok(base)
    }
    pub fn size(&self) -> usize {
        match self {
            Type::TyVoid => unimplemented!(),
            Type::TyChar => 1,
            Type::TyShort => 2,
            Type::TyInt => 4,
            Type::TyLong | Type::TyPtr(..) => 8,
            Type::TyArray { base, len } => base.size() * len,
            Type::TyStruct { size, .. } => *size,
            _ => unimplemented!(),
        }
    }
    pub fn align(&self) -> usize {
        match self {
            Type::TyVoid => unimplemented!(),
            Type::TyChar => 1,
            Type::TyShort => 2,
            Type::TyInt => 4,
            Type::TyLong | Type::TyPtr(..) => 8,
            Type::TyArray { base, .. } => base.align(),
            Type::TyStruct { align, .. } => *align,
            _ => unimplemented!(),
        }
    }
}
