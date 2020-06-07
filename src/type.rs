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
    TyInt,
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
            Type::TyInt => write!(f, "int"),
            Type::TyPtr(ty) => write!(f, "*{:?}", ty),
            Type::TyArray { base, len } => write!(f, "[{}]{:?}", len, base),
            Type::TyStruct { .. } => write!(f, "struct"),
            _ => write!(f, "function"),
        }
    }
}
impl Type {
    pub fn new_int() -> Self {
        Self::TyInt
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
        match self {
            Type::TyPtr(base) => Ok(*base.clone()),
            Type::TyArray { base, .. } => Ok(*base.clone()),
            _ => Err(TypeError {
                msg: "cannot dereference".to_owned(),
            }),
        }
    }
    pub fn size(&self) -> usize {
        match self {
            Type::TyChar => 1,
            Type::TyInt => 8,
            Type::TyPtr(..) => 8,
            Type::TyArray { base, len } => base.size() * len,
            Type::TyStruct { size, .. } => *size,
            _ => unimplemented!(),
        }
    }
    pub fn align(&self) -> usize {
        match self {
            Type::TyChar => 1,
            Type::TyInt => 8,
            Type::TyPtr(..) => 8,
            Type::TyArray { base, .. } => base.align(),
            Type::TyStruct { align, .. } => *align,
            _ => unimplemented!(),
        }
    }
}
