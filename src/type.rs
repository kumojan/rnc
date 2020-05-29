// use crate::parse::ParseError;
use std::fmt;

#[derive(Debug, Default)]
pub struct TypeError {
    pub msg: String,
}

#[allow(dead_code)]
#[derive(Clone, PartialEq)]
pub enum Type {
    TyInt,
    TyChar,
    TyPtr(Box<Type>),
    TyArray { base: Box<Type>, len: usize },
    TyFunction { arg: Box<Vec<Type>>, ret: Box<Type> },
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::TyInt => write!(f, "int"),
            Type::TyPtr(ty) => write!(f, "*{:?}", ty),
            Type::TyArray { base, len } => write!(f, "[{}]{:?}", len, base),
            _ => write!(f, "function"),
        }
    }
}
impl Type {
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
    pub fn to_ptr(&self) -> Self {
        Type::TyPtr(Box::new(self.clone()))
    }
    pub fn to_array(&self, len: usize) -> Self {
        Type::TyArray {
            len,
            base: Box::new(self.clone()),
        }
    }
    pub fn to_ptr_recursive(&self, depth: u8) -> Self {
        let mut ty = self.clone();
        for _ in 0..depth {
            ty = ty.to_ptr();
        }
        ty
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
            _ => unimplemented!(),
        }
    }
}
