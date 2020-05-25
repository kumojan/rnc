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
    TyPtr(Box<Type>),
    TyArray { ty: Box<Type>, len: usize },
    TyFunction { arg: Box<Vec<Type>>, ret: Box<Type> },
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::TyInt => write!(f, "int"),
            Type::TyPtr(ty) => write!(f, "*{:?}", ty),
            Type::TyArray { ty, len } => write!(f, "{:?}[{}]", ty, len),
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
    pub fn to_ptr(&self) -> Self {
        Type::TyPtr(Box::new(self.clone()))
    }
    pub fn to_ptr_recursive(&self, depth: u8) -> Self {
        let mut ty = self.clone();
        for _ in 0..depth {
            ty = ty.to_ptr();
        }
        ty
    }
    pub fn deref(&self) -> Result<Self, TypeError> {
        match self {
            Type::TyPtr(ty) => Ok(*ty.clone()),
            _ => Err(TypeError {
                msg: "cannot dereference".to_owned(),
            }),
        }
    }
    pub fn size(&self) -> usize {
        match self {
            Type::TyInt => 8,
            Type::TyPtr(..) => 8,
            Type::TyArray { ty, len } => ty.size() * len,
            _ => unimplemented!(),
        }
    }
}
