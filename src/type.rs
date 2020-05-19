use crate::parse::ParseError;
use std::fmt;

// TODO: 型の実装
#[derive(Clone, Copy, PartialEq)]
pub enum TypeKind {
    TyInt,
}
impl TypeKind {
    pub fn kind(self) -> Type {
        Type {
            kind: self,
            depth: 0,
        }
    }
}
impl fmt::Debug for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeKind::TyInt => write!(f, "int"),
        }
    }
}
#[derive(Clone, Copy)]
pub struct Type {
    pub kind: TypeKind,
    depth: u8, // ポインタなら1, ポインタのポインタなら2, ...
}
impl Type {
    pub fn new(kind: TypeKind) -> Self {
        Self { kind, depth: 0 }
    }
    pub fn is_ptr(&self) -> bool {
        self.depth > 0
    }
    pub fn to_ptr(&self) -> Self {
        Self {
            kind: self.kind,
            depth: self.depth + 1,
        }
    }
    pub fn deref(&self) -> Self {
        let mut d = self.clone();
        if d.depth == 0 {
            unimplemented!();
        } else {
            d.depth -= 1;
            d
        }
    }
}
impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{:?}", "*".repeat(self.depth as usize), self.kind,)
    }
}
