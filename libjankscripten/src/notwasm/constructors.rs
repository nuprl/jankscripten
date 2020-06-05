use super::syntax::*;
use std::collections::HashMap;

pub fn get_id_<S: Into<String>>(a: S, ty: Type) -> Atom {
    Atom::Id(id_(a), ty)
}
pub fn ht_get_(a: Atom, b: Atom, ty: Type) -> Atom {
    Atom::HTGet(Box::new(a), Box::new(b), ty)
}
pub fn ht_set_(a: Atom, b: Atom, c: Atom, ty: Type) -> Atom {
    Atom::HTSet(Box::new(a), Box::new(b), Box::new(c), ty)
}
pub fn i32_(a: i32) -> Atom {
    Atom::Lit(Lit::I32(a), Type::I32)
}
pub fn atom_(a: Atom) -> Expr {
    let ty = a.get_type();
    Expr::Atom(a, ty)
}
pub fn program_(functions: HashMap<Id, Function>) -> Program {
    Program {
        classes: HashMap::new(),
        functions,
        globals: HashMap::new(),
    }
}
pub fn program1_(func: Function) -> Program {
    let mut functions = HashMap::new();
    functions.insert(id_("main"), func);
    program_(functions)
}
pub fn test_program_(body: Stmt) -> Program {
    program1_(Function {
        locals: Vec::new(),
        body,
        ty: Type::Fn(vec![], Box::new(Type::I32)),
    })
}
pub fn id_<S: Into<String>>(a: S) -> Id {
    Id::Named(a.into())
}
pub fn ht_ty_(a: Type) -> Type {
    Type::HT(Box::new(a))
}
pub fn fn_ty_(b: Vec<Type>, a: Type) -> Type {
    Type::Fn(b, Box::new(a))
}
