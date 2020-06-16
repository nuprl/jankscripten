use super::syntax::*;
use std::collections::HashMap;

pub fn if_(a: Atom, b: Stmt, c: Stmt) -> Stmt {
    Stmt::If(a, Box::new(b), Box::new(c))
}
pub fn loop_(a: Stmt) -> Stmt {
    Stmt::Loop(Box::new(a))
}
pub fn label_(a: Id, b: Stmt) -> Stmt {
    Stmt::Label(a, Box::new(b))
}
pub fn get_id_<S: Into<String>>(a: S) -> Atom {
    Atom::Id(id_(a))
}
pub fn ht_get_(a: Atom, b: Key, ty: Type) -> Atom {
    Atom::HTGet(Box::new(a), b, ty)
}
pub fn i32_(a: i32) -> Atom {
    Atom::Lit(Lit::I32(a))
}
pub fn binary_(op: BinaryOp, a: Atom, b: Atom, ty: Type) -> Atom {
    Atom::Binary(op, Box::new(a), Box::new(b), ty)
}
pub fn atom_(a: Atom) -> Expr {
    Expr::Atom(a)
}
pub fn ht_set_(a: Atom, b: Key, c: Atom, ty: Type) -> Expr {
    Expr::HTSet(Box::new(a), b, Box::new(c), ty)
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
        params_tys: Vec::new(),
        ret_ty: Type::I32,
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
