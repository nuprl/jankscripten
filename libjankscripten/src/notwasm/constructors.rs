use super::syntax::*;
use std::collections::HashMap;

pub fn while_(test: Atom, body: Stmt) -> Stmt {
    label_(id_("$loop"), loop_(if_(test, body, break_(id_("$loop")))))
}

pub fn break_(id: Id) -> Stmt {
    Stmt::Break(id)
}

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
pub fn str_<S: Into<String>>(a: S) -> Atom {
    Atom::Lit(Lit::String(a.into()))
}
pub fn binary_(op: BinaryOp, a: Atom, b: Atom) -> Atom {
    Atom::Binary(op, Box::new(a), Box::new(b))
}
pub fn plus_(a: Atom, b: Atom) -> Atom {
    binary_(BinaryOp::I32Add, a, b, ty)
}
pub fn gt_(a: Atom, b: Atom) -> Atom {
    binary_(BinaryOp::I32Eq, a, b)
}
pub fn len_(a: Atom) -> Atom {
    Atom::StringLen(Box::new(a))
}
pub fn atom_(a: Atom) -> Expr {
    Expr::Atom(a)
}
pub fn ht_set_(a: Atom, b: Key, c: Atom, ty: Type) -> Expr {
    Expr::HTSet(a, b, c, ty)
}
pub fn program_(functions: HashMap<Id, Function>) -> Program {
    Program {
        classes: HashMap::new(),
        functions,
        globals: HashMap::new(),
        data: Vec::new(),
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
