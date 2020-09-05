use super::syntax::*;
use std::collections::HashMap;

pub fn while_(test: Atom, body: Stmt) -> Stmt {
    label_("$loop", loop_(if_(test, body, break_("$loop"))))
}

pub fn break_<L: Into<Label>>(l: L) -> Stmt {
    Stmt::Break(l.into())
}

pub fn if_(a: Atom, b: Stmt, c: Stmt) -> Stmt {
    Stmt::If(a, Box::new(b), Box::new(c))
}
pub fn loop_(a: Stmt) -> Stmt {
    Stmt::Loop(Box::new(a))
}
pub fn label_<L: Into<Label>>(a: L, b: Stmt) -> Stmt {
    Stmt::Label(a.into(), Box::new(b))
}
pub fn var_<S: Into<String>>(a: S, b: Expr) -> Stmt {
    Stmt::Var(VarStmt::new(id_(a), b))
}

/// NOTE: This function constructions a `ToAny` that lacks the type annotation
/// that is necessary for code generation. We must use the type-checker to fill
/// in the annotation before trying to generate code.
pub fn get_id_<S: Into<String>>(a: S) -> Atom {
    Atom::Id(id_(a))
}
pub fn to_any_(a: Atom) -> Atom {
    Atom::ToAny(ToAny::new(a))
}
pub fn from_any_(a: Atom, ty: Type) -> Atom {
    Atom::FromAny(Box::new(a), ty)
}
pub fn ht_get_(a: Atom, b: Atom) -> Atom {
    Atom::HTGet(Box::new(a), Box::new(b))
}
pub fn object_get_(a: Atom, b: Atom) -> Atom {
    Atom::ObjectGet(Box::new(a), Box::new(b))
}
pub fn index_(a: Atom, b: Atom) -> Atom {
    Atom::Index(Box::new(a), Box::new(b))
}
pub fn array_len_(a: Atom) -> Atom {
    Atom::ArrayLen(Box::new(a))
}
pub fn i32_(a: i32) -> Atom {
    Atom::Lit(Lit::I32(a))
}
pub fn f64_(a: f64) -> Atom {
    Atom::Lit(Lit::F64(a))
}
pub fn str_<S: Into<String>>(a: S) -> Atom {
    Atom::Lit(Lit::String(a.into()))
}
pub fn binary_(op: BinaryOp, a: Atom, b: Atom) -> Atom {
    Atom::Binary(op, Box::new(a), Box::new(b))
}
pub fn unary_(op: UnaryOp, a: Atom) -> Atom {
    Atom::Unary(op, Box::new(a))
}
pub fn sqrt_(a: Atom) -> Atom {
    Atom::Unary(UnaryOp::Sqrt, Box::new(a))
}
pub fn plus_(a: Atom, b: Atom) -> Atom {
    binary_(BinaryOp::I32Add, a, b)
}
pub fn gte_(a: Atom, b: Atom) -> Atom {
    binary_(BinaryOp::I32Ge, a, b)
}
pub fn lte_(a: Atom, b: Atom) -> Atom {
    binary_(BinaryOp::I32Le, a, b)
}
pub fn not_(a: Atom) -> Atom {
    // TODO(luna): this could be implemented with a UnaryOp i think
    binary_(BinaryOp::I32Sub, i32_(1), a)
}
pub fn band_(a: Atom, b: Atom) -> Atom {
    binary_(BinaryOp::I32And, a, b)
}
pub fn bor_(a: Atom, b: Atom) -> Atom {
    binary_(BinaryOp::I32Or, a, b)
}
pub fn eq_(a: Atom, b: Atom) -> Atom {
    binary_(BinaryOp::I32Eq, a, b)
}
pub fn len_(a: Atom) -> Atom {
    Atom::StringLen(Box::new(a))
}
pub fn deref_(a: Id) -> Atom {
    Atom::Deref(a)
}
pub const TRUE_: Atom = Atom::Lit(Lit::Bool(true));
pub const FALSE_: Atom = Atom::Lit(Lit::Bool(false));
pub fn atom_(a: Atom) -> Expr {
    Expr::Atom(a)
}
pub fn ht_set_(a: Atom, b: Atom, c: Atom) -> Expr {
    Expr::HTSet(a, b, c)
}
pub fn program_(functions: HashMap<Id, Function>) -> Program {
    Program {
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
pub fn program2_(main: Function, other: Function) -> Program {
    let mut functions = HashMap::new();
    functions.insert(id_("main"), main);
    functions.insert(id_("other"), other);
    program_(functions)
}
pub fn test_program_(body: Stmt) -> Program {
    program1_(func_i32_(body))
}
pub fn func_i32_(body: Stmt) -> Function {
    Function {
        body,
        fn_type: FnType {
            args: Vec::new(),
            result: Some(Box::new(Type::I32)),
        },
        params: Vec::new(),
    }
}
pub fn id_<S: Into<String>>(a: S) -> Id {
    Id::Named(a.into())
}
pub fn fn_ty_<I: Into<Option<Type>>>(b: Vec<Type>, a: I) -> Type {
    Type::Fn(FnType {
        args: b,
        result: a.into().map(|b| Box::new(b)),
    })
}
pub fn ref_ty_(a: Type) -> Type {
    Type::Ref(Box::new(a))
}
