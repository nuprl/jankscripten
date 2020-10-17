use super::syntax::*;
use std::collections::HashMap;

pub fn while_(test: Atom, body: Stmt, s: Span) -> Stmt {
    label_("$loop", loop_(if_(test, body, break_("$loop", s), s), s), s)
}

pub fn break_<L: Into<Label>>(l: L, s: Span) -> Stmt {
    Stmt::Break(l.into(), s)
}

pub fn if_(a: Atom, b: Stmt, c: Stmt, s: Span) -> Stmt {
    Stmt::If(a, Box::new(b), Box::new(c), s)
}
pub fn loop_(a: Stmt, s: Span) -> Stmt {
    Stmt::Loop(Box::new(a), s)
}
pub fn label_<L: Into<Label>>(a: L, b: Stmt, s: Span) -> Stmt {
    Stmt::Label(a.into(), Box::new(b), s)
}
pub fn var_<S: Into<String>>(a: S, b: Expr, s: Span) -> Stmt {
    Stmt::Var(VarStmt::new(id_(a), b), s)
}

/// NOTE: This function constructions a `ToAny` that lacks the type annotation
/// that is necessary for code generation. We must use the type-checker to fill
/// in the annotation before trying to generate code.
pub fn get_id_<S: Into<String>>(a: S, s: Span) -> Atom {
    Atom::Id(id_(a), s)
}
pub fn to_any_(a: Atom, s: Span) -> Atom {
    Atom::ToAny(ToAny::new(a), s)
}
pub fn from_any_(a: Atom, ty: Type, s: Span) -> Atom {
    Atom::FromAny(Box::new(a), ty, s)
}
pub fn ht_get_(a: Atom, b: Atom, s: Span) -> Atom {
    Atom::HTGet(Box::new(a), Box::new(b), s)
}
pub fn object_get_(a: Atom, b: Atom, s: Span) -> Atom {
    Atom::ObjectGet(Box::new(a), Box::new(b), s)
}
pub fn index_(a: Atom, b: Atom, s: Span) -> Atom {
    Atom::Index(Box::new(a), Box::new(b), s)
}
pub fn array_len_(a: Atom, s: Span) -> Atom {
    Atom::ArrayLen(Box::new(a), s)
}
pub fn i32_(a: i32, s: Span) -> Atom {
    Atom::Lit(Lit::I32(a), s)
}
pub fn f64_(a: f64, s: Span) -> Atom {
    Atom::Lit(Lit::F64(a), s)
}
pub fn str_<S: Into<String>>(a: S, s: Span) -> Atom {
    Atom::Lit(Lit::String(a.into()), s)
}
pub fn binary_(op: BinaryOp, a: Atom, b: Atom, s: Span) -> Atom {
    Atom::Binary(op, Box::new(a), Box::new(b), s)
}
pub fn unary_(op: UnaryOp, a: Atom, s: Span) -> Atom {
    Atom::Unary(op, Box::new(a), s)
}
pub fn sqrt_(a: Atom, s: Span) -> Atom {
    Atom::Unary(UnaryOp::Sqrt, Box::new(a), s)
}
pub fn plus_(a: Atom, b: Atom, s: Span) -> Atom {
    binary_(BinaryOp::I32Add, a, b, s)
}
pub fn gte_(a: Atom, b: Atom, s: Span) -> Atom {
    binary_(BinaryOp::I32Ge, a, b, s)
}
pub fn lte_(a: Atom, b: Atom, s: Span) -> Atom {
    binary_(BinaryOp::I32Le, a, b, s)
}
pub fn not_(a: Atom, s: Span) -> Atom {
    // TODO(luna, s): this could be implemented with a UnaryOp i think
    binary_(BinaryOp::I32Sub, i32_(1, s), a, s)
}
pub fn band_(a: Atom, b: Atom, s: Span) -> Atom {
    binary_(BinaryOp::I32And, a, b, s)
}
pub fn bor_(a: Atom, b: Atom, s: Span) -> Atom {
    binary_(BinaryOp::I32Or, a, b, s)
}
pub fn eq_(a: Atom, b: Atom, s: Span) -> Atom {
    binary_(BinaryOp::I32Eq, a, b, s)
}
pub fn len_(a: Atom, s: Span) -> Atom {
    Atom::StringLen(Box::new(a), s)
}
pub fn deref_(a: Atom, b: Type, s: Span) -> Atom {
    Atom::Deref(Box::new(a), b, s)
}
pub const TRUE_: Atom = Atom::Lit(Lit::Bool(true), DUMMY_SP);
pub const FALSE_: Atom = Atom::Lit(Lit::Bool(false), DUMMY_SP);
pub fn atom_(a: Atom, s: Span) -> Expr {
    Expr::Atom(a, s)
}
pub fn ht_set_(a: Atom, b: Atom, c: Atom, s: Span) -> Expr {
    Expr::HTSet(a, b, c, s)
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
    program1_(func_i32_(body, DUMMY_SP))
}
pub fn func_i32_(body: Stmt, _s: Span) -> Function {
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
pub fn fn_type_<I: Into<Option<Type>>>(b: Vec<Type>, a: I) -> FnType {
    FnType {
        args: b,
        result: a.into().map(|b| Box::new(b)),
    }
}
pub fn fn_ty_<I: Into<Option<Type>>>(b: Vec<Type>, a: I) -> Type {
    Type::Fn(fn_type_(b, a))
}
pub fn clos_ty_<I: Into<Option<Type>>>(b: Vec<Type>, a: I) -> Type {
    Type::Closure(fn_type_(b, a))
}
pub fn ref_ty_(a: Type) -> Type {
    Type::Ref(Box::new(a))
}
