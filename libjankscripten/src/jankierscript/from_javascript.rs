use super::super::javascript::constructors as Js_;
use super::super::javascript::syntax as Js;
use super::syntax::*;

fn unexpected(e: impl std::fmt::Debug) -> ! {
    panic!("JavaScript is not desugared correctly. Found {:?}", e);
}

fn lvalue(lval: Js::LValue) -> LValue {
    use Js::LValue as Js;
    use LValue::*;
    match lval {
        Js::Id(id) => Id(id),
        Js::Dot(e, x) => Dot(expr(e), x),
        Js::Bracket(e1, e2) => Bracket(expr(e1), expr(e2)),
    }
}

fn expr(e: Js::Expr) -> Expr {
    use Expr::*;
    use Js::{BinOp, Expr as E};
    match e {
        E::Lit(lit, s) => Lit(lit),
        E::Array(es, s) => Array(es.into_iter().map(|e| expr(e)).collect()),
        E::Object(kvs, s) => Object(kvs.into_iter().map(|(k, e, s)| (k, expr(e))).collect()),
        E::This => unexpected(&e),
        E::Id(id, s) => Id(id),
        E::Dot(e, x, s) => Dot(Box::new(expr(*e)), x),
        E::Bracket(e1, e2, s) => Bracket(Box::new(expr(*e1)), Box::new(expr(*e2))),
        E::New(_, _, s) => unexpected(&e),
        E::Unary(op, e, s) => Unary(op, Box::new(expr(*e))),
        E::Binary(BinOp::BinaryOp(op), e1, e2, s) => {
            Binary(op, Box::new(expr(*e1)), Box::new(expr(*e2)))
        }
        E::Binary(BinOp::LogicalOp(_), _, _, s) => unexpected(&e),
        E::UnaryAssign(_, _, s) => unexpected(&e),
        // Note that this is the ternary operator, not an if statement.
        E::If(_, _, _, s) => unexpected(&e),
        E::Assign(_, lval, e, s) => Assign(Box::new(lvalue(*lval)), Box::new(expr(*e))),
        E::Call(e, es, s) => Call(
            Box::new(expr(*e)),
            es.into_iter().map(|e| expr(e)).collect(),
        ),
        E::Func(_, args, body, s) => Func(
            None,
            args.into_iter().map(|x| (x, None)).collect(),
            Box::new(stmt(*body)),
        ),
        E::Seq(_, s) => unexpected(&e),
    }
}

fn stmt(s: Js::Stmt) -> Stmt {
    use Js::Stmt as Js;
    use Stmt::*;
    match s {
        Js::Block(stmts, s) => Block(stmts.into_iter().map(|s| stmt(s)).collect()),
        Js::Empty => Empty,
        Js::Expr(e, s) => Expr(Box::new(expr(*e))),
        Js::If(c, t, e, s) => If(Box::new(expr(*c)), Box::new(stmt(*t)), Box::new(stmt(*e))),
        Js::ForIn(_, _, _, _, s) => todo!("for in loops"),
        Js::Label(x, st, s) => Label(x, Box::new(stmt(*st))),
        Js::Break(x, s) => Break(x.unwrap()),
        Js::Continue(_, s) => unexpected(&s),
        Js::Switch(_, _, _, s) => unexpected(&s),
        Js::While(c, body, s) => {
            assert_eq!(*c, Js_::TRUE_, "desugaring should have removed while loops");
            Loop(Box::new(stmt(*body)))
        }
        Js::DoWhile(_, _, s) => unexpected(&s),
        Js::For(_, _, _, _, s) => unexpected(&s),
        Js::Catch(try_body, exception_name, catch_body, s) => Catch(
            Box::new(stmt(*try_body)),
            exception_name,
            Box::new(stmt(*catch_body)),
        ),
        Js::Finally(try_body, finally_body, s) => {
            Finally(Box::new(stmt(*try_body)), Box::new(stmt(*finally_body)))
        }
        Js::Throw(e, s) => Throw(Box::new(expr(*e))),
        Js::VarDecl(mut decls, s) => {
            if decls.len() != 1 {
                unexpected("multi-variable declaration");
            }
            vardecl(decls.remove(0))
        }
        Js::Func(_, _, _, s) => unexpected(&s),
        Js::Return(e, s) => Return(Box::new(expr(*e))),
    }
}

fn vardecl(decl: Js::VarDecl) -> Stmt {
    Stmt::Var(decl.name, None, Box::new(expr(*decl.named)))
}

/// Consumes a desugared JavaScript AST and transforms it into JavaScript.
pub fn from_javascript(js_prog: Js::Stmt) -> Stmt {
    stmt(js_prog)
}
