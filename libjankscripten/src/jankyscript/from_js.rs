use super::super::javascript::syntax as Js;
use super::super::shared::Type;
use super::constructors::*;
use super::syntax::*;
use crate::pos::Pos;

fn unexpected(e: impl std::fmt::Debug) -> ! {
    panic!("JavaScript is not desugared correctly. Found {:?}", e);
}

fn lvalue(lval: Js::LValue) -> LValue {
    use Js::LValue as Js;
    use LValue::*;
    match lval {
        Js::Id(id) => Id(id, Type::Missing),
        Js::Dot(e, x) => Dot(expr(e), x),
        Js::Bracket(e1, e2) => Bracket(expr(e1), expr(e2)),
    }
}

fn expr(e: Js::Expr) -> Expr {
    use Js::{BinOp, Expr as E};
    match e {
        E::Lit(lit, s) => Expr::Lit(lit, s),
        E::Array(es, s) => Expr::Array(es.into_iter().map(|e| expr(e)).collect(), s),
        E::Object(kvs, s) => Expr::Object(kvs.into_iter().map(|(k, e)| (k, expr(e))).collect(), s),
        E::This => unexpected(e),
        E::Id(id, s) => Expr::Id(id, Type::Missing, s),
        E::Dot(e, x, s) => Expr::Dot(Box::new(expr(*e)), x, s),
        E::Bracket(e1, e2, s) => Expr::Bracket(Box::new(expr(*e1)), Box::new(expr(*e2)), s),
        E::New(_, _, _) => unexpected(e),
        E::Unary(op, e, s) => Expr::JsOp(JsOp::Unary(op), vec![expr(*e)], s),
        E::Binary(BinOp::BinaryOp(op), e1, e2, s) => {
            Expr::JsOp(JsOp::Binary(op), vec![expr(*e1), expr(*e2)], s)
        }
        E::Binary(BinOp::LogicalOp(_), _, _, _) => unexpected(e),
        E::UnaryAssign(_, _, _) => unexpected(e),
        // Note that this is the ternary operator, not an if statement.
        E::If(_, _, _, _) => unexpected(e),
        E::Assign(_, lval, e, s) => Expr::Assign(Box::new(lvalue(*lval)), Box::new(expr(*e)), s),
        E::Call(e, es, s) => Expr::Call(
            Box::new(expr(*e)),
            es.into_iter().map(|e| expr(e)).collect(),
            s,
        ),
        E::Func(_, args, body, s) => func(
            args.into_iter().map(|x| (x, Type::Missing)).collect(),
            Type::Missing,
            stmt(*body),
            s,
        ),
        E::Seq(_, _) => unexpected(e),
    }
}

fn stmt(s: Js::Stmt) -> Stmt {
    use Js::Stmt as S;
    use Stmt::*;
    match s {
        S::Block(stmts, s) => Block(stmts.into_iter().map(|s| stmt(s)).collect(), s),
        S::Empty => Empty,
        S::Expr(e, s) => Expr(Box::new(expr(*e)), s),
        S::If(c, t, e, s) => If(
            Box::new(expr(*c)),
            Box::new(stmt(*t)),
            Box::new(stmt(*e)),
            s,
        ),
        S::ForIn(is_var, bind, container, body, s) => {
            assert!(!is_var, "for..in was not desugared to not use var");
            for_in_(bind, expr(*container), stmt(*body), s)
        }
        S::Label(x, st, s) => Label(x, Box::new(stmt(*st)), s),
        S::Break(x, s) => Break(x.unwrap(), s),
        S::Continue(_, s) => unexpected(&s),
        S::Switch(_, _, _, s) => unexpected(&s),
        S::While(c, body, s) => {
            if let Js::Expr::Lit(Lit::Bool(true), _) = *c {
                panic!("desugaring should have removed while loops");
            }
            Loop(Box::new(stmt(*body)), s)
        }
        S::DoWhile(_, _, s) => unexpected(&s),
        S::For(_, _, _, _, s) => unexpected(&s),
        S::Catch(try_body, exception_name, catch_body, s) => Catch(
            Box::new(stmt(*try_body)),
            exception_name,
            Box::new(stmt(*catch_body)),
            s,
        ),
        S::Finally(try_body, finally_body, s) => {
            Finally(Box::new(stmt(*try_body)), Box::new(stmt(*finally_body)), s)
        }
        S::Throw(e, s) => Throw(Box::new(expr(*e)), s),
        S::VarDecl(mut decls, s) => {
            if decls.len() != 1 {
                unexpected("multi-variable declaration");
            }
            vardecl(decls.remove(0), s)
        }
        S::Func(_, _, _, s) => unexpected(&s),
        S::Return(e, s) => Return(Box::new(expr(*e)), s),
    }
}

fn vardecl(decl: Js::VarDecl, s: Pos) -> Stmt {
    Stmt::Var(decl.name, Type::Missing, Box::new(expr(*decl.named)), s)
}

/// Consumes a desugared JavaScript AST and transforms it into JavaScript.
pub fn from_javascript(js_prog: Js::Stmt) -> Stmt {
    stmt(js_prog)
}
