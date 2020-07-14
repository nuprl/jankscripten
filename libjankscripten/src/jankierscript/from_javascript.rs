use super::super::javascript::{syntax as Js};
use super::syntax::*;

// if any of these functions panic, desugaring was not run or failed!
/// sfsfdsfdsfsfsdflkdsjafhlasfhjaksjf

fn lvalue(lval: Js::LValue) -> LValue {
    use Js::{LValue as Js};
    use LValue::*;
    match lval {
        Js::Id(id) => Id(id),
        Js::Dot(e, x) => Dot(expr(e), x),
        Js::Bracket(e1, e2) => Bracket(expr(e1), expr(e2))
    }
}

fn expr(e: Js::Expr) -> Expr {
    use Js::{Expr as Js};
    use Expr::*;
    match e {
        Js::Lit(lit) => Lit(lit),
        Js::Array(es) => Array(es.into_iter().map(|e| expr(e)).collect()),
        Js::Object(kvs) => Object(kvs.into_iter().map(|(k, e)| (k, expr(e))).collect()),
        Js::This => This,
        Js::Id(id) => Id(id),
        Js::Dot(e, x) => Dot(Box::new(expr(*e)), x),
        Js::Bracket(e1, e2) => Bracket(Box::new(expr(*e1)), Box::new(expr(*e2))),
        // We need to think about New
        Js::New(_, _) => todo!(),
        Js::Unary(op, e) => Unary(op, Box::new(expr(*e))),
        Js::Binary(op, e1, e2) => Binary(op, Box::new(expr(*e1)), Box::new(expr(*e2))),
        Js::UnaryAssign(_, _) => panic!(),
        Js::If(_, _, _) => panic!(),
        Js::Assign(_, lval, e) => Assign(Box::new(lvalue(*lval)), Box::new(expr(*e))),
        Js::Call(e, es) => Call(Box::new(expr(*e)),
            es.into_iter().map(|e| expr(e)).collect()),
        Js::Func(_, args, body) =>
            Func(None, args.into_iter().map(|x| (x, None)).collect(), Box::new(stmt(*body))),
        Js::Seq(_) => panic!(),    
    }
}

pub fn stmt(s: Js::Stmt) -> Stmt {
    use Js::{Stmt as Js};
    use Stmt::*;
    match s {
        Js::Block(stmts) => Block(stmts.into_iter().map(|s| stmt(s)).collect()),
        Js::Empty => Empty,
        Js::Expr(e) => panic!(),
        Js::If(c, t, e) => If(Box::new(expr(*c)), Box::new(stmt(*t)), Box::new(stmt(*e))),
        Js::ForIn(_, _, _, _) => todo!(),
        Js::Label(x, s) => Label(x, Box::new(stmt(*s))),
        Js::Break(x) => Break(x.unwrap()),
        Js::Continue(_) => panic!(),
        Js::Switch(_, _, _) => panic!(),
        Js::While(c, body) => While(Box::new(expr(*c)), Box::new(stmt(*body))),
        Js::DoWhile(stmt, cond) => panic!(),
        Js::For(_, _, _, _) => panic!(),
        Js::Catch(try_body, exception_name, catch_body) => 
            Catch(Box::new(stmt(*try_body)), exception_name, Box::new(stmt(*catch_body))),
        Js::Finally(try_body, finally_body) => 
            Finally(Box::new(stmt(*try_body)), Box::new(stmt(*finally_body))),
        Js::Throw(e) => Throw(Box::new(expr(*e))),
        Js::VarDecl(mut decls) => vardecl(decls.remove(0)),
        Js::Func(_, _, _) => panic!(),
        Js::Return(e) => Return(Box::new(expr(*e)))
    }
}

pub fn vardecl(decl: Js::VarDecl) -> Stmt {
    Stmt::Var(decl.name, None, Box::new(expr(*decl.named)))
}