use super::syntax::*;
use im_rc::HashSet as ImmHashSet;
type IdSet = ImmHashSet<Id>;

fn empty() -> IdSet {
    IdSet::new()
}

fn fv_lv(lv: &mut LValue) -> IdSet {
    use LValue::*;
    match lv {
        Id(x) => IdSet::unit(x.clone()),
        Dot(e, x) => fv_expr(e),
        Bracket(e1, e2) => fv_expr(e1).union(fv_expr(e2)),
    }
}

/// 'var_summary(stmt) = (declared_vars, referenced_vars)'
fn var_summary(stmt: &mut Stmt) -> (IdSet, IdSet) {
    use Stmt::*;
    match stmt {
        Var(x, _, e) => (IdSet::unit(x.clone()), fv_expr(e)),
        Block(stmts) => {
            let (declared, referenced): (Vec<_>, Vec<_>) =
                stmts.iter_mut().map(|s| var_summary(s)).unzip();
            (
                IdSet::unions(declared.into_iter()),
                IdSet::unions(referenced.into_iter()),
            )
        }
        Empty => (empty(), empty()),
        Expr(e) => (empty(), fv_expr(e)),
        If(cond, true_part, false_part) => {
            let (declared_in_true, referenced_in_true) = var_summary(true_part);
            let (declared_in_false, referenced_in_false) = var_summary(false_part);
            (
                declared_in_true.union(declared_in_false),
                fv_expr(cond)
                    .union(referenced_in_true)
                    .union(referenced_in_false),
            )
        }
        Loop(s) => var_summary(s),
        Break(_) => (empty(), empty()),
        Catch(body, exn_name, catch_body) => {
            let (declared_in_body, referenced_in_body) = var_summary(body);
            let (declared_in_catch_body, referenced_in_catch_body) = var_summary(catch_body);
            (
                declared_in_body.union(declared_in_catch_body),
                referenced_in_body.union(referenced_in_catch_body.without(exn_name)),
            )
        }
        Finally(body, finally_body) => {
            let (declared_in_body, referenced_in_body) = var_summary(body);
            let (declared_in_finally_body, referenced_in_finally_body) = var_summary(finally_body);
            (
                declared_in_body.union(declared_in_finally_body),
                referenced_in_body.union(referenced_in_finally_body),
            )
        }
        Label(_, s) => var_summary(s),
        Throw(e) => (empty(), fv_expr(e)),
        Return(e) => (empty(), fv_expr(e)),
    }
}

fn fv_stmt(stmt: &mut Stmt) -> IdSet {
    let (declared, referenced) = var_summary(stmt);
    referenced.relative_complement(declared)
}

fn fv_expr(expr: &mut Expr) -> IdSet {
    use Expr::*;
    match expr {
        Id(x) => IdSet::unit(x.clone()),
        Lit(_) => empty(),
        Object(kvs) => IdSet::unions(kvs.iter_mut().map(|(_, v)| fv_expr(v))),
        Array(es) => IdSet::unions(es.iter_mut().map(|e| fv_expr(e))),
        This => empty(),
        Dot(e, _) => fv_expr(e),
        Bracket(e1, e2) => fv_expr(e1).union(fv_expr(e2)),
        Unary(_, e) => fv_expr(e),
        Binary(_, e1, e2) => fv_expr(e1).union(fv_expr(e2)),
        Assign(lv, e) => fv_lv(lv).union(fv_expr(e)),
        New(e, es) | Call(e, es) => {
            fv_expr(e).union(IdSet::unions(es.iter_mut().map(|arg| fv_expr(arg))))
        }
        PrimCall(_, es) => IdSet::unions(es.iter_mut().map(|arg| fv_expr(arg))),
        Coercion(_, e) => fv_expr(e),
        Func(f) => {
            let fv_body = fv_stmt(&mut f.body);
            let fv = fv_body.relative_complement(f.arg_names().cloned().collect());
            f.free_vars = fv.clone();
            fv
        }
    }
}

/// Returns the set of free variables in the program. In addition, annotates
/// every function with its set of free variables.
pub fn free_vars(program: &mut Stmt) -> IdSet {
    fv_stmt(program)
}
