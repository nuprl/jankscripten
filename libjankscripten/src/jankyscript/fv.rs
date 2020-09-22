use super::syntax::*;
use im_rc::HashMap;
type IdMap = HashMap<Id, Type>;

fn empty() -> IdMap {
    IdMap::new()
}

fn fv_lv(lv: &mut LValue) -> IdMap {
    use LValue::*;
    match lv {
        Id(x, ty) => IdMap::unit(x.clone(), ty.clone()),
        // the "id" in dot is really a field
        Dot(e, _) => fv_expr(e),
        Bracket(e1, e2) => fv_expr(e1).union(fv_expr(e2)),
    }
}

/// 'var_summary(stmt) = (declared_vars, referenced_vars)'
fn var_summary(stmt: &mut Stmt) -> (IdMap, IdMap) {
    use Stmt::*;
    match stmt {
        Var(x, ty, e) => (IdMap::unit(x.clone(), ty.clone()), fv_expr(e)),
        Block(stmts) => {
            let (declared, referenced): (Vec<_>, Vec<_>) =
                stmts.iter_mut().map(|s| var_summary(s)).unzip();
            (
                IdMap::unions(declared.into_iter()),
                IdMap::unions(referenced.into_iter()),
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

fn fv_stmt(stmt: &mut Stmt) -> IdMap {
    let (declared, referenced) = var_summary(stmt);
    referenced.relative_complement(declared)
}

fn fv_expr(expr: &mut Expr) -> IdMap {
    use Expr::*;
    match expr {
        Id(x, ty) => IdMap::unit(x.clone(), ty.clone()),
        Lit(_) | EnvGet(..) => empty(),
        Object(kvs) => IdMap::unions(kvs.iter_mut().map(|(_, v)| fv_expr(v))),
        Array(es) => IdMap::unions(es.iter_mut().map(|e| fv_expr(e))),
        Dot(e, _) | Unary(_, e) | Coercion(_, e) | NewRef(e, ..) | Deref(e) => fv_expr(e),
        Bracket(e1, e2) => fv_expr(e1).union(fv_expr(e2)),
        Binary(_, e1, e2) => fv_expr(e1).union(fv_expr(e2)),
        Assign(lv, e) => fv_lv(lv).union(fv_expr(e)),
        Call(e, es) => fv_expr(e).union(IdMap::unions(es.iter_mut().map(|arg| fv_expr(arg)))),
        PrimCall(_, es) => IdMap::unions(es.iter_mut().map(|arg| fv_expr(arg))),
        Func(f) => {
            let fv_body = fv_stmt(&mut f.body);
            let fv = fv_body.relative_complement(f.args_with_typs.iter().cloned().collect());
            f.free_vars = fv.clone();
            fv
        }
        Store(..) => panic!("fv happens before box_assigns"),
    }
}

/// Returns the set of free variables in the program. In addition, annotates
/// every function with its set of free variables.
pub fn free_vars(program: &mut Stmt) -> IdMap {
    fv_stmt(program)
}
