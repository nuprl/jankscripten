use super::syntax::*;
use im_rc::HashSet as ImmHashSet;
type IdSet = ImmHashSet<Id>;

fn empty() -> IdSet {
    IdSet::new()
}

trait BothUnion {
    fn union(self, other: Self) -> Self;
}
impl BothUnion for (IdSet, IdSet) {
    fn union(self, other: Self) -> Self {
        (self.0.union(other.0), self.1.union(other.1))
    }
}
fn both_unions<I: Iterator<Item = (IdSet, IdSet)>>(all: I) -> (IdSet, IdSet) {
    let (a, b): (Vec<_>, Vec<_>) = all.unzip();
    (IdSet::unions(a), IdSet::unions(b))
}

/// (fvs, assigns)
fn fv_lv(lv: &mut LValue) -> (IdSet, IdSet) {
    use LValue::*;
    match lv {
        Id(x) => (IdSet::unit(x.clone()), empty()),
        Dot(e, x) => fv_expr(e),
        Bracket(e1, e2) => fv_expr(e1).union(fv_expr(e2)),
    }
}

/// 'var_summary(stmt) = (declared_vars, referenced_vars, assigns)'
fn var_summary(stmt: &mut Stmt) -> (IdSet, (IdSet, IdSet)) {
    use Stmt::*;
    match stmt {
        Var(x, _, e) => (IdSet::unit(x.clone()), fv_expr(e)),
        Block(stmts) => {
            let (declared, ref_assigned): (Vec<_>, Vec<_>) =
                stmts.iter_mut().map(|s| var_summary(s)).unzip();
            let (referenced, assigns): (Vec<_>, Vec<_>) = ref_assigned.into_iter().unzip();
            (
                IdSet::unions(declared.into_iter()),
                (
                    IdSet::unions(referenced.into_iter()),
                    IdSet::unions(assigns.into_iter()),
                ),
            )
        }
        Empty => (empty(), (empty(), empty())),
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
        Break(_) => (empty(), (empty(), empty())),
        Catch(body, exn_name, catch_body) => {
            let (declared_in_body, referenced_in_body) = var_summary(body);
            let (declared_in_catch_body, referenced_in_catch_body) = var_summary(catch_body);
            (
                declared_in_body
                    .union(declared_in_catch_body)
                    .union(IdSet::unit(exn_name.clone())),
                referenced_in_body.union(referenced_in_catch_body),
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

/// (fvs, assigns)
fn fv_stmt(stmt: &mut Stmt) -> (IdSet, IdSet) {
    let (declared, (referenced, assigns)) = var_summary(stmt);
    (referenced.relative_complement(declared), assigns)
}

/// (fvs, assigns)
fn fv_expr(expr: &mut Expr) -> (IdSet, IdSet) {
    use Expr::*;
    match expr {
        Id(x) => (IdSet::unit(x.clone()), empty()),
        Lit(_) => (empty(), empty()),
        Object(kvs) => both_unions(kvs.iter_mut().map(|(_, v)| fv_expr(v))),
        Array(es) => both_unions(es.iter_mut().map(|e| fv_expr(e))),
        Dot(e, _) | Unary(_, e) | Coercion(_, e) | NewRef(e, ..) | Deref(e) => fv_expr(e),
        Bracket(e1, e2) => fv_expr(e1).union(fv_expr(e2)),
        Binary(_, e1, e2) => fv_expr(e1).union(fv_expr(e2)),
        Assign(lv, e) => {
            let (fv, assigns) = fv_lv(lv).union(fv_expr(e));
            (
                fv,
                match &**lv {
                    LValue::Id(x) => assigns.update(x.clone()),
                    _ => assigns,
                },
            )
        }
        Call(e, es) => fv_expr(e).union(both_unions(es.iter_mut().map(|arg| fv_expr(arg)))),
        PrimCall(_, es) => both_unions(es.iter_mut().map(|arg| fv_expr(arg))),
        Func(f) => {
            let (fv_body, assigns_body) = fv_stmt(&mut f.body);
            let fv = fv_body.relative_complement(f.arg_names().cloned().collect());
            f.free_vars = fv.clone();
            (fv, assigns_body)
        }
        Store(id, e) => (IdSet::unit(id.clone()), empty()).union(fv_expr(e)),
    }
}

/// Returns a tuple with the set of all free variables and the set of all
/// variables ever assigned to throughout the program
///
/// In addition, annotates every function with its set of free variables.
pub fn free_vars(program: &mut Stmt) -> (IdSet, IdSet) {
    fv_stmt(program)
}
