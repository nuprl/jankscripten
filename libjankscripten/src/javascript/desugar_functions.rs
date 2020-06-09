use super::syntax::*;
use super::walk::*;
use super::parser::parse;
use super::*;
use resast::BinaryOp;
use super::constructors::*;

// We name all functions using “var”. Note that this involves “lifting” function declarations.
// We name the result of all function applications.


struct NameFunctions <'a> { ng: &'a mut NameGen }

fn lval_to_expr(lv: &mut LValue) -> Expr {
    match lv {
        LValue::Id(x) => id_(x.clone()),
        LValue::Dot(e, x) => dot_(e.clone(), x.clone()),
        LValue::Bracket(e1, e2) => bracket_(e1.clone(), e2.clone())
    }
}

fn desugar_assign_op(bin_op: BinaryOp, lv: &mut LValue, rhs: &mut Expr, loc: &mut Loc, ng: &mut NameGen) -> Expr {
    match lv {
        LValue::Dot(obj, id) => {
            match obj {
                Expr::Call(fid, args) => {
                    let ctx = if let Loc::Node(Context::Block(ctx), ..) = loc {
                        ctx
                    } else {
                        panic!("expected block context");
                    };
                    let temp_id = ng.fresh("temp");
                    ctx.insert(ctx.index, vardecl1_(temp_id.clone(), obj.clone()));
                    *lv = lval_dot_(id_(temp_id), id.clone());
                }
                _ => {}
            }
        } 
        _ => {}
    }
    assign_(
        lv.clone(),
        binary_(
            BinOp::BinaryOp(bin_op),
            lval_to_expr(lv),
            rhs.take()))
}

impl Visitor for NameFunctions<'_> {
    fn exit_stmt(&mut self, stmt: &mut Stmt, loc: &mut Loc) {
        match stmt {
            _ => {}
        }
    }
}


pub fn simpl(program: &mut Stmt) {
    let mut namegen = NameGen::default();
    let mut v = NameFunctions {ng: &mut namegen};
    program.walk(&mut v);
}

#[test]
fn anon_fn() {
    let prog = parse(r#"
        var f = function (a, b) {return a * b};
    "#).unwrap();
    println!("{:?}", prog);
}