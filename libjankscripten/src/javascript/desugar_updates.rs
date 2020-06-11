use super::syntax::*;
use super::walk::*;
use super::parser::parse;
use super::*;
use resast::BinaryOp;
use super::constructors::*;

//note: depends on function naming from desugar-functions in order to handle f().x += 1; cases
// x += 1;    =>    x = x + 1;

struct DesugarFancyUpdates <'a>(&'a mut NameGen);

impl Visitor for DesugarFancyUpdates<'_> {
    fn exit_expr(&mut self, expr: &mut Expr, loc: &Loc) {
        match expr {
            Expr::Assign(AssignOp::Equal, _lv, _rhs) => { }
            Expr::Assign(AssignOp::PlusEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Plus, lv, rhs, loc, self.0);
            },
            Expr::Assign(AssignOp::MinusEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Minus, lv, rhs, loc, self.0);
            },
            Expr::Assign(AssignOp::TimesEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Times, lv, rhs, loc, self.0);
            },
            Expr::Assign(AssignOp::DivEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Over, lv, rhs, loc, self.0);
            },
            Expr::Assign(AssignOp::ModEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Mod, lv, rhs, loc, self.0);
            },
            Expr::Assign(AssignOp::LeftShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::LeftShift, lv, rhs, loc, self.0);
            },
            Expr::Assign(AssignOp::RightShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::RightShift, lv, rhs, loc, self.0);
            },
            Expr::Assign(AssignOp::UnsignedRightShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::UnsignedRightShift, lv, rhs, loc, self.0);
            },
            Expr::Assign(AssignOp::OrEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Or, lv, rhs, loc, self.0);
            },
            Expr::Assign(AssignOp::XOrEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::XOr, lv, rhs, loc, self.0);
            },
            Expr::Assign(AssignOp::AndEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::And, lv, rhs, loc, self.0);
            },
            Expr::Assign(AssignOp::PowerOfEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::PowerOf, lv, rhs, loc, self.0);
            }
            _ => {}
        }
    }
}

fn lval_to_expr(lv: &mut LValue) -> Expr {
    match lv {
        LValue::Id(x) => id_(x.clone()),
        LValue::Dot(e, x) => dot_(e.clone(), x.clone()),
        LValue::Bracket(e1, e2) => bracket_(e1.clone(), e2.clone())
    }
}

fn desugar_assign_op(bin_op: BinaryOp, lv: &mut LValue, rhs: &mut Expr, loc: &Loc, ng: &mut NameGen) -> Expr {
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

pub fn simpl(program: &mut Stmt) {
    let mut ng = NameGen::default();
    let mut v = DesugarFancyUpdates(&mut ng);
    program.walk(&mut v);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::javascript::testing::*;
    //NOTE: cannot run this test until function naming is complete
    #[test]
    fn parse_pluseq() {    
        let mut prog = parse(r#"
            var MyObject =  { x: 1};
            function f () {
                return MyObject;
            }
            f().x += 1;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            var MyObject =  { x: 1};
            function f () {
                return MyObject;
            }
            var temp = f();
            temp.x = temp.x + 1;
        "#).unwrap();
        
        //assert_eq!(prog, result);
    }

    #[test]
    fn parse_minuseq() {
        let mut prog = parse(r#"
            x -= 1;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            x = x - 1;
        "#).unwrap();
        
        assert_eq!(prog, result);
    }

    #[test]
    fn parse_timeseq() {
        let mut prog = parse(r#"
            x *= 1;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            x = x * 1;
        "#).unwrap();
        
        assert_eq!(prog, result);
    }

    #[test]
    fn parse_diveq() {
        let mut prog = parse(r#"
            x /= 1;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            x = x / 1;
        "#).unwrap();
        
        assert_eq!(prog, result);
    }

    #[test]
    fn parse_modeq() {
        let mut prog = parse(r#"
            x %= 1;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            x = x % 1;
        "#).unwrap();
        
        assert_eq!(prog, result);
    }

    #[test]
    fn parse_lshifteq() {
        let mut prog = parse(r#"
            x <<= 1;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            x = x << 1;
        "#).unwrap();
        
        assert_eq!(prog, result);
    }

    #[test]
    fn parse_rshifteq() {
        let mut prog = parse(r#"
            x >>= 1;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            x = x >> 1;
        "#).unwrap();
        
        assert_eq!(prog, result);
    }

    #[test]
    fn parse_unsignedrshifteq() {
        let mut prog = parse(r#"
            x >>>= 1;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            x = x >>> 1;
        "#).unwrap();
        
        assert_eq!(prog, result);
    }

    #[test]
    fn parse_expeq() {
        let mut prog = parse(r#"
            x **= 1;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            x = x ** 1;
        "#).unwrap();
        
        assert_eq!(prog, result);
    }

    #[test]
    fn parse_oreq() {
        let mut prog = parse(r#"
            x |= true;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            x = x | true;
        "#).unwrap();
        
        assert_eq!(prog, result);
    }

    #[test]
    fn parse_xoreq() {
        let mut prog = parse(r#"
            x ^= true;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            x = x ^ true;
        "#).unwrap();
        
        assert_eq!(prog, result);
    }

    #[test]
    fn parse_andeq() {
        let mut prog = parse(r#"
            x &= true;
        "#).unwrap();
        simpl(&mut prog);

        let result = parse(r#"
            x = x & true;
        "#).unwrap();

        assert_eq!(prog, result);
    }
}