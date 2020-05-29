use super::syntax::*;
use super::walk::*;
use super::parser::parse;
use resast::BinaryOp;

struct MyVisitor { }

fn lval_to_expr(lv: &mut LValue) -> Expr {
    match lv {
        LValue::Id(x) => Expr::Id(x.clone()),
        LValue::Dot(e, x) => Expr::Dot(Box::new(e.clone()), x.clone()),
        LValue::Bracket(e1, e2) => Expr::Bracket(Box::new(e1.clone()), Box::new(e2.clone()))
    }
}

fn desugar_assign_op(bin_op: BinaryOp, lv: &mut LValue, rhs: &mut Expr) -> Expr {
    Expr::Assign(
        AssignOp::Equal,
        Box::new(lv.clone()),
        Box::new(
            Expr::Binary(
                BinOp::BinaryOp(bin_op),
                Box::new(lval_to_expr(lv)),
                Box::new(rhs.take()))))
}

impl Visitor for MyVisitor {
    fn exit_expr(&mut self, expr: &mut Expr, _loc: &mut Loc) {
        match expr {
            Expr::Assign(AssignOp::Equal, _lv, _rhs) => { }
            Expr::Assign(AssignOp::PlusEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Plus, lv, rhs);
            },
            Expr::Assign(AssignOp::MinusEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Minus, lv, rhs);
            },
            Expr::Assign(AssignOp::TimesEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Times, lv, rhs);
            },
            Expr::Assign(AssignOp::DivEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Over, lv, rhs);
            },
            Expr::Assign(AssignOp::ModEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Mod, lv, rhs);
            },
            Expr::Assign(AssignOp::LeftShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::LeftShift, lv, rhs);
            },
            Expr::Assign(AssignOp::RightShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::RightShift, lv, rhs);
            },
            Expr::Assign(AssignOp::UnsignedRightShiftEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::UnsignedRightShift, lv, rhs);
            },
            Expr::Assign(AssignOp::OrEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::Or, lv, rhs);
            },
            Expr::Assign(AssignOp::XOrEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::XOr, lv, rhs);
            },
            Expr::Assign(AssignOp::AndEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::And, lv, rhs);
            },
            Expr::Assign(AssignOp::PowerOfEqual, lv, rhs) => {
                *expr = desugar_assign_op(BinaryOp::PowerOf, lv, rhs);
            }
            _ => {}
        }
    }
}


pub fn simpl(program: &mut Stmt) {
    let mut v = MyVisitor { };
    program.walk(&mut v);
}

#[test]
fn parse_pluseq() {    
    let mut prog = parse(r#"
        x += 1;
    "#).unwrap();
    simpl(&mut prog);

    let result = parse(r#"
        x = x + 1;
    "#).unwrap();
    
    assert_eq!(prog, result);
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
