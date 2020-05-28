use super::syntax::*;
use super::walk::*;
use resast::BinaryOp;

struct MyVisitor { }

fn lval_to_expr(lv: &mut LValue) -> Expr {
    match lv {
        LValue::Id(x) => Expr::Id(x.clone()),
        LValue::Dot(e, x) => Expr::Dot(Box::new(e.clone()), x.clone()),
        LValue::Bracket(e1, e2) => Expr::Bracket(Box::new(e1.clone()), Box::new(e2.clone()))
    }
}

impl Visitor for MyVisitor {
    fn exit_expr(&mut self, expr: &mut Expr, _loc: &mut Loc) {
        match expr {
            Expr::Assign(AssignOp::Equal, _lv, _rhs) => { }
            Expr::Assign(AssignOp::PlusEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::Plus),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            Expr::Assign(AssignOp::MinusEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::Minus),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            Expr::Assign(AssignOp::TimesEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::Times),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            Expr::Assign(AssignOp::DivEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::Over),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            Expr::Assign(AssignOp::ModEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::Mod),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            Expr::Assign(AssignOp::LeftShiftEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::LeftShift),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            Expr::Assign(AssignOp::RightShiftEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::RightShift),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            Expr::Assign(AssignOp::UnsignedRightShiftEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::UnsignedRightShift),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            Expr::Assign(AssignOp::OrEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::Or),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            Expr::Assign(AssignOp::XOrEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::XOr),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            Expr::Assign(AssignOp::AndEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::And),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
            },
            Expr::Assign(AssignOp::PowerOfEqual, lv, rhs) => {
                *expr = Expr::Assign(
                    AssignOp::Equal,
                    lv.clone(),
                    Box::new(
                        Expr::Binary(
                            BinOp::BinaryOp(BinaryOp::PowerOf),
                            Box::new(lval_to_expr(lv.as_mut())),
                            Box::new(rhs.take()))));
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
    let mut expr = Expr::Assign(
        AssignOp::PlusEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(
            BinOp::BinaryOp(BinaryOp::Plus), 
            Box::new(Expr::Id(Id::Named("x".to_string()))), 
            Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}

#[test]
fn parse_minuseq() {
    let mut expr = Expr::Assign(
        AssignOp::MinusEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(
            BinOp::BinaryOp(BinaryOp::Minus), 
            Box::new(Expr::Id(Id::Named("x".to_string()))), 
            Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}

#[test]
fn parse_timeseq() {
    let mut expr = Expr::Assign(
        AssignOp::TimesEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(BinOp::BinaryOp(BinaryOp::Times), 
        Box::new(Expr::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}

#[test]
fn parse_diveq() {
    let mut expr = Expr::Assign(
        AssignOp::DivEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(
            BinOp::BinaryOp(BinaryOp::Over), 
            Box::new(Expr::Id(Id::Named("x".to_string()))), 
            Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}

#[test]
fn parse_modeq() {
    let mut expr = Expr::Assign(
        AssignOp::ModEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(
            BinOp::BinaryOp(BinaryOp::Mod), 
            Box::new(Expr::Id(Id::Named("x".to_string()))), 
            Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}

#[test]
fn parse_lshifteq() {
    let mut expr = Expr::Assign(
        AssignOp::LeftShiftEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(
            BinOp::BinaryOp(BinaryOp::LeftShift), 
            Box::new(Expr::Id(Id::Named("x".to_string()))), 
            Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}

#[test]
fn parse_rshifteq() {
    let mut expr = Expr::Assign(
        AssignOp::RightShiftEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(
            BinOp::BinaryOp(BinaryOp::RightShift), 
            Box::new(Expr::Id(Id::Named("x".to_string()))), 
            Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}

#[test]
fn parse_unsignedrshifteq() {
    let mut expr = Expr::Assign(
        AssignOp::UnsignedRightShiftEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(
            BinOp::BinaryOp(BinaryOp::UnsignedRightShift), 
            Box::new(Expr::Id(Id::Named("x".to_string()))), 
            Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}

#[test]
fn parse_expeq() {
    let mut expr = Expr::Assign(
        AssignOp::PowerOfEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(
            BinOp::BinaryOp(BinaryOp::PowerOf), 
            Box::new(Expr::Id(Id::Named("x".to_string()))), 
            Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}

#[test]
fn parse_oreq() {
    let mut expr = Expr::Assign(
        AssignOp::OrEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(
            BinOp::BinaryOp(BinaryOp::Or), 
            Box::new(Expr::Id(Id::Named("x".to_string()))), 
            Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}

#[test]
fn parse_xoreq() {
    let mut expr = Expr::Assign(
        AssignOp::XOrEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(
            BinOp::BinaryOp(BinaryOp::XOr), 
            Box::new(Expr::Id(Id::Named("x".to_string()))), 
            Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}

#[test]
fn parse_andeq() {
    let mut expr = Expr::Assign(
        AssignOp::AndEqual, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Lit(Lit::Num(Num::Int(1)))));
    let result = Expr::Assign(
        AssignOp::Equal, 
        Box::new(LValue::Id(Id::Named("x".to_string()))), 
        Box::new(Expr::Binary(
            BinOp::BinaryOp(BinaryOp::And), 
            Box::new(Expr::Id(Id::Named("x".to_string()))), 
            Box::new(Expr::Lit(Lit::Num(Num::Int(1)))))));
    expr.walk(&mut MyVisitor {});
    
    assert_eq!(expr, result);
}
