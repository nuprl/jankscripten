use super::syntax::{self, *};
use pretty::RcDoc;

// TODO: Expr::to_doc
//            If(cond, then, other) => RcDoc::text("if (")
//                .append(cond.to_doc())
//                .append(RcDoc::text(") {"))
//                .append(RcDoc::line())
//                .nest(1)
//                .append(then.to_doc())
//                .nest(-1)
//                .append(RcDoc::text("} else {"))
//                .nest(1)
//                .append(other.to_doc())
//                .nest(-1)
//                .append(RcDoc::text("}")),

// TODO(luna): Use BoxAllocator to avoid reference counting
impl Expr {
    pub fn to_doc(&self) -> RcDoc<()> {
        use Expr::*;
        match self {
            Lit(lit) => lit.to_doc(),
            Array(es) => RcDoc::text("[")
                .append(RcDoc::intersperse(
                    es.iter().map(Expr::to_doc),
                    RcDoc::text(", "),
                ))
                .append(RcDoc::text("]")),
            Object(kes) => RcDoc::text("{")
                .append(RcDoc::intersperse(
                    kes.iter().map(|(k, e)| {
                        (match k {
                            Key::Int(n) => RcDoc::text(format!("{}", n)),
                            Key::Str(s) => RcDoc::text(s),
                        })
                        .append(RcDoc::text(": "))
                        .append(e.to_doc())
                    }),
                    RcDoc::text(", "),
                ))
                .append("}"),
            This => RcDoc::text("this"),
            Id(x) => RcDoc::text(x),
            Dot(e, id) => e.to_doc().append(RcDoc::text(".")).append(RcDoc::text(id)),
            Bracket(cont, ind) => cont
                .to_doc()
                .append(RcDoc::text("["))
                .append(ind.to_doc())
                .append(RcDoc::text("]")),
            New(cons, args) => RcDoc::text("new ").append(fn_call_to_doc(cons, args)),
            Unary(op, e) => unary_op_to_doc(op).append(e.to_doc()),
            Binary(op, a, b) => a
                .to_doc()
                .append(RcDoc::space())
                .append(op.to_doc())
                .append(RcDoc::space())
                .append(b.to_doc()),
            UnaryAssign(op, lval) => match op {
                UnaryAssignOp::PreInc => RcDoc::text("++").append(lval.to_doc()),
                UnaryAssignOp::PreDec => RcDoc::text("--").append(lval.to_doc()),
                UnaryAssignOp::PostInc => lval.to_doc().append(RcDoc::text("++")),
                UnaryAssignOp::PostDec => lval.to_doc().append(RcDoc::text("--")),
            },
            If(cond, then, other) => cond
                .to_doc()
                .append(RcDoc::text(" ? "))
                .append(then.to_doc())
                .append(RcDoc::text(" : "))
                .append(other.to_doc()),
            Assign(op, lval, to) => lval
                .to_doc()
                .append(RcDoc::space())
                .append(assign_op_to_doc(op))
                .append(RcDoc::space())
                .append(to.to_doc()),
            Call(clos, args) => fn_call_to_doc(clos, args),
            Func(..) => todo!("Stmt pretty-print doesn't exist yet"),
            //Func(name_or_arrow, params, body) => match name {
            //Some(name) => RcDoc::text("function ").append(...)
            //None => (), // () => {}
            //}
            Seq(es) => RcDoc::intersperse(es.iter().map(Expr::to_doc), RcDoc::text(", ")),
        }
    }
    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

fn fn_call_to_doc<'a>(closure: &'a Expr, args: &'a [Expr]) -> RcDoc<'a, ()> {
    closure
        .to_doc()
        .append(RcDoc::text("("))
        .append(RcDoc::intersperse(
            args.iter().map(|e| e.to_doc()),
            RcDoc::text(", "),
        ))
        .append(RcDoc::text(")"))
}

impl Lit {
    pub fn to_doc(&self) -> RcDoc<()> {
        match self {
            // TODO(luna): handle multiline strings
            syntax::Lit::String(text) => RcDoc::text("\"")
                .append(RcDoc::text(text))
                .append(RcDoc::text("\"")),
            syntax::Lit::Regex(..) => todo!(),
            syntax::Lit::Bool(b) => match b {
                true => RcDoc::text("true"),
                false => RcDoc::text("false"),
            },
            syntax::Lit::Null => RcDoc::text("null"),
            syntax::Lit::Num(num) => match num {
                Num::Int(n) => RcDoc::text(format!("{}", n)),
                Num::Float(n) => RcDoc::text(format!("{}", n)),
            },
            syntax::Lit::Undefined => RcDoc::text("undefined"),
        }
    }
}

// can't impl foreign type
pub fn unary_op_to_doc(op: &UnaryOp) -> RcDoc<()> {
    use resast::UnaryOp::*;
    RcDoc::text(match op {
        Minus => "-",
        Plus => "+",
        Not => "!",
        Tilde => "~",
        TypeOf => "typeof ",
        Void => "void ",
        Delete => "del ",
    })
}

impl BinOp {
    pub fn to_doc(&self) -> RcDoc<()> {
        use resast::BinaryOp::*;
        RcDoc::text(match self {
            BinOp::BinaryOp(b) => match b {
                Equal => "==",
                NotEqual => "!=",
                StrictEqual => "===",
                StrictNotEqual => "!==",
                LessThan => "<",
                GreaterThan => ">",
                LessThanEqual => "<=",
                GreaterThanEqual => ">=",
                LeftShift => "<<",
                RightShift => ">>",
                UnsignedRightShift => ">>>",
                Plus => "+",
                Minus => "-",
                Times => "*",
                Over => "/",
                Mod => "%",
                Or => "|",
                XOr => "^",
                And => "&",
                In => "in",
                InstanceOf => "instanceof",
                PowerOf => "**",
            },
            BinOp::LogicalOp(o) => match o {
                resast::LogicalOp::And => "&&",
                resast::LogicalOp::Or => "||",
            },
        })
    }
}

impl LValue {
    pub fn to_doc(&self) -> RcDoc<()> {
        // TODO(luna): this is all duplicated with Expr
        match self {
            LValue::Id(id) => RcDoc::text(id),
            LValue::Dot(e, id) => e.to_doc().append(RcDoc::text(".")).append(RcDoc::text(id)),
            LValue::Bracket(cont, ind) => cont
                .to_doc()
                .append(RcDoc::text("["))
                .append(ind.to_doc())
                .append(RcDoc::text("]")),
        }
    }
}

fn assign_op_to_doc(op: &AssignOp) -> RcDoc<()> {
    use resast::AssignOp::*;
    RcDoc::text(match op {
        Equal => "=",
        PlusEqual => "+=",
        MinusEqual => "-=",
        TimesEqual => "*=",
        DivEqual => "/=",
        ModEqual => "%=",
        LeftShiftEqual => "<<=",
        RightShiftEqual => ">>=",
        UnsignedRightShiftEqual => ">>>=",
        OrEqual => "|=",
        XOrEqual => "^=",
        AndEqual => "&=",
        PowerOfEqual => "**=",
    })
}

#[cfg(test)]
mod test {
    use crate::javascript::parser::parse;
    use crate::javascript::Stmt;
    const WIDTH: usize = 80;
    //fn already_pretty(what: &str) {
    //assert_eq!(what, parse(what).unwrap().to_pretty(WIDTH));
    //}
    fn already_pretty_expr(what: &str) {
        println!("program string:\n{}", what);
        // we wrap the exp in a statement so the parser doesn't choke
        // parser chokes on some standalone expressions
        // TODO(luna): might need to add this to expr!
        let mut parsed = parse(&format!("while ({}) {{}}", what)).unwrap();
        println!("parsed:\n{:?}", parsed);
        if let Stmt::Block(ref mut x) = parsed {
            if x.len() == 1 {
                if let Some(Stmt::While(e, ..)) = x.pop() {
                    assert_eq!(what, e.to_pretty(WIDTH));
                    return;
                }
            }
        }
        panic!("not expr");
    }
    #[test]
    #[should_panic]
    fn not_pretty() {
        already_pretty_expr("   [   1,    2]");
    }
    #[test]
    fn literals() {
        already_pretty_expr(r#"[1, "two", null, true]"#);
    }
    #[test]
    fn object() {
        already_pretty_expr("{x: 10, y: null}");
    }
    #[test]
    fn new_and_this() {
        already_pretty_expr("new Thingy(this)");
    }
    #[test]
    fn id_dot() {
        already_pretty_expr("one.two");
    }
    #[test]
    fn ops() {
        already_pretty_expr("~5 & 9 instanceof SomeObject");
    }
    #[test]
    fn unary_lval() {
        already_pretty_expr("++x[5]");
    }
    #[test]
    fn is_this_haskell() {
        already_pretty_expr("x >>= 5");
    }
    #[test]
    fn ternary_if() {
        already_pretty_expr("true ? 5 : 3");
    }
    #[test]
    fn seq() {
        already_pretty_expr("x = 6, y");
    }
}
