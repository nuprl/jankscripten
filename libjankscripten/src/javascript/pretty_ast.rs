use super::syntax::{self, *};
use pretty::RcDoc as D;

const INDENT: isize = 4;

impl Stmt {
    pub fn to_doc(&self) -> D<()> {
        use Stmt::*;
        match self {
            Block(stmts) => D::text("{")
                .append(
                    D::line()
                        .append(D::intersperse(stmts.iter().map(Stmt::to_doc), D::line()))
                        .nest(INDENT),
                )
                .append(D::line())
                .append(D::text("}")),
            // TODO(luna): check? ()? {}?
            Empty => D::nil(),
            Expr(e) => e.to_doc().append(D::text(";")),
            If(cond, then, other) => D::text("if (")
                .append(cond.to_doc())
                // TODO(luna):
                // if (cond)
                //     one_off()
                .append(D::text(") "))
                .append(then.to_doc())
                // same here
                .append(D::text(" else "))
                .append(other.to_doc()),
            // TODO(luna): what to do about blocks being not pretty
            Switch(descr, cases, default) => D::text("switch (")
                .append(descr.to_doc())
                .append(D::text(") {"))
                .append(
                    D::line()
                        .append(D::intersperse(
                            cases.iter().map(|(e, s)| {
                                D::text("case ")
                                    .append(e.to_doc())
                                    .append(D::text(":"))
                                    .append(D::line().append(s.to_doc()).nest(INDENT))
                            }),
                            D::line(),
                        ))
                        .nest(INDENT),
                )
                .append(D::line().append(D::text("default:")).nest(INDENT))
                .append(D::line().append(default.to_doc()).nest(INDENT).nest(INDENT))
                // TODO: might have extra line, if default is Empty
                .append(D::line())
                .append(D::text("}")),
            While(cond, body) => D::text("while (")
                .append(cond.to_doc())
                .append(D::text(") "))
                .append(body.to_doc()),
            DoWhile(body, cond) => D::text("do ")
                .append(body.to_doc())
                .append(D::text(" while ("))
                .append(cond.to_doc())
                .append(D::text(")")),
            For(init, cond, advance, body) => D::text("for (")
                .append(init.to_doc())
                .append(D::text("; "))
                .append(cond.to_doc())
                .append(D::text("; "))
                .append(advance.to_doc())
                .append(D::text(") "))
                .append(body.to_doc()),
            ForIn(is_decl, name, container, body) => D::text("for (")
                .append(if *is_decl { D::text("let ") } else { D::nil() })
                .append(D::text(name))
                .append(D::text(" in "))
                .append(container.to_doc())
                .append(D::text(") "))
                .append(body.to_doc()),
            Label(name, stmt) => D::text(name).append(D::text(": ")).append(stmt.to_doc()),
            Break(maybe_lbl) => D::text("break")
                .append(option_label_to_doc(maybe_lbl))
                .append(D::text(";")),
            Continue(maybe_lbl) => D::text("continue")
                .append(option_label_to_doc(maybe_lbl))
                .append(D::text(";")),
            Catch(try_in, bind, catch) => D::text("try ")
                .append(try_in.to_doc())
                .append(D::text(" catch ("))
                .append(D::text(bind))
                .append(D::text(") "))
                .append(catch.to_doc()),
            Finally(try_catch, final_block) => try_catch
                .to_doc()
                .append(D::text(" finally "))
                .append(final_block.to_doc()),
            Throw(e) => D::text("throw ").append(e.to_doc()).append(D::text(";")),
            VarDecl(decls) => vardecls_to_doc(decls),
            Func(name, params, body) => func_to_doc(Some(name), params, body),
            Return(e) => D::text("return ").append(e.to_doc()).append(D::text(";")),
        }
    }
    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        let doc = self.to_doc();
        println!("{:?}", doc);
        doc.render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

// TODO(luna): Use BoxAllocator to avoid reference counting
impl Expr {
    pub fn to_doc(&self) -> D<()> {
        use Expr::*;
        match self {
            Lit(lit) => lit.to_doc(),
            Array(es) => D::text("[")
                .append(D::intersperse(es.iter().map(Expr::to_doc), D::text(", ")))
                .append(D::text("]")),
            Object(kes) => D::text("{")
                .append(D::intersperse(
                    kes.iter().map(|(k, e)| {
                        (match k {
                            Key::Int(n) => D::text(format!("{}", n)),
                            Key::Str(s) => D::text(s),
                        })
                        .append(D::text(": "))
                        .append(e.to_doc())
                    }),
                    D::text(", "),
                ))
                .append("}"),
            This => D::text("this"),
            Id(x) => D::text(x),
            Dot(e, id) => e.to_doc().append(D::text(".")).append(D::text(id)),
            Bracket(cont, ind) => cont
                .to_doc()
                .append(D::text("["))
                .append(ind.to_doc())
                .append(D::text("]")),
            New(cons, args) => D::text("new ").append(fn_call_to_doc(cons, args)),
            Unary(op, e) => unary_op_to_doc(op).append(e.to_doc()),
            Binary(op, a, b) => a
                .to_doc()
                .append(D::space())
                .append(op.to_doc())
                .append(D::space())
                .append(b.to_doc()),
            UnaryAssign(op, lval) => match op {
                UnaryAssignOp::PreInc => D::text("++").append(lval.to_doc()),
                UnaryAssignOp::PreDec => D::text("--").append(lval.to_doc()),
                UnaryAssignOp::PostInc => lval.to_doc().append(D::text("++")),
                UnaryAssignOp::PostDec => lval.to_doc().append(D::text("--")),
            },
            If(cond, then, other) => cond
                .to_doc()
                .append(D::text(" ? "))
                .append(then.to_doc())
                .append(D::text(" : "))
                .append(other.to_doc()),
            Assign(op, lval, to) => lval
                .to_doc()
                .append(D::space())
                .append(assign_op_to_doc(op))
                .append(D::space())
                .append(to.to_doc()),
            Call(clos, args) => fn_call_to_doc(clos, args),
            Func(maybe_name, params, body) => func_to_doc(maybe_name.as_ref(), params, body),
            Seq(es) => D::intersperse(es.iter().map(Expr::to_doc), D::text(", ")),
        }
    }
    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

fn fn_call_to_doc<'a>(closure: &'a Expr, args: &'a [Expr]) -> D<'a, ()> {
    closure
        .to_doc()
        .append(D::text("("))
        .append(D::intersperse(
            args.iter().map(|e| e.to_doc()),
            D::text(", "),
        ))
        .append(D::text(")"))
}

impl Lit {
    pub fn to_doc(&self) -> D<()> {
        match self {
            // TODO(luna): handle multiline strings
            syntax::Lit::String(text) => D::text("\"").append(D::text(text)).append(D::text("\"")),
            syntax::Lit::Regex(pattern, flags) => D::text("/")
                .append(D::text(pattern))
                .append(D::text("/"))
                .append(D::text(flags)),
            syntax::Lit::Bool(b) => match b {
                true => D::text("true"),
                false => D::text("false"),
            },
            syntax::Lit::Null => D::text("null"),
            syntax::Lit::Num(num) => match num {
                Num::Int(n) => D::text(format!("{}", n)),
                Num::Float(n) => D::text(format!("{}", n)),
            },
            syntax::Lit::Undefined => D::text("undefined"),
        }
    }
}

// can't impl foreign type
pub fn unary_op_to_doc(op: &UnaryOp) -> D<()> {
    use resast::UnaryOp::*;
    D::text(match op {
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
    pub fn to_doc(&self) -> D<()> {
        use resast::BinaryOp::*;
        D::text(match self {
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
    pub fn to_doc(&self) -> D<()> {
        // TODO(luna): this is all duplicated with Expr
        match self {
            LValue::Id(id) => D::text(id),
            LValue::Dot(e, id) => e.to_doc().append(D::text(".")).append(D::text(id)),
            LValue::Bracket(cont, ind) => cont
                .to_doc()
                .append(D::text("["))
                .append(ind.to_doc())
                .append(D::text("]")),
        }
    }
}

fn assign_op_to_doc(op: &AssignOp) -> D<()> {
    use resast::AssignOp::*;
    D::text(match op {
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

fn vardecls_to_doc(decls: &[VarDecl]) -> D<()> {
    let first = decls.first().expect("wouldn't exist without one");
    let rest = &decls[1..];
    // first with the let
    D::text("let ")
        .append(D::text(&first.name))
        .append(D::text(" = "))
        .append(first.named.to_doc())
        .append(D::text(", "))
        .append(D::intersperse(
            rest.iter().map(|d| {
                // no let this time
                D::text(&d.name)
                    .append(D::text(" = "))
                    .append(d.named.to_doc())
            }),
            D::text(", "),
        ))
}

impl ForInit {
    pub fn to_doc(&self) -> D<()> {
        match self {
            ForInit::Expr(e) => e.to_doc(),
            ForInit::Decl(ds) => vardecls_to_doc(ds),
        }
    }
}

fn option_label_to_doc(maybe_lbl: &Option<Id>) -> D<()> {
    if let Some(id) = maybe_lbl {
        D::text(" ").append(D::text(id))
    } else {
        D::nil()
    }
}

fn func_to_doc<'a>(maybe_name: Option<&'a Id>, params: &'a [Id], body: &'a Stmt) -> D<'a, ()> {
    D::text("function")
        .append(match maybe_name {
            Some(name) => D::space().append(D::text(name)),
            None => D::nil(),
        })
        .append(D::text("("))
        .append(D::intersperse(
            params.iter().map(|e| D::text(e)),
            D::text(", "),
        ))
        .append(D::text(") "))
        .append(body.to_doc())
}

#[cfg(test)]
mod test {
    use crate::javascript::parser::parse;
    use crate::javascript::Stmt;
    const WIDTH: usize = 80;
    fn already_pretty(what: &str) {
        let prettied = parse(what).unwrap().to_pretty(WIDTH);
        println!("original:\n{}", what);
        println!("out:\n{}", prettied);
        assert_eq!(what, prettied);
    }
    fn already_pretty_expr(what: &str) {
        println!("program string:\n{}", what);
        // we wrap the exp in a statement so the parser doesn't choke
        // parser chokes on some standalone expressions
        // TODO(luna): might need to add this to expr!
        let parsed = parse(&format!("while ({}) {{}}", what)).unwrap();
        println!("parsed:\n{:?}", parsed);
        if let Stmt::While(e, ..) = parsed {
            assert_eq!(what, e.to_pretty(WIDTH));
            return;
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
        already_pretty_expr(r#"[1, "two", null, true, /h.l*o/g]"#);
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
    #[ignore]
    #[test]
    fn switch() {
        already_pretty(
            "switch (5) {
    case 10:
        console.log(10);
    case 6:
    case 4:
        console.log(2);
    default:
        console.log(1)
}",
        );
    }
    #[test]
    fn block() {
        already_pretty(
            "{
    console.log(5);
}",
        );
    }
    #[test]
    fn while_loop() {
        already_pretty(
            "while (true) {
    console.log(5);
}",
        );
    }
    #[test]
    fn if_else_w_blocks() {
        already_pretty(
            "if (true) {
    console.log(5);
} else {
    console.log(10);
}",
        );
    }
    #[test]
    fn for_loop() {
        already_pretty(
            "for (let i = 0, y = 5; y < 5; ++x) {
    console.log(i);
}",
        );
    }
    #[test]
    fn functions() {
        already_pretty(
            "function hello(a, b) {
    return function(c) {
        console.log(hi);
    };
}",
        )
    }
}
