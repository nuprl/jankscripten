use super::syntax::{self, *};
use pretty::RcDoc as D;
use std::fmt::*;

const INDENT: isize = 4;

impl Display for Stmt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.to_doc().render_fmt(80, f)
    }
}

impl Stmt {
    pub fn to_doc(&self) -> D<()> {
        use Stmt::*;
        match self {
            Block(stmts, _) => D::text("{")
                .append(
                    D::line()
                        .append(D::intersperse(stmts.iter().map(Stmt::to_doc), D::line()))
                        .nest(INDENT),
                )
                .append(D::line())
                .append(D::text("}")),
            Empty => D::text(";"),
            Expr(e, _) => D::text("(")
                .append(e.to_doc())
                .append(D::text(")"))
                .append(D::text(";")),
            If(cond, then, other, _) => D::text("if (")
                .append(cond.to_doc())
                .append(D::text(") "))
                .append(then.to_doc())
                .append(D::text(" else "))
                .append(other.to_doc()),
            Switch(descr, cases, default, _) => D::text("switch (")
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
            While(cond, body, _) => D::text("while (")
                .append(cond.to_doc())
                .append(D::text(") "))
                .append(body.to_doc()),
            DoWhile(body, cond, _) => D::text("do ")
                .append(body.to_doc())
                .append(D::text(" while ("))
                .append(cond.to_doc())
                .append(D::text(")")),
            For(init, cond, advance, body, _) => D::text("for (")
                .append(init.to_doc())
                .append(D::text("; "))
                .append(cond.to_doc())
                .append(D::text("; "))
                .append(advance.to_doc())
                .append(D::text(") "))
                .append(body.to_doc()),
            ForIn(is_decl, name, container, body, _) => D::text("for (")
                .append(if *is_decl { D::text("var ") } else { D::nil() })
                .append(name.to_doc())
                .append(D::text(" in "))
                .append(container.to_doc())
                .append(D::text(") "))
                .append(body.to_doc()),
            Label(name, stmt, _) => name.to_doc().append(D::text(": ")).append(stmt.to_doc()),
            Break(maybe_lbl, _) => D::text("break")
                .append(option_label_to_doc(maybe_lbl))
                .append(D::text(";")),
            Continue(maybe_lbl, _) => D::text("continue")
                .append(option_label_to_doc(maybe_lbl))
                .append(D::text(";")),
            Catch(try_in, bind, catch, _) => D::text("try ")
                .append(try_in.to_doc())
                .append(D::text(" catch ("))
                .append(bind.to_doc())
                .append(D::text(") "))
                .append(catch.to_doc()),
            Finally(try_catch, final_block, _) => try_catch
                .to_doc()
                .append(D::text(" finally "))
                .append(final_block.to_doc()),
            Throw(e, _) => D::text("throw ").append(e.to_doc()).append(D::text(";")),
            VarDecl(decls, _) => vardecls_to_doc(decls).append(D::text(";")),
            Func(name, params, body, _) => func_to_doc(Some(name), params, body),
            Return(e, _) => D::text("return ").append(e.to_doc()).append(D::text(";")),
        }
    }
    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        let doc = self.to_doc();
        doc.render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

impl Expr {
    pub fn to_doc(&self) -> D<()> {
        use Expr::*;
        match self {
            Lit(lit, _) => lit.to_doc(),
            Array(es, _) => D::text("[")
                .append(D::intersperse(es.iter().map(Expr::to_doc), D::text(", ")))
                .append(D::text("]")),
            Object(kes, _) => D::text("{")
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
            Id(x, _) => x.to_doc(),
            Dot(e, id, _) => e.to_doc().append(D::text(".")).append(id.to_doc()),
            Bracket(cont, ind, _) => cont
                .to_doc()
                .append(D::text("["))
                .append(ind.to_doc())
                .append(D::text("]")),
            New(cons, args, _) => D::text("new ").append(fn_call_to_doc(cons, args)),
            Unary(op, e, _) => unary_op_to_doc(op)
                .append(D::text("("))
                .append(e.to_doc())
                .append(D::text(")")),
            Binary(op, a, b, _) => D::text("(")
                .append(a.to_doc())
                .append(D::space())
                .append(op.to_doc())
                .append(D::space())
                .append(b.to_doc())
                .append(D::text(")")),
            UnaryAssign(op, lval, _) => match op {
                UnaryAssignOp::PreInc => D::text("++").append(lval.to_doc()),
                UnaryAssignOp::PreDec => D::text("--").append(lval.to_doc()),
                UnaryAssignOp::PostInc => lval.to_doc().append(D::text("++")),
                UnaryAssignOp::PostDec => lval.to_doc().append(D::text("--")),
            },
            If(cond, then, other, _) => cond
                .to_doc()
                .append(D::text(" ? "))
                .append(then.to_doc())
                .append(D::text(" : "))
                .append(other.to_doc()),
            Assign(op, lval, to, _) => lval
                .to_doc()
                .append(D::space())
                .append(assign_op_to_doc(op))
                .append(D::space())
                .append(to.to_doc()),
            Call(clos, args, _) => fn_call_to_doc(clos, args),
            Func(maybe_name, params, body, _) => func_to_doc(maybe_name.as_ref(), params, body),
            Seq(es, _) => D::text("(")
                .append(D::intersperse(es.iter().map(Expr::to_doc), D::text(", ")))
                .append(D::text(")")),
        }
    }
    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

fn fn_call_to_doc<'a>(f: &'a Expr, args: &'a [Expr]) -> D<'a, ()> {
    f.to_doc()
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
            // TODO(arjun): This can be done more efficiently. However, we only
            // output JS for testing, so it is not important.
            syntax::Lit::String(text) => {
                let unescaped = text
                    .replace("\n", "\\n")
                    .replace('\\', "\\\\")
                    .replace("\"", "\\\"");
                D::text("\"")
                    .append(D::text(unescaped))
                    .append(D::text("\""))
            }
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

impl Id {
    pub fn to_doc(&self) -> D<()> {
        match self {
            Self::Named(name) => D::text(name),
            Self::Generated(generated) => D::text(format!("{}", generated)),
            Self::Bogus(txt) => D::text(txt.to_string()),
        }
    }
    pub fn to_pretty(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_doc().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }
}

// can't impl foreign type
fn unary_op_to_doc(op: &UnaryOp) -> D<()> {
    use UnaryOp::*;
    D::text(match op {
        Minus => "-",
        Plus => "+",
        Not => "!",
        Tilde => "~",
        TypeOf => "typeof ",
        Void => "void ",
        Delete => "delete ",
    })
}

impl BinOp {
    pub fn to_doc(&self) -> D<()> {
        use BinaryOp::*;
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
                LogicalOp::And => "&&",
                LogicalOp::Or => "||",
            },
        })
    }
}

impl LValue {
    pub fn to_doc(&self) -> D<()> {
        match self {
            LValue::Id(id) => id.to_doc(),
            LValue::Dot(e, id) => e.to_doc().append(D::text(".")).append(id.to_doc()),
            LValue::Bracket(cont, ind) => cont
                .to_doc()
                .append(D::text("["))
                .append(ind.to_doc())
                .append(D::text("]")),
        }
    }
}

fn assign_op_to_doc(op: &AssignOp) -> D<()> {
    use AssignOp::*;
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
    let with_var = D::text("var ")
        .append(first.name.to_doc())
        .append(D::text(" = "))
        .append(first.named.to_doc());
    if !rest.is_empty() {
        with_var.append(D::text(", ")).append(D::intersperse(
            rest.iter().map(|d| {
                // no let this time
                d.name
                    .to_doc()
                    .append(D::text(" = "))
                    .append(d.named.to_doc())
            }),
            D::text(", "),
        ))
    } else {
        with_var
    }
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
        D::text(" ").append(id.to_doc())
    } else {
        D::nil()
    }
}

fn func_to_doc<'a>(maybe_name: Option<&'a Id>, params: &'a [Id], body: &'a Stmt) -> D<'a, ()> {
    D::text("function")
        .append(match maybe_name {
            Some(name) => D::space().append(name.to_doc()),
            None => D::nil(),
        })
        .append(D::text("("))
        .append(D::intersperse(params.iter().map(Id::to_doc), D::text(", ")))
        .append(D::text(") "))
        .append(body.to_doc())
}

#[cfg(test)]
mod test {
    use crate::javascript::parse;
    const WIDTH: usize = 80;
    /// is actually parse_pretty_parse_pretty because span messiness. i think
    /// at this point this has been well-tested enough that that's sufficient
    /// (ie introduces the possibility that `pretty(ast) = ""` would pass,
    /// but we're beyond that
    fn parse_pretty_parse(js_code: &str) {
        let original_ast = parse(js_code).expect("invalid test doesn't parse");
        let pretty = original_ast.to_pretty(WIDTH);
        let pretty_ast = parse(&pretty)
            .expect(&format!("parsing pretty-printed string: {}", &pretty));
        let pretty_ast_pretty = pretty_ast.to_pretty(WIDTH);
        assert_eq!(pretty, pretty_ast_pretty);
    }
    fn parse_pretty_parse_expr(js_code: &str) {
        let modified = &format!("var $jnks_expr = {};", js_code);
        println!("program string:\n{}", modified);
        // we wrap the expr in a leading statement so the parser doesn't choke
        // parser chokes on some standalone expressions
        parse_pretty_parse(modified);
    }
    #[test]
    fn not_pretty() {
        parse_pretty_parse_expr("   [   1,    2]");
    }
    #[test]
    fn literals() {
        parse_pretty_parse_expr(r#"[1, "two", null, true]"#);
    }
    #[test]
    fn object() {
        parse_pretty_parse_expr("{x: 10, y: null}");
    }
    #[test]
    fn new_and_this() {
        parse_pretty_parse("new Thingy(this)");
    }
    #[test]
    fn id_dot() {
        parse_pretty_parse("one.two");
    }
    #[test]
    fn ops() {
        parse_pretty_parse("~5 & 9 instanceof SomeObject");
    }
    #[test]
    fn unary_lval() {
        parse_pretty_parse("++x[5]");
    }
    #[test]
    fn is_this_haskell() {
        parse_pretty_parse("x >>= 5");
    }
    #[test]
    fn ternary_if() {
        parse_pretty_parse("true ? 5 : 3");
    }
    #[test]
    fn seq() {
        parse_pretty_parse("x = 6, y");
    }
    #[ignore]
    #[test]
    fn switch() {
        parse_pretty_parse(
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
        parse_pretty_parse(
            "{
    console.log(5);
}",
        );
    }
    #[test]
    fn while_loop() {
        parse_pretty_parse(
            "while (true) {
    console.log(5);
}",
        );
    }
    #[test]
    fn if_else_w_blocks() {
        parse_pretty_parse(
            "if (true) {
    console.log(5);
} else {
    console.log(10);
}",
        );
    }
    #[test]
    fn for_loop() {
        parse_pretty_parse(
            "for (var i = 0, y = 5; y < 5; ++x) {
    console.log(i);
}",
        );
    }
    #[test]
    fn functions() {
        parse_pretty_parse(
            "function hello(a, b) {
    return function(c) {
        console.log(hi);
    };
}",
        )
    }

    #[test]
    fn seq_parenthesization() {
        parse_pretty_parse_expr(
            r#"
            x ? (y, z) : w
        "#,
        );
    }

    #[test]
    fn add_multiply_parenthesization() {
        parse_pretty_parse_expr(
            r#"
            (x + y) * z
        "#,
        );
    }

    #[test]
    fn top_level_application() {
        parse_pretty_parse(
            r#"
            (function() { }());
        "#,
        );
    }
}
