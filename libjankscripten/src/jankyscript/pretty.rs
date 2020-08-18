use super::syntax::*;

use crate::impl_Display_Pretty;
use crate::shared::pretty::Pretty;

impl Pretty for Type {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            Type::Float => pp.text("i32"),
            Type::Int => pp.text("f64"),
            Type::String => pp.text("string"),
            Type::Array => pp.text("array"),
            Type::Bool => pp.text("bool"),
            Type::DynObject => pp.text("DynObject"),
            Type::Function(args, ret) => pp.concat(vec![
                pp.intersperse(
                    args.iter().map(|t| t.pretty(pp)),
                    pp.text(",").append(pp.space()),
                )
                .parens(),
                pp.line(),
                pp.text("->"),
                pp.line(),
                ret.pretty(pp),
            ]),
            Type::Any => pp.text("any"),
        }
    }
}

impl Pretty for Coercion {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            Coercion::Id(t) => pp.text("id").append(t.pretty(pp).parens()),
            Coercion::Tag(t) => t.pretty(pp).append(pp.text("!")),
            Coercion::Untag(t) => t.pretty(pp).append(pp.text("?")),
            Coercion::IntToFloat => pp.text("i32_to_f64"),
            Coercion::FloatToInt => pp.text("f64_to_i32"),
            Coercion::Fun(args, ret) => pp.concat(vec![
                pp.intersperse(
                    args.iter().map(|c| c.pretty(pp)),
                    pp.text(",").append(pp.space()),
                )
                .parens(),
                pp.line(),
                pp.text("->"),
                pp.line(),
                ret.pretty(pp),
            ]),
            Coercion::Seq(c1, c2) => pp.concat(vec![c1.pretty(pp), pp.text(";"), c2.pretty(pp)]),
        }
    }
}

impl Pretty for LValue {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            LValue::Id(id) => pp.as_string(id),
            LValue::Dot(e, id) => e
                .pretty(pp)
                .append(pp.text("."))
                .append(pp.line_())
                .append(pp.as_string(id)),
            LValue::Bracket(e1, e2) => e1.pretty(pp).append(e2.pretty(pp).brackets()),
        }
    }
}

impl Pretty for Lit {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            Lit::String(s) => pp.text(s).double_quotes(), // TODO(michael) escaping?
            Lit::Regex(_s1, _s2) => todo!("regex literal pretty printing"),
            Lit::Bool(b) => pp.text(if *b { "true" } else { "false" }),
            Lit::Null => pp.text("null"),
            Lit::Num(Num::Int(i)) => pp.as_string(i),
            Lit::Num(Num::Float(f)) => {
                let mut s = format!("{}", f);
                if !s.contains('.') {
                    s = s + ".0"; // ensure we get a float!
                }
                pp.text(s)
            }
            Lit::Undefined => pp.text("undefined"),
        }
    }
}

impl Pretty for Key {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            Key::Int(i) => pp.as_string(i),
            Key::Str(s) => pp.text(s).single_quotes(),
        }
    }
}

impl Pretty for Expr {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            Expr::Lit(lit) => lit.pretty(pp),
            Expr::Array(es) => pp
                .intersperse(
                    es.iter().map(|e| e.pretty(pp)),
                    pp.text(",").append(pp.line()),
                )
                .brackets(),
            Expr::Object(kvs) => pp
                .intersperse(
                    kvs.iter().map(|(k, v)| {
                        pp.concat(vec![k.pretty(pp), pp.text(":"), pp.line(), v.pretty(pp)])
                    }),
                    pp.text(",").append(pp.line()),
                )
                .braces(),
            Expr::Id(id) => pp.as_string(id),
            Expr::Dot(e, id) => pp.concat(vec![
                e.pretty(pp),
                pp.text("."),
                pp.line_(),
                pp.as_string(id),
            ]),
            Expr::Bracket(e1, e2) => pp.concat(vec![e1.pretty(pp), e2.pretty(pp).brackets()]),
            Expr::Unary(op, e) => op.pretty(pp).append(e.pretty(pp).parens()),
            Expr::Binary(op, e1, e2) => pp.intersperse(
                vec![
                    e1.pretty(pp).parens(), // TODO(michael) sometimes omit parens?
                    op.pretty(pp),
                    e2.pretty(pp).parens(),
                ],
                pp.line(),
            ),
            Expr::Assign(lv, e) => pp.concat(vec![
                lv.pretty(pp),
                pp.space(),
                pp.text("="),
                pp.line(),
                e.pretty(pp),
            ]),
            Expr::Call(e, args) => pp.concat(vec![
                e.pretty(pp),
                pp.intersperse(
                    args.iter().map(|e| e.pretty(pp)),
                    pp.text(",").append(pp.line()),
                )
                .parens(),
            ]),
            Expr::PrimCall(rts, args) => pp.concat(vec![
                pp.text(rts.name()),
                pp.intersperse(
                    args.iter().map(|e| e.pretty(pp)),
                    pp.text(",").append(pp.line()),
                )
                .parens(),
            ]),
            Expr::Func(ret, args, e) => pp.concat(vec![
                pp.text("function"),
                pp.intersperse(
                    args.iter().map(|(x, t)| {
                        pp.concat(vec![
                            pp.as_string(x),
                            pp.space(),
                            pp.text(":"),
                            pp.space(),
                            t.pretty(pp),
                        ])
                        .group()
                    }),
                    pp.text(",").append(pp.space()),
                )
                .parens(),
                pp.line(),
                pp.text("->"),
                ret.pretty(pp),
                pp.line(),
                e.pretty(pp).nest(2).braces(),
            ]),
            Expr::Coercion(c, e) => {
                pp.concat(vec![c.pretty(pp).brackets(), pp.line(), e.pretty(pp)])
            }
        }
    }
}

impl Pretty for Stmt {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            Stmt::Var(id, t, e) => pp
                .concat(vec![
                    pp.text("let"),
                    pp.space(),
                    pp.as_string(id),
                    pp.space(),
                    pp.text(":"),
                    pp.space(),
                    t.pretty(pp),
                    pp.space(),
                    pp.text("="),
                ])
                .group()
                .append(pp.concat(vec![pp.line(), e.pretty(pp).nest(2).group(), pp.text(";")])),
            Stmt::Block(stmts) => pp
                .concat(vec![
                    pp.hardline(),
                    pp.intersperse(
                        stmts.iter().map(|s| s.pretty(pp).nest(2).group()),
                        pp.hardline(),
                    ),
                    pp.hardline(),
                ])
                .braces(),
            Stmt::Empty => pp.hardline(),
            Stmt::Expr(e) => e.pretty(pp).append(";").group(),
            Stmt::If(e1, s2, s3) => pp.intersperse(
                vec![
                    pp.concat(vec![pp.text("if"), pp.space(), e1.pretty(pp).parens()])
                        .group(),
                    s2.pretty(pp).nest(2),
                    pp.text("else"),
                    s3.pretty(pp).nest(2),
                ],
                pp.hardline(),
            ),
            Stmt::Loop(s) => pp.concat(vec![pp.text("loop"), pp.hardline(), s.pretty(pp)]),
            Stmt::Label(lbl, s) => pp.concat(vec![
                pp.as_string(lbl),
                pp.text(":"),
                pp.line(),
                s.pretty(pp).nest(2),
            ]),
            Stmt::Break(lbl) => pp.concat(vec![
                pp.text("break"),
                pp.space(),
                pp.as_string(lbl),
                pp.text(";"),
            ]),
            Stmt::Catch(s1, id, s2) => pp.concat(vec![
                pp.text("try"),
                pp.space(),
                s1.pretty(pp).nest(2),
                pp.hardline(),
                pp.concat(vec![pp.text("catch"), pp.space(), pp.as_string(id)])
                    .group(),
                pp.line(),
                s2.pretty(pp).nest(2),
            ]),
            Stmt::Finally(s1, s2) => pp.concat(vec![
                pp.text("try"),
                pp.space(),
                s1.pretty(pp).nest(2),
                pp.hardline(),
                pp.text("finally"),
                pp.line(),
                s2.pretty(pp).nest(2),
            ]),
            Stmt::Throw(e) => pp.concat(vec![
                pp.text("throw"),
                pp.space(),
                e.pretty(pp),
                pp.text(";"),
            ]),
            Stmt::Return(e) => pp.concat(vec![
                pp.text("return"),
                pp.space(),
                e.pretty(pp),
                pp.text(";"),
            ]),
        }
    }
}

impl_Display_Pretty!(LValue);
impl_Display_Pretty!(Expr);
impl_Display_Pretty!(Stmt);
