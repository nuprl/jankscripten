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
            Type::I32 => pp.text("i32"),
            Type::F64 => pp.text("f64"),
            Type::String => pp.text("str"),
            Type::HT => pp.text("ht"),
            Type::Array => pp.text("array"),
            Type::Bool => pp.text("bool"),
            Type::DynObject => pp.text("DynObject"),
            Type::Any => pp.text("any"),
            Type::Ref(t) => pp.concat(vec![pp.text("ref"), t.pretty(pp).parens()]),
            Type::Fn(fn_t) => fn_t.pretty(pp),
            Type::Closure(fn_t) => pp.text("clos").append(fn_t.pretty(pp)),
            Type::Env => pp.text("env"),
        }
    }
}

impl Pretty for FnType {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        pp.concat(vec![
            pp.intersperse(
                self.args.iter().map(|t| t.pretty(pp)),
                pp.text(",").append(pp.space()),
            )
            .parens(),
            pp.space(),
            pp.text("->"),
            pp.space(),
            self.result
                .as_ref()
                .map(|t| t.pretty(pp))
                .unwrap_or(pp.text("none")),
        ])
    }
}

impl Pretty for UnaryOp {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            UnaryOp::Sqrt => pp.text("Math.sqrt"),
            UnaryOp::Neg => pp.text("-"),
            UnaryOp::Eqz => pp.text("!"),
        }
    }
}

impl Pretty for BinaryOp {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            BinaryOp::PtrEq => pp.text("==="),
            BinaryOp::I32Eq => pp.text("==="),
            BinaryOp::I32Ne => pp.text("!="),
            BinaryOp::I32Add => pp.text("+"),
            BinaryOp::I32Sub => pp.text("-"),
            BinaryOp::I32Mul => pp.text("*"),
            BinaryOp::I32GT => pp.text(">"),
            BinaryOp::I32LT => pp.text("<"),
            BinaryOp::I32Ge => pp.text(">="),
            BinaryOp::I32Le => pp.text("<="),
            BinaryOp::I32And => pp.text("&"),
            BinaryOp::I32Or => pp.text("|"),
            BinaryOp::I32Div => pp.text("/"),
            BinaryOp::I32Rem => pp.text("%"),
            BinaryOp::I32Shl => pp.text("<<"),
            BinaryOp::I32Shr => pp.text(">>"),
            BinaryOp::F64Eq => pp.text("==="),
            BinaryOp::F64Ne => pp.text("!="),
            BinaryOp::F64Add => pp.text("+"),
            BinaryOp::F64Sub => pp.text("-"),
            BinaryOp::F64Mul => pp.text("*"),
            BinaryOp::F64Div => pp.text("/"),
            BinaryOp::F64LT => pp.text("<"),
            BinaryOp::F64GT => pp.text(">"),
            BinaryOp::F64Le => pp.text("<="),
            BinaryOp::F64Ge => pp.text(">="),
        }
    }
}

impl Pretty for Label {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            Label::Named(s) => pp.text(s),
            Label::App(n) => pp.concat(vec![pp.text("app"), pp.space(), pp.as_string(n)]),
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
            Lit::Bool(b) => pp.text(if *b { "true" } else { "false" }),
            Lit::I32(n) => pp.as_string(n),
            Lit::F64(x) => pp.text(format!("{}f", x)),
            // TOOD(arjun): Escape characters
            Lit::String(s) => pp.text("\"").append(pp.text(s)).append(pp.text("\"")),
            Lit::Interned(u) => pp.concat(vec![pp.text("interned"), pp.as_string(u).parens()]),
            Lit::Undefined => pp.text("undefined"),
            Lit::Null => pp.text("null"),
        }
    }
}

impl Pretty for ToAny {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        // NOTE(arjun): We do not print the type annotation, even if it exists, because the parser
        // canot parse it.
        pp.text("any").append(self.atom.pretty(pp).parens())
    }
}

impl Pretty for Atom {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        match self {
            Atom::Lit(l, _) => l.pretty(pp),
            Atom::ToAny(to_any, _) => to_any.pretty(pp),
            Atom::FromAny(a, t, _) => pp.concat(vec![
                a.pretty(pp),
                pp.space(),
                pp.text("as"),
                pp.space(),
                t.pretty(pp),
            ]),
            Atom::FloatToInt(a, _) => {
                pp.concat(vec![pp.text("float_to_int"), pp.space(), a.pretty(pp)])
            }
            Atom::IntToFloat(a, _) => {
                pp.concat(vec![pp.text("int_to_float"), pp.space(), a.pretty(pp)])
            }
            Atom::HTGet(l, r, _) => {
                pp.concat(vec![l.pretty(pp), pp.text(".htget"), r.pretty(pp).parens()])
            }
            Atom::ObjectGet(l, r, _) => match &**r {
                Atom::Lit(Lit::String(r), _) => {
                    l.pretty(pp).append(pp.text(".")).append(pp.text(r))
                }
                _ => l.pretty(pp).append(pp.text(".")).append(r.pretty(pp)),
            },
            Atom::Index(l, r, _) => pp.concat(vec![l.pretty(pp), r.pretty(pp).brackets()]),
            Atom::ArrayLen(a, _) => pp.concat(vec![a.pretty(pp), pp.text(".array_len()")]),
            Atom::Id(id, _) => pp.as_string(id),
            Atom::GetPrimFunc(id, _) => pp.concat(vec![pp.text("rt"), pp.as_string(id).parens()]),
            Atom::StringLen(a, _) => pp.concat(vec![a.pretty(pp), pp.text(".string_len()")]),
            Atom::Unary(op, a, _) => pp.concat(vec![op.pretty(pp), a.pretty(pp)]),
            Atom::Binary(op, l, r, _) => pp.concat(vec![l.pretty(pp), op.pretty(pp), r.pretty(pp)]),
            Atom::Deref(a, _, _) => pp.concat(vec![pp.text("*"), a.pretty(pp)]),
            Atom::EnvGet(index, t, _) => pp
                .text("env.")
                .append(pp.as_string(index))
                .append(pp.text(":"))
                .append(t.pretty(pp)),
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
            Expr::HT => pp.text("ht"),
            Expr::Array => pp.text("array"),
            Expr::Push(l, r, _) => {
                pp.concat(vec![l.pretty(pp), pp.text(".push"), r.pretty(pp).parens()])
            }
            Expr::ArraySet(a, b, c, _) => pp.concat(vec![
                pp.text("array_set"),
                pp.concat(vec![
                    a.pretty(pp),
                    pp.text(", "),
                    b.pretty(pp),
                    pp.text(", "),
                    c.pretty(pp),
                ])
                .braces(),
            ]),
            Expr::HTSet(a, b, c, _) => pp.concat(vec![
                pp.text("ht_set"),
                pp.concat(vec![
                    a.pretty(pp),
                    pp.text(", "),
                    b.pretty(pp),
                    pp.text(", "),
                    c.pretty(pp),
                ])
                .braces(),
            ]),
            Expr::Call(f, args, _) => pp.concat(vec![
                pp.as_string(f),
                pp.intersperse(
                    args.iter().map(|a| pp.as_string(a)),
                    pp.text(",").append(pp.space()),
                )
                .parens(),
            ]),
            Expr::ClosureCall(f, args, _) => pp.concat(vec![
                pp.as_string(f),
                pp.text("!"),
                pp.intersperse(
                    args.iter().map(|a| pp.as_string(a)),
                    pp.text(",").append(pp.space()),
                )
                .parens(),
            ]),

            Expr::PrimCall(rtsfun, args, _) => pp.concat(vec![
                pp.as_string(rtsfun),
                pp.intersperse(
                    args.iter().map(|a| a.pretty(pp)),
                    pp.text(",").append(pp.space()),
                )
                .parens(),
            ]),
            Expr::ObjectEmpty => pp.text("{}"),
            Expr::ObjectSet(a, Atom::Lit(Lit::String(s), _), c, _) => pp.concat(vec![
                a.pretty(pp),
                pp.text("."),
                pp.text(s),
                pp.text("="),
                c.pretty(pp),
                pp.text(";"),
            ]),
            Expr::ObjectSet(a, b, c, _) => pp.concat(vec![
                a.pretty(pp),
                pp.text("."),
                b.pretty(pp),
                pp.text("="),
                c.pretty(pp),
                pp.text(";"),
            ]),
            Expr::NewRef(a, ty, _) => pp.concat(vec![
                pp.text("new_ref::"),
                ty.pretty(pp),
                a.pretty(pp).parens(),
            ]),
            Expr::Atom(a, _) => a.pretty(pp),
            Expr::Closure(id, env, _) => pp.text("clos").append(
                pp.as_string(id)
                    .append(", ")
                    .append(
                        pp.intersperse(
                            env.iter()
                                .map(|(a, ty)| a.pretty(pp).append(": ").append(ty.pretty(pp))),
                            pp.text(", "),
                        ),
                    )
                    .parens(),
            ),
        }
    }
}

impl Pretty for VarStmt {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        if self.id == Id::from("_") {
            return self.named.pretty(pp);
        }
        pp.concat(vec![
            pp.text("var"),
            pp.space(),
            pp.as_string(self.id.clone()),
            self.ty
                .as_ref()
                .map(|t| pp.text(":").append(t.pretty(pp)))
                .unwrap_or(pp.space()),
            pp.space(),
            pp.text("="),
            pp.space(),
            self.named.pretty(pp),
            pp.text(";"),
        ])
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
            Stmt::Empty => pp.space(),
            Stmt::Var(var_stmt, _) => var_stmt.pretty(pp),
            Stmt::Expression(expr, _) => expr.pretty(pp),
            Stmt::Assign(x, expr, _) => pp.concat(vec![
                pp.as_string(x),
                pp.space(),
                pp.text("="),
                pp.space(),
                expr.pretty(pp),
                pp.text(";"),
            ]),
            Stmt::Store(x, expr, _) => pp.concat(vec![
                pp.text("*"),
                pp.as_string(x),
                pp.space(),
                pp.text("="),
                pp.space(),
                expr.pretty(pp),
                pp.text(";"),
            ]),
            Stmt::If(e, s1, s2, _) => pp.intersperse(
                vec![
                    pp.concat(vec![pp.text("if"), pp.space(), e.pretty(pp).parens()])
                        .group(),
                    s1.pretty(pp).nest(2),
                    pp.text("else"),
                    s2.pretty(pp).nest(2),
                ],
                pp.hardline(),
            ),
            Stmt::Loop(st, _) => pp.concat(vec![pp.text("loop"), pp.hardline(), st.pretty(pp)]),
            Stmt::Label(lbl, st, _) => pp.concat(vec![
                lbl.pretty(pp),
                pp.text(":"),
                pp.line(),
                st.pretty(pp).brackets().nest(2),
            ]),
            Stmt::Break(lbl, _) => pp.concat(vec![
                pp.text("break"),
                pp.space(),
                lbl.pretty(pp),
                pp.text(";"),
            ]),
            Stmt::Return(e, _) => pp.concat(vec![
                pp.text("return"),
                pp.space(),
                e.pretty(pp),
                pp.text(";"),
            ]),
            Stmt::Block(stmts, _) => pp
                .concat(vec![
                    pp.hardline(),
                    pp.intersperse(
                        stmts.iter().map(|s| s.pretty(pp).nest(2).group()),
                        pp.hardline(),
                    ),
                    pp.hardline(),
                ])
                .braces(),
            Stmt::Trap => pp.text("trap"),
            Stmt::Goto(lbl, _) => pp.concat(vec![
                pp.text("goto"),
                pp.space(),
                lbl.pretty(pp),
                pp.text(";"),
            ]),
        }
    }
}

impl Pretty for Global {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        pp.concat(vec![
            self.ty.pretty(pp),
            pp.space(),
            self.atom
                .as_ref()
                .map(|v| pp.text("= ").append(v.pretty(pp)))
                .unwrap_or(pp.space()),
            pp.text(";"),
        ])
    }
}

impl Pretty for Function {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        pp.concat(vec![
            pp.intersperse(
                self.params
                    .iter()
                    .zip(&self.fn_type.args)
                    .map(|(x, t)| pp.concat(vec![pp.as_string(x), pp.text(":"), t.pretty(pp)])),
                pp.text(",").append(pp.space()),
            )
            .parens(),
            pp.space(),
            // Print the result type, if there is one.
            self.fn_type
                .result
                .as_ref()
                .map(|result_ty| pp.text(":").append(pp.space()).append(result_ty.pretty(pp)))
                .unwrap_or(pp.space()),
            pp.space(),
            self.body.pretty(pp),
        ])
    }
}

impl Pretty for Program {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        pp.concat(vec![
            pp.intersperse(
                self.globals.iter().map(|(k, v)| {
                    pp.concat(vec![
                        pp.text("var"),
                        pp.space(),
                        pp.as_string(k),
                        pp.text(":"),
                        pp.space(),
                        v.pretty(pp),
                    ])
                }),
                pp.hardline(),
            )
            .nest(2),
            pp.hardline(),
            pp.hardline(),
            pp.intersperse(
                self.functions.iter().map(|(fn_name, v)| {
                    pp.concat(vec![
                        pp.text("function"),
                        pp.space(),
                        pp.as_string(fn_name),
                        pp.space(),
                        v.pretty(pp),
                    ])
                }),
                pp.hardline(),
            )
            .nest(2),
            // NOTE(arjun): Not displaying data segment
        ])
    }
}

fn hex_dump<'b, D, A>(pp: &'b D, arr: &'b [u8]) -> pretty::DocBuilder<'b, D, A>
where
    D: pretty::DocAllocator<'b, A>,
    A: std::clone::Clone,
    <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
{
    pp.intersperse(
        arr.iter().map(|h| pp.text(format!("{:02x}", h))),
        pp.space(),
    )
}

impl_Display_Pretty!(Program);
