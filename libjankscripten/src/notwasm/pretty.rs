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
            Type::String => pp.text("string"),
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
            Lit::F64(n) => pp.as_string(n),
            Lit::String(s) => pp.text(s),
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
        pp.concat(vec![
            self.atom.pretty(pp),
            pp.text(" as any::"),
            self.ty
                .as_ref()
                .map(|t| t.pretty(pp))
                .unwrap_or(pp.text("_")),
        ])
    }
}

impl Pretty for Atom {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        pp.concat(vec![
            pp.text("⚛️"),
            (match self {
                Atom::Lit(l) => l.pretty(pp),
                Atom::ToAny(to_any) => to_any.pretty(pp),
                Atom::FromAny(a, t) => pp.concat(vec![
                    a.pretty(pp),
                    pp.space(),
                    pp.text("as"),
                    pp.space(),
                    t.pretty(pp),
                ]),
                Atom::FloatToInt(a) => {
                    pp.concat(vec![pp.text("float_to_int"), pp.space(), a.pretty(pp)])
                }
                Atom::IntToFloat(a) => {
                    pp.concat(vec![pp.text("int_to_float"), pp.space(), a.pretty(pp)])
                }
                Atom::HTGet(l, r) => {
                    pp.concat(vec![l.pretty(pp), pp.text(".htget"), r.pretty(pp).parens()])
                }
                Atom::ObjectGet(l, r) => pp.concat(vec![
                    l.pretty(pp),
                    pp.text(".objget"),
                    r.pretty(pp).parens(),
                ]),
                Atom::Index(l, r) => pp.concat(vec![l.pretty(pp), r.pretty(pp).brackets()]),
                Atom::ArrayLen(a) => pp.concat(vec![a.pretty(pp), pp.text(".array_len()")]),
                Atom::Id(id) => pp.as_string(id),
                Atom::GetPrimFunc(id) => pp.concat(vec![pp.text("rt"), pp.as_string(id).parens()]),
                Atom::StringLen(a) => pp.concat(vec![a.pretty(pp), pp.text(".string_len()")]),
                Atom::Unary(op, a) => pp.concat(vec![op.pretty(pp), a.pretty(pp)]),
                Atom::Binary(op, l, r) => {
                    pp.concat(vec![l.pretty(pp), op.pretty(pp), r.pretty(pp)])
                }
                Atom::Deref(a, _) => pp.concat(vec![pp.text("*"), a.pretty(pp)]),
                Atom::EnvGet(index, _) => pp.text("env.").append(pp.as_string(index)),
            }),
            pp.text("⚛️"),
        ])
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
            Expr::Push(l, r) => {
                pp.concat(vec![l.pretty(pp), pp.text(".push"), r.pretty(pp).parens()])
            }
            Expr::ArraySet(a, b, c) => pp.concat(vec![
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
            Expr::HTSet(a, b, c) => pp.concat(vec![
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
            Expr::Call(f, args) | Expr::ClosureCall(f, args) => pp.concat(vec![
                pp.as_string(f),
                pp.intersperse(
                    args.iter().map(|a| pp.as_string(a)),
                    pp.text(",").append(pp.space()),
                )
                .parens(),
            ]),
            Expr::PrimCall(rtsfun, args) => pp.concat(vec![
                pp.as_string(rtsfun),
                pp.intersperse(
                    args.iter().map(|a| a.pretty(pp)),
                    pp.text(",").append(pp.space()),
                )
                .parens(),
            ]),
            Expr::ObjectEmpty => pp.text("{}"),
            Expr::ObjectSet(a, b, c) => pp.concat(vec![
                pp.text("obj_set"),
                pp.concat(vec![
                    a.pretty(pp),
                    pp.text(", "),
                    b.pretty(pp),
                    pp.text(", "),
                    c.pretty(pp),
                ])
                .braces(),
            ]),
            Expr::NewRef(a, ty) => pp.concat(vec![
                pp.text("new_ref::"),
                ty.pretty(pp),
                a.pretty(pp).parens(),
            ]),
            Expr::Atom(a) => a.pretty(pp),
            Expr::Closure(id, env) => pp.text("clos").append(
                pp.as_string(id)
                    .append(", ")
                    .append(
                        pp.intersperse(
                            env.iter()
                                .map(|(a, ty)| a.pretty(pp).append(": ").append(ty.pretty(pp))),
                            pp.text(", "),
                        )
                        .brackets(),
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
        pp.concat(vec![
            pp.text("var"),
            pp.space(),
            pp.as_string(self.id.clone()),
            pp.text(": "),
            self.ty
                .as_ref()
                .map(|t| t.pretty(pp))
                .unwrap_or(pp.text("none")),
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
            Stmt::Empty => pp.text("▢"),
            Stmt::Var(var_stmt) => var_stmt.pretty(pp),
            Stmt::Expression(expr) => expr.pretty(pp),
            Stmt::Assign(x, expr) => pp.concat(vec![
                pp.as_string(x),
                pp.space(),
                pp.text("="),
                pp.space(),
                expr.pretty(pp),
                pp.text(";"),
            ]),
            Stmt::Store(x, expr) => pp.concat(vec![
                pp.text("*"),
                pp.as_string(x),
                pp.space(),
                pp.text("="),
                pp.space(),
                expr.pretty(pp),
                pp.text(";"),
            ]),
            Stmt::If(e, s1, s2) => pp.intersperse(
                vec![
                    pp.concat(vec![pp.text("if"), pp.space(), e.pretty(pp).parens()])
                        .group(),
                    s1.pretty(pp).nest(2),
                    pp.text("else"),
                    s2.pretty(pp).nest(2),
                ],
                pp.hardline(),
            ),
            Stmt::Loop(s) => pp.concat(vec![pp.text("loop"), pp.hardline(), s.pretty(pp)]),
            Stmt::Label(lbl, s) => pp.concat(vec![
                lbl.pretty(pp),
                pp.text(":"),
                pp.line(),
                s.pretty(pp).brackets().nest(2),
            ]),
            Stmt::Break(lbl) => pp.concat(vec![
                pp.text("break"),
                pp.space(),
                lbl.pretty(pp),
                pp.text(";"),
            ]),
            Stmt::Return(e) => pp.concat(vec![
                pp.text("return"),
                pp.space(),
                e.pretty(pp),
                pp.text(";"),
            ]),
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
            Stmt::Trap => pp.text("trap"),
            Stmt::Goto(lbl) => pp.concat(vec![
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
            pp.text(if self.is_mut { "true" } else { "false" }),
            pp.text(","),
            pp.space(),
            self.ty.pretty(pp),
            pp.text(","),
            pp.space(),
            self.atom.pretty(pp),
        ])
        .angles()
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
            pp.text("function"),
            pp.space(),
            pp.intersperse(
                self.params.iter().map(|p| pp.as_string(p)),
                pp.text(",").append(pp.space()),
            )
            .parens(),
            pp.space(),
            pp.text(":"),
            pp.space(),
            self.fn_type.pretty(pp),
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
            pp.text("FUNCTIONS:"),
            pp.hardline(),
            pp.intersperse(
                self.functions.iter().map(|(k, v)| {
                    pp.concat(vec![
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
            pp.text("GLOBALS:"),
            pp.hardline(),
            pp.intersperse(
                self.globals.iter().map(|(k, v)| {
                    pp.concat(vec![
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
            pp.text("DATA:"),
            hex_dump(pp, &self.data),
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
