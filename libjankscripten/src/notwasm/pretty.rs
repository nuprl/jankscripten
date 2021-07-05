use super::syntax::*;

use crate::{prettyp, impl_Display_Pretty};
use crate::pretty::Pretty;

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
            BinaryOp::I32Xor => pp.text("^"),
            BinaryOp::I32Div => pp.text("/"),
            BinaryOp::I32Rem => pp.text("%"),
            BinaryOp::I32Shl => pp.text("<<"),
            BinaryOp::I32Shr => pp.text(">>"),
            BinaryOp::I32ShrU => pp.text(">>>"),
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

impl Pretty for Id {
    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A> 
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,    
    {
        pp.as_string(self)
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

impl Pretty for (Atom, Type) {

    fn pretty<'b, D, A>(&'b self, pp: &'b D) -> pretty::DocBuilder<'b, D, A>
    where
        D: pretty::DocAllocator<'b, A>,
        A: std::clone::Clone,
        <D as pretty::DocAllocator<'b, A>>::Doc: std::clone::Clone,
    {
        let (x, t) = self;
        prettyp!(pp, (seq (id x) ":" (id t)))
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
            Atom::PrimApp(f, args, _) => prettyp!(pp, (seq "@" (id f) (parens (comma_sep args)))),
            Atom::ToAny(to_any, _) => to_any.pretty(pp),
            Atom::FromAny(a, t, _) => prettyp!(pp, (seq (id a) space "as" space (id t))),
            Atom::FloatToInt(a, _) => prettyp!(pp, (seq "float_to_int" space (id a))),
            Atom::IntToFloat(a, _) => prettyp!(pp, (seq "int_to_float" space (id a))),
            Atom::ObjectGet(l, r, _) => match &**r {
                Atom::Lit(Lit::String(r), _) => prettyp!(pp, (seq (id l) ".htget" (id r))),
                _ => prettyp!(pp, (seq (id l) "." (id r))),
            },
            Atom::Id(id, _) => pp.as_string(id),          
            Atom::GetPrimFunc(id, _) => prettyp!(pp, (seq "rt" (parens (id id)))),
            Atom::Unary(op, a, _) => prettyp!(pp, (seq (id op) (id a))),
            Atom::Binary(op, l, r, _) => prettyp!(pp, (seq (id l) (id op) (id r))),
            Atom::Deref(a, _, _) => prettyp!(pp, (seq "*" (id a))),
            Atom::EnvGet(index, t, _) => prettyp!(pp, (seq "env." (id index) ":" (id t))),
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
            Expr::Push(l, r, _) =>
                // pp.concat(vec![l.pretty(pp), pp.text(".push"), r.pretty(pp).parens()]),
                prettyp!(pp, (seq (id l) ".push" (parens (id r)))),
            Expr::ArraySet(a, b, c, _) => /* pp.concat(vec![
                pp.text("array_set"),
                pp.concat(vec![
                    a.pretty(pp),
                    pp.text(", "),
                    b.pretty(pp),
                    pp.text(", "),
                    c.pretty(pp),
                ])
                .braces(),
            ]), */
                prettyp!(pp, (seq "array_set" (braces (seq (id a) "," (id b) "," (id c))))),
            Expr::HTSet(a, b, c, _) => /* pp.concat(vec![
                pp.text("ht_set"),
                pp.concat(vec![
                    a.pretty(pp),
                    pp.text(", "),
                    b.pretty(pp),
                    pp.text(", "),
                    c.pretty(pp),
                ])
                .braces(),
            ]), */
                prettyp!(pp, (seq "ht_set" (braces (seq (id a) "," (id b) "," (id c))))),
            Expr::Call(f, args, _) => /* pp.concat(vec![
                pp.as_string(f),
                pp.intersperse(
                    args.iter().map(|a| pp.as_string(a)),
                    pp.text(",").append(pp.space()),
                )
                .parens(),
            ]), */
                prettyp!(pp, (seq (id f) (parens (comma_sep args)))),
            Expr::ClosureCall(f, args, _) => prettyp!(pp, (seq (id f) "!" (parens (comma_sep args)))),
            Expr::PrimCall(rtsfun, args, _) => prettyp!(pp, (seq (as_string rtsfun) (parens (comma_sep args)))),
            Expr::ObjectEmpty => pp.text("{}"),
            Expr::ObjectSet(a, Atom::Lit(Lit::String(s), _), c, _) => prettyp!(pp, (seq (id a) "." (id s) "=" (id c) ";")),
            Expr::ObjectSet(a, b, c, _) => prettyp!(pp, (seq (id a) "." (id b) "=" (id c) ";")),
            Expr::NewRef(a, ty, _) => prettyp!(pp, (seq "new_ref::" (id ty) (parens (id a)))),
            Expr::Atom(a, _) => a.pretty(pp),
            Expr::Closure(id, env, _) => prettyp!(pp, (seq "clos" (id id) (comma_sep env))),
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
            Stmt::Assign(x, expr, _) => prettyp!(pp, (seq (id x) "=" (id expr) ";")),
            Stmt::Store(x, expr, _) => prettyp!(pp, (seq "*" (id x) "=" (id expr) ";")),
            Stmt::If(e, s1, s2, _) => prettyp!(pp, (seq (seq "if" space (parens (id e)) (nest (id s1)) "else" (nest (id s2))))),
            Stmt::Loop(st, _) => prettyp!(pp, (seq "loop" (id st))),
            Stmt::Label(lbl, st, _) => prettyp!(pp, (seq (id lbl) ":" line (id st))),
            Stmt::Break(lbl, _) => prettyp!(pp, (seq "break" space (id lbl) ";")),
            Stmt::Return(e, _) => prettyp!(pp, (seq "return" space (id e) ";")),
            Stmt::Block(stmts, _) => prettyp!(pp, (nest (braces (line_sep stmts)))),
            Stmt::Trap => pp.text("trap"),
            Stmt::Goto(lbl, _) => prettyp!(pp, (seq "goto" space (id lbl) ";")),
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

fn _hex_dump<'b, D, A>(pp: &'b D, arr: &'b [u8]) -> pretty::DocBuilder<'b, D, A>
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
