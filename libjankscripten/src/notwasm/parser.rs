use super::constructors as ctor;
use super::syntax::DUMMY_SP as D_S;
use super::syntax::*;
use combine::parser;
use combine::parser::char::{alpha_num, letter, string};
use combine::stream::state::State;
use combine::stream::Stream;
use combine::token;
use combine::{attempt, choice, eof, many, optional, satisfy, sep_by, value, Parser};
use combine_language::{Identifier, LanguageDef, LanguageEnv};
use std::collections::HashMap;

type Lang<'a, I> = LanguageEnv<'a, I>;

parser! {
    fn lit['a, 'b, I](lang: &'b Lang<'a, I>)(I) -> Lit
    where [I: Stream<Item = char>]
    {
        attempt(lang.float().skip(lang.symbol("f")).map(|n| Lit::F64(n)))
        .or(lang.integer().map(|n| Lit::I32(n as i32)))
        .or(lang.reserved("true").with(value(Lit::Bool(true))))
        .or(lang.reserved("false").with(value(Lit::Bool(false))))
        .or(lang.reserved("null").with(value(Lit::Null)))
        .or(lang.string_literal().map(|s| Lit::String(s)))

    }
}

parser! {
    fn id['a, 'b, I](lang: &'b Lang<'a, I>)(I) -> Id
    where [I: Stream<Item = char>]
    {
        // $IDENT is allowed in js and used in JankyScript
        optional(token('$')).and(lang.identifier()).map(|(dollar, x)|
            Id::Named(if dollar.is_some() {
                format!("{}{}", '$', x)
            } else {
                x.to_string()
            })
        )
    }
}

parser! {
    fn binop_prec_mul['a, 'b, I](lang: &'b Lang<'a, I>)(I) -> BinaryOp
    where [I: Stream<Item = char>]
    {
        lang.reserved_op("*").with(value(BinaryOp::I32Mul))
        .or(lang.reserved_op("*.").with(value(BinaryOp::F64Mul)))
        .or(lang.reserved_op("/.").with(value(BinaryOp::F64Div)))
    }
}
parser! {
    fn binop_prec_add['a, 'b, I](lang: &'b Lang<'a, I>)(I) -> BinaryOp
    where [I: Stream<Item = char>]
    {
        lang.reserved_op("+").with(value(BinaryOp::I32Add))
        .or(lang.reserved_op(">").with(value(BinaryOp::I32GT)))
        .or(lang.reserved_op("<").with(value(BinaryOp::I32LT)))
        .or(lang.reserved_op(">=").with(value(BinaryOp::I32Ge)))
        .or(lang.reserved_op("<=").with(value(BinaryOp::I32Le)))
        .or(lang.reserved_op("-").with(value(BinaryOp::I32Sub)))
        .or(lang.reserved_op("===").with(value(BinaryOp::PtrEq)))
        .or(lang.reserved_op("==").with(value(BinaryOp::I32Eq)))
        .or(lang.reserved_op("+.").with(value(BinaryOp::F64Add)))
        .or(lang.reserved_op("-.").with(value(BinaryOp::F64Sub)))
    }
}

parser! {
    fn product['a, 'b, I](lang: &'b Lang<'a, I>)(I) -> Atom
    where [I: Stream<Item = char>]
    {
        atom_item(lang)
            .and(optional(binop_prec_mul(lang)
                .and(product(lang))))
            .map(|(lhs, maybe_op_rhs)| match maybe_op_rhs {
                Some((op, rhs)) => ctor::binary_(op, lhs, rhs, D_S),
                None => lhs,
            })
    }
}

parser! {
    fn atom['a, 'b, I](lang: &'b Lang<'a, I>)(I) -> Atom
    where [I: Stream<Item = char>]
    {
        product(lang)
            .and(optional(binop_prec_add(lang)
                .and(atom(lang))))
            .map(|(lhs, maybe_op_rhs)| match maybe_op_rhs {
                Some((op, rhs)) => ctor::binary_(op, lhs, rhs, D_S),
                None => lhs,
            })
    }
}

parser! {
    fn atom_item['a, 'b, I](lang: &'b Lang<'a, I>)(I) -> Atom
    where [I: Stream<Item = char>]
    {
        lang.reserved("sqrt").with(lang.parens(atom(lang)))
            .map(|a| ctor::sqrt_(a, D_S))
        .or(lang.reserved("strlen").with(lang.parens(atom(lang)))
            .map(|a| ctor::len_(a, D_S)))
        .or(
            lang.reserved("any")
                .with(lang.parens(atom(lang)))
                .map(|a| ctor::to_any_(a, D_S)))
        .or(
            lang.reserved("env").skip(lang.reserved_op("."))
                .with(lang.integer())
                .skip(lang.reserved_op(":"))
                .and(type_(lang))
                .map(|(i, ty)| Atom::EnvGet(i as u32, ty, D_S)))
        .or(lang.reserved("rt").with(lang.parens(id(lang)))
            .map(|id| Atom::GetPrimFunc(id, D_S)))
        .or(lit(lang).map(|l| Atom::Lit(l, D_S)))
        .or(attempt(id(lang)
            .skip(lang.reserved_op("."))
            .and(id(lang))
            .map(|(x, field)|
                ctor::object_get_(
                    Atom::Id(x, D_S),
                    ctor::str_(field.into_name(), D_S),
                D_S)
            ))
        )
        .or(attempt(id(lang)
            .skip(lang.reserved_op("<<"))
            .and(id(lang))
            .map(|(x, field)| if field == ctor::id_("length") {
                ctor::array_len_(Atom::Id(x, D_S), D_S)
            } else {
                ctor::ht_get_(
                    Atom::Id(x, D_S),
                    ctor::str_(field.into_name(), D_S),
                D_S)
            }))
        )
        .or(attempt(id(lang)
            .skip(lang.reserved_op("["))
            .and(atom(lang))
            .skip(lang.reserved_op("]"))
            .map(|(array, index)| ctor::index_(
                Atom::Id(array, D_S),
                index,
            D_S)))
        )
        .or(id(lang).map(|i| Atom::Id(i, D_S)))
        .or(lang.parens(atom(lang)))
        .or(lang.reserved_op("*").with(atom(lang)).skip(lang.reserved_op(":"))
            .and(type_(lang)).map(|(a, ty)| ctor::deref_(a, ty, D_S)))
        .and(optional(
            lang.reserved("as").with(type_(lang))))
        .map(|(atom, maybe_as_ty)| match maybe_as_ty {
            Some(ty) => ctor::from_any_(atom, ty, D_S),
            None => atom,
        })
    }
}

parser! {
    fn expr['a, 'b, I](lang: &'b Lang<'a, I>)(I) ->  Expr
    where [ I: Stream<Item = char>]
    {
        attempt(lang.reserved_op("[]").map(|_| Expr::Array))
        .or(lang.reserved_op("HT{}").map(|_| Expr::HT))
        .or(lang.reserved_op("{}").map(|_| Expr::ObjectEmpty))
        .or(lang.reserved("clos").with(
                lang.parens(id(lang).skip(lang.reserved_op(","))
                    .and(sep_by(
                            atom(lang).skip(lang.reserved_op(":")).and(type_(lang)),
                            lang.reserved_op(",")))))
                .map(|(id, vars)| Expr::Closure(id, vars, D_S)))
        .or(lang.reserved("arrayPush")
            .with(lang.parens(atom(lang).skip(lang.reserved_op(",")).and(atom(lang))))
            .map(|(array, member)| Expr::Push(array, member, D_S)))
        .or(lang.reserved("sqrt").with(lang.parens(atom(lang)))
            .map(|a| Expr::Atom(ctor::sqrt_(a, D_S), D_S)))
        .or(lang.reserved_op("newRef").with(lang.parens(atom(lang).skip(lang.reserved_op(",")).and(type_(lang)))).map(|(val, ty)| Expr::NewRef(val, ty, D_S)))
        .or(attempt(id(lang).skip(lang.reserved_op("!"))
            .and(lang.parens(sep_by(id(lang), lang.reserved_op(","))))
            .map(|(f, args)| Expr::ClosureCall(f, args, D_S))))
        .or(attempt(id(lang)
            .and(lang.parens(sep_by(id(lang), lang.reserved_op(","))))
            .map(|(f, args)| Expr::Call(f, args, D_S))))
        .or(atom(lang).map(|a| Expr::Atom(a, D_S)))
    }
}

parser! {
    fn fn_type['a, 'b, I](lang: &'b Lang<'a,I>)(I) -> FnType
    where [I: Stream<Item = char>]
    {
        lang.parens(sep_by(type_(lang), lang.reserved_op(",")))
            .skip(lang.reserved_op("->"))
            .and(type_(lang).map(|t| Some(Box::new(t))).or(lang.reserved("void").map(|_| None)))
            .map(|(args, result)| FnType {
                args,
                result
            })
    }
}

parser! {
    fn type_['a, 'b, I](lang: &'b Lang<'a, I>)(I) -> Type
    where [ I: Stream<Item = char>]
    {
        lang.reserved("i32").with(value(Type::I32))
            .or(lang.reserved("str").with(value(Type::String)))
            .or(lang.reserved("bool").with(value(Type::Bool)))
            .or(fn_type(lang).map(|f| Type::Fn(f)))
            .or(lang.reserved("clos").with(fn_type(lang)).map(|f| Type::Closure(f)))
            .or(lang.reserved("f64").with(value(Type::F64)))
            .or(lang.reserved("any").with(value(Type::Any)))
            .or(lang.reserved("DynObject").with(value(Type::DynObject)))
            .or(lang.reserved("HT").with(value(Type::HT)))
            .or(lang.reserved("Array").with(value(Type::Array)))
            .or(lang.reserved("Ref").with(lang.parens(type_(lang))).map(|t| ctor::ref_ty_(t)))
            .or(lang.reserved("env").with(value(Type::Env)))
    }
}

parser! {
    fn stmt['a, 'b, I](lang: &'b Lang<'a,I>)(I) -> Stmt
    where [I: Stream<Item = char>]
    {
        let var = lang.reserved("var")
            .with(id(lang))
            .and(optional(lang.reserved_op(":").with(type_(lang))))
            .skip(lang.reserved_op("="))
            .and(expr(lang))
            .skip(lang.reserved_op(";"))
            .map(|((id, ty), named)| Stmt::Var(VarStmt { id, named, ty }, D_S));

        enum IdRhsInStmt {
            Expr(Expr),
            Stmt(Stmt)
        }

        let assign_or_label = attempt(id(lang)
           .and((lang.reserved_op("=").with(expr(lang)).skip(lang.reserved_op(";")).map(|e| IdRhsInStmt::Expr(e)))
                .or(lang.reserved_op(":").with(block(lang)).map(|s| IdRhsInStmt::Stmt(s))))
           .map(|(x, rhs)| match rhs {
               IdRhsInStmt::Expr(e) => Stmt::Assign(x, e, D_S),
               IdRhsInStmt::Stmt(s) => ctor::label_(x.into_name(), s, D_S)
           }));

        let object_set = id(lang)
            .skip(lang.reserved_op("."))
            .and(id(lang))
            .skip(lang.reserved_op("="))
            .and(atom(lang))
            .skip(lang.reserved_op(";"))
            .map(|((ht, field), atom)| Stmt::Var(
                VarStmt::new(ctor::id_("_"),
                Expr::ObjectSet(
                    Atom::Id(ht, D_S),
                    ctor::str_(field.into_name(), D_S),
                    atom,
                D_S)), D_S));

        let ht_set = attempt(id(lang)
            .skip(lang.reserved_op("<<"))
            .and(id(lang))
            .skip(lang.reserved_op("="))
            .and(atom(lang))
            .skip(lang.reserved_op(";"))
            .map(|((ht, field), atom)| Stmt::Var(
                VarStmt::new(
                    ctor::id_("_"),
                    Expr::HTSet(
                        Atom::Id(ht, D_S),
                        ctor::str_(field.into_name(), D_S),
                        atom, D_S)), D_S)));

        let if_ = lang.reserved("if")
            .with(lang.parens(atom(lang)))
            .and(block(lang))
            .skip(lang.reserved("else"))
            .and(block(lang))
            .map(|((a, s1), s2)| Stmt::If(a, Box::new(s1), Box::new(s2), D_S));

        let loop_ = lang.reserved("loop")
            .with(block(lang))
            .map(|s| Stmt::Loop(Box::new(s), D_S));

        let return_ = lang.reserved("return")
            .with(atom(lang))
            .skip(lang.reserved_op(";"))
            .map(|a| Stmt::Return(a, D_S));

        let break_ = lang.reserved("break")
            .with(id(lang))
            .skip(lang.reserved_op(";"))
            .map(|l| Stmt::Break(Label::Named(l.into_name()), D_S));

        let while_ = lang.reserved("while")
            .with(lang.parens(atom(lang)))
            .and(block(lang))
            .map(|(test,body)| ctor::while_(test, body, D_S));

        let store = lang.reserved_op("*")
            .with(id(lang))
            .skip(lang.reserved_op("="))
            .and(expr(lang))
            .skip(lang.reserved_op(";"))
            .map(|(id, expr)| Stmt::Store(id, expr, D_S));

        let expression = expr(lang).skip(lang.reserved_op(";")).map(|e| Stmt::Expression(e, D_S));

        choice((
            var,
            while_,
            if_,
            return_,
            block(lang),
            loop_,
            break_,
            assign_or_label,
            ht_set,
            attempt(object_set),
            store,
            expression
        ))
    }
}

parser! {
    fn block['a, 'b, I](lang: &'b Lang<'a,I>)(I) -> Stmt
    where [I: Stream<Item = char>]
    {
        lang.braces(many(stmt(lang)))
            .map(|ss| Stmt::Block(ss, D_S))
    }
}

parser! {
    fn global['a, 'b, I](lang: &'b Lang<'a,I>)(I) -> (Id, Global)
    where [I: Stream<Item = char>]
    {
        lang.reserved("const")
            .or(lang.reserved("var"))
            .and(id(lang))
            .skip(lang.reserved_op(":"))
            .and(type_(lang))
            .skip(lang.reserved_op("="))
            .and(atom(lang))
            .skip(lang.reserved_op(";"))
            .map(|(((const_or_var, name), ty), atom)| (
                name,
                Global {
                    is_mut: const_or_var == "var",
                    ty,
                    atom,
                }
            ))
    }
}

parser! {
    fn function['a, 'b, I](lang: &'b Lang<'a,I>)(I) -> (Id, Function)
    where [I: Stream<Item = char>]
    {
        lang.reserved("function")
        .with(id(lang))
        .and(lang.parens(
            sep_by(id(lang).skip(lang.reserved_op(":")).and(type_(lang)), lang.reserved_op(","))))
        .and(optional(lang.reserved_op(":").with(type_(lang))))
        .and(block(lang))
        .map(|(((f, params_tys), ret_ty), body): (((_, Vec<(Id, Type)>), _), _)| {
            let mut args = Vec::new();
            let mut params = Vec::new();
            for (param, arg) in params_tys {
                args.push(arg);
                params.push(param);
            }
            // TODO(luna): support void
            (f, Function {
                body,
                fn_type: FnType { args, result: ret_ty.and_then(|t| Some(Box::new(t))) },
                params
            })
        })
    }
}

parser! {
    fn program['a, 'b, I](lang: &'b Lang<'a,I>)(I) -> Program
    where [I: Stream<Item = char>]
    {
        lang.white_space()
        .with(many::<HashMap<_, _>, _>(global(lang)))
        .and(many::<HashMap<_, _>, _>(function(lang)))
        .skip(eof())
        .map(|(globals, functions)| {
            let data = Vec::new();
            Program { functions, globals, data }
        })
    }
}

pub fn parse(input: &str) -> Program {
    // NOTE(arjun): It would be nice to extract this language definition into
    // a function. But, I have no idea what its type should be, since the
    // input type is not &str, but a "stream" (defined in the combine library).
    let lang = LanguageEnv::new(LanguageDef {
        ident: Identifier {
            start: letter().or(token('$')).or(token('_')),
            rest: alpha_num().or(token('_')),
            reserved: [
                "as",
                "clos",
                "env",
                "if",
                "else",
                "true",
                "false",
                "function",
                "loop",
                "return",
                "i32",
                "string",
                "bool",
                "while",
                "f64",
                "HT",
                "Array",
                "const",
                "var",
                "arrayPush",
                "strlen",
                "any",
                "rt",
            ]
            .iter()
            .map(|x| (*x).into())
            .collect(),
        },
        // NOTE(arjun): This is from the combine-language documentation. I have
        // not bothered to understand it. But, it ought to define a pattern that
        // matches operators. Our operators are quite straightforward.
        op: Identifier {
            start: satisfy(|c| "+-*/[{:.<,=!".chars().any(|x| x == c)),
            rest: satisfy(|c| "]}.<>=".chars().any(|x| x == c)),
            reserved: [
                "=", "==", "===", "+", "-", "*", "/", "[]", "{}", ":", ".", "*.", "/.", "+.", "-.",
                "<", ",", "&", "<<", "->", "!",
            ]
            .iter()
            .map(|x| (*x).into())
            .collect(),
        },
        comment_start: string("/*").map(|_| ()),
        comment_end: string("*/").map(|_| ()),
        comment_line: string("//").map(|_| ()),
    });

    let mut parser = program(&lang);
    let input_stream = State::new(input);
    let p = parser.easy_parse(input_stream).expect("parsing error");
    return p.0;
}
