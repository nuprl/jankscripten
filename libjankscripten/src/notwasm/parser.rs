use super::constructors as ctor;
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
        .or(lang.reserved_op("-").with(value(BinaryOp::I32Sub)))
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
                Some((op, rhs)) => ctor::binary_(op, lhs, rhs),
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
                Some((op, rhs)) => ctor::binary_(op, lhs, rhs),
                None => lhs,
            })
    }
}

parser! {
    fn atom_item['a, 'b, I](lang: &'b Lang<'a, I>)(I) -> Atom
    where [I: Stream<Item = char>]
    {
        lang.reserved("sqrt").with(lang.parens(atom(lang)))
            .map(|a| ctor::sqrt_(a))
        .or(lang.reserved("strlen").with(lang.parens(atom(lang)))
            .map(|a| ctor::len_(a)))
        .or(lit(lang).map(|l| Atom::Lit(l)))
        .or(attempt(id(lang)
            .skip(lang.reserved_op("."))
            .and(id(lang))
            .skip(lang.reserved_op(":"))
            .and(type_(lang))
            .map(|((x, field), ty)|
                ctor::object_get_(
                    Atom::Id(x),
                    ctor::str_(field.into_name()),
                    ty
                )
            ))
        )
        .or(attempt(id(lang)
            .skip(lang.reserved_op(":"))
            .and(id(lang))
            .skip(lang.reserved_op(":"))
            .and(type_(lang))
            .map(|((x, field), ty)| if field == ctor::id_("length") {
                ctor::array_len_(Atom::Id(x), ty)
            } else {
                ctor::ht_get_(
                    Atom::Id(x),
                    ctor::str_(field.into_name()),
                    ty
                )
            }))
        )
        .or(attempt(id(lang)
            .skip(lang.reserved_op("["))
            .and(atom(lang))
            .skip(lang.reserved_op("]"))
            .skip(lang.reserved_op(":"))
            .and(type_(lang))
            .map(|((array, index), ty)| ctor::index_(
                Atom::Id(array),
                index,
                ty
            )))
        )
        .or(id(lang).map(|i| Atom::Id(i)))
        .or(lang.parens(atom(lang)))
        .or(lang.reserved_op("&").with(id(lang).map(|id| Atom::GetAddr(id))))
        .or(lang.reserved_op("*").with(id(lang).map(|id| Atom::Deref(id))))
    }
}

parser! {
    fn expr['a, 'b, I](lang: &'b Lang<'a, I>)(I) ->  Expr
    where [ I: Stream<Item = char>]
    {
        attempt(type_(lang).skip(lang.reserved_op("[]")).map(|ty| Expr::Array(ty)))
        .or(type_(lang).skip(lang.reserved_op("{}")).map(|ty| Expr::HT(ty)))
        .or(lang.reserved_op("{}").map(|_| Expr::ObjectEmpty))
        .or(lang.reserved("arrayPush")
            .with(lang.parens(atom(lang).skip(lang.reserved_op(",")).and(atom(lang))))
            .skip(lang.reserved_op(":"))
            .and(type_(lang))
            .map(|((array, member), ty)| Expr::Push(array, member, ty)))
        .or(lang.reserved("sqrt").with(lang.parens(atom(lang)))
            .map(|a| Expr::Atom(ctor::sqrt_(a))))
        .or(attempt(id(lang)
            .and(lang.parens(sep_by(id(lang), lang.reserved_op(","))))
            .map(|(f, args)| Expr::Call(f, args))))
        .or(atom(lang).map(|a| Expr::Atom(a)))
    }
}

parser! {
    fn type_['a, 'b, I](lang: &'b Lang<'a, I>)(I) -> Type
    where [ I: Stream<Item = char>]
    {
        lang.reserved("i32").with(value(Type::I32))
            .or(lang.reserved("str").with(value(Type::StrRef)))
            .or(lang.reserved("bool").with(value(Type::Bool)))
            .or(lang.parens(sep_by(type_(lang), lang.reserved_op(",")))
                .skip(lang.reserved_op("->"))
                .and(type_(lang))
                .map(|(args, result)| Type::Fn(args, Box::new(Some(result)))))
            .or(lang.reserved("f64").with(value(Type::F64)))
            .or(lang.reserved("AnyClass").with(value(Type::AnyClass)))
            .or(lang.reserved("HT").with(lang.parens(type_(lang))).map(|t| ctor::ht_ty_(t)))
            .or(lang.reserved("Array").with(lang.parens(type_(lang))).map(|t| ctor::array_ty_(t)))
            .or(lang.reserved("Ref").with(lang.parens(type_(lang)))).map(|t| ctor::ref_ty_(t))
    }
}

parser! {
    fn stmt['a, 'b, I](lang: &'b Lang<'a,I>)(I) -> Stmt
    where [I: Stream<Item = char>]
    {
        let var = lang.reserved("var")
            .with(id(lang))
            .skip(lang.reserved_op(":"))
            .and(type_(lang))
            .skip(lang.reserved_op("="))
            .and(expr(lang))
            .skip(lang.reserved_op(";"))
            .map(|((x,t),e)| Stmt::Var(x, e, t));

        enum IdRhsInStmt {
            Expr(Expr),
            Stmt(Stmt)
        }

        let assign_or_label = attempt(id(lang)
           .and((lang.reserved_op("=").with(expr(lang)).skip(lang.reserved_op(";")).map(|e| IdRhsInStmt::Expr(e)))
                .or(lang.reserved_op(":").with(block(lang)).map(|s| IdRhsInStmt::Stmt(s))))
           .map(|(x, rhs)| match rhs {
               IdRhsInStmt::Expr(e) => Stmt::Assign(x, e),
               IdRhsInStmt::Stmt(s) => ctor::label_(x.into_name(), s)
           }));

        let object_set = id(lang)
            .skip(lang.reserved_op("."))
            .and(id(lang))
            .skip(lang.reserved_op(":"))
            .and(type_(lang))
            .skip(lang.reserved_op("="))
            .and(atom(lang))
            .skip(lang.reserved_op(";"))
            .map(|(((ht, field), ty), atom)| Stmt::Var(
                ctor::id_("_"),
                Expr::ObjectSet(
                    Atom::Id(ht),
                    ctor::str_(field.into_name()),
                    atom,
                    ty.clone(),
                ),
                ty,
            ));

        let ht_set = attempt(id(lang)
            .skip(lang.reserved_op(":"))
            .and(id(lang))
            .skip(lang.reserved_op(":"))
            .and(type_(lang))
            .skip(lang.reserved_op("="))
            .and(atom(lang))
            .skip(lang.reserved_op(";"))
            .map(|(((ht, field), ty), atom)| Stmt::Var(
                ctor::id_("_"),
                Expr::HTSet(
                    Atom::Id(ht),
                    ctor::str_(field.into_name()),
                    atom,
                    ty.clone(),
                ),
                ty,
            )));

        let if_ = lang.reserved("if")
            .with(lang.parens(atom(lang)))
            .and(block(lang))
            .skip(lang.reserved("else"))
            .and(block(lang))
            .map(|((a, s1), s2)| Stmt::If(a, Box::new(s1), Box::new(s2)));

        let loop_ = lang.reserved("loop")
            .with(block(lang))
            .map(|s| Stmt::Loop(Box::new(s)));

        let return_ = lang.reserved("return")
            .with(atom(lang))
            .skip(lang.reserved_op(";"))
            .map(|a| Stmt::Return(a));

        let break_ = lang.reserved("break")
            .with(id(lang))
            .skip(lang.reserved_op(";"))
            .map(|l| Stmt::Break(Label::Named(l.into_name())));

        let while_ = lang.reserved("while")
            .with(lang.parens(atom(lang)))
            .and(block(lang))
            .map(|(test,body)| ctor::while_(test, body));

        /////////////////////////////////////////////////////////////
        // Mark: is it possible to skip this reserved op in the beginning of the parse chain?
        let store = lang.reserved_op("*") 
            .with(id(lang))
            .skip(lang.reserved_op("="))
            .and(expr(lang))
            .skip(lang.reserved_op(";"))
            .map(|(id, expr)| Stmt::Store(id, expr)); 
            // Stmt::Store(Id::Named("aa".to_string()), Expr::Atom(Atom::Id(Id::Named("ha".to_string())))));

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
            object_set,
            store,
        ))
    }
}

parser! {
    fn block['a, 'b, I](lang: &'b Lang<'a,I>)(I) -> Stmt
    where [I: Stream<Item = char>]
    {
        lang.braces(many(stmt(lang)))
            .map(|ss| Stmt::Block(ss))
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
        .skip(lang.reserved_op(":"))
        .and(type_(lang))
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
                fn_type: FnType { args, result: Some(ret_ty) },
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
            let classes = HashMap::new();
            let data = Vec::new();
            Program { functions, classes, globals, data }
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
            ]
            .iter()
            .map(|x| (*x).into())
            .collect(),
        },
        // NOTE(arjun): This is from the combine-language documentation. I have
        // not bothered to understand it. But, it ought to define a pattern that
        // matches operators. Our operators are quite straightforward.
        op: Identifier {
            start: satisfy(|c| "+-*/[{:.<,".chars().any(|x| x == c)),
            rest: satisfy(|c| "]}.".chars().any(|x| x == c)),
            reserved: [
                "+", "-", "*", "/", "[]", "{}", ":", ".", "*.", "/.", "+.", "-.", "<", ",", "&",
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
