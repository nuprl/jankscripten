use super::constructors as ctor;
use super::syntax::*;
use combine::position;
use combine::stream::state::SourcePosition;
use combine::parser;
use combine::parser::char::{alpha_num, letter, string};
use combine::stream::state::State;
use combine::stream::Stream;
use combine::token;
use combine::{attempt, choice, eof, many, optional, satisfy, sep_by, value, Parser};
use combine_language::{Identifier, LanguageDef, LanguageEnv};
use std::collections::HashMap;
use std::rc::Rc;
use crate::pos::Pos;

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
    fn binop_prec_add['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a, I>)(I) -> BinaryOp
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
    fn product['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a, I>)(I) -> Atom
    where [I: Stream<Item = char, Position = SourcePosition>]
    {
        position()
        .and(atom_item(file, lang))
            .and(optional(binop_prec_mul(lang)
                .and(product(file, lang))))
            .map(|((p, lhs), maybe_op_rhs)| match maybe_op_rhs {
                Some((op, rhs)) => ctor::binary_(op, lhs, rhs, Pos::from_combine(file, p)),
                None => lhs,
            })
    }
}

parser! {
    fn atom['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a, I>)(I) -> Atom
    where [I: Stream<Item = char, Position = SourcePosition>]
    {
        position()
        .and(product(file, lang))
            .and(optional(binop_prec_add(file, lang)
                .and(atom(file, lang))))
            .map(|((p, lhs), maybe_op_rhs)| match maybe_op_rhs {
                Some((op, rhs)) => ctor::binary_(op, lhs, rhs, Pos::from_combine(file, p)),
                None => lhs,
            })
    }
}

parser! {
    fn atom_item['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a, I>)(I) -> Atom
    where [I: Stream<Item = char, Position = SourcePosition>]
    {
        position().and(lang.reserved("sqrt").with(lang.parens(atom(file, lang))))
            .map(|(p, a)| ctor::sqrt_(a, Pos::from_combine(file, p)))
        .or(position().and(lang.reserved("strlen").with(lang.parens(atom(file, lang))))
            .map(|(p, a)| ctor::len_(a, Pos::from_combine(file, p))))
        .or(
            position().and(lang.reserved("any")
                .with(lang.parens(atom(file, lang))))
                .map(|(p, a)| ctor::to_any_(a, Pos::from_combine(file, p))))
        .or(
            position()
              .skip(lang.reserved("env"))
              .skip(lang.reserved_op("."))
              .and(lang.integer())
              .skip(lang.reserved_op(":"))
              .and(type_(file, lang))
              .map(|((p, i), ty)| Atom::EnvGet(i as u32, ty, Pos::from_combine(file, p))))
        .or(
            position()
                .skip(lang.reserved("rt"))
                .and(lang.parens(id(lang)))
                .map(|(p, id)| Atom::GetPrimFunc(id, Pos::from_combine(file, p))))
        .or(position()
            .and(lit(lang))
            .map(|(p, l)| Atom::Lit(l, Pos::from_combine(file, p))))
        .or(attempt(
            position()
                .and(id(lang))
                .skip(lang.reserved_op("."))
                .and(id(lang))
                .map(|((p, x), field)|
                    ctor::object_get_(
                        Atom::Id(x, Pos::from_combine(file, p)),
                        ctor::str_(field.into_name(), Pos::from_combine(file, p)),
                    Pos::from_combine(file, p))
            ))
        )
        .or(attempt(
            position()
                .and(id(lang))
                .skip(lang.reserved_op("<<"))
                .and(id(lang))
                .map(|((p, x), field)| if field == ctor::id_("length") {
                    ctor::array_len_(Atom::Id(x, Pos::from_combine(file, p)), Pos::from_combine(file, p))
                } else {
                    ctor::ht_get_(
                        Atom::Id(x, Pos::from_combine(file, p)),
                        ctor::str_(field.into_name(), Pos::from_combine(file, p)),
                    Pos::from_combine(file, p))
                }))
        )
        .or(attempt(
            position()
                .and(id(lang))
                .skip(lang.reserved_op("["))
                .and(atom(file, lang))
                .skip(lang.reserved_op("]"))
                .map(|((p, array), index)| ctor::index_(
                    Atom::Id(array, Pos::from_combine(file, p)),
                    index,
                Pos::from_combine(file, p))))
            )
        .or(position()
            .and(id(lang))
            .map(|(p, i)| Atom::Id(i, Pos::from_combine(file, p))))
        .or(lang.parens(atom(file, lang)))
        .or(
            position()
            .skip(lang.reserved_op("*"))
            .and(atom(file, lang))
            .skip(lang.reserved_op(":"))
            .and(type_(file, lang))
            .map(|((p, a), ty)| ctor::deref_(a, ty, Pos::from_combine(file, p))))
        .and(optional(
              position()
                .skip(lang.reserved("as"))
              .and(type_(file, lang))))
        .map(|(atom, maybe_as_ty)| match maybe_as_ty {
            Some((p, ty)) => ctor::from_any_(atom, ty, Pos::from_combine(file, p)),
            None => atom,
        })
    }
}

parser! {
    fn expr['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a, I>)(I) ->  Expr
    where [I: Stream<Item = char, Position = SourcePosition>]
    {
        attempt(lang.reserved_op("[]").map(|_| Expr::Array))
        .or(lang.reserved_op("HT{}").map(|_| Expr::HT))
        .or(lang.reserved_op("{}").map(|_| Expr::ObjectEmpty))
        .or(
            position()
                .skip(lang.reserved("clos"))
                .and(
                lang.parens(id(lang).skip(lang.reserved_op(","))
                    .and(sep_by(
                            atom(file, lang).skip(lang.reserved_op(":")).and(type_(file, lang)),
                            lang.reserved_op(",")))))
                .map(|(p, (id, vars))| Expr::Closure(id, vars, Pos::from_combine(file, p))))
        .or(position()
            .skip(lang.reserved("arrayPush"))
            .and(lang.parens(atom(file, lang).skip(lang.reserved_op(",")).and(atom(file, lang))))
            .map(|(p, (array, member))| Expr::Push(array, member, Pos::from_combine(file, p))))
        .or(
            position()
                .skip(lang.reserved("sqrt"))
                .and(lang.parens(atom(file, lang)))
            .map(|(p, a)| Expr::Atom(ctor::sqrt_(a, Pos::from_combine(file, p)), Pos::from_combine(file, p))))
        .or(
            position()
            .skip(lang.reserved_op("newRef"))
            .and(lang.parens(atom(file, lang).skip(lang.reserved_op(",")).and(type_(file, lang))))
            .map(|(p, (val, ty))| Expr::NewRef(val, ty, Pos::from_combine(file, p))))
        .or(
            attempt(
                position()
                    .and(id(lang))
                    .skip(lang.reserved_op("!"))
                    .and(lang.parens(sep_by(id(lang), lang.reserved_op(","))))
                    .map(|((p, f), args)| Expr::ClosureCall(f, args, Pos::from_combine(file, p)))))
        .or(
            attempt(
                position()
                    .and(id(lang))
                    .and(lang.parens(sep_by(id(lang), lang.reserved_op(","))))
                    .map(|((p, f), args)| Expr::Call(f, args, Pos::from_combine(file, p)))))
        .or(
            position()
                .and(atom(file, lang))
                .map(|(p, a)| Expr::Atom(a, Pos::from_combine(file, p))))
    }
}

parser! {
    fn fn_type['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a,I>)(I) -> FnType
    where [I: Stream<Item = char, Position = SourcePosition>]
    {
        lang.parens(sep_by(type_(file, lang), lang.reserved_op(",")))
            .skip(lang.reserved_op("->"))
            .and(type_(file, lang).map(|t| Some(Box::new(t))).or(lang.reserved("void").map(|_| None)))
            .map(|(args, result)| FnType {
                args,
                result
            })
    }
}

parser! {
    fn type_['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a, I>)(I) -> Type
    where [ I: Stream<Item = char, Position = SourcePosition>]
    {
        lang.reserved("i32").with(value(Type::I32))
            .or(lang.reserved("str").with(value(Type::String)))
            .or(lang.reserved("bool").with(value(Type::Bool)))
            .or(fn_type(file, lang).map(|f| Type::Fn(f)))
            .or(lang.reserved("clos").with(fn_type(file, lang)).map(|f| Type::Closure(f)))
            .or(lang.reserved("f64").with(value(Type::F64)))
            .or(lang.reserved("any").with(value(Type::Any)))
            .or(lang.reserved("DynObject").with(value(Type::DynObject)))
            .or(lang.reserved("HT").with(value(Type::HT)))
            .or(lang.reserved("Array").with(value(Type::Array)))
            .or(lang.reserved("Ref").with(lang.parens(type_(file, lang))).map(|t| ctor::ref_ty_(t)))
            .or(lang.reserved("env").with(value(Type::Env)))
    }
}

parser! {
    fn stmt['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a,I>)(I) -> Stmt
    where [I: Stream<Item = char, Position = SourcePosition>]
    {
        let var = position()
            .skip(lang.reserved("var"))
            .and(id(lang))
            .and(optional(lang.reserved_op(":").with(type_(file, lang))))
            .skip(lang.reserved_op("="))
            .and(expr(file, lang))
            .skip(lang.reserved_op(";"))
            .map(|(((p, id), ty), named)| Stmt::Var(VarStmt { id, named, ty }, Pos::from_combine(file, p)));

        enum IdRhsInStmt {
            Expr(Expr),
            Stmt(Stmt)
        }

        let assign_or_label = attempt(
            position()
            .and(id(lang))
           .and((lang.reserved_op("=").with(expr(file, lang)).skip(lang.reserved_op(";")).map(|e| IdRhsInStmt::Expr(e)))
                .or(lang.reserved_op(":").with(block(file, lang)).map(|s| IdRhsInStmt::Stmt(s))))
           .map(|((p, x), rhs)| match rhs {
               IdRhsInStmt::Expr(e) => Stmt::Assign(x, e, Pos::from_combine(file, p)),
               IdRhsInStmt::Stmt(s) => ctor::label_(x.into_name(), s, Pos::from_combine(file, p))
           }));

        let object_set = position()
            .and(id(lang))
            .skip(lang.reserved_op("."))
            .and(id(lang))
            .skip(lang.reserved_op("="))
            .and(atom(file, lang))
            .skip(lang.reserved_op(";"))
            .map(|(((p, ht), field), atom)| Stmt::Var(
                VarStmt::new(ctor::id_("_"),
                Expr::ObjectSet(
                    Atom::Id(ht, Pos::from_combine(file, p)),
                    ctor::str_(field.into_name(), Pos::from_combine(file, p)),
                    atom,
                Pos::from_combine(file, p))), Pos::from_combine(file, p)));

        let ht_set = attempt(
            position()
            .and(id(lang))
            .skip(lang.reserved_op("<<"))
            .and(id(lang))
            .skip(lang.reserved_op("="))
            .and(atom(file, lang))
            .skip(lang.reserved_op(";"))
            .map(|(((p, ht), field), atom)| Stmt::Var(
                VarStmt::new(
                    ctor::id_("_"),
                    Expr::HTSet(
                        Atom::Id(ht, Pos::from_combine(file, p)),
                        ctor::str_(field.into_name(), Pos::from_combine(file, p)),
                        atom, Pos::from_combine(file, p))), Pos::from_combine(file, p))));

        let if_ = position()
            .skip(lang.reserved("if"))
            .and(lang.parens(atom(file, lang)))
            .and(block(file, lang))
            .skip(lang.reserved("else"))
            .and(block(file, lang))
            .map(|(((p, a), s1), s2)| Stmt::If(a, Box::new(s1), Box::new(s2), Pos::from_combine(file, p)));

        let loop_ = position()
            .skip(lang.reserved("loop"))
            .and(block(file, lang))
            .map(|(p, s)| Stmt::Loop(Box::new(s), Pos::from_combine(file, p)));

        let return_ = position()
            .skip(lang.reserved("return"))
            .and(atom(file, lang))
            .skip(lang.reserved_op(";"))
            .map(|(p, a)| Stmt::Return(a, Pos::from_combine(file, p)));

        let break_ = lang.reserved("break")
            .with(position())
            .and(id(lang))
            .skip(lang.reserved_op(";"))
            .map(|(p, l)| Stmt::Break(Label::Named(l.into_name()), Pos::from_combine(file, p)));

        let while_ = lang.reserved("while")
            .with(position())
            .and(lang.parens(atom(file, lang)))
            .and(block(file, lang))
            .map(|((p, test),body)| ctor::while_(test, body, Pos::from_combine(file, p)));

        let store = lang.reserved_op("*")
            .with(position())
            .and(id(lang))
            .skip(lang.reserved_op("="))
            .and(expr(file, lang))
            .skip(lang.reserved_op(";"))
            .map(|((p, id), expr)| Stmt::Store(id, expr, Pos::from_combine(file, p)));

        let expression = 
            position()
            .and(expr(file, lang))
            .skip(lang.reserved_op(";"))
            .map(|(p, e)| Stmt::Expression(e, Pos::from_combine(file, p)));

        choice((
            var,
            while_,
            if_,
            return_,
            block(file, lang),
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
    fn block['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a,I>)(I) -> Stmt
    where [I: Stream<Item = char, Position = SourcePosition>]
    {
        lang.braces(position().and(many(stmt(file, lang))))
            .map(|(p, ss)| Stmt::Block(ss, Pos::from_combine(file, p)))
    }
}

parser! {
    fn global['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a,I>)(I) -> (Id, Global)
    where [I: Stream<Item = char, Position = SourcePosition>]
    {
        lang.reserved("const")
            .or(lang.reserved("var"))
            .and(id(lang))
            .skip(lang.reserved_op(":"))
            .and(type_(file, lang))
            .skip(optional(lang.reserved_op("=")))
            .and(optional(atom(file, lang)))
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
    fn function['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a,I>)(I) -> (Id, Function)
    where [I: Stream<Item = char, Position = SourcePosition>]
    {
        lang.reserved("function")
        .with(position())
        .and(id(lang))
        .and(lang.parens(
            sep_by(id(lang).skip(lang.reserved_op(":")).and(type_(file, lang)), lang.reserved_op(","))))
        .and(optional(lang.reserved_op(":").with(type_(file, lang))))
        .and(block(file, lang))
        .map(move |((((p, f), params_tys), ret_ty), body): (((_, Vec<(Id, Type)>), _), _)| {
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
                params,
                span: Pos::from_combine(file, p),
            })
        })
    }
}

parser! {
    fn program['a, 'b, I](file: &'b Rc<String>, lang: &'b Lang<'a,I>)(I) -> Program
    where [I: Stream<Item = char, Position = SourcePosition>]
    {
        lang.white_space()
        .with(many::<HashMap<_, _>, _>(global(file, lang)))
        .and(many::<HashMap<_, _>, _>(function(file, lang)))
        .skip(eof())
        .map(|(globals, functions)| {
            let data = Vec::new();
            Program { functions, globals, data }
        })
    }
}

pub fn parse(filename: &str, input: &str) -> Program {
    let filename = Rc::new(filename.to_string());
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

    let mut parser = program(&filename, &lang);
    let input_stream = State::new(input);
    let p = parser.easy_parse(input_stream).expect("parsing error");
    return p.0;
}
