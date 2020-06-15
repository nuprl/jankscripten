#![allow(dead_code, unused_imports)]
use super::syntax::*;

use std::collections::HashMap;
use combine::error::ParseError;
use combine::parser;
use combine::parser::char::{alpha_num, char, digit, spaces, string};
use combine::stream::easy;
use combine::stream::Stream;
use combine::{
    attempt, between, eof, many, many1, optional, satisfy_map, sep_end_by, token, Parser,
    sep_by, value, chainl1
};
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
enum Tok {
    LBrace,
    RBrace,
    LParen,
    RParen,
    Op(&'static str),
    Keyword(&'static str),
    Plus,
    Minus,
    Equalequal,
    If,
    Return,
    Else,
    Var,
    Semi,
    Comma,
    Colon,
    Equals,
    Function,
    I32,
    Int32(i32),
    Bool(bool),
    Id(String),
    Eof,
}

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Tok::*;
        match self {
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Equalequal => write!(f, "=="),
            If => write!(f, "if"),
            Return => write!(f, "return"),
            Else => write!(f, "else"),
            Function => write!(f, "function"),
            Var => write!(f, "var"),
            Semi => write!(f, ";"),
            Comma => write!(f, ","),
            Colon => write!(f, ":"),
            Equals => write!(f, "="),
            I32 => write!(f, "i32"),
            Int32(i) => write!(f, "{}", i),
            Id(x) => write!(f, "{}", &x),
            Bool(b) => write!(f, "{}", b),
            Op(s) => write!(f, "{}", s),
            Keyword(s) => write!(f, "{}", s),
            Eof => write!(f, "end-of-input"),
        }
    }
}

fn lex(s: &str) -> Result<Vec<Tok>, easy::ParseError<&str>> {
    // The string(..) parser consumes its input eagerly. For example,
    // string("AA") will fail on the input "AB", with "A" consumed. Therefore,
    // string("AA").or(string("AB")) cannot parse the input "AB", since the
    // left-hand side of the .or(..) consumes the "A". The simplest fix is
    // to write attempt(string("AA")).or(string("AB")). The attempt combinator
    // backtracks the input state if its argument fails to parse the input.

    let tok = // Symbols
        string("{").map(|_x| Tok::LBrace)
        .or(string(">").with(value(Tok::Op(">"))))
        .or(string("}").map(|_x| Tok::RBrace))
        .or(string("(").map(|_x| Tok::LParen))
        .or(string(")").map(|_x| Tok::RParen))
        .or(string(",").map(|_x| Tok::Comma))
        .or(string(";").map(|_x| Tok::Semi))
        .or(string(":").map(|_x| Tok::Colon))
        .or(string("+").map(|_x| Tok::Plus))
        .or(string("-").map(|_x| Tok::Minus))
        .or(attempt(string("==")).map(|_x| Tok::Equalequal))
        .or(string("=").map(|_x| Tok::Equals))
        // Keywords in alphabetical order
        .or(attempt(string("else")).map(|_x| Tok::Else))
        .or(attempt(string("false")).map(|_x| Tok::Bool(false)))
        .or(attempt(string("function")).map(|_x| Tok::Function))
        .or(attempt(string("i32")).map(|_x| Tok::I32))
        .or(attempt(string("if")).map(|_x| Tok::If))
        .or(attempt(string("loop")).with(value(Tok::Keyword("loop"))))
        .or(attempt(string("break")).with(value(Tok::Keyword("break"))))
        .or(attempt(string("return")).map(|_x| Tok::Return))
        .or(attempt(string("true")).map(|_x| Tok::Bool(true)))
        .or(attempt(string("var")).map(|_x| Tok::Var))
        // Numbers
        .or((optional(char('-')), many1(digit())).map(
            |(sign, digits): (Option<char>, String)| {
                let n = digits.parse::<i32>().unwrap();
                match sign {
                    Some('-') => Tok::Int32(-n),
                    _ => Tok::Int32(n),
                }
            },
        ))
        // Identifiers
        .or(many1(alpha_num()).map(|x: String| Tok::Id(x)));

    let ws = spaces();

    let mut toks = spaces()
        .with(sep_end_by(tok, ws))
        .skip(eof())
        .map(|mut tokens: Vec<Tok>| {
            tokens.push(Tok::Eof);
            tokens
        });
    toks.easy_parse(s).map(|tuple| tuple.0)
}


fn op<I>(op: &'static str) -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = Tok>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy_map(move |t| match t {
        Tok::Op(op_) if op == op_ => Some(()),
        _ => None
    })
}

fn kw<I>(kw: &'static str) -> impl Parser<Input = I, Output = ()>
where
    I: Stream<Item = Tok>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy_map(move |t| match t {
        Tok::Keyword(kw_) if kw == kw_ => Some(()),
        _ => None
    })
}

fn lit<I>() -> impl Parser<Input = I, Output = Lit>
where
    I: Stream<Item = Tok>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy_map(|t| match t {
        Tok::Int32(n) => Some(Lit::I32(n)),
        Tok::Bool(b) => Some(Lit::Bool(b)),
        _ => None,
    })
}

fn id<I>() -> impl Parser<Input = I, Output = Id>
where
    I: Stream<Item = Tok>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy_map(|t| match t {
        Tok::Id(x) => Some(Id::Named(x)),
        _ => None,
    })
}

parser! {
    fn binop[I]()(I) -> BinaryOp
    where [I: Stream<Item = Tok>]
    {
        token(Tok::Plus).with(value(BinaryOp::I32Add))
        .or(op(">").with(value(BinaryOp::I32GT)))
        .or(token(Tok::Minus).with(value(BinaryOp::I32Eq)))
        .or(token(Tok::Equalequal).with(value(BinaryOp::I32Sub)))
    }
}

parser! {
    fn atom[I]()(I) -> Atom
    where [I: Stream<Item = Tok>]
    {
        chainl1(
            atom_item(),
            binop().map(|op| move |lhs, rhs| Atom::Binary(op, Box::new(lhs), Box::new(rhs), Type::I32)))
    }    
}

parser! {
    fn atom_item[I]()(I) -> Atom
    where [I: Stream<Item = Tok>]
    {
        lit().map(|l| Atom::Lit(l))
        .or(id().map(|x| Atom::Id(x)))
        .or(between(token(Tok::LParen), token(Tok::RParen), atom()))
    }
}

enum AfterId {
    Args(Vec<Id>),
    Op(BinaryOp, Atom)
}

fn expr<I>() -> impl Parser<Input = I, Output = Expr>
where
    I: Stream<Item = Tok>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    id()
        .and(optional(
            between(token(Tok::LParen), token(Tok::RParen), sep_by(id(), token(Tok::Comma))).map(|args| AfterId::Args(args))
            .or(binop().and(atom()).map(|(op, e)| AfterId::Op(op, e)))))
        .map(|(f, opt_args)| match opt_args {
            None => Expr::Atom(Atom::Id(f)),
            Some(AfterId::Args(args)) => Expr::CallDirect(f, args),
            Some(AfterId::Op(op, e)) => Expr::Atom(Atom::Binary(op, Box::new(Atom::Id(f)), Box::new(e), Type::I32))
        })
        .or(atom().map(|a| Expr::Atom(a)))
}

fn type_<I>() -> impl Parser<Input = I, Output = Type>
where
    I: Stream<Item = Tok>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    satisfy_map(|t| match t {
        Tok::I32 => Some(Type::I32),
        _ => None,
    })
    // unimplemented!();
}

parser! {
    fn stmt[I]()(I) -> Stmt
    where [I: Stream<Item = Tok>]
    {
        let var = token(Tok::Var)
            .with(id())
            .skip(token(Tok::Colon))
            .and(type_())
            .skip(token(Tok::Equals))
            .and(expr())
            .skip(token(Tok::Semi))
            .map(|((x,t),e)| Stmt::Var(x, e, t));

        let assign = id()
            .skip(token(Tok::Equals))
            .and(expr())
            .skip(token(Tok::Semi))
            .map(|(x,e)| Stmt::Assign(x, e));

        let if_ = token(Tok::If)
            .with(between(token(Tok::LParen), token(Tok::RParen), atom()))
            .and(block())
            .skip(token(Tok::Else))
            .and(block())
            .map(|((a, s1), s2)| Stmt::If(a, Box::new(s1), Box::new(s2)));
        
        let loop_ = kw("loop")
            .with(block())
            .map(|s| Stmt::Loop(Box::new(s)));

        let return_ = token(Tok::Return)
            .with(atom())
            .skip(token(Tok::Semi))
            .map(|a| Stmt::Return(a));

        let break_ = kw("break")
            .with(id())
            .skip(token(Tok::Semi))
            .map(|l| Stmt::Break(l));

        var.or(assign).or(if_).or(return_).or(block()).or(loop_).or(break_)
    }
}

parser! {
    fn block[I]()(I) -> Stmt
    where [I: Stream<Item = Tok>]
    {
        between(token(Tok::LBrace), token(Tok::RBrace), many(stmt()))
            .map(|ss| Stmt::Block(ss))
    }
}

parser! {
    fn function[I]()(I) -> (Id, Function)
    where [I: Stream<Item = Tok>]
    {
        token(Tok::Function)
            .with(id())
            .and(
                between(
                    token(Tok::LParen),
                    token(Tok::RParen),
                    sep_by(id().skip(token(Tok::Colon)).and(type_()), token(Tok::Comma))))
            .skip(token(Tok::Colon))
            .and(type_())
            .and(block())
            .map(|(((f, params_tys), ret_ty), body)| 
            (f, Function { locals: vec![], body, params_tys, ret_ty }))
    }
}

parser! {
    fn program[I]()(I) -> Program
    where [I: Stream<Item = Tok>]
    {
        many::<HashMap<_, _>, _>(function())
            .skip(token(Tok::Eof))
            .map(|functions| {
                let classes = HashMap::new();
                let globals = HashMap::new();
                Program { functions, classes, globals }
            })
    }
}

pub fn parse(input: &str) -> Program {
    let tokens = lex(input).expect("lexing error");
    let program = program().easy_parse(&tokens[..]).expect("parsing error");
    program.0
}

