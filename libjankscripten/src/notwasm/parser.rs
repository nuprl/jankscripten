#![allow(dead_code, unused_imports)]
use super::syntax::*;

use std::fmt;
use combine::error::ParseError;
use combine::parser::char::{char, digit, spaces, string,alpha_num};
use combine::stream::easy;
use combine::EasyParser;
use combine::stream::Stream;
use combine::{
    attempt, between, eof, many1, optional, satisfy_map, sep_end_by, token,
    Parser,
};

#[derive(Debug, PartialEq, Clone)]
enum Tok {
    LBrace,
    RBrace,
    LParen,
    RParen,
    If,
    Break,
    Return,
    Else,
    Semi,
    Comma,
    Int32(i32),
    Id(String),
    Eof,
}

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn lex(s: &str) -> Result<Vec<Tok>, easy::ParseError<&str>> {
    let tok = string("{")
        .map(|_x| Tok::LBrace)
        .or(string("}").map(|_x| Tok::RBrace))
        .or(string("(").map(|_x| Tok::LParen))
        .or(string(")").map(|_x| Tok::RParen))
        .or(string("if").map(|_x| Tok::If))
        .or(string("else").map(|_x| Tok::Else))
        .or(string(",").map(|_x| Tok::Comma))
        .or(string(";").map(|_x| Tok::Semi))
        .or((optional(char('-').or(char('+'))), many1(digit())).map(
            |(sign, digits): (Option<char>, String)| {
                let n = digits.parse::<i32>().unwrap();
                match sign {
                    Some('-') => Tok::Int32(-n),
                    _ => Tok::Int32(n),
                }
            },
        ))
        .or(many1(alpha_num()).map(|x: String| Tok::Id(x)));

    let ws = spaces();

    let mut toks = spaces().with(sep_end_by(tok, ws)).skip(eof()).map(
        |mut tokens: Vec<Tok>| {
            tokens.push(Tok::Eof);
            tokens
        },
    );
    toks.easy_parse(s).map(|tuple| tuple.0)
}