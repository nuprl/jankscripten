use super::super::pos::Pos;
use lrlex::{LRNonStreamingLexer, LRNonStreamingLexerDef};
use lrpar::{Lexeme, Span};
use std::cell::RefCell;
use std::marker::PhantomPinned;
use std::pin::Pin;
use std::rc::Rc;

use super::lexer_l;
use super::parser_y;

pub struct PinnedLexer {
    inner: Pin<Box<PinnedLexerInner>>,
}

pub struct PinnedLexerInner {
    input: String,
    lexerdef: LRNonStreamingLexerDef<u32>,
    lexer: Option<LRNonStreamingLexer<'static, 'static, u32>>,
    _pin: PhantomPinned,
}

impl PinnedLexer {
    pub fn line_col(&self, span: Span) -> ((usize, usize), (usize, usize)) {
        use lrpar::NonStreamingLexer;
        self.inner.as_ref().lexer.as_ref().unwrap().line_col(span)
    }
}

pub fn parse(input: String) -> super::syntax::Program {
    let pinned_lexer_inner = PinnedLexerInner {
        input,
        lexerdef: lexer_l::lexerdef(),
        lexer: None,
        _pin: PhantomPinned,
    };
    let mut pinned_lexer = PinnedLexer {
        inner: Box::pin(pinned_lexer_inner),
    };

    let lexer: LRNonStreamingLexer<'static, 'static, u32> = unsafe {
        std::mem::transmute(
            pinned_lexer
                .inner
                .lexerdef
                .lexer(pinned_lexer.inner.input.as_str()),
        )
    };
    unsafe {
        let mut_ref = Pin::as_mut(&mut pinned_lexer.inner);
        Pin::get_unchecked_mut(mut_ref).lexer = Some(lexer);
    }

    LEXER.with(|l| {
        *l.borrow_mut() = Some(Rc::new(pinned_lexer));
        let borrowed = l.borrow();
        let pinned_lexer = borrowed.as_ref().unwrap().inner.as_ref();
        let lexer = pinned_lexer.lexer.as_ref().unwrap();
        let (res, errs) = parser_y::parse(lexer);
        if errs.len() == 0 {
            return res.unwrap();
        }
        for err in errs.into_iter() {
            eprintln!("{}", err.pp(lexer, &|t| parser_y::token_epp(t)));
        }
        panic!("parse error");
    })
}

thread_local!(static LEXER: RefCell<Option<Rc<PinnedLexer>>> = RefCell::new(None));

pub fn pos(p: Result<Lexeme<u32>, Lexeme<u32>>) -> Pos {
    let span = p.unwrap_or_else(|x| x).span();
    return LEXER
        .with(|pinned_lexer| Pos::from_grmtools(pinned_lexer.borrow().as_ref().unwrap(), span));
}

#[cfg(test)]
mod test {
    use super::super::syntax::*;
    use super::*;

    #[test]
    fn parse_ok() {
        parse("i32".to_string());
    }

    #[test]
    fn parse_err() {
        parse("(i32) -> -> f64".to_string());
        panic!("");
    }
}
