use super::super::pos::Pos;
use super::lexer_l;
use super::parser_y;
/// Unfortunately, this module is disasterously complicated, but it is necessary to keep the
/// rest of our toolchain simple.
///
/// Similar to SWC, an lrpar grammar makes it easy to access the span (offset) of a token, but
/// we have to call a helper function on the lexer to turn the offset into a line and column. That
/// function runs in O(n) time, where n is the length of the file. Thus, it is not a good idea
/// to call it at every note (parsing would become quadratic). Instead, at every AST node, we
/// store a span and a reference-counted call that contains the lexer.
///
/// However, the lexer data structure (LRNonStreamingLexerDef) stores borrowed references to the
/// input string. Thus, within reference counted cell, we must store both the input string and
/// the lexer. For example, we may try something like this:
///
/// ```
/// struct LexerAndInput {
///   input: String,
///   lexer: LRNonStreamingLexerDef<'a>, // where 'a is the lifetime of self.input
/// }
/// ```
///
/// Thus `LexerAndInput` is a self-referential structure, which must be pinned in memory and use
/// a modicum of unsafe code in initialization. This is a well-known unsafe idiom, which is
/// documented in the Rust reference for the `std::pin` module.
///
/// A final problem is that the semantic actions for Grmtools parsers cannot have any auxiliary
/// arguments, so there is no easy way to provide the `LexerAndInput` structure to the rules.
/// To workaround this limitation, this module uses thread-local state.
///
/// In the code below, the `PinnedLexer` struct is the boxed, and pinned version of
/// `LexerAndInput`.
use lrlex::{LRNonStreamingLexer, LRNonStreamingLexerDef};
use lrpar::{Lexeme, Span};
use std::cell::RefCell;
use std::marker::PhantomPinned;
use std::pin::Pin;
use std::rc::Rc;

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
    // Used in crate::pos
    pub fn line_col(&self, span: Span) -> ((usize, usize), (usize, usize)) {
        use lrpar::NonStreamingLexer;
        self.inner.as_ref().lexer.as_ref().unwrap().line_col(span)
    }
}

pub fn parse(_filename: &str, input: impl Into<String>) -> super::syntax::Program {
    let input = input.into();
    let pinned_lexer_inner = PinnedLexerInner {
        input,
        lexerdef: lexer_l::lexerdef(),
        lexer: None,
        _pin: PhantomPinned,
    };
    let mut pinned_lexer = PinnedLexer {
        inner: Box::pin(pinned_lexer_inner),
    };
    let lexer = pinned_lexer
        .inner
        .lexerdef
        .lexer(pinned_lexer.inner.input.as_str());
    let lexer: LRNonStreamingLexer<'static, 'static, u32> = unsafe { std::mem::transmute(lexer) };
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
        panic!("Error parsing NotWasm");
    })
}

thread_local!(static LEXER: RefCell<Option<Rc<PinnedLexer>>> = RefCell::new(None));

/// Used in parser.y
pub fn pos(p: Result<Lexeme<u32>, Lexeme<u32>>) -> Pos {
    let span = p.unwrap_or_else(|x| x).span();
    return LEXER
        .with(|pinned_lexer| Pos::from_grmtools(pinned_lexer.borrow().as_ref().unwrap(), span));
}
