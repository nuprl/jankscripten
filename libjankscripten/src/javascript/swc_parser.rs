// An implementation of our simplified JavaScript parser using swc_ecma_parser
// as a backend.

use super::constructors::*;
use super::syntax as S;
use swc_ecma_parser::*;
use swc_ecma_parser::{lexer, Parser, StringInput, Syntax};
use swc_common::{
    errors::{ColorConfig, Handler},
    FileName, FilePathMapping, SourceMap,
    sync::Lrc,
};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    /// An error from the SWC parser.
    #[error("SWC error")]
                 // TODO(mark): figure out how to print the error, it doesn't
                 //             implement the Display trait or anything like it
    SWC(swc_ecma_parser::error::Error),
    /// The Ressa AST had a JavaScript feature that we do not support.
    #[error("Unsupported: {0}")]
    Unsupported(String),
}

pub type ParseResult<T> = Result<T, ParseError>;

pub fn parse(js_code: &str) -> ParseResult<S::Stmt> {
    let cm: Lrc<SourceMap> = Default::default();

    // Real usage
    // let fm = cm
    //     .load_file(Path::new("test.js"))
    //     .expect("failed to load test.js");
    let fm = cm.new_source_file(
        FileName::Anon, // TODO(mark): give it the real filename
        js_code.into(),
    );

    let lexer = lexer::Lexer::new(
        // We want to parse ecmascript
        Syntax::Es(Default::default()),
        // JscTarget defaults to es5
        Default::default(),
        StringInput::from(&*fm),
        None,
    );

    let mut parser = Parser::new_from(lexer);

    // return simpl_program(ast);

    Ok(S::Stmt::Empty)
}

