// An implementation of our simplified JavaScript parser using swc_ecma_parser
// as a backend.

use super::constructors::*;
use super::syntax as S;
use swc_ecma_parser::{lexer, Parser, StringInput, Syntax};
use swc_common::{
    errors::{ColorConfig, Handler},
    FileName, FilePathMapping, SourceMap,
    sync::Lrc,
};
use swc_ecma_ast as swc;

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

impl From<swc_ecma_parser::error::Error> for ParseError {
    fn from(e: swc_ecma_parser::error::Error) -> ParseError {
        ParseError::SWC(e)
    }
}

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

    let script = parser.parse_script()?;

    // return simpl_program(ast);

    parse_script(script)
}

fn unsupported<T>() -> Result<T, ParseError> {
    return Err(ParseError::Unsupported("".to_string()));
}

fn unsupported_message<T>(msg: &str) -> Result<T, ParseError> {
    return Err(ParseError::Unsupported(msg.to_string()));
}

fn parse_script(script: swc::Script) -> ParseResult<S::Stmt> {
    Ok(S::Stmt::Block(parse_stmts(script.body)?))
}

fn parse_stmts(stmts: Vec<swc::Stmt>) -> ParseResult<Vec<S::Stmt>> {
    stmts.into_iter().map(|stmt| parse_stmt(stmt)).collect()
}

fn parse_stmt(stmt: swc::Stmt) -> ParseResult<S::Stmt> {
    use swc::Stmt::*;
    match stmt {
        Block(BlockStmt) => {
            todo!();
        }
        Empty(EmptyStmt) => {
            todo!();
        }
        Debugger(DebuggerStmt) => {
            todo!();
        }
        With(WithStmt) => {
            todo!();
        }
        Return(ReturnStmt) => {
            todo!();
        }
        Labeled(LabeledStmt) => {
            todo!();
        }
        Break(BreakStmt) => {
            todo!();
        }
        Continue(ContinueStmt) => {
            todo!();
        }
        If(IfStmt) => {
            todo!();
        }
        Switch(SwitchStmt) => {
            todo!();
        }
        Throw(ThrowStmt) => {
            todo!();
        }
        Try(TryStmt) => {
            todo!();
        }
        While(WhileStmt) => {
            todo!();
        }
        DoWhile(DoWhileStmt) => {
            todo!();
        }
        For(ForStmt) => {
            todo!();
        }
        ForIn(ForInStmt) => {
            todo!();
        }
        ForOf(ForOfStmt) => {
            todo!();
        }
        Decl(decl) => {
            todo!();
        }
        Expr(ExprStmt) => {
            todo!();
        }
    }
}
