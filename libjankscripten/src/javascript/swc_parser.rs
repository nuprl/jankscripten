// An implementation of our simplified JavaScript parser using swc_ecma_parser
// as a backend.

use super::constructors::*;
use super::syntax as S;
use swc_common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    FileName, FilePathMapping, SourceMap, Span,
};
use swc_ecma_ast as swc;
use swc_ecma_parser::{lexer, Parser, StringInput, Syntax};

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
    let source_map: SourceMap = Default::default();

    // Real usage
    // let fm = cm
    //     .load_file(Path::new("test.js"))
    //     .expect("failed to load test.js");
    let source_file = source_map.new_source_file(
        FileName::Anon, // TODO(mark): give it the real filename
        js_code.into(),
    );

    let lexer = lexer::Lexer::new(
        // We want to parse ecmascript
        Syntax::Es(Default::default()),
        // JscTarget defaults to es5
        Default::default(),
        StringInput::from(&*source_file),
        None,
    );

    let mut parser = Parser::new_from(lexer);

    let script = parser.parse_script()?;

    // return simpl_program(ast);

    parse_script(script, &source_map)
}

fn unsupported<T>(span: Span, source_map: &SourceMap) -> Result<T, ParseError> {
    unsupported_message("unsupported feature", span, source_map)
}

fn unsupported_message<T>(msg: &str, span: Span, source_map: &SourceMap) -> Result<T, ParseError> {
    Err(ParseError::Unsupported(format!(
        "{} at {}",
        msg,
        source_map.span_to_string(span)
    )))
}

fn parse_script(script: swc::Script, source_map: &SourceMap) -> ParseResult<S::Stmt> {
    Ok(S::Stmt::Block(parse_stmts(script.body, source_map)?))
}

fn parse_stmts(stmts: Vec<swc::Stmt>, source_map: &SourceMap) -> ParseResult<Vec<S::Stmt>> {
    stmts
        .into_iter()
        .map(|stmt| parse_stmt(stmt, source_map))
        .collect()
}

fn parse_stmt(stmt: swc::Stmt, source_map: &SourceMap) -> ParseResult<S::Stmt> {
    use swc::Stmt::*;
    match stmt {
        Block(block_stmt) => {
            let parsed_stmts = parse_stmts(block_stmt.stmts, source_map)?;
            Ok(S::Stmt::Block(parsed_stmts))
        }
        Empty(empty_stmt) => Ok(S::Stmt::Empty),
        Debugger(debugger_stmt) => unsupported(debugger_stmt.span, source_map),
        With(with_stmt) => unsupported(with_stmt.span, source_map),
        Return(return_stmt) => Ok(S::Stmt::Return(parse_maybe_expr(
            return_stmt.arg,
            source_map,
        )?)),
        Labeled(labeled_stmt) => {
            todo!();
        }
        Break(break_stmt) => {
            todo!();
        }
        Continue(continue_stmt) => {
            todo!();
        }
        If(if_stmt) => {
            todo!();
        }
        Switch(switch_stmt) => {
            todo!();
        }
        Throw(throw_stmt) => {
            todo!();
        }
        Try(try_stmt) => {
            todo!();
        }
        While(while_stmt) => {
            todo!();
        }
        DoWhile(do_while_stmt) => {
            todo!();
        }
        For(for_stmt) => {
            todo!();
        }
        ForIn(for_in_stmt) => {
            todo!();
        }
        ForOf(for_of_stmt) => {
            todo!();
        }
        Decl(decl) => {
            todo!();
        }
        Expr(expr_stmt) => {
            todo!();
        }
    }
}

// this function receives and returns Boxed expressions because optional
// expression are boxed in both the parser and our AST.
fn parse_maybe_expr(
    maybe_expr: Option<Box<swc::Expr>>,
    source_map: &SourceMap,
) -> ParseResult<Box<S::Expr>> {
    match maybe_expr {
        None => Ok(Box::new(UNDEFINED_)),
        Some(expr) => Ok(Box::new(parse_expr(*expr, source_map)?)),
    }
}

fn parse_expr(expr: swc::Expr, source_map: &SourceMap) -> ParseResult<S::Expr> {
    use swc::Expr::*;
    match expr {
        This(this_expr) => {
            todo!();
        }
        Array(array_lit) => {
            todo!();
        }
        Object(object_lit) => {
            todo!();
        }
        Fn(fn_expr) => {
            todo!();
        }
        Unary(unary_expr) => {
            todo!();
        }
        Update(update_expr) => {
            todo!();
        }
        Bin(bin_expr) => {
            todo!();
        }
        Assign(assign_expr) => {
            todo!();
        }
        Member(member_expr) => {
            todo!();
        }
        Cond(cond_expr) => {
            todo!();
        }
        Call(call_expr) => {
            todo!();
        }
        New(new_expr) => {
            todo!();
        }
        Seq(seq_expr) => {
            todo!();
        }
        Ident(ident) => {
            todo!();
        }
        Lit(lit) => {
            todo!();
        }
        Tpl(tpl) => {
            todo!();
        }
        TaggedTpl(tagged_tpl) => {
            todo!();
        }
        Arrow(arrow_expr) => {
            todo!();
        }
        Class(class_expr) => {
            todo!();
        }
        Yield(yield_expr) => {
            todo!();
        }
        MetaProp(meta_prop_expr) => {
            todo!();
        }
        Await(await_expr) => {
            todo!();
        }
        Paren(paren_expr) => {
            todo!();
        }
        JSXMember(jsx_member_expr) => {
            todo!();
        }
        JSXNamespacedName(jsx_namespaced_name) => {
            todo!();
        }
        JSXEmpty(jsx_empty) => {
            todo!();
        }
        JSXElement(jsx_element) => {
            todo!();
        }
        JSXFragment(jsx_fragment) => {
            todo!();
        }
        TsTypeAssertion(ts_type_assertion) => {
            todo!();
        }
        TsConstAssertion(ts_const_assertion) => {
            todo!();
        }
        TsNonNull(ts_non_null_expr) => {
            todo!();
        }
        TsTypeCast(ts_type_cast_expr) => {
            todo!();
        }
        TsAs(ts_as_expr) => {
            todo!();
        }
        PrivateName(private_name) => {
            todo!();
        }
        OptChain(opt_chain_expr) => {
            todo!();
        }
        Invalid(invalid) => {
            todo!();
        }
    }
}
