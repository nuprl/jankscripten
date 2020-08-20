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

/// Turn errors from swc, the parsing library, into our parse errors
impl From<swc_ecma_parser::error::Error> for ParseError {
    fn from(e: swc_ecma_parser::error::Error) -> ParseError {
        ParseError::SWC(e)
    }
}

/// Parse a full JavaScript program from source code into our JavaScript AST.
pub fn parse(js_code: &str) -> ParseResult<S::Stmt> {
    // The SourceMap keeps track of all the source files given to the parser.
    // All sourcespans given to us by the parser library are actually indices
    // into this source map.
    let source_map: SourceMap = Default::default();

    // Register our JavaScript file with the source map
    let source_file = source_map.new_source_file(
        FileName::Anon, // TODO(mark): give it the real filename
        js_code.into(),
    );

    // Create a lexer for the parser
    let lexer = lexer::Lexer::new(
        // We want to parse ecmascript
        Syntax::Es(Default::default()),
        // JscTarget defaults to es5
        Default::default(),
        StringInput::from(&*source_file),
        None,
    );

    // Create the actual parser
    let mut parser = Parser::new_from(lexer);

    // Parse our script into the library's AST
    let script = parser.parse_script()?;

    // Parse the library's AST into our AST
    parse_script(script, &source_map)
}

/// A parsing result used for an unsupported feature of JavaScript.
fn unsupported<T>(span: Span, source_map: &SourceMap) -> Result<T, ParseError> {
    unsupported_message("unsupported feature", span, source_map)
}

/// A parsing result used for an unsupported feature of JavaScript, with a
/// customizable message.
fn unsupported_message<T>(msg: &str, span: Span, source_map: &SourceMap) -> Result<T, ParseError> {
    Err(ParseError::Unsupported(format!(
        "{} at {}",
        msg,
        source_map.span_to_string(span)
    )))
}

impl From<swc::Ident> for S::Id {
    fn from(ident: swc::Ident) -> S::Id {
        S::Id::Named(ident.sym.to_string())
    }
}

/// Parse an entire swc script
fn parse_script(script: swc::Script, source_map: &SourceMap) -> ParseResult<S::Stmt> {
    Ok(S::Stmt::Block(parse_stmts(script.body, source_map)?))
}

/// Parse multiple swc statements.
fn parse_stmts(stmts: Vec<swc::Stmt>, source_map: &SourceMap) -> ParseResult<Vec<S::Stmt>> {
    stmts
        .into_iter()
        .map(|stmt| parse_stmt(stmt, source_map))
        .collect()
}

/// Parse an swc statement.
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
        Return(return_stmt) => Ok(return_(parse_opt_expr(return_stmt.arg, source_map)?)),
        Labeled(labeled_stmt) => {
            todo!();
        }
        Break(break_stmt) => Ok(break_(break_stmt.label)),
        Continue(continue_stmt) => {
            todo!();
        }
        If(if_stmt) => {
            // test
            let cond_expr = parse_expr(*if_stmt.test, source_map)?;

            // consequent
            let then_stmt = parse_stmt(*if_stmt.cons, source_map)?;

            // alternate
            let else_stmt = parse_opt_stmt(if_stmt.alt, source_map)?;

            Ok(if_(cond_expr, then_stmt, else_stmt))
        }
        Switch(switch_stmt) => {
            todo!();
        }
        Throw(throw_stmt) => Ok(throw_(parse_expr(*throw_stmt.arg, source_map)?)),
        Try(try_stmt) => {
            // deal with each possible part of the try statement separately,
            // wrap them as we encounter the different layers.

            // 1. block
            let stmt = parse_block(try_stmt.block, source_map)?;

            // 2. handler
            let stmt = match try_stmt.handler {
                None => stmt,

                Some(swc::CatchClause {
                    param: Some(pattern),
                    body,
                    span,
                }) => catch_(
                    stmt,
                    parse_pattern(pattern, span, source_map)?,
                    parse_block(body, source_map)?,
                ),

                Some(_) => return unsupported(try_stmt.span, source_map),
            };

            // 3. finalizer
            let stmt = match try_stmt.finalizer {
                None => stmt,
                Some(block) => finally_(stmt, parse_block(block, source_map)?),
            };

            // we're done
            Ok(stmt)
        }
        While(while_stmt) => {
            let test = parse_expr(*while_stmt.test, source_map)?;
            let body = parse_stmt(*while_stmt.body, source_map)?;
            Ok(while_(test, body))
        }
        DoWhile(do_while_stmt) => {
            let body = parse_stmt(*do_while_stmt.body, source_map)?;
            let test = parse_expr(*do_while_stmt.test, source_map)?;
            Ok(dowhile_(body, test))
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

/// Parse an optional swc statement. This function receives Boxed stmts
/// because optional stmts are boxed in swc.
fn parse_opt_stmt(
    opt_stmt: Option<Box<swc::Stmt>>,
    source_map: &SourceMap,
) -> ParseResult<S::Stmt> {
    match opt_stmt {
        None => Ok(S::Stmt::Empty),
        Some(stmt) => Ok(parse_stmt(*stmt, source_map)?),
    }
}

/// Parse an swc expression.
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

/// Parse an optional expression. This function receives and returns Boxed
/// expressions because optional expression are boxed in both the parser and
/// our AST.
fn parse_opt_expr(
    opt_expr: Option<Box<swc::Expr>>,
    source_map: &SourceMap,
) -> ParseResult<S::Expr> {
    match opt_expr {
        None => Ok(UNDEFINED_),
        Some(expr) => Ok(parse_expr(*expr, source_map)?),
    }
}

/// Parse an swc block statement.
fn parse_block(block: swc::BlockStmt, source_map: &SourceMap) -> ParseResult<S::Stmt> {
    Ok(S::Stmt::Block(parse_stmts(block.stmts, source_map)?))
}

/// Parse an swc pattern. `span` should be the source location of the
/// surrounding expr/stmt. `span` is used for error reporting purposes.
fn parse_pattern(pattern: swc::Pat, span: Span, source_map: &SourceMap) -> ParseResult<S::Id> {
    use swc::Pat::*;
    match pattern {
        Ident(ident) => Ok(S::Id::from(ident)),
        _ => unsupported(span, source_map),
    }
}
