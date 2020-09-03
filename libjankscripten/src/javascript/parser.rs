// An implementation of our simplified JavaScript parser using swc_ecma_parser
// as a backend.

use super::constructors::*;
use super::syntax as S;
use swc_atoms::JsWord;
use swc_common::{FileName, SourceMap, Span};
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
    /// An error while parsing a string literal
    #[error("String literal parse error: {0}")]
    String(String),
    /// The SWC AST had a JavaScript feature that we do not support.
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

macro_rules! unsupported {
    ($span:expr, $source_map:expr) => {
        unsupported_message(
            &format!(
                "(generated at {}:{}:{}) unsupported feature",
                file!(),
                line!(),
                column!()
            ),
            $span,
            $source_map,
        )
    };
}

/// A parsing result used for an unsupported feature of JavaScript, with a
/// customizable message.
fn unsupported_message<T>(msg: &str, span: Span, source_map: &SourceMap) -> Result<T, ParseError> {
    Err(unsupported_error(msg, span, source_map))
}

/// Construct a parse *error* with the given message and location.
fn unsupported_error(msg: &str, span: Span, source_map: &SourceMap) -> ParseError {
    ParseError::Unsupported(format!("{} at {}", msg, source_map.span_to_string(span)))
}

/// An error occurred while attempting to parse a string literal from the SWC AST
fn str_error(msg: &str, span: Span, source_map: &SourceMap) -> ParseError {
    ParseError::String(format!(
        "tried to parse string literal at {} but failed at {}",
        source_map.span_to_string(span),
        msg
    ))
}

// parse an id out of an swc identifier
fn parse_id(ident: swc::Ident) -> S::Id {
    S::Id::Named(ident.sym.to_string())
}

/// Parse an entire swc script
fn parse_script(script: swc::Script, source_map: &SourceMap) -> ParseResult<S::Stmt> {
    let mut stmts = parse_stmts(script.body, source_map)?;

    // Desugaring expects the program to have a single block statement at the
    // top of the program AST. If the entire program is already surrounded in a
    // block statement, we'll just return that one. Otherwise, we manually wrap
    // it in a block statement.

    if stmts.len() == 1 {
        if let S::Stmt::Block(_) = stmts[0] {
            return Ok(stmts.pop().unwrap());
        }
    }

    Ok(S::Stmt::Block(stmts))
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
        Block(block_stmt) => parse_block(block_stmt, source_map),
        Break(break_stmt) => Ok(break_(break_stmt.label.map(parse_id))),
        Continue(continue_stmt) => Ok(continue_(continue_stmt.label.map(parse_id))),
        Debugger(debugger_stmt) => unsupported!(debugger_stmt.span, source_map),
        Decl(decl) => {
            let decl = parse_decl(decl, source_map)?;
            Ok(decl)
        }
        DoWhile(do_while_stmt) => {
            let body = parse_stmt(*do_while_stmt.body, source_map)?;
            let test = parse_expr(*do_while_stmt.test, source_map)?;
            Ok(dowhile_(body, test))
        }
        Empty(empty_stmt) => Ok(S::Stmt::Empty),
        Expr(swc::ExprStmt { span, expr }) => Ok(expr_(parse_expr(*expr, source_map)?)),
        For(for_stmt) => {
            let init = match for_stmt.init {
                None => S::ForInit::Expr(Box::new(UNDEFINED_)),
                Some(swc::VarDeclOrExpr::Expr(e)) => {
                    S::ForInit::Expr(Box::new(parse_expr(*e, source_map)?))
                }
                Some(swc::VarDeclOrExpr::VarDecl(swc::VarDecl {
                    span,
                    kind,
                    declare,
                    decls,
                })) => {
                    if kind != swc::VarDeclKind::Var {
                        return unsupported_message(
                            "only var-declared variables are supported",
                            span,
                            source_map,
                        );
                    }
                    let decls: ParseResult<Vec<_>> = decls
                        .into_iter()
                        .map(|d| parse_var_declarator(d, source_map))
                        .collect();
                    S::ForInit::Decl(decls?)
                }
            };
            Ok(for_(
                init,
                parse_opt_expr(for_stmt.test, source_map)?,
                parse_opt_expr(for_stmt.update, source_map)?,
                parse_stmt(*for_stmt.body, source_map)?,
            ))
        }
        ForIn(swc::ForInStmt {
            left,
            right,
            body,
            span,
        }) => {
            // figure out if we're declaring a variable as part of this for in,
            // or if we're reusing an already-bound identifier. if it's neither
            // of these, we don't support it.
            let (is_var, id) = match left {
                // re-using an already-bound identifier
                swc::VarDeclOrPat::Pat(swc::Pat::Ident(ident)) => (false, ident),
                swc::VarDeclOrPat::Pat(swc::Pat::Expr(boxed_expr)) => {
                    // nested match because you can't match inside boxes without
                    // nightly rust
                    match *boxed_expr {
                        swc::Expr::Ident(ident) => (false, ident),
                        _ => {
                            return unsupported_message(
                                "unsupported expression in a for-in loop declaration",
                                span,
                                source_map,
                            );
                        }
                    }
                }

                // var case
                swc::VarDeclOrPat::VarDecl(swc::VarDecl {
                    span,
                    kind: swc::VarDeclKind::Var, // no `let`
                    declare: _,
                    mut decls,
                }) => {
                    if decls.len() != 1 {
                        return unsupported_message(
                            "only a single var decl is allowed",
                            span,
                            source_map,
                        );
                    }
                    match decls.remove(0) {
                        // a single decl
                        swc::VarDeclarator {
                            span: _,
                            init: None,                   // no initializer
                            name: swc::Pat::Ident(ident), // no obj destructuring
                            definite: _,
                        } => (true, ident),
                        // any other type of decl
                        _ => {
                            return unsupported_message(
                                "only var decls are allowed here",
                                span,
                                source_map,
                            );
                        }
                    }
                }

                // The program may pattern match on the index, which we do not support.
                other => {
                    return unsupported_message(
                        &format!("unsupported index in a for-in loop: {:?}", other),
                        span,
                        source_map,
                    );
                }
            };

            Ok(forin_(
                is_var,
                parse_id(id),
                parse_expr(*right, source_map)?,
                parse_stmt(*body, source_map)?,
            ))
        }
        ForOf(for_of_stmt) => unsupported!(for_of_stmt.span, source_map),
        If(if_stmt) => {
            // test
            let cond_expr = parse_expr(*if_stmt.test, source_map)?;

            // consequent
            let then_stmt = parse_stmt(*if_stmt.cons, source_map)?;

            // alternate
            let else_stmt = parse_opt_stmt(if_stmt.alt, source_map)?;

            Ok(if_(cond_expr, then_stmt, else_stmt))
        }
        Labeled(labeled_stmt) => Ok(label_(
            parse_id(labeled_stmt.label),
            parse_stmt(*labeled_stmt.body, source_map)?,
        )),
        Return(return_stmt) => Ok(return_(parse_opt_expr(return_stmt.arg, source_map)?)),
        Switch(swc::SwitchStmt {
            discriminant,
            cases,
            span,
        }) => {
            // parse cases
            let cases: Result<Vec<_>, _> = cases
                .into_iter()
                .map(|c| parse_switch_case(c, source_map))
                .collect();

            // partition cases into two groups:
            // 1. regular cases
            // 2. default cases (should only be one)
            let (cases, mut default_case) = cases?
                .into_iter()
                .partition::<Vec<_>, _>(|(test, _)| test.is_some());

            // try to separate out the default case from the regular cases
            let default_case = match default_case.len() {
                0 => S::Stmt::Empty,
                1 => default_case.remove(0).1,
                _ => panic!("switch with multiple default cases"),
            };
            let cases = cases.into_iter().map(|(test, body)| (test.unwrap(), body));

            // put it all together
            Ok(switch_(
                parse_expr(*discriminant, source_map)?,
                cases.collect(),
                default_case,
            ))
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
                    parse_id_from_pattern(pattern, span, source_map)?,
                    parse_block(body, source_map)?,
                ),

                Some(_) => return unsupported!(try_stmt.span, source_map),
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
        With(with_stmt) => unsupported!(with_stmt.span, source_map),
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
        Array(swc::ArrayLit { elems, span }) => {
            let elems: ParseResult<Vec<_>> = elems
                .into_iter()
                .map(|e| parse_opt_expr_or_spread(e, source_map))
                .collect();
            Ok(S::Expr::Array(elems?))
        }
        Arrow(arrow_expr) => unsupported!(arrow_expr.span, source_map),
        Assign(swc::AssignExpr {
            left,
            op,
            right,
            span,
        }) => {
            let op = parse_assign_op(op, span, source_map)?;
            let left = parse_pat_or_expr(left, span, source_map)?;
            let right = parse_expr(*right, source_map)?;
            Ok(op_assign_(op, left, right))
        }
        Await(await_expr) => unsupported!(await_expr.span, source_map),
        Bin(swc::BinExpr {
            op,
            left,
            right,
            span,
        }) => {
            let op = parse_binary_op(op, span, source_map)?;
            let left = parse_expr(*left, source_map)?;
            let right = parse_expr(*right, source_map)?;

            Ok(binary_(op, left, right))
        }
        Class(class_expr) => unsupported!(class_expr.class.span, source_map),
        Call(swc::CallExpr {
            args,
            callee,
            span,
            type_args,
        }) => {
            let args: ParseResult<Vec<_>> = args
                .into_iter()
                .map(|e| parse_expr_or_spread(e, source_map))
                .collect();
            let callee = parse_expr_or_super(callee, source_map);
            Ok(call_(callee?, args?))
        }
        Cond(swc::CondExpr {
            test,
            cons,
            alt,
            span,
        }) => {
            let test = parse_expr(*test, source_map)?;
            let cons = parse_expr(*cons, source_map)?;
            let alt = parse_expr(*alt, source_map)?;
            Ok(if_expr_(test, cons, alt))
        }
        Fn(swc::FnExpr { ident, function }) => {
            // parse parts
            let ident = match ident {
                Some(ident) => Some(parse_id(ident)),
                None => None,
            };
            let (params, body) = parse_function(function, source_map)?;

            // put it all together
            Ok(expr_func_(ident, params, body))
        }
        Ident(ident) => Ok(id_(parse_id(ident))),
        Invalid(invalid) => unsupported!(invalid.span, source_map),
        JSXElement(jsx_element) => unsupported!(jsx_element.span, source_map),
        JSXEmpty(jsx_empty) => unsupported!(jsx_empty.span, source_map),
        JSXFragment(jsx_fragment) => unsupported!(jsx_fragment.span, source_map),
        JSXMember(jsx_member_expr) => unsupported!(jsx_member_expr.prop.span, source_map),
        JSXNamespacedName(jsx_namespaced_name) => {
            unsupported!(jsx_namespaced_name.name.span, source_map)
        }
        Lit(lit) => Ok(S::Expr::Lit(parse_lit(lit, source_map)?)),
        Member(swc::MemberExpr {
            obj,
            prop,
            computed,
            span,
        }) => {
            let obj = parse_expr_or_super(obj, source_map)?;
            if computed {
                Ok(bracket_(obj, parse_expr(*prop, source_map)?))
            } else {
                match *prop {
                    Ident(id) => Ok(dot_(obj, parse_id(id))),
                    _ => unsupported!(span, source_map),
                }
            }
        }
        MetaProp(swc::MetaPropExpr {
            meta: swc::Ident { span, .. },
            ..
        }) => unsupported!(span, source_map), // new.target
        New(swc::NewExpr {
            callee, args, span, ..
        }) => {
            // args are technically optional, as the parentheses in a new
            // call are optional for zero-arg constructors. for example,
            //     new Date();
            // is equivalent to
            //     new Date;
            // omitting parentheses in this case makes no difference but it can
            // cause weird problems in more complicated uses.

            let args: ParseResult<Vec<_>> = match args {
                Some(args) => args
                    .into_iter()
                    .map(|eos| parse_expr_or_spread(eos, source_map))
                    .collect(),
                None => Ok(Vec::new()),
            };
            let callee = parse_expr(*callee, source_map);
            Ok(new_(callee?, args?))
        }
        Object(swc::ObjectLit { props, span }) => {
            let props: ParseResult<Vec<_>> = props
                .into_iter()
                .map(|p| parse_prop_or_spread(p, span, source_map))
                .collect();
            Ok(S::Expr::Object(props?))
        }
        OptChain(swc::OptChainExpr { span, .. }) => unsupported!(span, source_map),
        Paren(swc::ParenExpr { expr, .. }) => parse_expr(*expr, source_map),
        PrivateName(private_name) => unsupported!(private_name.span, source_map),
        Seq(swc::SeqExpr { exprs, span }) => {
            let exprs: ParseResult<Vec<_>> = exprs
                .into_iter()
                .map(|e| parse_expr(*e, source_map))
                .collect();
            Ok(S::Expr::Seq(exprs?))
        }
        TaggedTpl(tagged_tpl) => unsupported!(tagged_tpl.span, source_map),
        This(swc::ThisExpr { span }) => Ok(S::Expr::This),
        Tpl(tpl) => unsupported!(tpl.span, source_map),
        TsAs(ts_as_expr) => unsupported!(ts_as_expr.span, source_map),
        TsConstAssertion(ts_const_assertion) => unsupported!(ts_const_assertion.span, source_map),
        TsNonNull(ts_non_null_expr) => unsupported!(ts_non_null_expr.span, source_map),
        TsTypeAssertion(ts_type_assertion) => unsupported!(ts_type_assertion.span, source_map),
        TsTypeCast(ts_type_cast_expr) => unsupported!(ts_type_cast_expr.span, source_map),
        Unary(swc::UnaryExpr { op, arg, span }) => {
            let op = parse_unary_op(op, span, source_map)?;
            let arg = parse_expr(*arg, source_map)?;
            Ok(unary_(op, arg))
        }
        Update(swc::UpdateExpr {
            span,
            op,
            prefix,
            arg,
        }) => {
            let op = parse_update_op(op, prefix, span, source_map)?;
            let arg = parse_lvalue_from_expr(*arg, span, source_map)?;

            Ok(unaryassign_(op, arg))
        }
        Yield(yield_expr) => unsupported!(yield_expr.span, source_map),
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

/// Parse an swc pattern expecting an id. `span` should be the source location of
/// the surrounding expr/stmt. `span` is used for error reporting purposes.
fn parse_id_from_pattern(
    pattern: swc::Pat,
    span: Span,
    source_map: &SourceMap,
) -> ParseResult<S::Id> {
    use swc::Pat::*;
    match pattern {
        Ident(ident) => Ok(parse_id(ident)),
        _ => unsupported!(span, source_map),
    }
}

/// Parse an swc pattern. `span` should be the source location of the
/// surrounding expr/stmt. `span` is used for error reporting purposes.
fn parse_lvalue_from_pattern(
    pattern: swc::Pat,
    span: Span,
    source_map: &SourceMap,
) -> ParseResult<S::LValue> {
    use swc::Pat::*;
    match pattern {
        Ident(ident) => Ok(S::LValue::Id(parse_id(ident))),
        Expr(expr) => parse_lvalue_from_expr(*expr, span, source_map),
        _ => unsupported!(span, source_map),
    }
}

fn parse_var_declarator(
    var_decl: swc::VarDeclarator,
    source_map: &SourceMap,
) -> ParseResult<S::VarDecl> {
    Ok(S::VarDecl {
        name: parse_id_from_pattern(var_decl.name, var_decl.span, source_map)?,
        named: Box::new(parse_opt_expr(var_decl.init, source_map)?),
    })
}

fn parse_switch_case(
    case: swc::SwitchCase,
    source_map: &SourceMap,
) -> ParseResult<(Option<S::Expr>, S::Stmt)> {
    let test = match case.test {
        None => None,
        Some(e) => Some(parse_expr(*e, source_map)?),
    };
    Ok((test, S::Stmt::Block(parse_stmts(case.cons, source_map)?)))
}

fn parse_expr_or_super(eos: swc::ExprOrSuper, source_map: &SourceMap) -> ParseResult<S::Expr> {
    use swc::ExprOrSuper::*;
    match eos {
        Expr(expr) => parse_expr(*expr, source_map),
        Super(swc::Super { span }) => unsupported!(span, source_map),
    }
}

fn parse_expr_or_spread(eos: swc::ExprOrSpread, source_map: &SourceMap) -> ParseResult<S::Expr> {
    match eos.spread {
        None => parse_expr(*eos.expr, source_map),
        Some(span) => unsupported!(span, source_map),
    }
}

fn parse_opt_expr_or_spread(
    oeos: Option<swc::ExprOrSpread>,
    source_map: &SourceMap,
) -> ParseResult<S::Expr> {
    match oeos {
        Some(eos) => parse_expr_or_spread(eos, source_map),
        None => Ok(UNDEFINED_), // optional expr gets undefined
    }
}

fn parse_func_arg(arg: swc::Param, source_map: &SourceMap) -> ParseResult<S::Id> {
    Ok(parse_id_from_pattern(arg.pat, arg.span, source_map)?)
}

fn parse_lit(lit: swc::Lit, source_map: &SourceMap) -> ParseResult<S::Lit> {
    use swc::Lit::*;
    match lit {
        Str(swc::Str { value, span, .. }) => {
            Ok(S::Lit::String(parse_string(value, span, source_map)?))
        }
        Bool(swc::Bool { value, span }) => Ok(S::Lit::Bool(value)),
        Null(swc::Null { span }) => Ok(S::Lit::Null),
        Num(swc::Number { value, span }) => Ok(S::Lit::Num(parse_num(value))),
        BigInt(swc::BigInt { value, span }) => {
            unsupported_message("big int literal", span, source_map)
        }
        Regex(swc::Regex { exp, flags, span }) => {
            unsupported_message("regex not yet supported", span, source_map)
        }
        JSXText(swc::JSXText { span, .. }) => {
            unsupported_message("jsx string literal", span, source_map)
        }
    }
}

// TODO(arjun): Someone needs to read the ECMAScript specification to confirm
// that the code here is legit.
/// Parse a string literal from the SWC AST. `span` should be the sourcespan of
/// the surrounding expression.
fn parse_string(s: JsWord, span: Span, source_map: &SourceMap) -> ParseResult<String> {
    let literal_chars = s.to_string();

    let mut buf = String::with_capacity(literal_chars.len());
    let mut iter = literal_chars.chars().peekable();
    while let Some(ch) = iter.next() {
        if ch != '\\' {
            buf.push(ch);
            continue;
        }
        match iter
            .next()
            .ok_or(str_error("character after backslash", span, source_map))?
        {
            '\'' | '"' | '\\' => buf.push(ch),
            'n' => buf.push('\n'),
            'r' => buf.push('\r'),
            't' => buf.push('\t'),
            'f' => buf.push('\x0C'),
            'b' => buf.push('\x08'),
            'v' => buf.push('\x0B'),
            'x' => {
                let s = format!(
                    "{}{}",
                    iter.next()
                        .ok_or(str_error("first hex digit after \\x", span, source_map))?,
                    iter.next()
                        .ok_or(str_error("second hex digit after \\x", span, source_map))?,
                );
                let n = u8::from_str_radix(&s, 16).map_err(|_| {
                    str_error(&format!("invalid escape \\x{}", &s), span, source_map)
                })?;
                buf.push(n as char);
            }
            'u' => {
                let s = format!(
                    "{}{}{}{}",
                    iter.next()
                        .ok_or(str_error("first hex digit after \\x", span, source_map))?,
                    iter.next()
                        .ok_or(str_error("second hex digit after \\x", span, source_map))?,
                    iter.next()
                        .ok_or(str_error("third hex digit after \\x", span, source_map))?,
                    iter.next()
                        .ok_or(str_error("fourth hex digit after \\x", span, source_map))?
                );
                let n = u16::from_str_radix(&s, 16).map_err(|_| {
                    str_error(&format!("invalid unicode escape {}", &s), span, source_map)
                })?;
                buf.push(std::char::from_u32(n as u32).ok_or(str_error(
                    &format!("invalid Unicode character {}", n),
                    span,
                    source_map,
                ))?);
            }
            ch => {
                if ch < '0' || ch > '9' {
                    // JavaScript allows you to escape any character. If it's not a valid escape
                    // sequence, you just get the character itself. For example, '\😂' === '😂'.
                    buf.push(ch);
                } else {
                    let mut octal_str = String::with_capacity(2);
                    // First octal digit
                    octal_str.push(ch);
                    // Potentially octal digit
                    match iter.peek() {
                        Some(ch) if *ch >= '0' && *ch <= '7' => {
                            octal_str.push(*ch);
                            iter.next(); // consume
                        }
                        _ => (),
                    }
                    let n = u32::from_str_radix(&octal_str, 8).map_err(|_| {
                        str_error(
                            &format!("invalid octal escape \\u{}", &octal_str),
                            span,
                            source_map,
                        )
                    })?;
                    // 2-digit octal value is in range for UTF-8, thus unwrap should succeed
                    buf.push(std::char::from_u32(n).unwrap());
                }
            }
        }
    }
    return Ok(buf);
}

/// `span` is the span of the surrounding object literal.
fn parse_prop_or_spread(
    pos: swc::PropOrSpread,
    span: Span,
    source_map: &SourceMap,
) -> ParseResult<(S::Key, S::Expr)> {
    match pos {
        swc::PropOrSpread::Prop(prop) => parse_prop(*prop, span, source_map),
        _ => unsupported_message("", span, source_map),
    }
}

/// `span` is the span of the surrounding object literal.
fn parse_prop(
    prop: swc::Prop,
    span: Span,
    source_map: &SourceMap,
) -> ParseResult<(S::Key, S::Expr)> {
    use swc::Prop::*;
    match prop {
        KeyValue(swc::KeyValueProp { key, value }) => {
            let key = parse_prop_name(key, span, source_map)?;
            let value = parse_expr(*value, source_map)?;

            Ok((key, value))
        }
        _ => unsupported_message("object literal key type", span, source_map),
    }
}

/// `span` is the span of the surrounding object literal.
fn parse_prop_name(name: swc::PropName, span: Span, source_map: &SourceMap) -> ParseResult<S::Key> {
    use swc::PropName::*;
    match name {
        Ident(swc::Ident { sym, span, .. }) => Ok(S::Key::Str(sym.to_string())),
        Str(swc::Str { value, span, .. }) => Ok(S::Key::Str(value.to_string())),
        Num(swc::Number { value, span }) => {
            // see if this is a float or an int
            match parse_num(value) {
                S::Num::Int(i) => Ok(S::Key::Int(i)),
                S::Num::Float(_) => unsupported_message("float as prop key", span, source_map),
            }
        }
        Computed(_) => unsupported_message("computed prop name", span, source_map),
    }
}

fn parse_binary_op(op: swc::BinaryOp, span: Span, source_map: &SourceMap) -> ParseResult<S::BinOp> {
    use swc::BinaryOp::*;
    use S::BinOp::*;
    use S::BinaryOp as B;
    use S::LogicalOp as L;
    match op {
        // `==`
        EqEq => Ok(BinaryOp(B::Equal)),
        // `!=`
        NotEq => Ok(BinaryOp(B::NotEqual)),
        // `===`
        EqEqEq => Ok(BinaryOp(B::StrictEqual)),
        // `!==`
        NotEqEq => Ok(BinaryOp(B::StrictNotEqual)),
        // `<`
        Lt => Ok(BinaryOp(B::LessThan)),
        // `<=`
        LtEq => Ok(BinaryOp(B::LessThanEqual)),
        // `>`
        Gt => Ok(BinaryOp(B::GreaterThan)),
        // `>=`
        GtEq => Ok(BinaryOp(B::GreaterThanEqual)),
        // `<<`
        LShift => Ok(BinaryOp(B::LeftShift)),
        // `>>`
        RShift => Ok(BinaryOp(B::RightShift)),
        // `>>>`
        ZeroFillRShift => Ok(BinaryOp(B::UnsignedRightShift)),
        // `+`
        Add => Ok(BinaryOp(B::Plus)),
        // `-`
        Sub => Ok(BinaryOp(B::Minus)),
        // `*`
        Mul => Ok(BinaryOp(B::Times)),
        // `/`
        Div => Ok(BinaryOp(B::Over)),
        // `%`
        Mod => Ok(BinaryOp(B::Mod)),
        // `|`
        BitOr => Ok(BinaryOp(B::Or)),
        // `^`
        BitXor => Ok(BinaryOp(B::XOr)),
        // `&`
        BitAnd => Ok(BinaryOp(B::And)),
        // `in`
        In => Ok(BinaryOp(B::In)),
        // `instanceof`
        InstanceOf => Ok(BinaryOp(B::InstanceOf)),
        // `**`
        Exp => Ok(BinaryOp(B::PowerOf)),
        // `??`
        NullishCoalescing => unsupported!(span, source_map),
        // `||`
        LogicalOr => Ok(LogicalOp(L::Or)),
        // `&&`
        LogicalAnd => Ok(LogicalOp(L::And)),
    }
}

fn parse_unary_op(op: swc::UnaryOp, span: Span, source_map: &SourceMap) -> ParseResult<S::UnaryOp> {
    use swc::UnaryOp::*;
    use S::UnaryOp as U;
    match op {
        // `-`
        Minus => Ok(U::Minus),
        // `+`
        Plus => Ok(U::Plus),
        // `!`
        Bang => Ok(U::Not),
        // `~`
        Tilde => Ok(U::Tilde),
        // `typeof`
        TypeOf => Ok(U::TypeOf),
        // `void`
        Void => Ok(U::Void),
        // `delete`
        Delete => Ok(U::Delete),
    }
}

fn parse_assign_op(
    op: swc::AssignOp,
    span: Span,
    source_map: &SourceMap,
) -> ParseResult<S::AssignOp> {
    use swc::AssignOp::*;
    use S::AssignOp as A;
    match op {
        // `=`
        Assign => Ok(A::Equal),
        // Ok(`+=)`
        AddAssign => Ok(A::PlusEqual),
        // `-=`
        SubAssign => Ok(A::MinusEqual),
        // `*=`
        MulAssign => Ok(A::TimesEqual),
        // `/=`
        DivAssign => Ok(A::DivEqual),
        // `%=`
        ModAssign => Ok(A::ModEqual),
        // `<<=`
        LShiftAssign => Ok(A::LeftShiftEqual),
        // `>>=`
        RShiftAssign => Ok(A::RightShiftEqual),
        // `>>>=`
        ZeroFillRShiftAssign => Ok(A::UnsignedRightShiftEqual),
        // `|=`
        BitOrAssign => Ok(A::OrEqual),
        // `^=`
        BitXorAssign => Ok(A::XOrEqual),
        // `&=`
        BitAndAssign => Ok(A::AndEqual),
        // `**=`
        ExpAssign => Ok(A::PowerOfEqual),
        // `&&=`
        AndAssign => unsupported!(span, source_map),
        // `||=`
        OrAssign => unsupported!(span, source_map),
        // `??=`
        NullishAssign => unsupported!(span, source_map),
    }
}

fn parse_update_op(
    op: swc::UpdateOp,
    prefix: bool,
    span: Span,
    source_map: &SourceMap,
) -> ParseResult<S::UnaryAssignOp> {
    use swc::UpdateOp::*;
    use S::UnaryAssignOp as U;
    let op = match (op, prefix) {
        // `++`
        (PlusPlus, true) => U::PreInc,
        (PlusPlus, false) => U::PostInc,
        // `--`
        (MinusMinus, true) => U::PreDec,
        (MinusMinus, false) => U::PostDec,
    };

    Ok(op)
}

/// Parse an expression into an lvalue. If the expression can't be turned into
/// lvalue, an error will be returned.
/// `span` is the span of the surrounding expr.
fn parse_lvalue_from_expr(
    expr: swc::Expr,
    span: Span,
    source_map: &SourceMap,
) -> ParseResult<S::LValue> {
    use swc::Expr::*;
    match expr {
        // simple id case
        Ident(id) => Ok(S::LValue::Id(parse_id(id))),

        // object field lookup: `obj.prop`
        Member(swc::MemberExpr {
            obj,
            prop,
            computed: false,
            span,
        }) => match *prop {
            Ident(prop) => {
                let obj = parse_expr_or_super(obj, source_map)?;
                let prop = parse_id(prop);
                Ok(S::LValue::Dot(obj, prop))
            }
            other => unsupported_message(
                &format!("unexpected syntax on RHS of dot: {:?}", other),
                span,
                source_map,
            ),
        },

        // computed object field lookup: `obj[prop]`
        Member(swc::MemberExpr {
            obj,
            prop,
            computed: true,
            span,
        }) => {
            let obj = parse_expr_or_super(obj, source_map)?;
            let prop = parse_expr(*prop, source_map)?;
            Ok(S::LValue::Bracket(obj, prop))
        }

        // nothing else is a valid lvalue
        _other => unsupported_message("invalid lvalue", span, source_map),
    }
}

/// Parses a pattern or expression. We currently only support parsing
/// lvalues out of these. `span` is the span of the surrounding expression.
fn parse_pat_or_expr(
    poe: swc::PatOrExpr,
    span: Span,
    source_map: &SourceMap,
) -> ParseResult<S::LValue> {
    use swc::PatOrExpr::*;
    match poe {
        Expr(expr) => parse_lvalue_from_expr(*expr, span, source_map),
        Pat(pat) => parse_lvalue_from_pattern(*pat, span, source_map),
    }
}

fn parse_decl(decl: swc::Decl, source_map: &SourceMap) -> ParseResult<S::Stmt> {
    use swc::Decl::*;
    // only var and func declarations are supported
    match decl {
        Var(swc::VarDecl {
            span,
            kind, //: swc::VarDeclKind::Var,
            declare: _,
            decls,
        }) => {
            let decls: ParseResult<Vec<_>> = decls
                .into_iter()
                .map(|d| parse_var_declarator(d, source_map))
                .collect();
            Ok(S::Stmt::VarDecl(decls?))
        }
        Fn(swc::FnDecl {
            ident,
            declare: _,
            function,
        }) => {
            let ident = parse_id(ident);
            let (params, body) = parse_function(function, source_map)?;
            Ok(S::Stmt::Func(ident, params, Box::new(body)))
        }
        unsupported_decl => unsupported!(span_from_decl(unsupported_decl), source_map),
    }
}

fn parse_function(
    function: swc::Function,
    source_map: &SourceMap,
) -> ParseResult<(Vec<S::Id>, S::Stmt)> {
    let swc::Function {
        params,
        decorators,
        span,
        body,
        is_generator,
        is_async,
        ..
    } = function;
    // rule out cases we don't handle
    if is_generator {
        return unsupported_message("generators not supported", span, source_map);
    }
    if is_async {
        return unsupported_message("async not supported", span, source_map);
    }
    if decorators.len() > 0 {
        return unsupported_message("class decorators not supported", span, source_map);
    }
    let params: ParseResult<Vec<_>> = params
        .into_iter()
        .map(|p| parse_func_arg(p, source_map))
        .collect();
    let body = match body {
        Some(block) => parse_block(block, source_map)?,
        None => S::Stmt::Empty,
    };

    // put it all together
    Ok((params?, body))
}

/// Convert a numeric value from the parser into our AST's numbers.
///
/// This is tricky because our parser only stores numeric values in f64's,
/// and we store both floats and ints. This is a hacky solution that should
/// work for most programs, but there may be a way to get more info from the
/// parser and return a more precise result.
fn parse_num(value: f64) -> S::Num {
    // SWC represents all real numeric literals with f64.
    // Now we have to figure out if the user gave a value that should
    // be represented with a Num or a Float.

    // TODO(mark): hopefully this doesn't break anything, but if it does,
    //             look into how we could get more info from SWC about
    //             the num lit

    // does converting f64 -> i32 -> f64 yield the original value?
    if (value as i32) as f64 == value {
        // if so, this PROBABLY should be represented with an int
        S::Num::Int(value as i32)
    } else {
        S::Num::Float(value)
    }
}

/// Get the span out of a decl.
/// sidenote: one of the most annoying functions I've ever written. just
/// include a span in the decl and this would be so easy.......
fn span_from_decl(decl: swc::Decl) -> Span {
    use swc::Decl::*;
    use swc::*;
    match decl {
        Class(ClassDecl {
            class: swc::Class { span, .. },
            ..
        })
        | Fn(FnDecl {
            function: Function { span, .. },
            ..
        })
        | Var(VarDecl { span, .. })
        | TsInterface(TsInterfaceDecl { span, .. })
        | TsTypeAlias(TsTypeAliasDecl { span, .. })
        | TsEnum(TsEnumDecl { span, .. })
        | TsModule(TsModuleDecl { span, .. }) => span,
    }
}
