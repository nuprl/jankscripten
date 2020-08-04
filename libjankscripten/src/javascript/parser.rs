//! A "parser" for JavaScript.
//!
//! This isn't really a parser. We use the Ressa crate to parse JavaScript.
//! However, the JavaScript AST that Ressa produces covers new language features
//! that we do not need to support because (1) our benchmark compilers do not
//! produce JavaScript that uses them, and (2) they can be desugared if needed.
//! Thus this "parser" calls the Ressa parser and transforms the Ressa AST
//! to our simpler AST.
use super::constructors::*;
use super::syntax as S;
use resast::prelude::*;
use ressa::Parser;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    /// An error from the Ressa parser.
    #[error("{0}")]
    Ressa(#[from] ressa::Error),
    /// The Ressa AST had a JavaScript feature that we do not support.
    #[error("Unsupported: {0}")]
    Unsupported(String),
}

fn unsupported<T>() -> Result<T, ParseError> {
    return Err(ParseError::Unsupported("".to_string()));
}

fn unsupported_message<T>(msg: &str) -> Result<T, ParseError> {
    return Err(ParseError::Unsupported(msg.to_string()));
}

pub type ParseResult<T> = Result<T, ParseError>;

pub fn parse(js_code: &str) -> ParseResult<S::Stmt> {
    let mut parser = Parser::new(&js_code).unwrap();
    let ast = parser.parse()?;
    return simpl_program(ast);
}

fn simpl_lvalue<'a>(expr: Expr<'a>) -> ParseResult<S::LValue> {
    match expr {
        Expr::Ident(id) => Ok(S::LValue::Id(id.name.into())),
        Expr::Member(MemberExpr {
            object,
            property,
            computed: false,
        }) => match *property {
            Expr::Ident(prop) => Ok(S::LValue::Dot(simpl_expr(*object)?, prop.name.into())),
            other => unsupported_message(&format!("unexpected syntax on RHS of dot: {:?}", other)),
        },
        Expr::Member(MemberExpr {
            object,
            property,
            computed: true,
        }) => Ok(S::LValue::Bracket(
            simpl_expr(*object)?,
            simpl_expr(*property)?,
        )),
        _other => unsupported(),
    }
}

fn simpl_assign_left<'a>(assign_left: AssignLeft<'a>) -> ParseResult<S::LValue> {
    match assign_left {
        AssignLeft::Expr(e) => simpl_lvalue(*e),
        _other => unsupported(),
    }
}

fn simpl_string_lit<'a>(string_lit: StringLit<'a>) -> String {
    match string_lit {
        StringLit::Single(s) => s.into_owned(),
        StringLit::Double(s) => s.into_owned(),
    }
}

fn simpl_prop_key<'a>(prop_key: PropKey) -> ParseResult<S::Key> {
    match prop_key {
        PropKey::Pat(p) => Ok(S::Key::Str(simpl_pat_str(p)?)),
        PropKey::Expr(Expr::Ident(x)) => Ok(S::Key::Str(x.name.into_owned())),
        PropKey::Lit(Lit::String(s)) => Ok(S::Key::Str(simpl_string_lit(s))),
        PropKey::Lit(Lit::Number(s)) => match s.parse::<i32>() {
            Ok(n) => Ok(S::Key::Int(n)),
            Err(_) => unsupported_message(&format!("could not parse {} as an integer", s)),
        },
        other => unsupported_message(&format!("not a property key: {:?}", other)),
    }
}

fn simpl_obj_prop<'a>(obj_prop: ObjProp<'a>) -> ParseResult<(S::Key, S::Expr)> {
    match obj_prop {
        ObjProp::Spread(_) => unsupported(),
        ObjProp::Prop(Prop {
            key,
            value,
            kind,
            method,
            computed,
            short_hand,
            is_static,
        }) => {
            // NOTE(arjun): I do not know what this is.
            if short_hand {
                return unsupported();
            }
            if computed {
                return unsupported();
            }
            if is_static {
                return unsupported();
            }
            if method {
                return unsupported();
            }
            if kind != PropKind::Init {
                return unsupported();
            }
            let expr = match value {
                PropValue::Expr(e) => simpl_expr(e)?,
                _ => return unsupported(),
            };
            Ok((simpl_prop_key(key)?, expr))
        }
    }
}

fn simpl_lit<'a>(lit: Lit<'a>) -> ParseResult<S::Lit> {
    match lit {
        Lit::Boolean(b) => Ok(S::Lit::Bool(b)),
        Lit::Null => Ok(S::Lit::Null),
        Lit::Number(n) => Ok(S::Lit::Num(parse_number(&n))),
        Lit::RegEx(RegEx { pattern, flags }) => {
            Ok(S::Lit::Regex(pattern.into_owned(), flags.into_owned()))
        }
        Lit::String(s) => Ok(S::Lit::String(parse_string(&s))),
        Lit::Template(_) => unsupported(),
    }
}

fn simpl_expr<'a>(expr: Expr<'a>) -> ParseResult<S::Expr> {
    match expr {
        Expr::Array(items) => {
            let items: ParseResult<Vec<_>> = items.into_iter().map(simpl_opt_expr).collect();
            Ok(S::Expr::Array(items?))
        }
        Expr::ArrowFunc(_) => unsupported(),
        // NOTE(arjun): I have no idea what this is!
        Expr::ArrowParamPlaceHolder(_, _) => unsupported(),
        Expr::Assign(AssignExpr {
            operator,
            left,
            right,
        }) => Ok(op_assign_(
            operator,
            simpl_assign_left(left)?,
            simpl_expr(*right)?,
        )),
        Expr::Await(_) => unsupported(),
        Expr::Binary(BinaryExpr {
            operator,
            left,
            right,
        }) => Ok(binary_(
            S::BinOp::BinaryOp(operator),
            simpl_expr(*left)?,
            simpl_expr(*right)?,
        )),
        Expr::Class(_) => unsupported(),
        Expr::Call(CallExpr { callee, arguments }) => {
            let arguments: ParseResult<Vec<_>> = arguments.into_iter().map(simpl_expr).collect();
            Ok(call_(simpl_expr(*callee)?, arguments?))
        }
        Expr::Conditional(ConditionalExpr {
            test,
            alternate,
            consequent,
        }) => Ok(if_expr_(
            simpl_expr(*test)?,
            simpl_expr(*consequent)?,
            simpl_expr(*alternate)?,
        )),
        Expr::Func(Func {
            id,
            params,
            body,
            generator,
            is_async,
        }) => {
            if generator {
                return unsupported();
            }
            if is_async {
                return unsupported();
            }
            let id = match id {
                Some(ident) => Some(ident.name.into_owned()),
                None => None,
            };
            let params: ParseResult<Vec<_>> = params.into_iter().map(simpl_func_arg).collect();
            let FuncBody(parts) = body;
            Ok(expr_func_(id, params?, simpl_program_parts(parts)?))
        }
        Expr::Ident(id) => Ok(id_(id.name)),
        Expr::Logical(LogicalExpr {
            operator,
            left,
            right,
        }) => Ok(binary_(
            S::BinOp::LogicalOp(operator),
            simpl_expr(*left)?,
            simpl_expr(*right)?,
        )),
        Expr::Lit(l) => Ok(S::Expr::Lit(simpl_lit(l)?)),
        Expr::Member(MemberExpr {
            object,
            property,
            computed,
        }) => {
            if computed {
                Ok(bracket_(simpl_expr(*object)?, simpl_expr(*property)?))
            } else {
                match *property {
                    Expr::Ident(id) => Ok(dot_(simpl_expr(*object)?, id.name)),
                    _other => unsupported(),
                }
            }
        }
        Expr::MetaProp(_) => unsupported(), // new.target
        Expr::New(NewExpr { callee, arguments }) => {
            let arguments: ParseResult<Vec<_>> = arguments.into_iter().map(simpl_expr).collect();
            Ok(new_(simpl_expr(*callee)?, arguments?))
        }
        Expr::Obj(props) => {
            let props: ParseResult<Vec<_>> = props.into_iter().map(simpl_obj_prop).collect();
            Ok(S::Expr::Object(props?))
        }
        Expr::Sequence(exprs) => {
            let exprs: ParseResult<Vec<_>> = exprs.into_iter().map(simpl_expr).collect();
            Ok(S::Expr::Seq(exprs?))
        }
        Expr::Spread(_) => unsupported(),
        Expr::Super => unsupported(),
        Expr::TaggedTemplate(_) => unsupported(),
        Expr::This => Ok(S::Expr::This),
        Expr::Unary(UnaryExpr {
            operator,
            prefix,
            argument,
        }) => {
            // NOTE(arjun): I cannot think of any postfix unary operators!
            assert!(prefix == true);
            Ok(unary_(operator, simpl_expr(*argument)?))
        }
        Expr::Update(UpdateExpr {
            operator,
            argument,
            prefix,
        }) => {
            let op = match (operator, prefix) {
                (UpdateOp::Decrement, true) => S::UnaryAssignOp::PreDec,
                (UpdateOp::Decrement, false) => S::UnaryAssignOp::PostDec,
                (UpdateOp::Increment, true) => S::UnaryAssignOp::PreInc,
                (UpdateOp::Increment, false) => S::UnaryAssignOp::PostInc,
            };
            Ok(unaryassign_(op, simpl_lvalue(*argument)?))
        }
        Expr::Yield(_) => unsupported(),
    }
}

fn simpl_opt_expr<'a>(opt_expr: Option<Expr<'a>>) -> ParseResult<S::Expr> {
    match opt_expr {
        None => Ok(UNDEFINED_),
        Some(e) => Ok(simpl_expr(e)?),
    }
}

fn simpl_opt_stmt<'a>(opt_stmt: Option<Stmt<'a>>) -> ParseResult<S::Stmt> {
    match opt_stmt {
        None => Ok(S::Stmt::Empty),
        Some(e) => Ok(simpl_stmt(e)?),
    }
}

fn simpl_switch_case<'a>(case: SwitchCase<'a>) -> ParseResult<(Option<S::Expr>, S::Stmt)> {
    let test = match case.test {
        None => None,
        Some(e) => Some(simpl_expr(e)?),
    };
    Ok((test, simpl_program_parts(case.consequent)?))
}

fn simpl_pat<'a>(pat: Pat<'a>) -> ParseResult<S::Id> {
    match pat {
        Pat::Ident(ident) => Ok(ident.name.into()),
        _ => unsupported(),
    }
}
fn simpl_pat_str<'a>(pat: Pat<'a>) -> ParseResult<String> {
    match pat {
        Pat::Ident(ident) => Ok(ident.name.into()),
        _ => unsupported(),
    }
}

fn simpl_stmt<'a>(stmt: Stmt<'a>) -> ParseResult<S::Stmt> {
    match stmt {
        Stmt::Expr(e) => Ok(expr_(simpl_expr(e)?)),
        Stmt::Block(BlockStmt(parts)) => {
            let stmts: Result<Vec<_>, _> = parts
                .into_iter()
                .map(|part| simpl_program_part(part))
                .collect();
            Ok(S::Stmt::Block(stmts?))
        }
        Stmt::Empty => Ok(S::Stmt::Empty),
        Stmt::Debugger => unimplemented!(),
        Stmt::With(_) => unsupported(),
        Stmt::Return(oe) => Ok(return_(simpl_opt_expr(oe)?)),
        Stmt::Labeled(LabeledStmt { label, body }) => Ok(label_(label.name, simpl_stmt(*body)?)),
        Stmt::Break(opt_id) => Ok(break_(opt_id.map(|l| l.name.into_owned()))),
        Stmt::Continue(opt_id) => Ok(continue_(opt_id.map(|l| l.name.into_owned()))),
        Stmt::If(IfStmt {
            test,
            consequent,
            alternate,
        }) => Ok(if_(
            simpl_expr(test)?,
            simpl_stmt(*consequent)?,
            simpl_opt_stmt(alternate.map(|b| *b))?,
        )),
        Stmt::Switch(SwitchStmt {
            discriminant,
            cases,
        }) => {
            let cases: Result<Vec<_>, _> = cases.into_iter().map(simpl_switch_case).collect();
            let (cases, mut default_case) = cases?
                .into_iter()
                .partition::<Vec<_>, _>(|(test, _)| test.is_some());
            let default_case = match default_case.len() {
                0 => S::Stmt::Empty,
                1 => default_case.remove(0).1,
                _ => panic!("switch with multiple default cases"),
            };
            let cases = cases.into_iter().map(|(test, body)| (test.unwrap(), body));
            Ok(switch_(
                simpl_expr(discriminant)?,
                cases.collect(),
                default_case,
            ))
        }
        Stmt::Throw(e) => Ok(throw_(simpl_expr(e)?)),
        Stmt::Try(TryStmt {
            block,
            handler,
            finalizer,
        }) => {
            let stmt = simpl_block(block)?;
            let stmt = match handler {
                None => stmt,
                Some(CatchClause {
                    param: Some(pat),
                    body,
                }) => catch_(stmt, simpl_pat(pat)?, simpl_block(body)?),
                Some(_) => return unsupported(),
            };
            let stmt = match finalizer {
                None => stmt,
                Some(block) => finally_(stmt, simpl_block(block)?),
            };
            Ok(stmt)
        }
        Stmt::While(WhileStmt { test, body }) => Ok(while_(simpl_expr(test)?, simpl_stmt(*body)?)),
        Stmt::DoWhile(DoWhileStmt { test, body }) => {
            Ok(dowhile_(simpl_stmt(*body)?, simpl_expr(test)?))
        }
        Stmt::For(ForStmt {
            init,
            test,
            update,
            body,
        }) => {
            let init = match init {
                None => S::ForInit::Expr(Box::new(UNDEFINED_)),
                Some(LoopInit::Expr(e)) => S::ForInit::Expr(Box::new(simpl_expr(e)?)),
                Some(LoopInit::Variable(_kind, decls)) => {
                    if _kind != VarKind::Var {
                        return unsupported_message("only var-declared variables are supported");
                    }
                    let decls: ParseResult<Vec<_>> =
                        decls.into_iter().map(simpl_var_decl).collect();
                    S::ForInit::Decl(decls?)
                }
            };
            Ok(for_(
                init,
                simpl_opt_expr(test)?,
                simpl_opt_expr(update)?,
                simpl_stmt(*body)?,
            ))
        }
        Stmt::ForIn(ForInStmt { left, right, body }) => {
            let (is_var, id) = match left {
                LoopLeft::Variable(
                    _,
                    VarDecl {
                        id: Pat::Ident(id),
                        init: _,
                    },
                ) => (true, id.name.into_owned()),
                LoopLeft::Expr(Expr::Ident(x)) => (false, x.name.into_owned()),
                // The program may pattern match on the index, which we do not support.
                other => {
                    return unsupported_message(&format!(
                        "unsupported index in a for-in loop: {:?}",
                        other
                    ))
                }
            };

            Ok(forin_(is_var, id, simpl_expr(right)?, simpl_stmt(*body)?))
        }
        Stmt::ForOf(_) => unsupported(),
        Stmt::Var(decls) => {
            let decls: ParseResult<Vec<_>> = decls.into_iter().map(simpl_var_decl).collect();
            Ok(S::Stmt::VarDecl(decls?))
        }
    }
}

fn simpl_var_decl<'a>(var_decl: VarDecl<'a>) -> ParseResult<S::VarDecl> {
    Ok(S::VarDecl {
        name: simpl_pat(var_decl.id)?,
        named: Box::new(simpl_opt_expr(var_decl.init)?),
    })
}

fn simpl_func_arg<'a>(func_arg: FuncArg<'a>) -> ParseResult<S::Id> {
    match func_arg {
        FuncArg::Expr(_) => unsupported(),
        FuncArg::Pat(p) => simpl_pat(p),
    }
}

fn simpl_decl<'a>(decl: Decl<'a>) -> ParseResult<S::Stmt> {
    match decl {
        Decl::Class(_) => unsupported(),
        Decl::Import(_) => unsupported(),
        Decl::Export(_) => unsupported(),
        Decl::Var(_kind, decls) => {
            let decls: ParseResult<Vec<_>> = decls.into_iter().map(simpl_var_decl).collect();
            Ok(S::Stmt::VarDecl(decls?))
        }
        Decl::Func(Func {
            id,
            params,
            body,
            generator,
            is_async,
        }) => {
            if generator {
                return unsupported();
            }
            if is_async {
                return unsupported();
            }
            let id = match id {
                Some(ident) => ident.name.into_owned(),
                None => return unsupported(),
            };
            let params: ParseResult<Vec<_>> = params.into_iter().map(simpl_func_arg).collect();
            let FuncBody(parts) = body;
            Ok(func_(id, params?, simpl_program_parts(parts)?))
        }
    }
}

fn simpl_block<'a>(block: BlockStmt<'a>) -> ParseResult<S::Stmt> {
    let BlockStmt(parts) = block;
    return simpl_program_parts(parts);
}

fn simpl_program_parts<'a>(parts: Vec<ProgramPart<'a>>) -> ParseResult<S::Stmt> {
    let ss: ParseResult<Vec<_>> = parts.into_iter().map(simpl_program_part).collect();
    Ok(S::Stmt::Block(ss?))
}

fn simpl_program_part<'a>(part: ProgramPart<'a>) -> ParseResult<S::Stmt> {
    match part {
        ProgramPart::Stmt(s) => Ok(simpl_stmt(s)?),
        ProgramPart::Decl(d) => Ok(simpl_decl(d)?),
        // This is likely 'use strict' and we ignore it.
        ProgramPart::Dir(_) => Ok(S::Stmt::Empty),
    }
}

fn simpl_program<'a>(program: Program<'a>) -> ParseResult<S::Stmt> {
    match program {
        Program::Mod(_) => unsupported(),
        Program::Script(parts) => {
            let maybe_stmts: Result<Vec<_>, _> = parts
                .into_iter()
                .map(|part| simpl_program_part(part))
                .collect();
            let mut stmts = maybe_stmts?;
            if stmts.len() == 1 {
                // Ensure the outermost statement is always a block, but not a double-nested block.
                match stmts[0] {
                    S::Stmt::Block(_) => Ok(stmts.pop().unwrap()),
                    _ => Ok(S::Stmt::Block(vec![stmts.pop().unwrap()])),
                }
            } else {
                Ok(S::Stmt::Block(stmts))
            }
        }
    }
}

// TODO(arjun): Someone needs to read the ECMAScript specification to confirm
// that the code here is legit.
fn parse_number(s: &str) -> S::Num {
    if s.starts_with("0x") {
        // It looks like a hex literal in JavaScript can be an
        // unsigned 32-bit integer. I presume we are doing the right thing by
        // casting the u32 to an i32, but I am not certain.
        return u32::from_str_radix(&s[2..], 16)
            .map(|i| S::Num::Int(i as i32))
            // This seems silly. You can write a hex literal that
            // is larger than the largest u32.
            .or_else(|_err| u64::from_str_radix(&s[2..], 16).map(|i| S::Num::Float(i as f64)))
            .expect(&format!("Ressa did not parse hex value correctly ({})", &s));
    }

    // TODO(arjun): JavaScript supports octal, which this does not parse.
    return i32::from_str_radix(s, 10)
        .map(|i| S::Num::Int(i))
        .or_else(|_err| s.parse::<f64>().map(|x| S::Num::Float(x)))
        .expect(&format!("Cannot parse {} as a number", s));
}

// TODO(arjun): Someone needs to read the ECMAScript specification to confirm
// that the code here is legit.
fn parse_string<'a>(s: &StringLit<'a>) -> String {
    let literal_chars = match s {
        StringLit::Double(s) => s,
        StringLit::Single(s) => s,
    };

    let mut buf = String::with_capacity(literal_chars.len());
    let mut iter = literal_chars.chars();
    while let Some(ch) = iter.next() {
        if ch != '\\' {
            buf.push(ch);
            continue;
        }
        match iter.next().expect("character after backslash") {
            '\'' | '"' | '\\' => buf.push(ch),
            'n' => buf.push('\n'),
            'r' => buf.push('\r'),
            't' => buf.push('\t'),
            'f' => buf.push('\x0C'),
            'b' => buf.push('\x08'),
            'v' => buf.push('\x0B'),
            '0' => buf.push('\0'),
            'x' => {
                let s = format!(
                    "{}{}",
                    iter.next().expect("first hex digit after \\x"),
                    iter.next().expect("second hex digit after \\x")
                );
                let n = u8::from_str_radix(&s, 16)
                    .expect(&format!("invalid escape \\x{} (Ressa issue)", &s));
                buf.push(n as char);
            }
            'u' => {
                let s = format!(
                    "{}{}{}{}",
                    iter.next().expect("first hex digit after \\x"),
                    iter.next().expect("second hex digit after \\x"),
                    iter.next().expect("third hex digit after \\x"),
                    iter.next().expect("fourth hex digit after \\x")
                );
                let n = u16::from_str_radix(&s, 16)
                    .expect(&format!("invalid escape \\u{} (Ressa issue)", &s));
                buf.push(
                    std::char::from_u32(n as u32).expect("invalid Unicode character from Ressa"),
                );
            }
            ch => {
                if ch >= '0' || ch <= '9' {
                    // These are yet another special case!
                    panic!("unsupported escape");
                }
                buf.push(ch);
            }
        }
    }
    return buf;
}

#[cfg(test)]
mod tests {
    use super::super::syntax::*;
    use super::*;

    #[test]
    fn parse_int() {
        assert_eq!(parse_number("205"), Num::Int(205));
    }

    #[test]
    fn parse_hex() {
        assert_eq!(parse_number("0xff"), Num::Int(255));
    }

    #[test]
    fn parse_float() {
        assert_eq!(parse_number("3.14"), Num::Float(3.14));
    }
}
