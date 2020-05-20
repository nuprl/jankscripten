use proc_macro2::{Ident, Punct, TokenStream, TokenTree};
use quote::quote;
use std::collections::HashMap;

/// easily construct [libjankscripten::syntax]
///
/// ```ignore
/// let y = Stmt::Empty;
/// let bind = stmt! {
///     let x = 5;
///     #y
/// };
/// ```
///
/// taking heavily from Mara Bos!
/// https://blog.m-ou.se/writing-python-inside-rust-2/
#[proc_macro]
pub fn stmt(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = TokenStream::from(input);
    let mut out = Parsible::default();
    process_input(input, &mut out);
    // another limitation of quote!
    let stmts_k = out.stmts_k;
    let stmts_v = out.stmts_v;
    let exprs_k = out.exprs_k;
    let exprs_v = out.exprs_v;
    let js = out.js;
    (quote! {
        {
            let mut stmts = std::collections::HashMap::new();
            #(stmts.insert(#stmts_k, #stmts_v);)*
            let mut exprs = std::collections::HashMap::new();
            #(exprs.insert(#exprs_k, #exprs_v);)*
            crate::javascript::quote::unplaceholder(#js, stmts, exprs)
        }
    })
    .into()
}

/// same as [stmt] but outputs an Expr
#[proc_macro]
pub fn expr(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = TokenStream::from(input);
    let mut out = Parsible::default();
    process_input(input, &mut out);
    let stmts_k = out.stmts_k;
    let stmts_v = out.stmts_v;
    let exprs_k = out.exprs_k;
    let exprs_v = out.exprs_v;
    let js = out.js;
    (quote! {
        {
            let mut stmts = std::collections::HashMap::new();
            #(stmts.insert(#stmts_k, #stmts_v);)*
            let mut exprs = std::collections::HashMap::new();
            #(exprs.insert(#exprs_k, #exprs_v);)*
            crate::javascript::quote::unplaceholder_expr(#js, stmts, exprs)
        }
    })
    .into()
}

fn process_input(input: TokenStream, out: &mut Parsible) {
    let mut input = input.into_iter();
    while let Some(t) = input.next() {
        match &t {
            TokenTree::Punct(t) => {
                match t.as_char() {
                    '#' | '@' => {
                        let mut next = input.next();
                        let mut deref = false;
                        if let Some(TokenTree::Punct(ref p)) = next {
                            if p.as_char() == '*' {
                                deref = true;
                                next = input.next();
                            }
                        }
                        if let Some(TokenTree::Ident(var)) = next {
                            let varname = format!("$jen_{}", var);
                            let (k, v) = match t.as_char() {
                                '#' => {
                                    // throw is chosen as a simply statement that has
                                    // an identifier for placeholder
                                    out.js.push_str(&format!("throw {};", varname));
                                    (&mut out.stmts_k, &mut out.stmts_v)
                                }
                                '@' => {
                                    // id is chosen as a simply statement that has an
                                    // identifier for placeholder
                                    out.js.push_str(&format!("{}", varname));
                                    (&mut out.exprs_k, &mut out.exprs_v)
                                }
                                _ => unreachable!(),
                            };
                            k.push(varname.to_string());
                            v.push(if deref { quote!(*#var) } else { quote!(#var) });
                        } else {
                            panic!("incorrect #quote/@quote/#*quote usage");
                        }
                    }
                    _ => out.js.push_str(&t.to_string()),
                }
            }
            TokenTree::Group(g) => {
                let s = g.to_string();
                out.js.push_str(&s[..1]);
                process_input(g.stream(), out);
                out.js.push_str(&s[s.len() - 1..]);
            }
            _ => {
                out.js.push_str(&t.to_string());
                out.js.push(' ');
            }
        }
    }
}

#[derive(Default)]
struct Parsible {
    // making them tuples confuses quote! fsr
    stmts_k: Vec<String>,
    stmts_v: Vec<TokenStream>,
    exprs_k: Vec<String>,
    exprs_v: Vec<TokenStream>,
    js: String,
}

#[cfg(test)]
mod tests {}
