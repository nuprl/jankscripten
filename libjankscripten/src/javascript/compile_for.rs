//use super::cons::*;
use super::cons::*;
use super::Stmt::*;
use super::{Expr, *};

pub fn compile_for(mut script: Stmt) -> Stmt {
    script = compile_for_in(script);
    script = loops_to_while(script);
    label_loops(script)
}

/// forin -> for
fn compile_for_in(script: Stmt) -> Stmt {
    script.replace(&mut |node| match node {
        ForIn(declare, id, container, body) => {
            let keys = stmt!({
                let $jen_container = @*container;
                let $jen_keys = $jen_container.keys();
            });
            let for_init = ForInit::Decl(vardecl1_("$jen_i", Expr::Lit(Lit::Num(Num::Int(0)))));
            let assign_to = expr!($jen_container[$jen_keys[$jen_i]]);
            let declaration = if declare {
                Stmt::VarDecl(vardecl1_(id, assign_to))
            } else {
                Stmt::Expr(Box::new(Expr::Assign(
                    AssignOp::Equal,
                    Box::new(LValue::Id(id)),
                    Box::new(assign_to),
                )))
            };
            let new_body = stmt!({
                #declaration
                #*body
            });
            let for_loop = for_(
                for_init,
                expr!($jen_i < $jen_keys.length),
                expr!(++$jen_i),
                new_body,
            );
            stmt!({ #keys #for_loop })
        }
        _ => node,
    })
}

fn loops_to_while(script: Stmt) -> Stmt {
    script.replace(&mut |node| match node {
        For(init, cond, advance, body) => {
            let init = match init {
                ForInit::Expr(e) => stmt!(@*e),
                ForInit::Decl(ds) => Stmt::VarDecl(ds),
            };
            stmt! {
                #init
                while (@*cond) {
                    #*body
                    @*advance
                }
            }
        }
        DoWhile(body, cond) => {
            stmt! {
                let $jen_once = true;
                while ($jen_once || @*cond) {
                    $jen_once = false;
                    #*body
                }
            }
        }
        _ => node,
    })
}

/// labels all while / break as expected; labels block inside of while and
/// changes continue to break
///
/// please see Stopify/normalize-js/ts/desugarLoop.ts:WhileStatement
fn label_loops(script: Stmt) -> Stmt {
    let mut label_i = 0;
    let mut break_label = None;
    let mut cont_label = None;
    // avoids adding a label recursively forever
    let mut is_labeled = false;
    script.replace(&mut |node| match node {
        Break(None) => {
            is_labeled = false;
            Break(break_label.clone())
        }
        Continue(None) => {
            is_labeled = false;
            Break(cont_label.clone())
        }
        Label(..) => {
            is_labeled = true;
            node
        }
        While(cond, body) if !is_labeled => {
            is_labeled = true;
            let break_name = format!("$break_{}", label_i);
            let cont_name = format!("$cont_{}", label_i);
            label_i += 1;
            break_label = Some(break_name.clone());
            cont_label = Some(cont_name.clone());
            let body_with_cont = Label(cont_name, body);
            let with_cont = While(cond, Box::new(body_with_cont));
            Label(break_name, Box::new(with_cont))
        }
        _ => {
            is_labeled = false;
            node
        }
    })
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn for_in_correct() {
        let for_in_loop = stmt! {
            let a = [1, 2, 3];
            for (let i in a) {
                console.log(a[i]);
            }
        };
        let for_loop = stmt! {
            let a = [1, 2, 3];
            {
                {
                    let $jen_container = a;
                    let $jen_keys = $jen_container.keys();
                }
                for (let $jen_i=0; $jen_i < $jen_keys.length; ++$jen_i) {
                    let i = $jen_container[$jen_keys[$jen_i]];
                    {
                        console.log(a[i]);
                    }
                }
            }
        };
        assert_eq!(compile_for_in(for_in_loop), for_loop);
    }
    #[test]
    fn for_in_no_declare() {
        let for_in_loop = stmt! {
            let a = [1, 2, 3];
            let i = 0;
            for (i in a) {
                console.log(a[i]);
            }
        };
        let for_loop = stmt! {
            let a = [1, 2, 3];
            let i = 0;
            {
                {
                    let $jen_container = a;
                    let $jen_keys = $jen_container.keys();
                }
                for (let $jen_i=0; $jen_i < $jen_keys.length; ++$jen_i) {
                    i = $jen_container[$jen_keys[$jen_i]];
                    {
                        console.log(a[i]);
                    }
                }
            }
        };
        assert_eq!(compile_for_in(for_in_loop), for_loop);
    }
    #[test]
    fn test_for_to_while() {
        let for_loop = stmt! {
            for (let i=0; i<10; ++i) {
                console.log(i);
            }
            do {
                console.log(i);
            } while (3 < 4)
        };
        let while_loop = stmt! {
            {
                let i=0;
                while (i<10) {
                    {
                        console.log(i);
                    }
                    ++i
                }
            }
            {
                let $jen_once = true;
                while ($jen_once || 3 < 4) {
                    $jen_once = false;
                    {
                        console.log(i);
                    }
                }
            }
        };
        assert_eq!(loops_to_while(for_loop), while_loop);
    }
    #[test]
    fn test_labels() {
        let unlabeled = stmt! {
            while (true) {
                break;
                while (true) {
                    continue;
                }
            }
        };
        let labeled = stmt! {
            $break_0: while (true) $cont_0: {
                break $break_0;
                $break_1: while (true) $cont_1: {
                    break $cont_1;
                }
            }
        };
        assert_eq!(label_loops(unlabeled), labeled);
    }
    #[test]
    fn full_unit() {
        let unlabeled = stmt! {
            for (let i in [1, 2, 3]) {
                break;
            }
        };
        let labeled = stmt! {
            {
                let $jen_container = [1, 2, 3];
                let $jen_keys = $jen_container.keys();
            }
            {
                let $jen_i = 0;
                $break_0: while ($jen_i<$jen_keys.length) $cont_0: {
                    {
                        let i = $jen_container[$jen_keys[$jen_i]];
                        {
                            break $break_0;
                        }
                    }
                    ++$jen_i;
                }
            }
        };
        assert_eq!(compile_for(unlabeled), labeled);
    }
}
