//use super::cons::*;
use super::cons::*;
use super::Stmt::*;
use super::{Expr, *};

const LABEL_PREFIX: &str = "$LABEL_";

pub fn compile_for(mut script: Stmt) -> Stmt {
    script = compile_for_in(script);
    script = for_to_while(script);
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

fn for_to_while(script: Stmt) -> Stmt {
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
        _ => node,
    })
}

/// label loops, always break/continue to labels
/// label all breaks and continues to closest loop if not already labeled
fn label_loops(script: Stmt) -> Stmt {
    let mut label_i = 0;
    let mut closest_loop = None;
    // avoids adding a label recursively forever
    let mut is_labeled = false;
    script.replace(&mut |node| match node {
        Break(None) => {
            is_labeled = false;
            Break(closest_loop.clone())
        }
        Continue(None) => {
            is_labeled = false;
            Continue(closest_loop.clone())
        }
        Label(..) => {
            is_labeled = true;
            node
        }
        While(..) => {
            if !is_labeled {
                is_labeled = true;
                let label = format!("{}{}", LABEL_PREFIX, label_i);
                label_i += 1;
                closest_loop = Some(label.clone());
                Label(label, Box::new(node))
            } else {
                node
            }
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
        };
        let while_loop = stmt! {
            let i=0;
            while (i<10) {
                {
                    console.log(i);
                }
                ++i
            }
        };
        assert_eq!(for_to_while(for_loop), while_loop);
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
            $LABEL_0: while (true) {
                break $LABEL_0;
                $LABEL_1: while (true) {
                    continue $LABEL_1;
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
                $LABEL_0: while ($jen_i<$jen_keys.length) {
                    {
                        let i = $jen_container[$jen_keys[$jen_i]];
                        {
                            break $LABEL_0;
                        }
                    }
                    ++$jen_i;
                }
            }
        };
        assert_eq!(compile_for(unlabeled), labeled);
    }
}
