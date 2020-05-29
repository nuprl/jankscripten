use super::syntax::*;
use super::walk::*;
use super::*;
use super::constructors::*;
use resast::BinaryOp;
use resast::LogicalOp;

struct SwitchToIf { }


// NOTE (jenna): i don't like all this cloning but also don't know enough
// about rust to know how to fix it right now 
fn name_breaks(stmts: &Stmt, name: &Id) -> Stmt {
    let mut new_vec = vec![];
    match stmts {
        Stmt::Block(v) => {
            for s in v {
                match s {
                    Stmt::Break(None) => {
                        new_vec.push(Stmt::Break(Some(name.clone())));
                    } 
                    _ => {
                        new_vec.push(s.clone());
                    }
                }
            }
        }
        _ => { 
            // I believe the parser only produces blocks 
            // when pairing stmts with cases so this doesnt matter
        }
    }
    Stmt::Block(new_vec)
}

impl Visitor for SwitchToIf {
    fn exit_stmt(&mut self, stmt: &mut Stmt) {
        let mut ng = NameGen::default();
        let name = ng.fresh("switch");

        match stmt {
            Stmt::Switch(expr, cases, default) => { //cases = vec<(expr, stmt)>
                let test = &**expr;
                let mut v = vec![
                    vardecl1_("fallthrough", Expr::Lit(Lit::Bool(false))),
                    vardecl1_("test", test.clone())
                ];

                // create if statements for cases (test = e || fallthrough)
                for (e, s) in cases {
                    v.push(
                        Stmt::If(
                            Box::new(Expr::Binary(
                                BinOp::LogicalOp(LogicalOp::Or),
                                Box::new(Expr::Binary(
                                    BinOp::BinaryOp(BinaryOp::StrictEqual), 
                                    Box::new(Expr::Id(Id::Named("test".to_string()))),
                                    Box::new(e.clone()))),
                                Box::new(Expr::Id(Id::Named("fallthrough".to_string()))))),
                            Box::new(name_breaks(s, &name)),
                            Box::new(Stmt::Empty)))
                }

                // add default case 
                let d = &**default;
                match d {
                    Stmt::Block(dv) => { //i think this is always a block
                        for s in dv {
                            v.push(s.clone());
                        }
                    },
                    _ => v.push(d.clone())
                }

                //create labeled block w if statements/default 
                *stmt = Stmt::Label(
                    name,
                    Box::new(Stmt::Block(v)))
            }
            _ => {}
        }
    }
}

// TODO (jenna): testing -- current "tests" for own use 
// Notes:
// * won't be able to get an Id::Generated just by parsing ifs, so
//   assert-eq! won't work if doing that to compare 
// * not sure if all possible breaks will be handled -- need to look
//   into break behavior some more 

#[test] 
fn parse_switch() {
    let prog = parse(r#"
        var x = 1;
        switch(x) {
            case 0:
                x = 1;
                break;
            case 1:
                x = 2;
            default:
                x = 3;
                x = 4;
        }
    "#).unwrap();

    print!("{:?}", prog);
}

#[test] 
fn parse_if() {
    let prog = parse(r#"
        sw: {
            let fallthrough = false;
            let test = x;
            if (test === 0 || fallthrough) {
                x = 1;
                break sw;
            } if (test === 1 || fallthrough) {
                x = 2;
            } 
            x = 3;
            x = 4;
        }
    "#).unwrap();
    print!("{:?}", prog);
}

#[test]
fn switchtoif() {
    let mut prog = parse(r#"
        switch(x) {
            case 0: 
                x = 1;
                break;
            case 1: 
                x = 2;
            default: 
                x = 3;
                x = 4;
        }
    "#).unwrap();

    prog.walk(&mut SwitchToIf {});
    print!("{:?}", prog);
}