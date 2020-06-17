use super::syntax::*;
use super::walk::*;
use super::*;
use super::constructors::*;
use resast::BinaryOp;
use resast::LogicalOp;

struct SwitchToIf<'a> { ng: &'a mut NameGen }

fn name_breaks(stmts: &Stmt, name: &Id, fallthrough: &Id) -> Stmt {
    let mut new_vec = vec![expr_(assign_(fallthrough.clone(), TRUE_))];
    match stmts {
        Stmt::Block(v) => {
            for s in v {
                match s {
                    Stmt::Break(None) => {
                        new_vec.push(break_(Some(name.clone())));
                    } 
                    _ => {
                        new_vec.push(s.clone());
                    }
                }
            }
        }
        _ => { 
            panic!("Block expected");
        }
    }
    Stmt::Block(new_vec)
}

impl Visitor for SwitchToIf<'_> {
    fn exit_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Switch(expr, cases, default) => { //cases = vec<(expr, stmt)>
                let name = self.ng.fresh("sw");
                let test = expr.take();
                let test_id = self.ng.fresh("test");
                let fallthrough = self.ng.fresh("fallthrough");

                let mut v = vec![
                    vardecl1_(fallthrough.clone(), FALSE_),
                    vardecl1_(test_id.clone(), test)
                ];

                // create if statements for cases (test === e || fallthrough)
                for (e, s) in cases {
                    v.push(
                        if_(
                            binary_(
                                BinOp::LogicalOp(LogicalOp::Or),
                                binary_(
                                    BinOp::BinaryOp(BinaryOp::StrictEqual), 
                                    id_(test_id.clone()),
                                    e.clone()),
                                id_(fallthrough.clone())),
                            name_breaks(s, &name, &fallthrough),
                            Stmt::Empty))
                }

                // add default case (if applicable)
                let mut d = default.take();
                match d {
                    Stmt::Block(mut dv) => {
                        for s in dv.drain(0..) {
                            v.push(s);
                        }
                    },
                    Stmt::Empty => {},
                    _ => {
                        panic!("Block or Empty expected");
                    }
                }

                //create labeled block w if statements/default 
                *stmt = label_(
                    name,
                    Stmt::Block(v))
            }
            _ => {}
        }
    }
}

pub fn simpl(program: &mut Stmt, namegen: &mut NameGen) {
    let mut v = SwitchToIf { ng: namegen };
    program.walk(&mut v);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::javascript::testing::*;

    #[test]
    fn switchtoif_break() {
        let prog = r#"
            var x = 0;
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
        "#;
        desugar_okay(prog, simpl);
    }

    #[test]
    fn switchtoif_nobreak() {
        let prog = r#"
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
        "#;
        desugar_okay(prog, simpl);
    }

    #[test]
    fn switchtoif_latebreak() {
        let prog = r#"
            var x = 0;
            switch(x) {
                case 0: 
                    x = 1;
                case 1: 
                    x = 2;
                    break;
                default: 
                    x = 3;
                    x = 4;
            }
        "#;
        desugar_okay(prog, simpl);
    }

    #[test]
    fn switchtoif_nodefault() {
        let prog = r#"
            var x = 2;
            switch(x) {
                case 0: 
                    x = 1;
                    break;
                case 1: 
                    x = 2;
            }
        "#;
        desugar_okay(prog, simpl);
    }
}