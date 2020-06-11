use super::syntax::*;
use super::walk::*;
use super::*;
use super::constructors::*;
use resast::BinaryOp;
use resast::LogicalOp;

struct SwitchToIf<'a>(&'a mut NameGen);

// NOTE (jenna): i don't like all this cloning but also don't know enough
// about rust to know if i can avoid it :/
fn name_breaks(stmts: &Stmt, name: &Id) -> Stmt {
    let mut new_vec = vec![];
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
        let name = self.0.fresh("sw");

        match stmt {
            Stmt::Switch(expr, cases, default) => { //cases = vec<(expr, stmt)>
                let test = &**expr;
                let mut v = vec![
                    vardecl1_("fallthrough", FALSE_),
                    vardecl1_("test", test.clone())
                ];

                // create if statements for cases (test === e || fallthrough)
                for (e, s) in cases {
                    v.push(
                        if_(
                            binary_(
                                BinOp::LogicalOp(LogicalOp::Or),
                                binary_(
                                    BinOp::BinaryOp(BinaryOp::StrictEqual), 
                                    id_("test".to_string()),
                                    e.clone()),
                                id_("fallthrough".to_string())),
                            name_breaks(s, &name),
                            Stmt::Empty))
                }

                // add default case 
                let d = &**default;
                match d {
                    Stmt::Block(dv) => { //i think this is always a block
                        for s in dv {
                            v.push(s.clone());
                        }
                    },
                    _ => {}
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

pub fn simpl(program: &mut Stmt, ng: &mut NameGen) {
    let mut v = SwitchToIf(ng);
    program.walk(&mut v);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::javascript::testing::*;

    #[test]
    fn switchtoif() {
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
        let mut parsed = parse(prog).unwrap();

        let result = parse(r#"
            var x = 1;
            $jen_sw_9: {
                let fallthrough = false;
                let test = x;
                if (test === 0 || fallthrough) {
                    x = 1;
                    break $jen_sw_9;
                } if (test === 1 || fallthrough) {
                    x = 2;
                } 
                x = 3;
                x = 4;
            }
        "#).unwrap();
        
        let mut ng = NameGen::default();
        simpl(&mut parsed, &mut ng);
        
        assert_eq!(parsed.to_pretty(80), result.to_pretty(80));
        desugar_okay(prog, simpl);
    }

    #[test]
    fn switchtoif_no_default() {
        let prog = r#"
            var x = 1;
            switch(x) {
                case 0: 
                    x = 1;
                    break;
                case 1: 
                    x = 2;
            }
        "#;

        let mut parsed = parse(prog).unwrap();

        let result = parse(r#"
            var x = 1;
            $jen_sw_7: {
                let fallthrough = false;
                let test = x;
                if (test === 0 || fallthrough) {
                    x = 1;
                    break $jen_sw_7;
                } if (test === 1 || fallthrough) {
                    x = 2;
                } 
            }
        "#).unwrap();

        let mut ng = NameGen::default();
        simpl(&mut parsed, &mut ng);
        
        assert_eq!(parsed.to_pretty(80), result.to_pretty(80));
        desugar_okay(prog, simpl);
    }

}