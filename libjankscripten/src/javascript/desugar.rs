use super::*;

pub fn desugar(stmt: &mut Stmt, ng: &mut NameGen) {
    desugar_switch::desugar_switch(stmt, ng);
    // rdep: do..while uses ||
    // dep: desugar_loops needs to come after desugar_switch 
    desugar_loops::desugar_loops(stmt, ng);
    // dep: desugar_logical needs loop conds to be simple
    desugar_logical::desugar_logical(stmt, ng);
    desugar_function_applications::desugar_function_applications(stmt, ng);
    //dep: desugar_updates needs desugar_function_applications to work properly
    desugar_updates::desugar_updates(stmt, ng);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::javascript::testing::desugar_okay;

    fn okay(script: &str) {
        desugar_okay(script, desugar);
    }

    #[test]
    fn desugar_do_while() {
        okay(
            "var r = 0;
            do {
                r += 1;
            } while (r < 10);
            r;",
        );
    }
    #[test]
    fn desugar_for_loops() {
        okay(
            "var r;
            for (r=0; r<10; ++r) {
                r += 1;
            }
            r;",
        );
    }
    #[test]
    fn desugar_labeled_continue() {
        okay(
            "var r = 0;
            label: for (var i=0; i<10; ++i) {
                r += i;
                continue label;
                r = 0;
            }
            r;",
        );
    }

    #[test]
    fn desugar_parse_pluseq() {    
        let prog = r#"
            var MyObject =  { x: 1};
            function f () {
                return MyObject;
            }
            f().x += 1;
        "#;
        
        okay(prog);
    }

    #[test]
    fn desugar_parse_minuseq() {
        let prog = r#"
            var x = 5;
            x -= 1;
        "#;
        
        okay(prog);
    }

    #[test]
    fn desugar_parse_timeseq() {
        let prog = r#"
            var x = 5;
            x *= 1;
        "#;
        
        okay(prog);
    }

    #[test]
    fn desugar_parse_diveq() {
        let prog = r#"
            var x = 5;
            x /= 1;
        "#;
        
        okay(prog);
    }

    #[test]
    fn desugar_parse_modeq() {
        let prog = r#"
            var x = 5;
            x %= 1;
        "#;
        
        okay(prog);
    }

    #[test]
    fn desugar_parse_lshifteq() {
        let prog = r#"
            var x = 5;
            x <<= 1;
        "#;

        okay(prog);
    }

    #[test]
    fn desugar_parse_rshifteq() {
        let prog = r#"
            var x = 5;
            x >>= 1;
        "#;
        
        okay(prog);
    }

    #[test]
    fn desugar_parse_unsignedrshifteq() {
        let prog = r#"
            var x = 5;
            x >>>= 1;
        "#;
        
        okay(prog);
    }

    #[test]
    fn desugar_parse_expeq() {
        let prog = r#"
            var x = 5;
            x **= 1;
        "#;
        
        okay(prog);
    }

    #[test]
    fn desugar_parse_oreq() {
        let prog = r#"
            var x = false;
            x |= true;
        "#;
        
        okay(prog);
    }

    #[test]
    fn desugar_parse_xoreq() {
        let prog = r#"
            var x = true;
            x ^= true;
        "#;
        
        okay(prog);
    }

    #[test]
    fn desugar_parse_andeq() {
        let prog = r#"
            var x = false;
            x &= true;
        "#;

        okay(prog);
    }

    #[test]
    fn desugar_switchtoif_break() {
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
        okay(prog);
    }

    #[test]
    fn desugar_switchtoif_nobreak() {
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
        okay(prog);
    }

    #[test]
    fn desugar_switchtoif_latebreak() {
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
        okay(prog);
    }

    #[test]
    fn desugar_switchtoif_nodefault() {
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
        okay(prog);
    }

    #[test]
    fn desugar_name_call_fancyupdate() {
        let prog =r#"
            function f(arg) {
                return arg;
            }
            var x = 1;
            x += f(1);
            x;
        "#;
        okay(prog);
    }

    #[test]
    fn desugar_name_call_dot() {
        let prog =r#"
            var obj = { x: 1 }
            function f() {
                return obj;
            }
            f().x += 1;
            f().x;
        "#;
        okay(prog);
    }

    #[test]
    fn desugar_name_call_nested() {
        let prog = r#"
            function f(arg) {
                return arg + 1;
            }
            function g() {
                return 5;
            }
            console.log(f(g()));
        "#;

        okay(prog);
    }

    #[test]
    fn desugar_name_call_decl1() {
        let prog = r#"
            function f() {
                return 1;
            }
            var x = f();
            x;
        "#;

        okay(prog);
    }

    #[test]
    fn desugar_name_call_decl2() {
        let prog = r#"
            function f(arg) {
                return arg+1;
            }
            function g() {
                return 5;
            }
            var x = f(g());
            x;
        "#;

        okay(prog);
    }

    #[test]
    fn desugar_name_call_simpleupdate() {
        let prog = r#"
            function f() {
                return 1;
            }
            var x = 2;
            x = f();
            x;
        "#;

        okay(prog);
    }

    #[test]
    fn desugar_test_for_to_while() {
        let program =
            "var r=0;
            for (var i=0; i<10; ++i) {
                ++r;
            }
            r;";
        okay(program);
    }

    #[test]
    fn desugar_nested_loops() {
        let program = 
            "while (true) {
                while (false) {}
                break;
            }
            10;";
        okay(program);
    }

    #[test]
    fn desugar_continue_labeled_loop() {
        let program = 
            "var r=0;
            program_label: while (r === 0) {
                while (true) {
                    ++r;
                    continue program_label;
                }
            }
            r;";
        okay(program);
    }

    #[test]
    fn desugar_simple_while() {
        let program = 
            "var x = 0;
            while (x < 10) {
                ++x;
            }
            x;";
        okay(program);
    }

    #[test]
    fn desugar_stopify_for_labels() {
        let program = "
            var i = 0;
            l: for (var j = 0; j < 10; j++) {
                if (j % 2 === 0) {
                    i++;
                    do {
                        continue l;
                    } while (0);
                    i++;
                }
            }
            i;";
        okay(program);
    }

    #[test]
    fn desugar_stopify_continue_nested() {
        let program = "
            var i = 0;
            var j = 8;

            checkiandj: while (i < 4) {
                i += 1;

                checkj: while (j > 4) {
                    j -= 1;
                    if ((j % 2) === 0) {
                        i = 5;
                        continue checkiandj;
                    }
                }
            }
            // TODO(luna): ({i: i, j: j}) goes thru parser+pretty as {i: i,
            // j: j} (no parens) which isn't valid, not sure why
            i;";
        okay(program);
    }

    #[test]
    fn desugar_labeled_block_to_loop() {
        let program = "
            var x = true;
            while (true) {
                bogus: {
                    break;
                }
                // should never run
                var x = false;
                break;
            }
            x;";
        okay(program);
    }
    
    #[test]
    fn desugar_do_while_continue() {
        let program = "
            var x = 0;
            do {
                x += 1;
                if (x < 5)
                    continue;
                x *= 10;
            } while (x < 10);
            x";
        okay(program);
    }

    #[test]
    fn desugar_ops() {
        let program = "var x = true && false ? true || false : false; x";
        okay(program);
    }
    #[test]
    fn desugar_if_expr() {
        let program = "var x = true ? 1 : 2; x";
        okay(program);
    }
    #[test]
    fn desugar_seq() {
        okay(
            "var x = true;
            var r = true;
            while (x = false, x) {
                // shouldn't happen
                r = false;
                break;
            }
            r;"
        );
    }
}
