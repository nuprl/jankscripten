use super::*;

pub fn desugar(stmt: &mut Stmt, ng: &mut NameGen) {
    // rdep: do..while uses ||
    desugar_loops::desugar_loops(stmt, ng);
    // dep: desugar_logical needs loop conds to be simple
    desugar_logical::desugar_logical(stmt, ng);
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::javascript::testing::desugar_okay;
    fn okay(script: &str) {
        desugar_okay(script, desugar);
    }
    #[test]
    fn do_while() {
        okay(
            "var r = 0;
            do {
                r += 1;
            } while (r < 10);
            r;",
        );
    }
    #[test]
    fn for_loops() {
        okay(
            "var r;
            for (r=0; r<10; ++r) {
                r += 1;
            }
            r;",
        );
    }
    #[test]
    fn labeled_continue() {
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
}
