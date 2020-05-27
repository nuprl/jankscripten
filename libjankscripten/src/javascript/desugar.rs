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
    #[test]
    fn do_while() {
        desugar_okay(
            "var r = 0;
            do {
                r += 1;
            } while (r < 10);
            r;",
            desugar,
        );
    }
}
