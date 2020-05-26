//! the [NameGen] struct for generating names

use super::Id;
use std::collections::HashMap;

/// a mutable reference to one global NameGen struct should be passed to
/// every compile step that requires creating fresh names (names that are part
/// of compiler-generated code)
///
/// the original NameGen should be created with [Default::default]:
///
/// ```
/// # use libjankscripten::javascript::*;
/// # let mut script = Stmt::Empty;
/// fn pass(stmt: &mut Stmt, ng: &mut NameGen) {} // ...
/// let mut ng = NameGen::default();
/// pass(&mut script, &mut ng);
/// ```
#[derive(Default)]
pub struct NameGen {
    next_name: HashMap<&'static str, usize>,
}
impl NameGen {
    /// fresh should be given a descriptive name of the role the name
    /// is playing
    ///
    /// ```
    /// # use libjankscripten::javascript::*;
    /// # let mut ng = NameGen::default();
    /// let break_name = ng.fresh("break");
    /// ```
    pub fn fresh(&mut self, name: &'static str) -> Id {
        Id::Generated(
            name,
            *self
                .next_name
                .entry(name)
                .and_modify(|i| *i += 1)
                .or_insert(0),
        )
    }
}

#[cfg(test)]
mod test {
    use super::{Id, NameGen};
    #[test]
    fn increases_sometimes() {
        let mut ng = NameGen::default();
        assert_eq!(ng.fresh("steven"), Id::Generated("steven", 0));
        assert_eq!(ng.fresh("steven"), Id::Generated("steven", 1));
        assert_eq!(ng.fresh("spinel"), Id::Generated("spinel", 0));
        assert_eq!(ng.fresh("steven"), Id::Generated("steven", 2));
    }
}
