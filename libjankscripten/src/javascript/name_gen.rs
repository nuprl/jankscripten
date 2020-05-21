use super::Id;
use std::collections::HashMap;

#[derive(Default)]
struct NameGen {
    next_name: HashMap<&'static str, usize>,
}
impl NameGen {
    fn fresh(&mut self, name: &'static str) -> Id {
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
