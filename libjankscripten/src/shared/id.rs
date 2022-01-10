use derive_more::Display;
/// Identifiers and a fresh name generator
use std::collections::HashMap;

/// Identifiers
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Display)]
pub enum Id {
    #[display(fmt = "{}", _0)]
    Named(String),
    #[display(fmt = "{}", _0)]
    Generated(Generated),
    /// Bogus is a bit of a hack to work with the Rust type system. There are situations where it is
    /// convenient to move identifiers out of collections that are then consumed. However, they must
    /// be replaced with  another identifier. Instead of using `Option<Id>`, which is very
    /// irritating, we replace the identifier with `Bogus`. So, don't construct a `Bogus` unless you
    /// are sure you don't need it.
    #[display(fmt = "bogus({})", _0)]
    Bogus(&'static str),
}

/// The fields are not public, so the only way to build this is with a `NameGen`.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash, Display)]
#[display(fmt = "$jnks_{}_{}", base_name, index)]
pub struct Generated {
    base_name: &'static str,
    index: usize,
}

/// A fresh name generator that should be passed to
/// every stage of the compiler that needs fresh names.
///
/// Create the name generator as follows:
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

impl Id {
    // TODO(arjun): We should eliminate this method. It is only used in the parser, and a cleaner
    // approach would be to have a parser combinator that returns a String instead of an Id.
    pub fn into_name(self) -> String {
        match self {
            Id::Named(s) => s,
            _ => panic!("into_name on Id::Generated"),
        }
    }
    pub fn name(&self) -> &str {
        match self {
            Id::Named(s) => s,
            _ => panic!("name on Id::Generated"),
        }
    }
}

impl<T: Into<String>> From<T> for Id {
    fn from(i: T) -> Self {
        Id::Named(i.into())
    }
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
        Id::Generated(Generated {
            base_name: name,
            index: *self
                .next_name
                .entry(name)
                .and_modify(|i| *i += 1)
                .or_insert(0),
        })
    }
}
