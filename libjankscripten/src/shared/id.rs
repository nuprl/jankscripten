#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Hash)]
pub enum Id {
    Named(String),
    Generated(&'static str, usize),
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
}

impl<T: Into<String>> From<T> for Id {
    fn from(i: T) -> Self {
        Id::Named(i.into())
    }
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Id::Named(name) => write!(f, "{}", name),
            Id::Generated(name, size) => write!(f, "{}-{}", name, size),
        }
    }
}
