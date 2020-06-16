//! The types associated with the Jankyscript language.

#[derive(Debug, Clone)]
pub enum Type {
    Metavar(usize),
    Any,
    Int,
    Float,
    String_,
    Bool
    // TODO: others
}