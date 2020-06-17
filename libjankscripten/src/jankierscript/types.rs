//! The types associated with the Jankyscript language.

#[derive(Debug, Clone, PartialEq)]
pub enum GroundType {
    Float,
    Bool,
    Function(usize),
    Any
    // TODO: others
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Ground(GroundType),
    Function(Vec<Type>, Box<Type>)
    // TODO: others
}