use super::types::Type;

#[derive(Debug)]
pub enum Coercion {
    Tag(Type),
    Untag(Type),
    Id(Type),
    Seq(Box<Coercion>, Box<Coercion>)
    // TODO: fun_n
}