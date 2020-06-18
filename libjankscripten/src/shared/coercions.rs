use super::types::Type;

#[derive(Debug)]
pub enum Coercion {
    Tag(Type),
    Untag(Type),
    Id(Type),
    Seq(Box<Coercion>, Box<Coercion>)
}

pub fn cseq_(c1: Coercion, c2: Coercion) -> Coercion {
    Coercion::Seq(Box::new(c1), Box::new(c2))
}