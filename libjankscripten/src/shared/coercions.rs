use super::types::Type;

#[derive(Debug, PartialEq, Clone)]
/// Coercion : S -> T
///
/// Every coercion turns into a call to a function in the runtime system.
pub enum Coercion {
    /// Coercion::Tag(t) : t -> Any
    /// Note that not all values may be injected in Any.
    Tag(Type),
    /// Coercion::Untag(t) : Any -> t
    /// Note that untagging will fail if the Any-typed value contains an element
    /// that does not have the type t.
    Untag(Type),
    /// Coercion::IntToFloat : Int -> Float
    /// Corresponds to f64.convert_i32_s
    IntToFloat,
    /// Coercion::FloatToInt : Float -> Int
    /// Corresponds to i32.trunc_f64_s
    FloatToInt,
    /// Coercion::Fun(args, ret)
    ///
    /// Assume exactly one argument:
    ///
    /// Coercion::Fun([arg], ret) where arg : S -> S' and ret : T -> T'
    /// Coercion::Fun([arg], ret) : (S' -> T) -> (S -> T')
    Fun(Vec<Coercion>, Box<Coercion>),
    // Coercion::Id(t) : t -> t
    Id(Type),
    /// Coercion::Seq(t1, t2) where t1 : S -> U and t2 : U -> T
    /// has the type S -> Ts
    Seq(Box<Coercion>, Box<Coercion>),
    Meta(Type, Type),
}

impl Coercion {
    /// "Smart constructor" that eliminates unnecessary identity coercions in
    /// a sequence.
    pub fn seq(c1: Coercion, c2: Coercion) -> Coercion {
        match (c1, c2) {
            (c1, Coercion::Id(_)) => c1,
            (Coercion::Id(_), c2) => c2,
            (Coercion::Fun(args2, ret2), Coercion::Fun(args1, ret1)) => Coercion::Fun(
                args1
                    .into_iter()
                    .zip(args2)
                    .map(|(a1, a2)| Coercion::seq(a1, a2))
                    .collect(),
                Box::new(Coercion::seq(*ret2, *ret1)),
            ),
            (c1, c2) => Coercion::Seq(Box::new(c1), Box::new(c2)),
        }
    }

    pub fn fun(cargs: Vec<Coercion>, cret: Coercion) -> Coercion {
        // TODO(michael) is there a way to avoid the cloning?
        if let Coercion::Id(ret) = &cret {
            if let Some(args) = &cargs
                .iter()
                .map(|c| match c {
                    Coercion::Id(t) => Some(t.clone()),
                    _ => None,
                })
                .collect::<Option<Vec<_>>>()
            {
                return Coercion::Id(Type::Function(args.clone(), Box::new(ret.clone())));
            }
        }

        Coercion::Fun(cargs, Box::new(cret))
    }
}

pub fn cseq_(c1: Coercion, c2: Coercion) -> Coercion {
    Coercion::Seq(Box::new(c1), Box::new(c2))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn fun_all_id_is_id() {
        assert_eq!(
            Coercion::fun(vec![], Coercion::Id(Type::Bool)),
            Coercion::Id(Type::Function(vec![], Box::new(Type::Bool)))
        );

        let args = vec![Type::Bool, Type::Float];
        assert_eq!(
            Coercion::fun(
                args.iter().map(|t| Coercion::Id(t.clone())).collect(),
                Coercion::Id(Type::Bool)
            ),
            Coercion::Id(Type::Function(args, Box::new(Type::Bool)))
        );

        let args = vec![Coercion::Id(Type::Bool), Coercion::IntToFloat];
        assert_eq!(
            Coercion::fun(args.clone(), Coercion::Id(Type::Int)),
            Coercion::Fun(args, Box::new(Coercion::Id(Type::Int)))
        );

        let args = vec![Coercion::Id(Type::Bool), Coercion::Id(Type::Float)];
        assert_eq!(
            Coercion::fun(args.clone(), Coercion::FloatToInt),
            Coercion::Fun(args, Box::new(Coercion::FloatToInt))
        );
    }
}
