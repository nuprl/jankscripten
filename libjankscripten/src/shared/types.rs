//! The types associated with the JankyScript language.

// TODO(arjun): should be exactly the same as NotWasm types for a first pass.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Any,
    Float,
    Int,
    Bool,
    Function(Vec<Type>, Box<Type>),
    String,
    Array,
    DynObject,
    // TODO: others
}

impl Type {
    pub fn is_ground(&self) -> bool {
        match self {
            Type::Function(args, result_type) => {
                match **result_type {
                    Type::Any => {
                        for a in args.iter() {
                            if let Type::Any = a {
                                // pass
                            } else {
                                return false;
                            }
                        }
                        return true;
                    }
                    _ => false,
                }
            }
            _ => true,
        }
    }

    pub fn ground_function(n: usize) -> Type {
        Type::Function(vec![Type::Any; n], Box::new(Type::Any))
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Float => "i32",
                Type::Int => "f64",
                Type::String => "string",
                Type::Array => "array",
                Type::Bool => "bool",
                Type::DynObject => "DynObject",
                Type::Function(..) => "fn",
                Type::Any => "any",
            }
        )
    }
}
