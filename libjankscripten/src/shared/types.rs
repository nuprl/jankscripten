//! The types associated with the JankyScript language.

use crate::notwasm::syntax::{FnType, Type as NotWasmType};

// TODO(arjun): should be exactly the same as NotWasm types for a first pass.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Missing,
    Any,
    Float,
    Int,
    Bool,
    Function(Vec<Type>, Box<Type>),
    String,
    Array,
    DynObject,
    /// Ref(T) is the type of heap-allocated boxes that contain values of type
    /// T.
    Ref(Box<Type>),
    Metavar(usize),
}

impl Type {
    pub fn take(&mut self) -> Type {
        std::mem::replace(self, Type::Missing)
    }

    pub fn unwrap_fun(&self) -> (&Vec<Type>, &Type) {
        match self {
            Type::Function(args, ret) => (args, ret),
            _ => panic!("unwrap_fun expects a function"),
        }
    }

    pub fn notwasm_typ(&self, allow_closure: bool) -> NotWasmType {
        match self {
            Type::Missing => panic!("received Type::Missing"),
            Type::Any => NotWasmType::Any,
            Type::Float => NotWasmType::F64,
            Type::Int => NotWasmType::I32,
            Type::Bool => NotWasmType::Bool,
            Type::Function(arg_typs, ret_ty) => {
                let result = Some(Box::new(ret_ty.notwasm_typ(allow_closure)));
                if allow_closure {
                    let args = std::iter::once(NotWasmType::Env)
                        .chain(arg_typs.iter().map(|t| t.notwasm_typ(allow_closure)))
                        .collect();
                    NotWasmType::Closure(FnType { args, result })
                } else {
                    let args = arg_typs
                        .iter()
                        .map(|t| t.notwasm_typ(allow_closure))
                        .collect();
                    NotWasmType::Fn(FnType { args, result })
                }
            }
            Type::String => NotWasmType::String,
            Type::Array => NotWasmType::Array,
            Type::DynObject => NotWasmType::DynObject,
            Type::Ref(of) => NotWasmType::Ref(Box::new(of.notwasm_typ(allow_closure))),
            Type::Metavar(_) => panic!("Metavar received"),
        }
    }

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

impl Default for Type {
    /// The default type is `Type::Missing`.
    fn default() -> Self {
        Type::Missing
    }
}

// TODO(arjun): Refactor to use derive display
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Missing => "_",
                Type::Float => "i32",
                Type::Int => "f64",
                Type::String => "string",
                Type::Array => "array",
                Type::Bool => "bool",
                Type::DynObject => "DynObject",
                Type::Function(..) => "fn",
                Type::Any => "any",
                Type::Ref(..) => "ref",
                Type::Metavar(..) => "metavar",
            }
        )
    }
}
