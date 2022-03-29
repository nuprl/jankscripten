//! Names and types of functions that the jankscripten runtime system exposes.
//!
//! We use the `RTSFunction` enumeration to refer to a jankscripten runtime function in both
//! JankyScript and NotWasm. When introducing a new function in the runtime system, add a case to
//! `RTSFunction`, add fill in its name and type in the '.name' and '.janky_typ' methods, which
//! are all defined in this file.

use super::jankyscript::syntax::Type::{self, *};
use strum_macros::EnumIter;

#[derive(Debug, Clone, PartialEq, EnumIter, Eq, Hash)]
pub enum RTSFunction {
    Todo(&'static str),
    // Type-specialized methods. They are always implemented in rust, with a
    // name given algorithmically based on the type and method name. The full type is
    // also provided, because it's generated in the methods table
    Method(&'static str, Type),
    // unary ops
    Typeof,
    Delete,
    Void,
    Neg,
    // janky binops
    Plus,
    Minus,
    Times,
    Over,
    Mod,
    ModF64,
    StrictEqual,
    Equal,
    StrictNotEqual,
    NotEqual,
    InstanceOf,
    In,
    BitwiseNot,
    Import(std::string::String),
    StringCat,
}

// The name of a runtime function implementation.
pub enum RTSFunctionImpl {
    /// A runtime function implemented in Rust.
    Rust(std::string::String),
    /// A runtime function implemented in NotWasm.
    NotWasm(&'static str),
}

impl RTSFunction {
    /// The name of a function in the runtime system. This function can be
    /// implemented in either the Rust runtime or the NotWasm runtime.
    /// If it's implemented in Rust, make sure to prefix the function
    /// implementation with `[no_mangle]`.
    pub fn name(&self) -> RTSFunctionImpl {
        use RTSFunction::*;
        use RTSFunctionImpl::*;
        match self {
            Todo(name) => todo!("unimplemented operator: {}", name),
            Method(..) => Rust(self.to_string()),
            Typeof => Rust("janky_typeof".into()),
            Delete => Rust("janky_delete".into()),
            Void => Rust("janky_void".into()),
            Neg => Rust("janky_neg".into()),
            Plus => NotWasm("jnks_plus".into()), // Implemented in NotWasm RT
            Minus => Rust("janky_minus".into()),
            Times => Rust("janky_times".into()),
            Over => Rust("janky_over".into()),
            Mod => Rust("janky_mod".into()),
            ModF64 => Rust("janky_mod_f64".into()),
            StrictEqual => Rust("janky_strict_equal".into()),
            Equal => Rust("janky_equal".into()),
            StrictNotEqual => Rust("janky_strict_not_equal".into()),
            NotEqual => Rust("janky_not_equal".into()),
            InstanceOf => Rust("instance_of".into()),
            In => Rust("janky_in".into()),
            BitwiseNot => Rust("janky_not".into()),
            Import(name) => Rust(name.clone()),
            StringCat => Rust("string_append".into()),
        }
    }

    /// The type of the function, expressed as a JankyScript type. The compiler translates these
    /// types to NotWasm types.
    ///
    /// Some gotchas to keep in mind:
    ///
    /// JavaScript's `delete` operator makes no sense. Behold:
    ///
    /// ```javascript-console
    /// > let o = { x: 3 };
    /// undefined
    /// > delete o.x
    /// true
    /// > delete o.y
    /// true
    /// > delete 23
    /// true
    /// ```
    ///
    /// It is remarkable that delete is not a binary operator. Also, why does it return `true`?
    /// It returns false when the "property is an own non-configurable property, in which case,
    /// false is returned in non-strict mode."
    ///
    /// That sentence makes me want to fall asleep.
    pub fn janky_typ(&self) -> Type {
        use RTSFunction::*;
        match self {
            Todo(name) => todo!("unimplemented operator: {}", name),
            Method(_, ty) => ty.clone(),
            Typeof => Function(vec![Any], Box::new(String)),
            // the second operand of InstanceOf is really "a function" but we don't have a type for that
            Delete | InstanceOf => Function(vec![Any, Any], Box::new(Bool)),
            Void => Function(vec![Any], Box::new(Any)),
            Neg => Function(vec![Any], Box::new(Any)),
            Plus | Minus | Times | Mod => Function(vec![Any, Any], Box::new(Any)),
            Over => Function(vec![Any, Any], Box::new(Float)),
            ModF64 => Function(vec![Float, Float], Box::new(Float)),
            StrictEqual | Equal | StrictNotEqual | NotEqual | In => {
                Function(vec![Any, Any], Box::new(Bool))
            }
            BitwiseNot => Function(vec![Int], Box::new(Int)),
            Import(..) => panic!("unimplemented function: {}", self),
            StringCat => Function(vec![String, String], Box::new(String)),
        }
    }
}

impl std::fmt::Display for RTSFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RTSFunction::*;
        write!(
            f,
            "{}",
            match self {
                Todo(s) => s,
                Method(name, ty) => {
                    return write!(f, "{}_{}", ty.unwrap_fun().0[0], name);
                }
                Typeof => "typeof",
                Delete => "delete",
                Void => "void",
                Plus => "+",
                Minus => "-",
                Neg => "-",
                Times => "*",
                Over => "/",
                Mod => "%",
                ModF64 => "%.",
                StrictEqual => "===",
                Equal => "==",
                StrictNotEqual => "!==",
                NotEqual => "!=",
                InstanceOf => "instanceof",
                In => "in",
                BitwiseNot => "~",
                Import(_s) => "import",
                StringCat => "..",
            }
        )
    }
}
