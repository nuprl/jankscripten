//! Names and types of functions that the jankscripten runtime system exposes.
//!
//! We use the `RTSFunction` enumeration to refer to a jankscripten runtime function in both
//! JankyScript and NotWasm. When introducing a new function in the runtime system, add a case to
//! `RTSFunction`, add fill in its name and type in the '.name' and '.janky_typ' methods, which
//! are all defined in this file.

use super::jankyscript::syntax::Type::{self, *};
use strum_macros::EnumIter;

#[derive(Debug, Clone, Copy, PartialEq, EnumIter)]
pub enum RTSFunction {
    Todo(&'static str),
    // ???
    LogAny,
    // unary ops
    Typeof,
    Delete,
    Void,
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
}

impl RTSFunction {
    /// The name of a function in the runtime system. This is the name that the runtime  exports,
    /// using `[no_mangle]`.
    pub fn name(&self) -> &'static str {
        use RTSFunction::*;
        match self {
            Todo(name) => todo!("unimplemented operator: {}", name),
            LogAny => "log_any",
            Typeof => "janky_typeof",
            Delete => "janky_delete",
            Void => "janky_void",
            Plus => "janky_plus",
            Minus => "janky_minus",
            Times => "janky_times",
            Over => "janky_over",
            Mod => "janky_mod",
            ModF64 => "janky_mod_f64",
            StrictEqual => "janky_strict_equal",
            Equal => "janky_equal",
            StrictNotEqual => "janky_strict_not_equal",
            NotEqual => "janky_not_equal",
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
            LogAny => Function(vec![Any, Any], Box::new(Any)),
            Typeof => Function(vec![Any], Box::new(String)),
            Delete => Function(vec![Any, Any], Box::new(Bool)),
            Void => Function(vec![Any], Box::new(Any)),
            Plus | Minus | Times | Mod => Function(vec![Any, Any], Box::new(Any)),
            Over => Function(vec![Any, Any], Box::new(Float)),
            ModF64 => Function(vec![Float, Float], Box::new(Float)),
            StrictEqual | Equal | StrictNotEqual | NotEqual => {
                Function(vec![Any, Any], Box::new(Bool))
            }
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
                LogAny => "logany",
                Typeof => "typeof",
                Delete => "delete",
                Void => "void",
                Plus => "+",
                Minus => "-",
                Times => "*",
                Over => "/",
                Mod => "%",
                ModF64 => "%.",
                StrictEqual => "===",
                Equal => "==",
                StrictNotEqual => "!==",
                NotEqual => "!=",
            }
        )
    }
}
