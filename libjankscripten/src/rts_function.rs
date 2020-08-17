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
    Typeof,
    Delete,
    Plus,
    Over,
    Mod,
    ModF64,
    Equal,
    LogAny,
}

impl RTSFunction {
    /// The name of a function in the runtime system. This is the name that the runtime  exports,
    /// using `[no_mangle]`.
    pub fn name(&self) -> &'static str {
        match self {
            RTSFunction::Todo(name) => todo!("unimplemented operator: {}", name),
            RTSFunction::Typeof => "janky_typeof",
            RTSFunction::Delete => "janky_delete",
            RTSFunction::Plus => "janky_plus",
            RTSFunction::Over => "janky_over",
            RTSFunction::Mod => "janky_mod",
            RTSFunction::ModF64 => "janky_mod_f64",
            RTSFunction::Equal => "janky_equal",
            RTSFunction::LogAny => "log_any",
        }
    }

    /// The type of the function, expressed as a JankyScript type. The compiler translates these
    /// types to NotWasm types.
    ///
    /// Some gotchas to keep in mind:
    ///
    /// JavaScript's `delete` operator makes no sense. Behold:
    ///
    /// ```ignore
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
        match self {
            RTSFunction::Todo(name) => todo!("unimplemented operator: {}", name),
            RTSFunction::Typeof => Function(vec![Any], Box::new(String)),
            RTSFunction::Delete => Function(vec![Any, Any], Box::new(Bool)),
            RTSFunction::Plus => Function(vec![Any, Any], Box::new(Any)),
            RTSFunction::Over => Function(vec![Any, Any], Box::new(Float)),
            RTSFunction::Mod => Function(vec![Any, Any], Box::new(Any)),
            RTSFunction::ModF64 => Function(vec![Float, Float], Box::new(Float)),
            RTSFunction::Equal => Function(vec![Any, Any], Box::new(Bool)),
            RTSFunction::LogAny => Function(vec![Any], Box::new(Any)),
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
                Typeof => "typeof",
                Delete => "delete",
                Plus => "+",
                Over => "over",
                Mod => "%",
                ModF64 => "%64",
                Equal => "==",
                LogAny => "logany",
            }
        )
    }
}
