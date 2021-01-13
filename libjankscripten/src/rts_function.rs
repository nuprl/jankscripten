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
    InstanceOf,
    In,
}

// The name of a runtime function implementation.
pub enum RTSFunctionImpl {
    /// A runtime function implemented in Rust.
    Rust(&'static str),
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
            LogAny => Rust("log_any"),
            Typeof => Rust("janky_typeof"),
            Delete => Rust("janky_delete"),
            Void => Rust("janky_void"),
            Plus => NotWasm("jnks_plus"), // Implemented in NotWasm RT
            Minus => Rust("janky_minus"),
            Times => Rust("janky_times"),
            Over => Rust("janky_over"),
            Mod => Rust("janky_mod"),
            ModF64 => Rust("janky_mod_f64"),
            StrictEqual => Rust("janky_strict_equal"),
            Equal => Rust("janky_equal"),
            StrictNotEqual => Rust("janky_strict_not_equal"),
            NotEqual => Rust("janky_not_equal"),
            InstanceOf => Rust("instance_of"),
            In => Rust("janky_in"),
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
            // the second operand of InstanceOf is really "any closure" but we don't have a type for that
            Delete | InstanceOf => Function(vec![Any, Any], Box::new(Bool)),
            Void => Function(vec![Any], Box::new(Any)),
            Plus | Minus | Times | Mod => Function(vec![Any, Any], Box::new(Any)),
            Over => Function(vec![Any, Any], Box::new(Float)),
            ModF64 => Function(vec![Float, Float], Box::new(Float)),
            StrictEqual | Equal | StrictNotEqual | NotEqual | In => {
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
                InstanceOf => "instanceof",
                In => "in",
            }
        )
    }
}
