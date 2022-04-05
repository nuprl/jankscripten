//! weird jankyscript / javascript coercions on sets of notwasm types, objects,
//! and more
//!
//! the purpose of this file has become a bit confused. ideally, i'd like a structure like this:
//!
//! - coercions.rs: all the *basic* coercions of jankyscript from each notwasm
//!   type to each notwasm type. this is currently mixed in with any_value.rs
//! - jnks_coercions.rs(?): more complex coercions on, ie, tuples of coercions,
//!   other javascript coercions like abstract equality and such (ie what's in
//!   here!)
//! - any_value.rs: just code for what any values are and printing them. even
//!   though this breaks tradition of "constructor for a type goes in <type>.rs"; i
//!   think we can all agree that constructing an any is very different. alternatively,
//!   constructors *into* any value, which are simple, could be included, but the
//!   coercions out that we have in there right now could move here

use crate::any_value::{AnyValue as Any, *};
use crate::heap_types::*;

pub fn i32s_or_as_f64s<T, F, I>(a: Any, b: Any, floats: F, ints: I) -> Option<T>
where
    F: FnOnce(f64, f64) -> T,
    I: FnOnce(i32, i32) -> T,
{
    match (*a, *b) {
        (AnyEnum::F64(a), AnyEnum::F64(b)) => Some(floats(unsafe { *a }, unsafe { *b })),
        (AnyEnum::I32(a), AnyEnum::F64(b)) => Some(floats(a as f64, unsafe { *b })),
        (AnyEnum::F64(a), AnyEnum::I32(b)) => Some(floats(unsafe { *a }, b as f64)),
        (AnyEnum::I32(a), AnyEnum::I32(b)) => Some(ints(a, b)),
        _ => None,
    }
}

pub fn i32s_or_as_f64s_any(
    a: Any,
    b: Any,
    floats: fn(f64, f64) -> f64,
    ints: fn(i32, i32) -> i32,
) -> Option<Any> {
    i32s_or_as_f64s(
        a,
        b,
        |a, b| f64_to_any(floats(a, b)),
        |a, b| any_from_i32(ints(a, b)),
    )
}

/// if a is *already* an object (no coercion), return Some(a as
/// Object). otherwise return None
pub fn match_object(a: AnyEnum) -> Option<ObjectPtr> {
    if let AnyEnum::Ptr(p) = a {
        if let HeapRefView::ObjectPtrPtr(obj) = p.view() {
            Some(obj)
        } else {
            None
        }
    } else {
        None
    }
}

/// adapted from https://ecma-international.org/ecma-262/5.1/#sec-11.9.3
pub fn abstract_eq(a: AnyEnum, b: AnyEnum) -> bool {
    // 1. same type
    // number == number
    if let Some(res) = i32s_or_as_f64s(a.into(), b.into(), |a, b| a == b, |a, b| a == b) {
        return res;
    }
    // rest
    match (a, b) {
        (AnyEnum::Bool(a), AnyEnum::Bool(b)) => return a == b,
        (AnyEnum::Ptr(a), AnyEnum::Ptr(b)) => match (a.view(), b.view()) {
            (HeapRefView::String(a), HeapRefView::String(b)) => return a == b,
            (HeapRefView::NonPtr32(_), _) | (_, HeapRefView::NonPtr32(_)) => {
                panic!("ref is not a value")
            }
            _ => todo!(),
        },
        // when fns become closures they might need to gain an impl PartialEq
        // which should only return true on pointer equality
        (AnyEnum::Closure(a), AnyEnum::Closure(b)) => return a == b,
        (AnyEnum::Undefined, AnyEnum::Undefined) => return true,
        (AnyEnum::Null, AnyEnum::Null) => return true,
        // not the same type. on to rule 2!
        _ => (),
    }
    // even rules:
    if let Some(res) = even_abstract_eq(a, b) {
        res
    // odd rules
    } else if let Some(res) = even_abstract_eq(b, a) {
        res
    } else {
        // 10
        false
    }
}
/// only implementing even numbered rules (not 10), then going to match on the reverse
fn even_abstract_eq(a: AnyEnum, b: AnyEnum) -> Option<bool> {
    Some(match (a, b) {
        // 2
        (AnyEnum::Null, AnyEnum::Undefined) => true,
        // 10, kinda
        (AnyEnum::Null, _) => return None,
        (AnyEnum::Undefined, _) => return None,
        // rules 4 and 8
        (a, AnyEnum::Ptr(p)) => match p.view() {
            // 4
            HeapRefView::String(s) => match a {
                AnyEnum::I32(i) => i as f64 == s.parse().unwrap_or(f64::NAN),
                AnyEnum::F64(f) => (unsafe { *f }) == s.parse().unwrap_or(f64::NAN),
                // presumably will be caught on the right side on the odd pass
                _ => return None,
            },
            // 8
            HeapRefView::NonPtr32(_) => log_panic!("ref is not a value"),
            // 8
            got => log_panic!("javascript spec ToPrimitive / DefaultValue {:?}", got),
        },
        // 6
        (AnyEnum::Bool(to_num), _) => abstract_eq(AnyEnum::I32(to_num as i32), b),
        // onto odd rules!
        _ => return None,
    })
}
