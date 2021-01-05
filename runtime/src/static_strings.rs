//! Init-interned strings to reference
//!
//! if you would like an interned string, add it to the struct definition and
//! the appropriate spot in init() (make sure it's less than 32 bytes)
//!
//! Because rust doesn't inline the exact same way notwasm does,
//! rather than allocate the string "__proto__" / "prototype" / etc whenever we
//! need them, we allocate and initialize them all once, during init (doing it at
//! compile time would be ideal, but rust's const is not yet useful enough to make
//! this convenient)

use crate::allocator::{Tag, TypeTag};
use crate::heap_types::StringPtr;

static mut STATIC_STRINGS: Option<StaticStrings> = None;

#[derive(Clone, Copy)]
pub struct StaticStrings {
    pub __proto__: StringPtr,
    pub prototype: StringPtr,
}

pub fn init() {
    unsafe {
        STATIC_STRINGS = Some(StaticStrings {
            __proto__: mkstr32("__proto__"),
            prototype: mkstr32("prototype"),
        });
    }
}

pub fn static_strings() -> &'static StaticStrings {
    unsafe { &STATIC_STRINGS }.as_ref().unwrap()
}

/// a string of at-most 32 bytes
#[allow(dead_code)]
#[derive(Debug)]
#[repr(C)]
struct Str32 {
    tag: Tag,
    len: u32,
    data: [u8; 32],
}

/// alternatives:
///
/// 1. actually use the heap allocation. disadvantage: have to jury-rig garbage
///    collection to not garbage collect them
/// 2. use a vec. disadvantage: annoying to build
fn mkstr32(s: &'static str) -> StringPtr {
    let mut short_str = Str32 {
        tag: Tag::with_type(TypeTag::String),
        len: u32::to_le(s.len() as u32),
        data: [0; 32],
    };
    unsafe {
        std::ptr::copy_nonoverlapping(s.as_ptr(), &mut short_str.data as *mut _ as *mut _, s.len());
        let short_str_leaked = Box::leak(Box::new(short_str)) as *mut _ as *mut Tag;
        StringPtr::new(short_str_leaked)
    }
}
