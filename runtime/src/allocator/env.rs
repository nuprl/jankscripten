use super::constants::DATA_OFFSET;
use super::Heap;
use crate::{AnyEnum, AnyPtr};

/// this is a heap-allocated environment stored in a closure
///
/// as a first pass i've decided to describe environments as an array of
/// enums. we can definitely optimize this!
///
/// 1. we could reduce their size by not padding values (right now it's 128
///    per item because tag + up to 64 bits of data)
/// 2. we could lead with all the non ptr values in one clump to be skipped
///    by GC
/// 3. we could separate any, ptr, and closure, into separate arrays that
///    could probably be more efficiently traversed because pipelining and
///    all that (each size is known before reading/marking it). this might
///    only be a gain for larger environments
///
/// Tag | EnvMem
pub type EnvPtr = FixedArrayPtr<EnvItemMem>;

/// this is a datatype that should never be constructed except by the
/// allocator!!!
///
/// it is used to describe a the environment data that's stored in the heap
/// and should only be manipulated by EnvPtr!!!
enum EnvItemMem {
    /// i32, bool
    NonPtr32(i32),
    /// this is just f64 i think
    NonPtr64(i64),
    /// ref, array, etc
    Ptr(AnyPtr),
    Any(AnyEnum),
    Closure(Closure),
}

#[test]
fn size() {
    println!("{}", std::mem::size_of::<EnvItem>());
}
