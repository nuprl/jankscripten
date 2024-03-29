use crate::heap_types::{EnvPtr, ObjectPtr};
use crate::{heap, AnyValue};

#[no_mangle]
pub unsafe extern "C" fn env_alloc(length: u32, fn_obj: ObjectPtr) -> EnvPtr {
    heap().alloc_env_or_gc(length, fn_obj)
}

/// this has! to return the EnvPtr because otherwise we'd need an intermediate
/// ID to properly generate init_at chains and it'd be a total pain in the AST!
#[no_mangle]
pub unsafe extern "C" fn env_init_at(mut env: EnvPtr, index: usize, item: AnyValue) -> EnvPtr {
    env.init_at(index, *item);
    env
}
