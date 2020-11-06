use super::{Heap, HeapPtr, ObjectPtr, Tag};
use crate::AnyEnum;

/// this is a heap-allocated environment stored in a closure
///
/// as a first pass i've decided to describe environments as an array of
/// anys. we can definitely optimize this!
///
/// 1. we could avoid boxing floats by using an Any+Float enum (this would
///    ~double env sizes)
/// 2. we could lead with all the non ptr values in one clump to be skipped
///    by GC
/// 3. we could separate any, ptr, and closure, into separate arrays that
///    could probably be more efficiently traversed because pipelining and
///    all that (each size is known before reading/marking it). this might
///    only be a gain for larger environments
/// 4. one thing we don't have to worry about is the tags slowing static
///    retrievals down. i feel fairly confident we can do hella math in
///    the compiler and turn EnvGet into (local.get 0, typ.load STATIC_OFFSET)
///
/// Tag | u32 | Object | [EnvItem]
///        ^      ^^
///       len   fn_obj
///
/// WARNING: breaking tradition with other pointer objects, this object does
/// not handle 64-bit and 32-bit architectures differently. the tag is always
/// treated as if it is 4 bytes long
#[derive(Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct EnvPtr {
    ptr: *mut Tag,
}

const LEN_OFFSET: usize = 1;
const FN_OBJ_OFFSET: usize = 2;
const ENV_ITEM_OFFSET: usize = 3;

impl EnvPtr {
    /// # Safety
    ///
    /// ptr should point to an aligned Env tag; there should be 4 +
    /// sizeof(EnvItem) * length bytes allocated after the tag!
    /// the array does not have to be initialized, however until it is,
    /// garbage collection is unsound
    pub unsafe fn new(ptr: *mut Tag) -> Self {
        Self { ptr }
    }
    /// # Safety
    ///
    /// nothing should ever happen to the environment, period. it can't be
    /// stored in a structure. it can't be dereferenced, it can't be
    /// initialized, sliced, nothing
    pub unsafe fn null() -> Self {
        Self {
            ptr: std::ptr::null_mut(),
        }
    }
    /// # Safety
    ///
    /// ptr should point to an aligned Env tag; there should be 4 +
    /// sizeof(EnvItem) * length bytes allocated after the tag!
    /// the array does not have to be initialized, however until it is,
    /// garbage collection is unsound
    ///
    /// # other considerations
    ///
    /// if you need a zero-length environment, you should use nullptr
    pub unsafe fn init(ptr: *mut Tag, length: u32, fn_obj: ObjectPtr) -> Self {
        (ptr.add(LEN_OFFSET) as *mut u32).write(length);
        (ptr.add(FN_OBJ_OFFSET) as *mut ObjectPtr).write(fn_obj);
        Self { ptr }
    }

    pub fn len(&self) -> usize {
        // SAFETY: the length was added in new, as long as it hasn't been
        // overwritten by UB, it's still there
        unsafe { *(self.ptr.add(LEN_OFFSET) as *const u32) as usize }
    }

    pub fn fn_obj(&self) -> ObjectPtr {
        // SAFETY: the fn obj was added in new, as long as it hasn't been
        // overwritten by UB, it's still there
        unsafe { *(self.ptr.add(FN_OBJ_OFFSET) as *const ObjectPtr) }
    }

    /// initialize the index field of the pointer. this must be called on
    /// every index from 0 to length - 1 before garbage collection is sound
    ///
    /// # Safety
    ///
    /// index must not exceed the length the environment was initialized with
    pub unsafe fn init_at(&mut self, index: usize, item: EnvItem) {
        // this cannot use slice because assigning to a &mut calls drop;
        // we want to initialize for the first time using ptr.write
        self.slice_ptr().add(index).write(item);
    }

    /// # Safety
    ///
    /// must have called [init_at] for every item in length
    unsafe fn slice(&self) -> &[EnvItem] {
        std::slice::from_raw_parts(self.slice_ptr(), self.len())
    }
    /// the slice may point to garbage EnvItems
    fn slice_ptr(&self) -> *mut EnvItem {
        // SAFETY: as long as new was called correctly, this is a valid
        // pointer with possibly garbage data. it MAY point one byte past
        // the heap if length is zero, but that is actually defined behavior
        unsafe { self.ptr.add(ENV_ITEM_OFFSET) as *mut EnvItem }
    }
}
impl HeapPtr for EnvPtr {
    fn get_ptr(&self) -> *mut Tag {
        self.ptr
    }
    fn get_data_size(&self, _heap: &Heap) -> usize {
        std::mem::size_of::<EnvItem>() * self.len() + 4 + std::mem::size_of::<ObjectPtr>()
    }
    /// # Safety
    ///
    /// **THIS IS UNSAFE**!!! it can't be tagged unsafe because it's a trait,
    /// but, until [init_at] has been called for every item, it is unsound
    fn get_gc_ptrs(&self, _: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        // 1. Collect pointers for closed over values
        // SAFETY: this actually isn't safe!!!
        let (mut tags, f64s) = AnyEnum::iter_to_ptrs(unsafe { self.slice().iter() });

        // 2. Collect pointer for this closure's function object
        tags.push(self.fn_obj().get_ptr());

        // 3. return the ptrs
        (tags, f64s)
    }
}

impl std::fmt::Debug for EnvPtr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{{ env: {:?}, obj: {:?} }}",
            unsafe { self.slice().iter().collect::<Vec<_>>() },
            self.fn_obj()
        )
    }
}

type EnvItem = AnyEnum;

#[cfg(test)]
mod test {
    use super::*;
    use crate::env::*;
    use crate::heap;
    use crate::init;
    use crate::object::object_empty;
    use crate::AnyEnum;
    use wasm_bindgen_test::wasm_bindgen_test;

    #[wasm_bindgen_test]
    fn env_fn_obj() {
        init();

        let fn_obj = object_empty();
        let env = unsafe {
            // Expr::Closure
            let env = env_alloc(3, fn_obj);
            env_init_at(env, 0, AnyEnum::I32(5).into());
            env_init_at(env, 1, AnyEnum::I32(6).into());
            env_init_at(env, 2, AnyEnum::I32(7).into())
        };

        let mut got_fn_obj: ObjectPtr = env.fn_obj();
        assert_eq!(fn_obj, got_fn_obj);
        got_fn_obj.insert(heap(), "x".into(), AnyEnum::I32(10).into(), &mut -1);
        assert_eq!(
            got_fn_obj.get(heap(), "x".into(), &mut -1),
            AnyEnum::I32(10)
        );

        let env_items = unsafe { env.slice() };
        assert_eq!(env_items[0], AnyEnum::I32(5));
        assert_eq!(env_items[1], AnyEnum::I32(6));
        assert_eq!(env_items[2], AnyEnum::I32(7));
    }
}
