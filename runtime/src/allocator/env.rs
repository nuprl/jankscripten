use super::constants::DATA_OFFSET;
use super::{Heap, HeapPtr, Tag};
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
/// Tag | u32 | [EnvItem]
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct EnvPtr {
    ptr: *mut Tag,
}

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
    /// ptr should point to an aligned Env tag; there should be 4 +
    /// sizeof(EnvItem) * length bytes allocated after the tag!
    /// the array does not have to be initialized, however until it is,
    /// garbage collection is unsound
    ///
    /// # other considerations
    ///
    /// if you need a zero-length environment, you should use nullptr
    pub unsafe fn init(ptr: *mut Tag, length: u32) -> Self {
        (ptr.add(DATA_OFFSET) as *mut u32).write(length);
        Self { ptr }
    }
    pub fn len(&self) -> usize {
        // SAFETY: the length was added in new, as long as it hasn't been
        // overwritten by UB, it's still there
        unsafe { *(self.ptr.add(DATA_OFFSET) as *const u32) as usize }
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
        unsafe { self.ptr.add(DATA_OFFSET + 1) as *mut EnvItem }
    }
}
impl HeapPtr for EnvPtr {
    fn get_ptr(&self) -> *mut Tag {
        self.ptr
    }
    fn get_data_size(&self, _heap: &Heap) -> usize {
        std::mem::size_of::<EnvItem>() * self.len() + 4
    }
    /// # Safety
    ///
    /// **THIS IS UNSAFE**!!! it can't be tagged unsafe because it's a trait,
    /// but, until [init_at] has been called for every item, it is unsound
    fn get_gc_ptrs(&self, _: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        // SAFETY: this actually isn't safe!!!
        AnyEnum::iter_to_ptrs(unsafe { self.slice().iter() })
    }
}

type EnvItem = AnyEnum;
