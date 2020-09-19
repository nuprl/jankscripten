use super::{Heap, HeapPtr, Tag, DATA_OFFSET};
use std::marker::PhantomData;
use std::mem::size_of;

#[derive(Clone)]
pub struct FixedArrayPtr<T> {
    ptr: *mut Tag,
    _phantom: PhantomData<T>,
}
impl<T: Clone> Copy for FixedArrayPtr<T> {}

impl<T> FixedArrayPtr<T> {
    /// # Safety
    ///
    /// ptr should point to a valid tag, followed by a 4-byte length
    /// encoding in memory; it should be aligned, then followed
    /// by a [EnvItemMem] of strictly that length
    const unsafe fn new(ptr: *mut Tag) -> Self {
        Self {
            ptr,
            _phantom: PhantomData,
        }
    }
    fn len(&self) -> usize {
        // we specify little endian because we actually create arrays from
        // the compile for StringPtr (interning). now wasm is little endian
        // but it feels just a bit... like it should be specified just in case
        u32::from_le(unsafe { *(self.ptr.add(DATA_OFFSET) as *const u32) }) as usize
    }
}
impl<T> HeapPtr for FixedArrayPtr<T> {
    fn get_ptr(&self) -> *mut Tag {
        self.ptr
    }
    fn get_data_size(&self, _heap: &Heap) -> usize {
        self.len() * size_of::<T>() + 4
    }
}
