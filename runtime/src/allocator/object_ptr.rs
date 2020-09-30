use super::constants::DATA_OFFSET;
use super::heap_values::*;
use super::{Heap, ALIGNMENT};
use crate::heap_types::StringPtr;
use crate::{AnyEnum, AnyValue};
use std::ops::{Deref, DerefMut};

/// A managed pointer to a `ObjectDataPtr`. this level of indirection is
/// needed to update objects when reallocated
///
/// This looks like this:
/// Tag(1) | pointer to ObjectDataPtr(1)
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(transparent)]
pub struct ObjectPtr {
    ptr: *mut Tag,
}

/// A managed pointer to an Object, specified by a Class
///
/// It looks like this:
/// Tag(4/8) | field(12/16) | field(12/16) | ...
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(transparent)]
pub struct ObjectDataPtr {
    ptr: *mut Tag,
}

impl HeapPtr for ObjectDataPtr {
    fn get_ptr(&self) -> *mut Tag {
        return self.ptr;
    }

    fn get_data_size(&self, heap: &Heap) -> usize {
        heap.object_data_size(heap.get_class_size(self.class_tag()))
    }

    fn get_gc_ptrs(&self, heap: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        // TODO(luna): we should remove the option anyway
        AnyEnum::iter_to_ptrs(
            self.as_array(heap)
                .iter()
                .map(|x| x.as_ref().unwrap_or(&AnyEnum::Undefined)),
        )
    }
}

impl ObjectDataPtr {
    /// This function is unsafe, because (1) we do not check that the class_tag
    /// is valid, and (2) we assume that `ptr` is valid.
    pub unsafe fn new(ptr: *mut Tag) -> Self {
        assert_eq!((*ptr).type_tag, TypeTag::DynObject);
        ObjectDataPtr { ptr }
    }

    pub fn class_tag(&self) -> u16 {
        let tag = unsafe { *self.ptr };
        debug_assert_eq!(tag.type_tag, TypeTag::DynObject);
        tag.class_tag
    }

    pub fn read_at(&self, heap: &Heap, index: usize) -> Option<AnyEnum> {
        debug_assert!(index < heap.get_class_size(self.class_tag()));
        debug_assert!(unsafe { *self.ptr }.type_tag == TypeTag::DynObject);
        let values = unsafe { self.ptr.add(DATA_OFFSET) as *mut Option<AnyEnum> };
        unsafe { *values.add(index) }
    }

    pub fn write_at(&self, heap: &Heap, index: usize, value: AnyValue) {
        debug_assert!(index < heap.get_class_size(self.class_tag()));
        let values = unsafe { self.ptr.add(DATA_OFFSET) as *mut Option<AnyEnum> };
        let ptr = unsafe { values.add(index) };
        unsafe {
            *ptr = Some(*value);
        }
    }

    /// if name is found, write to it. if not, transition, clone, write, and
    /// return new pointer. this should be called by ObjectPtr only
    #[must_use]
    fn insert(self, heap: &Heap, name: StringPtr, value: AnyValue, cache: &mut isize) -> Self {
        let class_tag = self.class_tag();
        let mut classes = heap.classes.borrow_mut();
        let class = classes.get_class(class_tag);
        match class.lookup(name, cache) {
            Some(offset) => {
                drop(class);
                drop(classes);
                self.write_at(heap, offset, value);
                self
            }
            None => {
                let size = class.size;
                drop(class);
                let new_tag = classes.transition(class_tag, name);
                drop(classes);
                let new_object = heap.alloc_object_data_or_gc(new_tag);
                for i in 0..size {
                    if let Some(val) = self.read_at(heap, i) {
                        new_object.write_at(heap, i, val.into());
                    }
                }
                new_object.write_at(heap, size, value);
                new_object
            }
        }
    }

    pub fn get(&self, heap: &Heap, name: StringPtr, cache: &mut isize) -> Option<AnyEnum> {
        let class_tag = self.class_tag();
        let classes = heap.classes.borrow();
        let class = classes.get_class(class_tag);
        let offset = class.lookup(name, cache)?;
        self.read_at(heap, offset)
    }

    fn as_array(&self, heap: &Heap) -> &mut [Option<AnyEnum>] {
        let num_ptrs = heap.get_class_size(self.class_tag());
        let members_ptr: *mut Option<AnyEnum> = unsafe { data_ptr(self.ptr) };
        unsafe { std::slice::from_raw_parts_mut(members_ptr, num_ptrs) }
    }
}

impl ObjectPtr {
    pub const unsafe fn new(ptr: *mut Tag) -> Self {
        Self { ptr }
    }
    /// if name is found, write to it. if not, transition, clone, write, and
    /// update pointer
    /// TODO: updating this pointer in particular isn't enough. i think we
    /// have to have a double-pointer situation
    /// (ObjectPtr -> ArrayPtr -> [u8; n])
    pub fn insert(
        &mut self,
        heap: &Heap,
        name: StringPtr,
        value: AnyValue,
        cache: &mut isize,
    ) -> AnyValue {
        let data = &mut **self;
        let new = data.insert(heap, name, value, cache);
        unsafe { *(self.ptr.add(DATA_OFFSET) as *mut ObjectDataPtr) = new };
        value
    }
}
impl Deref for ObjectPtr {
    type Target = ObjectDataPtr;
    fn deref(&self) -> &Self::Target {
        unsafe { &*(self.ptr.add(DATA_OFFSET) as *const ObjectDataPtr) }
    }
}
impl DerefMut for ObjectPtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.ptr.add(DATA_OFFSET) as *mut ObjectDataPtr) }
    }
}
impl HeapPtr for ObjectPtr {
    fn get_ptr(&self) -> *mut Tag {
        self.ptr
    }
    fn get_data_size(&self, _heap: &Heap) -> usize {
        ALIGNMENT
    }
    fn get_gc_ptrs(&self, _heap: &Heap) -> (Vec<*mut Tag>, Vec<*mut *const f64>) {
        (vec![(**self).get_ptr() as *const _ as *mut _], vec![])
    }
}

#[cfg(test)]
mod test {
    use wasm_bindgen_test::wasm_bindgen_test;
    #[wasm_bindgen_test]
    #[test]
    fn object_ptr_get_ptrs() {
        use super::{HeapPtr, TypeTag, ALIGNMENT};
        use crate::Heap;
        let heap = Heap::new((ALIGNMENT * 7) as isize);
        let obj_ptr = heap.alloc_object(0).unwrap();
        let ptrs = obj_ptr.get_gc_ptrs(&heap);
        assert_eq!(ptrs.0.len(), 1);
        assert_eq!(unsafe { *(ptrs.0[0]) }.type_tag, TypeTag::DynObject);
    }
}
