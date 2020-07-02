use super::super::StrPtr;
use super::constants::DATA_OFFSET;
use super::heap_values::*;
use super::{Heap, ALIGNMENT};
use crate::any::Any;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

/// A managed pointer to a `ObjectDataPtr`. this level of indirection is
/// needed to update objects when reallocated
///
/// This looks like this:
/// Tag(1) | pointer to ObjectDataPtr(1)
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(transparent)]
pub struct ObjectPtr<'a> {
    ptr: *mut Tag,
    _phantom: PhantomData<&'a ()>,
}

/// A managed pointer to an Object, specified by a Class
///
/// It looks like this:
/// Tag(4/8) | field(12/16) | field(12/16) | ...
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(transparent)]
pub struct ObjectDataPtr<'a> {
    ptr: *mut Tag,
    _phantom: PhantomData<&'a ()>,
}

impl<'a> HeapPtr for ObjectDataPtr<'a> {
    fn get_ptr(&self) -> *mut Tag {
        return self.ptr;
    }

    fn get_data_size(&self, heap: &Heap) -> usize {
        heap.object_data_size(self.class_tag())
    }
}

impl<'a> ObjectDataPtr<'a> {
    /// This function is unsafe, because (1) we do not check that the class_tag
    /// is valid, and (2) we assume that `ptr` is valid.
    pub unsafe fn new(ptr: *mut Tag) -> Self {
        assert_eq!((*ptr).type_tag, TypeTag::Class);
        ObjectDataPtr {
            ptr,
            _phantom: PhantomData,
        }
    }

    pub fn class_tag(&self) -> u16 {
        let tag = unsafe { *self.ptr };
        debug_assert_eq!(tag.type_tag, TypeTag::Class);
        tag.class_tag
    }

    pub fn read_at(&self, heap: &'a Heap, index: usize) -> Option<Any<'a>> {
        debug_assert!(index < heap.get_class_size(self.class_tag()));
        debug_assert!(unsafe { *self.ptr }.type_tag == TypeTag::Class);
        let values = unsafe { self.ptr.add(DATA_OFFSET) as *mut Option<Any> };
        unsafe { *values.add(index) }
    }

    pub fn write_at(&self, heap: &'a Heap, index: usize, value: Any) {
        debug_assert!(index < heap.get_class_size(self.class_tag()));
        let values = unsafe { self.ptr.add(DATA_OFFSET) as *mut Option<Any> };
        let ptr = unsafe { values.add(index) };
        unsafe {
            *ptr = Some(value);
        }
    }

    /// if name is found, write to it. if not, transition, clone, write, and
    /// return new pointer. this should be called by ObjectPtr only
    #[must_use]
    fn insert(self, heap: &'a Heap, name: StrPtr, value: Any, cache: &mut isize) -> Option<Self> {
        let class_tag = self.class_tag();
        let mut classes = heap.classes.borrow_mut();
        let class = classes.get_class(class_tag);
        match class.lookup(name, cache) {
            Some(offset) => {
                drop(class);
                drop(classes);
                self.write_at(heap, offset, value);
                Some(self)
            }
            None => {
                let size = class.size;
                drop(class);
                let new_tag = classes.transition(class_tag, name);
                drop(classes);
                let new_object = heap.alloc_object_data(new_tag)?;
                for i in 0..size {
                    if let Some(val) = self.read_at(heap, i) {
                        new_object.write_at(heap, i, val);
                    }
                }
                new_object.write_at(heap, size, value);
                Some(new_object)
            }
        }
    }

    pub fn get(&self, heap: &'a Heap, name: StrPtr, cache: &mut isize) -> Option<Any<'a>> {
        let class_tag = self.class_tag();
        let classes = heap.classes.borrow();
        let class = classes.get_class(class_tag);
        let offset = class.lookup(name, cache)?;
        self.read_at(heap, offset)
    }
}

impl<'a> ObjectPtr<'a> {
    pub unsafe fn new(ptr: *mut Tag) -> Self {
        Self {
            ptr,
            _phantom: PhantomData,
        }
    }
    /// if name is found, write to it. if not, transition, clone, write, and
    /// update pointer
    /// TODO: updating this pointer in particular isn't enough. i think we
    /// have to have a double-pointer situation
    /// (ObjectPtr -> ArrayPtr -> [u8; n])
    pub fn insert(
        &mut self,
        heap: &'a Heap,
        name: StrPtr,
        value: Any<'a>,
        cache: &mut isize,
    ) -> Option<Any> {
        let data = &mut **self;
        let new = data.insert(heap, name, value, cache)?;
        unsafe { *(self.ptr.add(DATA_OFFSET) as *mut ObjectDataPtr) = new };
        Some(value)
    }
}
impl<'a> Deref for ObjectPtr<'a> {
    type Target = ObjectDataPtr<'a>;
    fn deref(&self) -> &Self::Target {
        unsafe { &*(self.ptr.add(DATA_OFFSET) as *const ObjectDataPtr) }
    }
}
impl<'a> DerefMut for ObjectPtr<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self.ptr.add(DATA_OFFSET) as *mut ObjectDataPtr) }
    }
}
impl<'a> HeapPtr for ObjectPtr<'a> {
    fn get_ptr(&self) -> *mut Tag {
        self.ptr
    }
    fn get_data_size(&self, _heap: &Heap) -> usize {
        ALIGNMENT
    }
}
