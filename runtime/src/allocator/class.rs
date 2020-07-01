use super::super::StrPtr;
use super::constants::*;
use super::heap_values::*;
use super::{Heap, ALIGNMENT};
use std::marker::PhantomData;

/// A managed pointer to a `Class`
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(transparent)]
pub struct ClassPtr<'a> {
    ptr: *mut Class,
    _phantom: PhantomData<&'a ()>,
}

/// this describes the layout of the memory an ClassPtr points to
#[repr(C)]
struct Class {
    /// 1
    tag: Tag,
    /// 3
    offsets: Vec<(StrPtr, u32)>,
    /// 3
    transitions: Vec<(StrPtr, u32)>,
    /// 1 x n
    ///
    /// ```
    /// let offset = offsets.linear_search(field_name);
    /// class.fields.add(offset)
    /// ```
    fields: *mut Tag,
}

impl<'a> HeapPtr for ClassPtr<'a> {
    fn get_ptr(&self) -> *mut Tag {
        return self.ptr as *mut Tag;
    }

    fn get_data_size(&self, heap: &Heap) -> usize {
        let tag = unsafe { (*self.ptr).tag };
        let class_tag = tag.class_tag;
        let num_elements = heap.classes.get_container_size(&class_tag);
        return num_elements * ALIGNMENT;
    }
}

impl<'a> ClassPtr<'a> {
    /// This function is unsafe, because (1) we do not check that the class_tag
    /// is valid, and (2) we assume that `ptr` is valid.
    pub unsafe fn new(ptr: *mut Tag) -> Self {
        assert_eq!((*ptr).type_tag, TypeTag::Class);
        ClassPtr {
            ptr: ptr as *mut Class,
            _phantom: PhantomData,
        }
    }

    pub fn class_tag(&self) -> u16 {
        let tag = unsafe { (*self.ptr).tag };
        assert_eq!(tag.type_tag, TypeTag::Class);
        tag.class_tag
    }

    pub fn read_at(&self, heap: &'a Heap, index: usize) -> Option<AnyPtr<'a>> {
        let type_tag = self.class_tag();
        let len = heap.classes.get_container_size(&type_tag);
        assert!(index < len);
        let values = unsafe { &mut (*self.ptr).fields as *mut *mut Tag };
        let ptr = unsafe { *values.add(index) };

        if ptr.is_null() {
            None
        } else {
            Some(unsafe { AnyPtr::new(ptr) })
        }
    }

    pub fn write_at<P: HeapPtr>(&self, heap: &'a Heap, index: usize, value: P) {
        let type_tag = self.class_tag();
        let len = heap.classes.get_container_size(&type_tag);
        assert!(index < len);
        let values = unsafe { &mut (*self.ptr).fields as *mut *mut Tag };
        let ptr = unsafe { values.add(index) };
        unsafe {
            ptr.write(value.get_ptr());
        }
    }
}
