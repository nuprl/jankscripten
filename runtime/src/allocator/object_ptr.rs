use super::constants::DATA_OFFSET;
use super::heap_values::*;
use super::{Heap, ALIGNMENT};
use crate::heap_types::StringPtr;
use crate::{AnyEnum, AnyValue};
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
        heap.object_data_size(heap.get_class_size(self.class_tag()))
    }

    fn get_gc_ptrs(&self, heap: &Heap) -> Vec<*mut Tag> {
        let mut rv = vec![];
        for member in self.as_array(heap) {
            if let Some(any) = member {
                rv.append(&mut any.get_gc_ptrs(heap));
            }
        }
        rv
    }
    fn get_gc_f64s(&mut self, heap: &Heap) -> Vec<*mut *const f64> {
        let mut rv = vec![];
        for member in self.as_array(heap) {
            if let Some(any) = member {
                rv.append(&mut any.get_gc_f64s(heap));
            }
        }
        rv
    }
}

impl<'a> ObjectDataPtr<'a> {
    /// This function is unsafe, because (1) we do not check that the class_tag
    /// is valid, and (2) we assume that `ptr` is valid.
    pub unsafe fn new(ptr: *mut Tag) -> Self {
        assert_eq!((*ptr).type_tag, TypeTag::DynObject);
        ObjectDataPtr {
            ptr,
            _phantom: PhantomData,
        }
    }

    pub fn class_tag(&self) -> u16 {
        let tag = unsafe { *self.ptr };
        debug_assert_eq!(tag.type_tag, TypeTag::DynObject);
        tag.class_tag
    }

    pub fn read_at(&self, heap: &'a Heap, index: usize) -> Option<AnyEnum<'a>> {
        debug_assert!(index < heap.get_class_size(self.class_tag()));
        debug_assert!(unsafe { *self.ptr }.type_tag == TypeTag::DynObject);
        let values = unsafe { self.ptr.add(DATA_OFFSET) as *mut Option<AnyEnum> };
        unsafe { *values.add(index) }
    }

    pub fn write_at(&self, heap: &'a Heap, index: usize, value: AnyValue) {
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
    fn insert(self, heap: &'a Heap, name: StringPtr, value: AnyValue, cache: &mut isize) -> Self {
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

    /// Reads a property from an object, searching up the prototype chain if
    /// necessary. Returns `undefined` if the property doesn't exist anywhere
    /// on the prototype chain.
    pub fn get(&self, heap: &'a Heap, name: StringPtr, cache: &mut isize) -> AnyEnum<'a> {
        // Reading a property from an object is a complicated process because
        // of the prototype chain.
        //
        // There are 3 cases to consider for a property read `obj[prop]`:
        //
        // 1. `obj` has a field named `prop`       ~~> return `obj[prop]`
        //
        // 2. `obj` has a field named "__proto__"  ~~> return `obj.__proto__[prop]`
        //                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        //                                             this will recursively search
        //                                                  the prototype chain
        //
        // 3. "__proto__" field is missing or null ~~> return `undefined`

        // Test Case 1: `obj` has a field named `prop`.
        // Check `obj`'s class for `prop`
        let class_tag = self.class_tag();
        let classes = heap.classes.borrow();
        let class = classes.get_class(class_tag);
        // TODO(mark): currently, the field cache is assumed to be valid
        //             without checking the class of the object whose
        //             offsets were cached. we must ensure the class is
        //             identical before using the cache.
        let maybe_offset = class.lookup(name, cache);

        if let Some(offset) = maybe_offset {
            // this is Case 1
            return self
                .read_at(heap, offset)
                .expect("object missing offset spceified by its hidden class");
        }

        // Test Case 2: `obj` has a field named "__proto__".
        let maybe_proto_offset = class.lookup("__proto__".into(), &mut -1);
        if let Some(proto_offset) = maybe_proto_offset {
            // Get the prototype object
            let proto_val = self.read_at(heap, proto_offset).unwrap();

            // Is it a real object that we can read from? As opposed to `null`
            // or any other type of value.
            if let AnyEnum::Ptr(proto_ptr) = proto_val {
                if let HeapRefView::ObjectPtrPtr(proto_obj) = proto_ptr.view() {
                    // this is Case 2. Perform the same read on the proto obj.

                    // -1 because we don't cache reads on the prototype chain
                    return proto_obj.get(heap, name, &mut -1);
                }
            }
        }

        // This is Case 3. `obj` doesn't have `prop`, and `__proto__` is
        // missing or not an object.

        return AnyEnum::Undefined;
    }

    fn as_array(&self, heap: &Heap) -> &mut [Option<AnyEnum>] {
        let num_ptrs = heap.get_class_size(self.class_tag());
        let members_ptr: *mut Option<AnyEnum> = unsafe { data_ptr(self.ptr) };
        unsafe { std::slice::from_raw_parts_mut(members_ptr, num_ptrs) }
    }
}

impl<'a> ObjectPtr<'a> {
    pub const unsafe fn new(ptr: *mut Tag) -> Self {
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
    fn get_gc_ptrs(&self, _heap: &Heap) -> Vec<*mut Tag> {
        vec![(**self).get_ptr()]
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
        assert_eq!(ptrs.len(), 1);
        assert_eq!(unsafe { *ptrs[0] }.type_tag, TypeTag::DynObject);
    }
}
