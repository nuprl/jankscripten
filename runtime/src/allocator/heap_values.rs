
//! Values on the managed heap.
//! 

use std::marker::PhantomData;
use super::constants::*;
use super::*;

/// The first machine-word of every heap value is a `Tag`, and every pointer
/// into the heap must point to a `Tag`. In other words, we do *not* support
/// interior pointers.
#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Tag {
    I32,
    // The value inside the tag is the address where the array of pointers
    // begins, for this object.
    Object(u16)
}

/// Every pointer into the heap points to a tag, thus we could build an API
/// where every function consumes and produces `*mut Tag`-typed values. However,
/// it would be really easy to make mistakes. Instead, our API has a Rust type
/// for each variant of `Tag`. Moreover, each Rust type impements the
/// `HeapPtr` trait, to get low-level access to the heap, which is needed to
/// actually manipulate heap values.
pub trait HeapPtr {
    fn get_ptr(&self) -> *mut Tag;
    /// The size of the data that follows the tag. i.e., this must exclude the
    /// size of the tag.
    fn get_data_size(&self) -> usize;
}

/// Returns a pointer to the data that follows the tag. Note that the size of
/// the tag may be smaller than a machine word.
unsafe fn data_ptr<T>(tag_ptr: *mut Tag) -> *mut T {
    return std::mem::transmute(tag_ptr.add(DATA_OFFSET));
}

/// A pointer to an arbitrary `Tag`.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct AnyPtr<'a> {
    ptr: *mut Tag,
    _phantom: PhantomData<&'a ()>
}

/// We can safely turn a `HeapRef` into a more specific type of pointer using
/// the `view` method, which produces a `HeapRefView`.
pub enum HeapRefView<'a> {
    I32(I32Ref<'a>),
    Object(ObjectRef<'a>)
}

impl<'a> HeapPtr for AnyPtr<'a> {
    fn get_ptr(&self) -> *mut Tag {
        return self.ptr;
    }

    fn get_data_size(&self) -> usize {
        unimplemented!();
    }
}

impl<'a> AnyPtr<'a> {

    pub unsafe fn new(ptr: *mut Tag) -> Self {
        return AnyPtr { ptr, _phantom: PhantomData };
    }

    /// Discriminate on the tag, and return a more specific `HeapPtr` that
    /// points to the same heap value.
    pub fn view(&self) -> HeapRefView<'a> {
        let heap_ref : Tag = unsafe {  *self.ptr };
        match heap_ref {
            Tag::I32 => HeapRefView::I32(I32Ref::new(self.ptr)),
            Tag::Object(_) => HeapRefView::Object(unsafe { ObjectRef::new(self.ptr) })
        }
    }
}

/// A pointer to a `Tag::I32`.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct I32Ref<'a> {
    ptr: *mut Tag,
    _phantom: PhantomData<&'a ()>
}

impl<'a> HeapPtr for I32Ref<'a> {
    fn get_ptr(&self) -> *mut Tag {
        return self.ptr;
    }

    fn get_data_size(&self) -> usize {
        return 4;
    }
}

impl<'a> I32Ref<'a> {

    pub fn new(ptr: *mut Tag) -> Self {
        unsafe {
            assert_eq!(ptr.read(), Tag::I32);
        }
        return I32Ref { ptr: ptr, _phantom: PhantomData };
    }

    pub fn read(&self) -> i32 {
        unsafe {
            return *data_ptr(self.ptr);
        }
    }

    pub fn write(&self, value: i32) {
        unsafe {
            let p = data_ptr::<i32>(self.ptr);
            p.write(value);
        }
    }
}

/// A pointer to a `Tag::Object`.
#[derive(Debug, PartialEq, Clone, Copy)]
pub struct ObjectRef<'a> {
    ptr: *mut Tag,
    _phantom: PhantomData<&'a ()>
}

impl<'a> HeapPtr for ObjectRef<'a> {
    fn get_ptr(&self) -> *mut Tag {
        return self.ptr;
    }

    fn get_data_size(&self) -> usize {
        unimplemented!();
    }
}

impl<'a> ObjectRef<'a> {

    pub unsafe fn new(ptr: *mut Tag) -> Self {
        return ObjectRef { ptr, _phantom: PhantomData };
    }

    pub fn class_tag(&self) -> u16 {
        match unsafe { self.ptr.read() } {
            Tag::Object(x) => x,
            _ => panic!("not an object")
        }
    }

    pub fn read_at(&self, heap: &'a Heap, index: usize) -> Option<AnyPtr<'a>> {
        let type_tag = self.class_tag();
        let len = *heap.container_sizes.get(&type_tag).unwrap();
        assert!(index < len);
        let values : *mut *mut Tag = unsafe { std::mem::transmute(self.ptr.add(DATA_OFFSET)) };
        let ptr = unsafe { &mut *values.add(index) };

        let ptr2 = *ptr;
        if ptr2.is_null() {
            return None;
        }
        return Some(unsafe { AnyPtr::new(ptr2) });
    }

    pub fn write_at<P: HeapPtr>(&self, heap: &'a Heap, index: usize, value: P) {
        let type_tag = self.class_tag();
        let len = *heap.container_sizes.get(&type_tag).unwrap();
        assert!(index < len);
        let values : *mut *mut Tag = unsafe { std::mem::transmute(self.ptr.add(DATA_OFFSET)) };
        let ptr = unsafe { values.add(index) };
        unsafe {
            ptr.write(value.get_ptr());
        }
    }

}


impl Tag {


    /**
     * Returns a reference to a slice of values that immediately follow this
     * tag. This method assumes that the tag is truly followed by `len`
     * `U`-typed values.
     */
    pub unsafe fn slice_ref<U>(&mut self, len: usize) -> &mut [U] {
        let self_ptr = self as *mut Tag;
        let data_ptr: *mut U = std::mem::transmute(self_ptr.add(1));
        return std::slice::from_raw_parts_mut(data_ptr, len);
    }
    
}

