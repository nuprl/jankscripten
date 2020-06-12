//! Values on the managed heap.
//!

use super::constants::*;
use super::heap_types::*;
use super::layout;
use super::*;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

/// The first machine-word of every heap value is a `Tag`, and every pointer
/// into the heap must point to a `Tag`. In other words, we do *not* support
/// interior pointers.
#[derive(PartialEq, Debug, Copy, Clone)]
#[repr(packed(4))]
pub struct Tag {
    pub marked: bool,
    pub type_tag: TypeTag,
    /// The `class_tag` is only meaningful if the `type_tag == TypeTag::Objet`.
    pub class_tag: u16,
}

impl Tag {
    pub fn with_type(type_tag: TypeTag) -> Self {
        Tag {
            marked: false,
            type_tag,
            class_tag: 0,
        }
    }

    pub fn i32() -> Self {
        Self::with_type(TypeTag::I32)
    }
    pub fn string() -> Self {
        Self::with_type(TypeTag::String)
    }

    pub fn object(class_tag: u16) -> Self {
        Tag {
            marked: false,
            type_tag: TypeTag::Object,
            class_tag,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
#[repr(u8)]
pub enum TypeTag {
    I32,
    String,
    // The value inside the tag is the address where the array of pointers
    // begins, for this object.
    Object,
}

/// Every pointer into the heap points to a tag, thus we could build an API
/// where every function consumes and produces `*mut Tag`-typed values. However,
/// it would be really easy to make mistakes. Instead, our API has a Rust type
/// for each variant of `Tag`. Moreover, each Rust type impements the
/// `HeapPtr` trait, to get low-level access to the heap, which is needed to
/// actually manipulate heap values.
///
pub trait HeapPtr {
    fn get_ptr(&self) -> *mut Tag;
    /// The size of the data that follows the tag. i.e., this must exclude the
    /// size of the tag.
    fn get_data_size(&self, heap: &Heap) -> usize;
}

/// Returns a pointer to the data that follows the tag. Note that the size of
/// the tag may be smaller than a machine word.
pub unsafe fn data_ptr<T>(tag_ptr: *mut Tag) -> *mut T {
    return std::mem::transmute(tag_ptr.add(DATA_OFFSET));
}

/// A pointer to an arbitrary `Tag`. We use `repr(transparent)` as an
/// optimization: the `AnyPtr` is represented as an ordinary pointer.
/// In principle, this allows us to `transmute` between `*mut Tag` and `AnyPtr`.
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(transparent)]
pub struct AnyPtr<'a> {
    ptr: *mut Tag,
    _phantom: PhantomData<&'a ()>,
}

/// We can safely turn a `HeapRef` into a more specific type of pointer using
/// the `view` method, which produces a `HeapRefView`.
pub enum HeapRefView<'a> {
    I32(I32Ptr<'a>),
    String(StringPtr<'a>),
    Object(ObjectPtr<'a>),
}

impl<'a> HeapPtr for HeapRefView<'a> {
    fn get_ptr(&self) -> *mut Tag {
        match self {
            HeapRefView::I32(ptr) => ptr.get_ptr(),
            HeapRefView::String(ptr) => ptr.get_ptr(),
            HeapRefView::Object(ptr) => ptr.get_ptr(),
        }
    }

    fn get_data_size(&self, heap: &Heap) -> usize {
        match self {
            HeapRefView::I32(ptr) => ptr.get_data_size(heap),
            HeapRefView::String(ptr) => ptr.get_data_size(heap),
            HeapRefView::Object(ptr) => ptr.get_data_size(heap),
        }
    }
}

impl<'a> HeapPtr for AnyPtr<'a> {
    fn get_ptr(&self) -> *mut Tag {
        return self.ptr;
    }

    fn get_data_size(&self, heap: &Heap) -> usize {
        return self.view().get_data_size(heap);
    }
}

impl<'a> AnyPtr<'a> {
    pub unsafe fn new(ptr: *mut Tag) -> Self {
        AnyPtr {
            ptr,
            _phantom: PhantomData,
        }
    }

    /// Discriminate on the tag, and return a more specific `HeapPtr` that
    /// points to the same heap value.
    pub fn view(&self) -> HeapRefView<'a> {
        let heap_ref: Tag = unsafe { *self.ptr };
        match heap_ref.type_tag {
            TypeTag::I32 => HeapRefView::I32(I32Ptr::new(self.ptr)),
            TypeTag::String => HeapRefView::String(StringPtr::new(self.ptr)),
            TypeTag::Object => HeapRefView::Object(unsafe { ObjectPtr::new(self.ptr) }),
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct TypePtr<'a, T> {
    ptr: *mut Tag,
    _phantom: PhantomData<&'a T>,
}

impl<'a, T> HeapPtr for TypePtr<'a, T> {
    fn get_ptr(&self) -> *mut Tag {
        self.ptr
    }

    fn get_data_size(&self, _heap: &Heap) -> usize {
        std::mem::size_of::<T>()
    }
}

impl<'a, T> TypePtr<'a, T> {
    pub fn size() -> isize {
        return layout::layout_aligned::<T>(ALIGNMENT).size() as isize;
    }

    // safety: Tag must match T
    pub unsafe fn new_unchecked(ptr: *mut Tag) -> Self {
        TypePtr {
            ptr,
            _phantom: PhantomData,
        }
    }
    pub fn new_checked(ptr: *mut Tag, type_tag: TypeTag) -> Self {
        assert_eq!(unsafe { ptr.read().type_tag }, type_tag);
        unsafe { Self::new_unchecked(ptr) }
    }

    fn data(&self) -> &mut T {
        unsafe { &mut *data_ptr(self.ptr) }
    }

    pub fn read(&self) -> T {
        unsafe { std::ptr::read(data_ptr(self.ptr)) }
    }

    pub fn write(&self, val: T) {
        unsafe { std::ptr::write(data_ptr(self.ptr), val) }
    }
}

impl<'a, T> Deref for TypePtr<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.data()
    }
}
impl<'a, T> DerefMut for TypePtr<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.data()
    }
}

/// A pointer to a `Tag::Object`.
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(transparent)]
pub struct ObjectPtr<'a> {
    ptr: *mut Tag,
    _phantom: PhantomData<&'a ()>,
}

impl<'a> HeapPtr for ObjectPtr<'a> {
    fn get_ptr(&self) -> *mut Tag {
        return self.ptr;
    }

    fn get_data_size(&self, heap: &Heap) -> usize {
        let tag = unsafe { self.ptr.read() };
        let class_tag = tag.class_tag;
        let num_elements = heap.container_sizes.get(&class_tag).unwrap();
        return num_elements * ALIGNMENT;
    }
}

impl<'a> ObjectPtr<'a> {
    /// This function is unsafe, because (1) we do not check that the class_tag
    /// is valid, and (2) we assume that `ptr` is valid.
    pub unsafe fn new(ptr: *mut Tag) -> Self {
        assert_eq!(ptr.read().type_tag, TypeTag::Object);
        return ObjectPtr {
            ptr,
            _phantom: PhantomData,
        };
    }

    pub fn class_tag(&self) -> u16 {
        let tag = unsafe { self.ptr.read() };
        assert_eq!(tag.type_tag, TypeTag::Object);
        return tag.class_tag;
    }

    pub fn read_at(&self, heap: &'a Heap, index: usize) -> Option<AnyPtr<'a>> {
        let type_tag = self.class_tag();
        let len = *heap.container_sizes.get(&type_tag).unwrap();
        assert!(index < len);
        let values: *mut *mut Tag = unsafe { std::mem::transmute(self.ptr.add(DATA_OFFSET)) };
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
        let values: *mut *mut Tag = unsafe { std::mem::transmute(self.ptr.add(DATA_OFFSET)) };
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
