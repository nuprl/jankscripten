//! Values on the managed heap.
//!

use super::constants::*;
use super::heap_types::*;
use super::layout;
use super::*;
use crate::Any;
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
    HTAny,
    HTI32,
    Any,
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
pub trait HeapPtr {
    fn get_ptr(&self) -> *mut Tag;
    /// The size of the data that follows the tag. i.e., this must exclude the
    /// size of the tag.
    fn get_data_size(&self, heap: &Heap) -> usize;
    /// this is not Drop::drop, don't call it unless you're the GC
    ///
    /// drops any data on the unmanaged heap. default implementation does
    /// nothing, because most structures ideally shouldn't allocate on the
    /// rust heap
    fn final_drop(&self) {}
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
#[derive(Clone, Copy)]
pub enum HeapRefView<'a> {
    I32(I32Ptr<'a>),
    String(StringPtr<'a>),
    HTAny(HTPtr<'a, Any>),
    HTI32(HTPtr<'a, i32>),
    Any(AnyJSPtr<'a>),
    Object(ObjectPtr<'a>),
}
impl<'a> HeapRefView<'a> {
    /// Return a less specific `HeapPtr` that points to the same heap value,
    /// for implementing HeapPtr
    fn heap_ptr(&'a self) -> &'a dyn HeapPtr {
        match self {
            Self::I32(val) => val,
            Self::String(val) => val,
            Self::HTAny(val) => val,
            Self::HTI32(val) => val,
            Self::Any(val) => val,
            Self::Object(val) => val,
        }
    }
}
// TODO(luna): this could be better achieved by Deref if i can figure out
// the darn lifetimes
impl<'a> HeapPtr for HeapRefView<'a> {
    fn get_ptr(&self) -> *mut Tag {
        self.heap_ptr().get_ptr()
    }
    fn get_data_size(&self, heap: &Heap) -> usize {
        self.heap_ptr().get_data_size(heap)
    }
    fn final_drop(&self) {
        self.heap_ptr().final_drop()
    }
}

impl<'a> AnyPtr<'a> {
    pub unsafe fn new(ptr: *mut Tag) -> Self {
        AnyPtr {
            ptr,
            _phantom: PhantomData,
        }
    }

    /// Discriminate on the tag, and return a more specific `HeapRefView` that
    /// points to the same heap value.
    pub fn view(&self) -> HeapRefView<'a> {
        let heap_ref: Tag = unsafe { *self.ptr };
        match heap_ref.type_tag {
            TypeTag::I32 => HeapRefView::I32(unsafe { I32Ptr::new_tag_unchecked(self.ptr) }),
            TypeTag::String => {
                HeapRefView::String(unsafe { StringPtr::new_tag_unchecked(self.ptr) })
            }
            TypeTag::HTAny => {
                HeapRefView::HTAny(unsafe { HTPtr::<Any>::new_tag_unchecked(self.ptr) })
            }
            TypeTag::HTI32 => {
                HeapRefView::HTI32(unsafe { HTPtr::<i32>::new_tag_unchecked(self.ptr) })
            }
            TypeTag::Any => HeapRefView::Any(unsafe { AnyJSPtr::new_tag_unchecked(self.ptr) }),
            TypeTag::Object => HeapRefView::Object(unsafe { ObjectPtr::new(self.ptr) }),
        }
    }
}
// Deref as well
impl<'a> HeapPtr for AnyPtr<'a> {
    fn get_ptr(&self) -> *mut Tag {
        self.view().get_ptr()
    }
    fn get_data_size(&self, heap: &Heap) -> usize {
        self.view().get_data_size(heap)
    }
    fn final_drop(&self) {
        self.view().final_drop()
    }
}

#[derive(Debug)]
#[repr(transparent)]
pub struct TypePtr<'a, T> {
    ptr: *mut Tag,
    _phantom: PhantomData<&'a ()>,
    _drop: PhantomData<T>,
}
// these are necessary because if T is not clone/copy, derive will try not
// to let it clone/copy, but this is a pointer and should copy
impl<'a, T> Clone for TypePtr<'a, T> {
    fn clone(&self) -> TypePtr<'a, T> {
        TypePtr {
            ptr: self.ptr,
            _phantom: PhantomData,
            _drop: PhantomData,
        }
    }
}
impl<'a, T> Copy for TypePtr<'a, T> {}

impl<'a, T> HeapPtr for TypePtr<'a, T> {
    fn get_ptr(&self) -> *mut Tag {
        self.ptr
    }

    fn get_data_size(&self, _heap: &Heap) -> usize {
        std::mem::size_of::<T>()
    }

    fn final_drop(&self) {
        unsafe { std::ptr::drop_in_place::<T>(data_ptr(self.ptr)) }
    }
}

impl<'a, T> TypePtr<'a, T> {
    pub fn size() -> isize {
        return layout::layout_aligned::<T>(ALIGNMENT).size() as isize;
    }

    // safety: Tag must match T, value must be immediately initialized
    // with write
    unsafe fn new_tag_unchecked(ptr: *mut Tag) -> Self {
        TypePtr {
            ptr,
            _phantom: PhantomData,
            _drop: PhantomData,
        }
    }
    // safety: data_ptr must be initialized with write
    unsafe fn new_tag(ptr: *mut Tag, type_tag: TypeTag) -> Self {
        assert_eq!(ptr.read().type_tag, type_tag);
        Self::new_tag_unchecked(ptr)
    }
    pub fn new(ptr: *mut Tag, type_tag: TypeTag, value: T) -> Self {
        let mut this = unsafe { Self::new_tag(ptr, type_tag) };
        this.write(value);
        this
    }

    /// safe alternatives to read / write where read() doesn't cause drop
    pub fn get(&self) -> &T {
        unsafe { &*data_ptr(self.ptr) }
    }
    /// write data, dropping old data. if you're not the allocator, this is
    /// what you want
    pub fn get_mut(&mut self) -> &mut T {
        unsafe { &mut *data_ptr(self.ptr) }
    }
    /// write data without dropping old rust-owned data. this should only
    /// really be used by the allocater for writing to uninitialized tag
    fn write(&mut self, val: T) {
        unsafe { std::ptr::write(data_ptr(self.ptr), val) }
    }
}

impl<'a, T> Deref for TypePtr<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.get()
    }
}
impl<'a, T> DerefMut for TypePtr<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }
}

impl<'a, T: std::hash::Hash> std::hash::Hash for TypePtr<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get().hash(state)
    }
}
impl<T: PartialEq> PartialEq<TypePtr<'_, T>> for TypePtr<'_, T> {
    fn eq(&self, other: &TypePtr<'_, T>) -> bool {
        self.get() == other.get()
    }
}
impl<T: PartialEq> Eq for TypePtr<'_, T> {}

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
