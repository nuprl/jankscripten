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

    pub fn object(class_tag: u16) -> Self {
        Tag {
            marked: false,
            type_tag: TypeTag::DynObject,
            class_tag,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone)]
#[repr(u8)]
pub enum TypeTag {
    I32,
    F64,
    String,
    HT,
    Array,
    Any,
    // The value inside the tag is the address where the array of pointers
    // begins, for this object.
    DynObject,
    ObjectPtrPtr,
}

/// Every pointer into the heap points to a tag, thus we could build an API
/// where every function consumes and produces `*mut Tag`-typed values. However,
/// it would be really easy to make mistakes. Instead, our API has a Rust type
/// for each variant of `Tag`. Moreover, each Rust type implements the
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
    /// nothing, because most structures do not allocate on the
    /// rust heap
    fn final_drop(&self) {}
}

/// Returns a pointer to the data that follows the tag.
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

/// We can safely turn an `AnyPtr` into a more specific type of pointer using
/// the `view` method, which produces a `HeapRefView`.
#[derive(Clone, Copy)]
pub enum HeapRefView<'a> {
    I32(I32Ptr<'a>),
    F64(F64Ptr<'a>),
    String(StringPtr<'a>),
    HT(HTPtr<'a>),
    Array(ArrayPtr<'a>),
    Any(AnyJSPtr<'a>),
    Class(ObjectDataPtr<'a>),
    ObjectPtrPtr(ObjectPtr<'a>),
}
impl<'a> HeapRefView<'a> {
    /// Return a less specific `HeapPtr` that points to the same heap value,
    /// for implementing HeapPtr
    fn heap_ptr(&'a self) -> &'a dyn HeapPtr {
        match self {
            Self::I32(val) => val,
            Self::F64(val) => val,
            Self::String(val) => val,
            Self::HT(val) => val,
            Self::Array(val) => val,
            Self::Any(val) => val,
            Self::Class(val) => val,
            Self::ObjectPtrPtr(val) => val,
        }
    }
}
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
            TypeTag::F64 => HeapRefView::F64(unsafe { F64Ptr::new_tag_unchecked(self.ptr) }),
            TypeTag::String => {
                HeapRefView::String(unsafe { StringPtr::new_tag_unchecked(self.ptr) })
            }
            TypeTag::HT => HeapRefView::HT(unsafe { HTPtr::new_tag_unchecked(self.ptr) }),
            TypeTag::Array => HeapRefView::Array(unsafe { ArrayPtr::new_tag_unchecked(self.ptr) }),
            TypeTag::Any => HeapRefView::Any(unsafe { AnyJSPtr::new_tag_unchecked(self.ptr) }),
            TypeTag::DynObject => HeapRefView::Class(unsafe { ObjectDataPtr::new(self.ptr) }),
            TypeTag::ObjectPtrPtr => HeapRefView::ObjectPtrPtr(unsafe { ObjectPtr::new(self.ptr) }),
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
    _phantom: PhantomData<&'a T>,
}
// these are necessary because if T is not clone/copy, derive will try not
// to let it clone/copy, but this is a pointer and should copy
impl<'a, T> Clone for TypePtr<'a, T> {
    fn clone(&self) -> TypePtr<'a, T> {
        TypePtr {
            ptr: self.ptr,
            _phantom: PhantomData,
        }
    }
}
impl<'a, T> Copy for TypePtr<'a, T> {}

impl<'a, T> HeapPtr for TypePtr<'a, T> {
    fn get_ptr(&self) -> *mut Tag {
        self.ptr
    }

    fn get_data_size(&self, _heap: &Heap) -> usize {
        return layout::layout_aligned::<T>(ALIGNMENT).size();
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
    pub unsafe fn new_tag_unchecked(ptr: *mut Tag) -> Self {
        TypePtr {
            ptr,
            _phantom: PhantomData,
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
    pub fn get_mut(&mut self) -> &'a mut T {
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

impl<'a, T> From<TypePtr<'a, T>> for AnyPtr<'a> {
    fn from(ptr: TypePtr<'a, T>) -> Self {
        // safety: TypePtr is valid, so an AnyPtr will be valid
        unsafe { AnyPtr::new(ptr.get_ptr()) }
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
        let data_ptr: *mut U = std::mem::transmute(self_ptr.add(DATA_OFFSET));
        return std::slice::from_raw_parts_mut(data_ptr, len);
    }
}
