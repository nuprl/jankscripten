//! managed allocation. most allocations should be made through [Heap]

use crate::{AnyEnum, AnyValue};
use std::alloc;
use std::alloc::Layout;
use std::cell::RefCell;
mod class_list;
mod constants;
mod heap_values;
mod layout;
mod object_ptr;
use crate::util::*;

pub mod heap_types;
pub use heap_values::AnyPtr;
pub use heap_values::HeapPtr;
pub use heap_values::HeapRefView;
mod f64_allocator;

use class_list::ClassList;
use constants::*;
use f64_allocator::F64Allocator;
use heap_types::*;
pub use heap_values::Tag;
use heap_values::*;

#[cfg(test)]
mod tests;

/// When running a program, we have a single global Heap ([crate::heap()]).
/// However, during testing, we create several heaps.
pub struct Heap {
    buffer: *mut u8,
    pub f64_allocator: RefCell<F64Allocator>,
    size: isize,
    free_list: RefCell<FreeList>,
    tag_size: isize,
    pub classes: RefCell<ClassList>,
    /// We initialize this to the empty stack. Before calling [Heap::gc()], the
    /// shadow stack must contain all GC roots.
    shadow_stack: RefCell<Vec<Vec<Option<*mut Tag>>>>,
}

#[derive(Debug)]
struct Block {
    start: *mut u8,
    size: isize,
}

enum FreeList {
    Nil,
    Block(Block, Box<FreeList>),
}

impl FreeList {
    pub fn new(start: *mut u8, size: isize) -> Self {
        let block = Block { start, size };
        return FreeList::Block(block, Box::new(FreeList::Nil));
    }

    pub fn insert(mut self, start: *mut u8, size: isize) -> FreeList {
        match &mut self {
            FreeList::Nil => {
                return FreeList::new(start, size);
            }
            FreeList::Block(block, rest) => {
                let new_block_end = unsafe { start.add(size as usize) };
                if new_block_end < block.start {
                    // +-----------+------------+-------+
                    // | new block | used space | block |
                    // +-----------+------------+-------+
                    let new_block = Block { start, size };
                    return FreeList::Block(new_block, Box::new(self));
                } else if new_block_end == block.start {
                    // +-----------+-------+
                    // | new block | block |
                    // +-----------+-------+
                    block.start = start;
                    block.size = block.size + size;
                    return self;
                } else if start == unsafe { start.add(size as usize) } {
                    // +-------+-----------+
                    // | block | new block |
                    // +-------+-----------+
                    block.size = block.size + size;
                    return self;
                } else if start == block.start && size == block.size {
                    // This can happen, because this is the world's worst
                    // mark and sweep collector.
                    return self;
                } else {
                    // +-------+-----------------------------+-----------+
                    // | block | mix of used and free blocks | new block |
                    // +-------+-----------------------------+-----------+
                    let rest2 = std::mem::replace(rest.as_mut(), FreeList::Nil);
                    *rest.as_mut() = rest2.insert(start, size);
                    return self;
                }
            }
        }
    }

    /**
     * Returns the address of a free block of size `size`, and updates the
     * free size.
     */
    pub fn find_free_size(&mut self, size: isize) -> Option<*mut u8> {
        // TODO(arjun): Make iterative for performance.
        match self {
            FreeList::Block(block, next) => {
                if size == block.size {
                    let addr = block.start;
                    let next = std::mem::replace(next.as_mut(), FreeList::Nil);
                    *self = next;
                    return Some(addr);
                } else if size < block.size {
                    block.size = block.size - size;
                    let addr = block.start;
                    block.start = unsafe { block.start.add(size as usize) };
                    return Some(addr);
                } else
                /* size > block.size */
                {
                    return Self::find_free_size(next.as_mut(), size);
                }
            }
            FreeList::Nil => {
                return None;
            }
        }
    }
}

impl Heap {
    pub fn new(size: isize) -> Self {
        let layout = Layout::from_size_align(size as usize, ALIGNMENT).unwrap();
        let f64_allocator = RefCell::new(F64Allocator::new());
        let buffer = unsafe { alloc::alloc_zeroed(layout) };
        let free_list = RefCell::new(FreeList::new(buffer, size));
        let tag_size = layout::layout_aligned::<Tag>(ALIGNMENT).size() as isize;
        let classes = RefCell::new(ClassList::new());
        let shadow_stack = RefCell::new(vec![vec![]]);
        return Heap {
            buffer,
            f64_allocator,
            size,
            free_list,
            tag_size,
            classes,
            shadow_stack,
        };
    }

    // NOTE(arjun): It is now clear to me that the lifetime parameter on heap values
    // is pointless.
    pub fn f64_to_any(&self, x: f64) -> AnyValue {
        let mut opt_ptr = self.f64_allocator.borrow_mut().alloc(x);
        if let None = opt_ptr {
            self.gc();
            // TODO(arjun): This is a design flaw. We have to borrow_mut again,
            // because self.gc also borrows the f64_allocator. Frankly, we
            // should use a raw pointer.
            opt_ptr = self.f64_allocator.borrow_mut().alloc(x);
        }
        let ptr = unwrap_log(opt_ptr, "out of f64 memory");
        let any = AnyEnum::F64(ptr);
        unsafe { std::mem::transmute(any) }
    }

    /**
     * Allocates a primitive value on the heap, and returns a reference to the
     * tag that precedes the primitive.
     *
     * Primitive values are defined as those with [HasTag] implemented
     */
    pub fn alloc<T: HasTag>(&self, value: T) -> Result<TypePtr<T>, T> {
        self.alloc_tag(T::get_tag(), value)
    }
    pub fn alloc_or_gc<T: HasTag + std::fmt::Debug>(&self, value: T) -> TypePtr<T> {
        match self.alloc(value) {
            Ok(ptr) => ptr,
            Err(value) => {
                self.gc();
                // TODO(luna): grow?
                self.alloc(value).expect("out of memory even after gc")
            }
        }
    }
    fn alloc_tag<T>(&self, tag: Tag, value: T) -> Result<TypePtr<T>, T> {
        let opt_ptr = self
            .free_list
            .borrow_mut()
            .find_free_size(self.tag_size + TypePtr::<T>::size());
        match opt_ptr {
            None => Err(value),
            Some(ptr) => {
                let tag_ptr: *mut Tag = ptr as *mut Tag;
                unsafe { tag_ptr.write(tag) };
                let val_ref = TypePtr::<T>::new(tag_ptr, tag.type_tag, value);
                Ok(val_ref)
            }
        }
    }

    pub fn alloc_object(&self, type_tag: u16) -> Option<ObjectPtr> {
        let object_data = self.alloc_object_data(type_tag)?;
        // we explicitly make sure we're calling TypePtr's get_ptr
        // implementation so we don't get the pointer of the object data. this
        // should work properly now that ObjectDataPtr implements HasTag,
        // but if it gets messed up it leads to insidious memory bugs
        let rv = Some(unsafe { ObjectPtr::new(HeapPtr::get_ptr(&self.alloc(object_data).ok()?)) });
        rv
    }
    pub fn alloc_object_or_gc(&self, type_tag: u16) -> ObjectPtr {
        // TODO(luna): alloc_object isn't really as atomic as it needs to be
        match self.alloc_object(type_tag) {
            Some(ptr) => ptr,
            None => {
                self.gc();
                // TODO(luna): grow?
                self.alloc_object(type_tag)
                    .expect("out of memory even after gc")
            }
        }
    }
    pub fn alloc_object_data_or_gc(&self, type_tag: u16) -> ObjectDataPtr {
        match self.alloc_object_data(type_tag) {
            Some(ptr) => ptr,
            None => {
                self.gc();
                // TODO(luna): grow?
                self.alloc_object_data(type_tag)
                    .expect("out of memory even after gc")
            }
        }
    }
    fn alloc_object_data(&self, type_tag: u16) -> Option<ObjectDataPtr> {
        let num_elements = self.get_class_size(type_tag);
        let elements_size = self.object_data_size(type_tag) as isize;
        let opt_ptr = self
            .free_list
            .borrow_mut()
            .find_free_size(self.tag_size + elements_size);
        match opt_ptr {
            None => None,
            Some(ptr) => {
                let tag_ptr: *mut Tag = ptr as *mut Tag;
                let tag_ref: &mut Tag = unsafe { &mut *tag_ptr };
                *tag_ref = Tag::object(type_tag);
                let values_slice = unsafe { tag_ref.slice_ref::<Option<AnyEnum>>(num_elements) };
                for opt_any in values_slice.iter_mut() {
                    *opt_any = None;
                }
                return Some(unsafe { ObjectDataPtr::new(tag_ptr) });
            }
        }
    }

    pub fn object_data_size(&self, type_tag: u16) -> usize {
        let num_elements = self.get_class_size(type_tag);
        Layout::array::<Option<AnyEnum>>(num_elements)
            .unwrap()
            .size()
    }

    pub fn get_class_size(&self, class_tag: u16) -> usize {
        self.classes.borrow().get_class_size(class_tag)
    }

    pub fn push_shadow_frame(&self, slots: usize) {
        let mut shadow_stack = self.shadow_stack.borrow_mut();
        shadow_stack.push(vec![None; slots]);
    }

    /// # Safety
    ///
    /// calling this without an appropriate push_shadow_frame, or before
    /// all locals in the topmost frame have gone out of scope, results in
    /// [Heap::gc] being unsafe (will free in-use data)
    pub unsafe fn pop_shadow_frame(&self) {
        let mut shadow_stack = self.shadow_stack.borrow_mut();
        shadow_stack.pop();
    }

    pub fn set_in_current_shadow_frame_slot(&self, slot: usize, ptr: *mut Tag) {
        let mut shadow_stack = self.shadow_stack.borrow_mut();
        shadow_stack.last_mut().unwrap()[slot] = Some(ptr);
    }

    pub fn set_any_in_current_shadow_frame_slot(&self, slot: usize, any: AnyValue) {
        match *any {
            AnyEnum::Ptr(ptr) => {
                self.set_in_current_shadow_frame_slot(slot, ptr.get_ptr());
            }
            _ => {
                let mut shadow_stack = self.shadow_stack.borrow_mut();
                shadow_stack.last_mut().unwrap()[slot] = None;
            }
        }
    }

    /// # Safety
    ///
    /// if push_shadow_frame / pop_shadow_frame / set_in_current_shadow_frame_slot were
    /// used correctly (tagged unsafe), this is safe
    pub fn gc(&self) {
        let roots = self
            .shadow_stack
            .borrow()
            .iter()
            .flatten()
            .flatten()
            .map(|refptr| *refptr)
            .collect::<Vec<*mut Tag>>();
        self.mark_phase(roots);
        self.sweep_phase();
    }

    fn mark_phase(&self, roots: Vec<*mut Tag>) {
        let mut current_roots = roots;
        let mut new_roots = Vec::<*mut Tag>::new();

        let mut f64_allocator = self.f64_allocator.borrow_mut();
        f64_allocator.semispace_swap();

        while current_roots.is_empty() == false {
            for root in current_roots.drain(0..) {
                let tag = unsafe { &mut *root };

                if tag.marked == true {
                    continue;
                }
                tag.marked = true;

                let mut any_ptr = unsafe { AnyPtr::new(root) };
                new_roots.append(&mut any_ptr.get_gc_ptrs(self));
                for ptr in any_ptr.get_gc_f64s(self) {
                    unsafe { *ptr = f64_allocator.alloc(**ptr).unwrap() }
                }
            }
            std::mem::swap(&mut current_roots, &mut new_roots);
        }
    }

    fn sweep_phase(&self) {
        use std::borrow::BorrowMut;
        let mut free_list = self.free_list.borrow_mut();

        let borrowed_free_list: &mut FreeList = free_list.borrow_mut();
        let mut free_list_ptr = std::mem::replace(borrowed_free_list, FreeList::Nil);

        let mut ptr: *mut u8 = self.buffer;
        let heap_max = unsafe { self.buffer.offset(self.size) };
        while ptr < heap_max {
            let any_ptr = unsafe { AnyPtr::new(std::mem::transmute(ptr)) };
            let tag_ref = unsafe { &mut *any_ptr.get_ptr() };
            let size = self.tag_size as usize + any_ptr.get_data_size(self);
            if tag_ref.marked {
                tag_ref.marked = false;
            } else {
                free_list_ptr = free_list_ptr.insert(ptr, size as isize);
                // drop any rust memory that may exist
                any_ptr.final_drop();
            }
            ptr = unsafe { ptr.add(size) };
        }
        *borrowed_free_list = free_list_ptr;
    }

    #[cfg(test)]
    pub fn raw(&self) -> &[u8] {
        return unsafe { std::slice::from_raw_parts(self.buffer, self.size as usize) };
    }
}
