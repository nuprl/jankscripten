//! managed allocation. most allocations should be made through [Heap]

use crate::{AnyEnum, AnyValue};
use std::alloc;
use std::alloc::Layout;
use std::cell::RefCell;
mod class_list;
mod constants;
mod env;
mod heap_values;
mod layout;
mod object_ptr;
mod string;
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

#[derive(Debug, Clone, Copy)]
struct Block {
    start: *mut u8,
    size: isize,
}

#[derive(Clone, Debug)]
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
        // this has to be iterative because no tail call optimization in wasm
        // (this might be an issue for jankscripten as well tbh)
        let mut to = &mut self;
        loop {
            match to {
                FreeList::Nil => {
                    *to = FreeList::new(start, size);
                    break;
                }
                FreeList::Block(block, _) => {
                    let new_block_end = unsafe { start.add(size as usize) };
                    let old_block_end = unsafe { block.start.offset(block.size) };
                    if new_block_end < block.start {
                        // +-----------+------------+-------+
                        // | new block | used space | block |
                        // +-----------+------------+-------+
                        log!("    b");
                        let new_block = Block { start, size };
                        let real_to = std::mem::replace(to, FreeList::Nil);
                        *to = FreeList::Block(new_block, Box::new(real_to));
                        break;
                    } else if new_block_end == block.start {
                        // +-----------+-------+
                        // | new block | block |
                        // +-----------+-------+
                        log!("g");
                        block.start = start;
                        block.size = block.size + size;
                        break;
                    // note that this used to be new_block_end, which never
                    // happens. probably a typo
                    } else if start == old_block_end {
                        // +-------+-----------+
                        // | block | new block |
                        // +-------+-----------+
                        log!("  t");
                        block.size += size;
                        // if this is the case, we might be able to combine
                        // parts of our free list. this prevents annoying call
                        // stack problems that can't even be avoided by iterating
                        // because it happens in Clone::clone()
                        if let FreeList::Block(block, rest) = to {
                            if let FreeList::Block(after_block, after_rest) = rest.as_mut() {
                                if new_block_end >= after_block.start {
                                    // +-------+-----------+-------------+
                                    // | block | new block | after block |
                                    // +-------+-----------+-------------+
                                    log!("GREATTT~~~~!!!!");
                                    block.size += after_block.size;
                                    *rest.as_mut() = std::mem::replace(after_rest, FreeList::Nil);
                                } else {
                                    //log!("{:x?} vs {:x?}", new_block_end, after_block.start);
                                    //log!("{:x?}", self);
                                }
                            }
                        } else {
                            log_panic!("unreachable");
                        }
                        break;
                    } else if start == block.start && size == block.size {
                        // This can happen, because this is the world's worst
                        // mark and sweep collector.
                        log!("doing the weird free list thing");
                        break;
                    } else {
                        // +-------+-----------------------------+-----------+
                        // | block | mix of used and free blocks | new block |
                        // +-------+-----------------------------+-----------+
                        // borrow checker annoyingness
                        if let FreeList::Block(_, rest) = to {
                            to = rest;
                        } else {
                            log_panic!("unreachable");
                        }
                    }
                }
            }
        }
        self
    }

    /**
     * Returns the address of a free block of size `size`, and updates the
     * free size.
     */
    pub fn find_free_size(&mut self, size: isize) -> Option<*mut u8> {
        let mut to = self;
        loop {
            match to {
                FreeList::Block(block, _) => {
                    if size == block.size {
                        let addr = block.start;
                        if let FreeList::Block(_, next) = to {
                            let next = std::mem::replace(next.as_mut(), FreeList::Nil);
                            *to = next;
                            break Some(addr);
                        } else {
                            log_panic!("unreachable");
                        }
                    } else if size < block.size {
                        block.size = block.size - size;
                        let addr = block.start;
                        block.start = unsafe { block.start.add(size as usize) };
                        break Some(addr);
                    } else
                    /* size > block.size */
                    {
                        if let FreeList::Block(_, next) = to {
                            to = next.as_mut();
                        } else {
                            log_panic!("unreachable");
                        }
                    }
                }
                FreeList::Nil => {
                    break None;
                }
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
        let shadow_stack = RefCell::new(vec![]);
        log!("{:x?}", unsafe { buffer.offset(size) });
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
        AnyEnum::F64(self.alloc_f64_or_gc(x)).into()
    }

    pub fn alloc_f64_or_gc(&self, x: f64) -> *const f64 {
        let mut opt_ptr = self.f64_allocator.borrow_mut().alloc(x);
        if let None = opt_ptr {
            self.gc();
            // TODO(arjun): This is a design flaw. We have to borrow_mut again,
            // because self.gc also borrows the f64_allocator. Frankly, we
            // should use a raw pointer.
            opt_ptr = self.f64_allocator.borrow_mut().alloc(x);
        }
        unwrap_log(opt_ptr, "out of f64 memory")
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
        Some(unsafe { ObjectPtr::new(HeapPtr::get_ptr(&self.alloc(object_data).ok()?)) })
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
        let elements_size = self.object_data_size(num_elements) as isize;
        let tag_ptr = unsafe { self.alloc_slice(Tag::object(type_tag), elements_size) }?;
        let values_slice = unsafe { (*tag_ptr).slice_ref::<Option<AnyEnum>>(num_elements) };
        for opt_any in values_slice.iter_mut() {
            *opt_any = None;
        }
        Some(unsafe { ObjectDataPtr::new(tag_ptr) })
    }
    pub fn alloc_str(&self, s: &str) -> Option<StringPtr> {
        let from_str = s.as_ptr();
        // + 4 for the length (not the tag, which isn't included)
        let size = s.len() + 4;
        unsafe {
            let tag_ptr = self.alloc_slice(Tag::with_type(TypeTag::String), size as isize)?;
            let len_ptr = tag_ptr.add(DATA_OFFSET) as *mut u32;
            len_ptr.write(u32::to_le(s.len() as u32));
            let into_str = len_ptr.add(1) as *mut u8;
            std::ptr::copy_nonoverlapping(from_str, into_str, s.len());
            Some(StringPtr::new(tag_ptr))
        }
    }
    pub fn alloc_str_or_gc(&self, s: &str) -> StringPtr {
        match self.alloc_str(s) {
            Some(ptr) => ptr,
            None => {
                self.gc();
                // TODO(luna): grow?
                self.alloc_str(s).expect("out of memory even after gc")
            }
        }
    }
    /// # Safety
    ///
    /// [alloc_env_or_gc]
    unsafe fn alloc_env(&self, length: u32, fn_obj: ObjectPtr) -> Option<EnvPtr> {
        // + 4 for the length (not the tag, which isn't included)
        let size = std::mem::size_of::<AnyEnum>() * length as usize // EnvItems
            + 4  // Len
            + std::mem::size_of::<ObjectPtr>(); // Fn obj pointer
        let tag_ptr = self.alloc_slice(Tag::with_type(TypeTag::Env), size as isize)?;
        Some(EnvPtr::init(tag_ptr, length, fn_obj))
    }
    /// SAFETY:
    ///
    /// this is unsafe for the same reason as EnvPtr::new(); it makes GC do
    /// UB unless/until you fill in the environment with values
    pub unsafe fn alloc_env_or_gc(&self, length: u32, fn_obj: ObjectPtr) -> EnvPtr {
        match self.alloc_env(length, fn_obj) {
            Some(ptr) => ptr,
            None => {
                self.gc();
                self.alloc_env(length, fn_obj)
                    .expect("out of memory even after gc")
            }
        }
    }
    /// allocate a tag immediately followed by slice of memory of a fixed
    /// size in bytes
    ///
    /// # Safety
    ///
    /// size must be exactly AnyPtr::new(&tag).get_data_size(). size must
    /// be aligned
    ///
    /// TODO(luna): i want to provide resizable Array within the allocator
    /// because it shouldn't be hard with everything we currently have and
    /// it prevents OOM-before-allocator-knows-it. then we could back Object
    /// with an Array, delete ObjectDataPtr, and get O(n) cached inserts
    unsafe fn alloc_slice(&self, tag: Tag, size: isize) -> Option<*mut Tag> {
        let ptr = self
            .free_list
            .borrow_mut()
            .find_free_size(self.tag_size + size)?;
        let tag_ptr = ptr as *mut Tag;
        tag_ptr.write(tag);
        Some(tag_ptr)
    }

    pub fn object_data_size(&self, num_elements: usize) -> usize {
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

    pub fn set_in_shadow_frame_slot(&self, frame: usize, slot: usize, ptr: Option<*mut Tag>) {
        let mut shadow_stack = self.shadow_stack.borrow_mut();
        shadow_stack[frame][slot] = ptr;
    }
    pub fn set_in_current_shadow_frame_slot(&self, slot: usize, ptr: Option<*mut Tag>) {
        let mut shadow_stack = self.shadow_stack.borrow_mut();
        shadow_stack.last_mut().unwrap()[slot] = ptr;
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
        self.mark_phase(roots, |_| ());
        self.sweep_phase();
    }

    /// # Safety
    ///
    /// if push_shadow_frame / pop_shadow_frame / set_in_current_shadow_frame_slot were
    /// used correctly (tagged unsafe), this is safe
    pub fn heap_dump(&self) {
        let roots = self
            .shadow_stack
            .borrow()
            .iter()
            .flatten()
            .flatten()
            .map(|refptr| *refptr)
            .collect::<Vec<*mut Tag>>();
        log!("===== BEGIN JANKYSCRIPT HEAP DUMP =====");
        self.mark_phase(roots, |any| {
            log!("{:x?}    {:?}", any.get_ptr(), any.view())
        });
        log!("=====    BEGIN HEAP DUMP SWEEP    =====");
        self.sweep_phase();
        log!("=====  END JANKYSCRIPT HEAP DUMP  =====");
    }

    fn mark_phase(&self, roots: Vec<*mut Tag>, dump: impl Fn(AnyPtr)) {
        log!("=====    BEGIN JANKYPSCRIPT GC    =====");
        let mut current_roots = roots;
        let mut new_roots = Vec::<*mut Tag>::new();

        let mut f64_allocator = self.f64_allocator.borrow_mut();
        f64_allocator.semispace_swap();

        // this is just for debugging, but i'm going to leave it in because i
        // don't foresee not needing it any time soon
        let mut count = 0;
        while current_roots.is_empty() == false {
            for root in current_roots.drain(0..) {
                let tag = unsafe { &mut *root };

                if tag.marked == true {
                    continue;
                }
                tag.marked = true;
                count += 1;

                let any_ptr = unsafe { AnyPtr::new(root) };
                dump(any_ptr);

                let (mut tags, f64s) = any_ptr.get_gc_ptrs(self);
                new_roots.append(&mut tags);
                for ptr in f64s {
                    unsafe { *ptr = f64_allocator.alloc(**ptr).unwrap() }
                }
            }
            std::mem::swap(&mut current_roots, &mut new_roots);
        }
        log!("===== MARKED {} OBJECTS =====", count);
    }

    fn sweep_phase(&self) {
        use std::borrow::BorrowMut;
        let mut free_list = self.free_list.borrow_mut();

        // we construct a little zipper (i think) that has the free list we've
        // already considered in before and the rest of the free list in after. as
        // we loop, we add new and old free blocks to before, removing old ones
        // from after, until after is Nil and before is our new free list
        let mut free_list_before = FreeList::Nil;
        // technically free_list_before is always being added exactly to its
        // semi-tail. if i could figure out how to wrangle the borrow checker into
        // letting me keep a reference to it, insert wouldn't even have to loop
        let free_list_after: &mut FreeList = free_list.borrow_mut();

        let mut ptr: *mut u8 = self.buffer;
        let heap_max = unsafe { self.buffer.offset(self.size) };
        let mut count = 0;
        while ptr < heap_max {
            match free_list_after {
                FreeList::Block(block, ref mut rest) => {
                    let block_end = unsafe { block.start.offset(block.size) };
                    if ptr < block.start {
                        // ptr is definitely currently allocated
                    } else if ptr >= block.start && ptr < block_end {
                        // ptr is already free. skip to the end of the block
                        ptr = block_end;
                        free_list_before = free_list_before.insert(block.start, block.size);
                        let rest2 = std::mem::replace(rest.as_mut(), FreeList::Nil);
                        *free_list_after = rest2;
                        continue;
                    } else {
                        // ptr is past the end of this block, which means
                        // nothing was free inside this free block. that
                        // shouldn't happen
                        log_panic!("nothing free was free");
                    }
                }
                // ptr is definitely currently allocated
                FreeList::Nil => (),
            }
            let any_ptr = unsafe { AnyPtr::new(std::mem::transmute(ptr)) };
            let tag_ref = unsafe { &mut *any_ptr.get_ptr() };
            let size = self.tag_size as usize + any_ptr.get_data_size(self);
            if tag_ref.marked {
                tag_ref.marked = false;
            } else {
                if tag_ref.type_tag != TypeTag::DynObject {
                    log!("free {:x?}: {:?} (size: {})", ptr, any_ptr.view(), size);
                }
                free_list_before = free_list_before.insert(ptr, size as isize);
                // drop any rust memory that may exist
                any_ptr.final_drop();
                count += 1;
            }
            ptr = unsafe { ptr.add(size) };
        }
        // everything is before us now
        *free_list = free_list_before;
        log!("=====      FREED {} OBJECTS     =====", count);
        log!("=====      END JANKYSCRIPT GC     =====");
    }

    #[cfg(test)]
    pub fn raw(&self) -> &[u8] {
        return unsafe { std::slice::from_raw_parts(self.buffer, self.size as usize) };
    }
}
