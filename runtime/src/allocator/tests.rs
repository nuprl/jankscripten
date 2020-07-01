use super::class_list::Class;
use super::*;
use crate::string::str_as_ptr;
use wasm_bindgen_test::*;

#[test]
#[wasm_bindgen_test]
fn allocs() {
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    let x = heap.alloc(32).expect("first alloc");
    assert_eq!(*x.get(), 32);
    let y = heap.alloc(64).expect("second alloc");
    assert_eq!(*y.get(), 64);
    assert_eq!(*x.get(), 32);
    assert!(heap.alloc(12).is_none());
}

#[test]
#[wasm_bindgen_test]
fn trivial_gc() {
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    let x = heap.alloc(32).expect("first allocation failed");
    assert_eq!(
        *x.get(),
        32,
        "first value was not written to the heap correctly"
    );
    let y = heap.alloc(64).expect("second allocation failed");
    assert_eq!(
        *y.get(),
        64,
        "second value was not written to the heap correctly"
    );
    assert_eq!(
        *x.get(),
        32,
        "second allocation corrupted the first value on the heap"
    );
    assert!(
        heap.alloc(12).is_none(),
        "third allocation should fail due to lack of space"
    );
    heap.push_shadow_frame(&[x.get_ptr()]);
    unsafe { heap.gc() };
    let z = heap.alloc(128).expect("GC failed to free enough memory");
    assert_eq!(*z.get(), 128, "allocation should succeed after GC");
    assert_eq!(
        *x.get(),
        32,
        "allocation after GC corrupted the first value on the heap"
    );
}

#[test]
#[wasm_bindgen_test]
fn update_prims() {
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    heap.alloc(32).expect("first alloc");
    let mut ptr = heap.alloc(64).expect("second alloc");
    *ptr.get_mut() = 128;
    let raw = heap.raw();
    assert_eq!(raw[ALIGNMENT], 32);
    assert_eq!(raw[ALIGNMENT * 3], 128);
}

#[test]
#[wasm_bindgen_test]
fn alloc_container1() {
    let heap = Heap::new(64);
    let empty_type = heap
        .classes
        .borrow_mut()
        .new_class_type(Class::new());
    let one_type = heap
        .classes
        .borrow_mut()
        .transition(empty_type, str_as_ptr("x"));
    let type_tag = heap
        .classes
        .borrow_mut()
        .transition(one_type, str_as_ptr("y"));
    heap.alloc(32).expect("first alloc");
    let container = heap.alloc_class(type_tag).expect("second alloc");
    assert_eq!(container.read_at(&heap, 0), None);
    assert_eq!(container.read_at(&heap, 1), None);
}

#[test]
#[wasm_bindgen_test]
fn insert_object() {
    let heap = Heap::new(64);
    let empty_type = heap
        .classes
        .borrow_mut()
        .new_class_type(Class::new());
    let x = heap.alloc(32).expect("first alloc");
    let empty = heap.alloc_class(empty_type).expect("second alloc");
    let with_x = empty
        .insert(&heap, str_as_ptr("x"), x)
        .expect("couldn't allocate again");
    match with_x.read_at(&heap, 0).expect("no 0").view() {
        HeapRefView::I32(i) => assert_eq!(*i, 32),
        _ => panic!("not an int"),
    }
}

#[test]
fn alloc_container2() {
    let heap = Heap::new(40);
    let empty_type = heap
        .classes
        .borrow_mut()
        .new_class_type(Class::new());
    let one_type = heap
        .classes
        .borrow_mut()
        .transition(empty_type, str_as_ptr("x"));
    let type_tag = heap
        .classes
        .borrow_mut()
        .transition(one_type, str_as_ptr("y"));
    let container = heap.alloc_class(type_tag).expect("second alloc");
    let mut x = heap.alloc(200).expect("second alloc");
    container.write_at(&heap, 0, x);
    *x.get_mut() = 100;
    let elt = container.read_at(&heap, 0).expect("read");
    match elt.view() {
        HeapRefView::I32(prim) => assert_eq!(100, *prim.get()),
        _ => assert!(false),
    }
}

#[test]
fn alloc_container3() {
    let heap = Heap::new(40);
    let empty_type = heap
        .classes
        .borrow_mut()
        .new_class_type(Class::new());
    let one_type = heap
        .classes
        .borrow_mut()
        .transition(empty_type, str_as_ptr("x"));
    let type_tag = heap
        .classes
        .borrow_mut()
        .transition(one_type, str_as_ptr("y"));
    let container = heap.alloc_class(type_tag).expect("second alloc");
    let mut x = heap.alloc(200).expect("second alloc");
    container.write_at(&heap, 1, x);
    *x.get_mut() = 100;
    let elt = container.read_at(&heap, 1).expect("read");
    match elt.view() {
        HeapRefView::I32(prim) => assert_eq!(100, *prim.get()),
        _ => assert!(false),
    }
}

#[wasm_bindgen_test]
#[test]
fn string_read_read_write() {
    // String = Vec<u8> = {addr, size, alloc}
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    let mut x = heap.alloc(String::from("steven")).expect("first alloc");
    assert_eq!(x.get().as_str(), "steven");
    assert_eq!(x.get().as_str(), "steven");
    x.get_mut().push_str(" universe");
    assert_eq!(x.get().as_str(), "steven universe");
}

#[test]
fn string_ptr_drop_read() {
    // String = Vec<u8> = {addr, size, alloc}
    let heap = Heap::new((ALIGNMENT * 4) as isize);
    let x = heap.alloc(String::from("steven")).expect("first alloc");
    let copied = x.clone();
    drop(copied);
    assert_eq!(x.get().as_str(), "steven");
    drop(x);
}

#[test]
fn too_big() {
    // String = Vec<u8> = {addr, size, alloc}
    let heap = Heap::new((ALIGNMENT * 2) as isize);
    assert!(heap.alloc(String::from("hi")).is_none());
}
