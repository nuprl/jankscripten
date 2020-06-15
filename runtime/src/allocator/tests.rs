use super::*;
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
    let mut heap = Heap::new(64);
    let type_tag = heap.new_container_type(2);
    heap.alloc(32).expect("first alloc");
    let container = heap.alloc_container(type_tag).expect("second alloc");
    assert_eq!(container.read_at(&heap, 0), None);
    assert_eq!(container.read_at(&heap, 1), None);
}

#[test]
fn alloc_container2() {
    let mut heap = Heap::new(40);
    let type_tag = heap.new_container_type(2);
    let container = heap.alloc_container(type_tag).expect("second alloc");
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
    let mut heap = Heap::new(40);
    let type_tag = heap.new_container_type(2);
    let container = heap.alloc_container(type_tag).expect("second alloc");
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
