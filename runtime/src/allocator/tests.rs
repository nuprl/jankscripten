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
    assert!(heap.alloc(12).is_err());
}

// #[test]
// #[wasm_bindgen_test]
// fn gc_enter_exit() {
//     let heap = Heap::new((ALIGNMENT * 4) as isize);
//     heap.push_shadow_frame(&[]);
//     let x = heap.alloc(32).expect("first allocation failed");
//     assert_eq!(
//         *x.get(),
//         32,
//         "first value was not written to the heap correctly"
//     );
//     heap.push_shadow_frame(&[]);
//     let y = heap.alloc(64).expect("second allocation failed");
//     assert_eq!(
//         *y.get(),
//         64,
//         "second value was not written to the heap correctly"
//     );
//     assert_eq!(
//         *x.get(),
//         32,
//         "second allocation corrupted the first value on the heap"
//     );
//     assert!(
//         heap.alloc(12).is_err(),
//         "third allocation should fail due to lack of space"
//     );
//     unsafe { heap.pop_shadow_frame() };
//     heap.gc();
//     let z = heap.alloc(128).expect("GC failed to free enough memory");
//     assert_eq!(*z.get(), 128, "allocation should succeed after GC");
//     assert_eq!(
//         *x.get(),
//         32,
//         "allocation after GC corrupted the first value on the heap"
//     );
// }

// #[test]
// #[wasm_bindgen_test]
// fn alloc_or_gc_gcs() {
//     let heap = Heap::new((ALIGNMENT * 4) as isize);
//     heap.push_shadow_frame(&[]);
//     let x = heap.alloc(32).expect("first allocation failed");
//     assert_eq!(
//         *x.get(),
//         32,
//         "first value was not written to the heap correctly"
//     );
//     heap.push_shadow_frame(&[]);
//     let y = heap.alloc(64).expect("second allocation failed");
//     assert_eq!(
//         *y.get(),
//         64,
//         "second value was not written to the heap correctly"
//     );
//     assert_eq!(
//         *x.get(),
//         32,
//         "second allocation corrupted the first value on the heap"
//     );
//     assert!(
//         heap.alloc(12).is_err(),
//         "third allocation should fail due to lack of space"
//     );
//     unsafe { heap.pop_shadow_frame() };
//     let z = heap.alloc_or_gc(128);
//     assert_eq!(
//         *z.get(),
//         128,
//         "allocation should succeed after automatic GC"
//     );
//     assert_eq!(
//         *x.get(),
//         32,
//         "allocation after GC corrupted the first value on the heap"
//     );
// }

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
    let heap = Heap::new(128);
    let one_type = heap.classes.borrow_mut().transition(0, str_as_ptr("x"));
    let type_tag = heap
        .classes
        .borrow_mut()
        .transition(one_type, str_as_ptr("y"));
    heap.alloc(32).expect("first alloc");
    let container = heap.alloc_object(type_tag).expect("second alloc");
    assert_eq!(container.read_at(&heap, 0), None);
    assert_eq!(container.read_at(&heap, 1), None);
}

#[test]
#[wasm_bindgen_test]
fn insert_object() {
    let heap = Heap::new(128);
    let mut obj = heap.alloc_object(0).expect("second alloc");
    let mut cache = -1;
    assert_eq!(
        obj.insert(&heap, str_as_ptr("x"), AnyEnum::I32(32).into(), &mut cache),
        AnyEnum::I32(32).into()
    );
    assert_eq!(
        obj.insert(&heap, str_as_ptr("x"), AnyEnum::I32(32).into(), &mut cache),
        AnyEnum::I32(32).into()
    );
    assert!(matches!(
        obj.get(&heap, str_as_ptr("x"), &mut cache).expect("no x"),
        AnyEnum::I32(32)
    ));
    assert!(matches!(
        obj.get(&heap, str_as_ptr("x"), &mut -1).expect("no x"),
        AnyEnum::I32(32)
    ));
}

#[test]
fn alloc_container2() {
    let heap = Heap::new(128);
    let empty_type = heap.classes.borrow_mut().new_class_type(Class::new());
    let one_type = heap
        .classes
        .borrow_mut()
        .transition(empty_type, str_as_ptr("x"));
    let type_tag = heap
        .classes
        .borrow_mut()
        .transition(one_type, str_as_ptr("y"));
    let container = heap.alloc_object(type_tag).expect("second alloc");
    let mut x: AnyJSPtr<'_> = heap.alloc(AnyEnum::I32(200).into()).expect("second alloc");
    container.write_at(&heap, 0, AnyEnum::Ptr(x.into()).into());
    *x.get_mut() = AnyEnum::I32(100).into();
    let elt = container.read_at(&heap, 0).expect("read");
    match elt {
        AnyEnum::Ptr(ptr) => match ptr.view() {
            HeapRefView::Any(any) => assert_eq!(AnyEnum::I32(100), **any.get()),
            _ => panic!("not an any"),
        },
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
    assert!(heap.alloc(String::from("hi")).is_err());
}
