(module
  ;; (param out: &Num i32)
  (import "runtime" "num_i32" (func $num_i32 (param i32) (param i32)))
  ;; (param out: &Num f64)
  (import "runtime" "num_f64" (func $num_f64 (param i32) (param f64)))
  ;; Num = (kind: i32 _align: i32 lo: i32 hi: i32)
  ;; (param out: &Num a: &Num b: &Num)
  (import "runtime" "add_num" (func $add_num (param i32 i32 i32)))
  ;; (param num: &Num) (result i32)
  (import "runtime" "num_as_i32" (func $num_as_i32 (param i32) (result i32)))
  (import "runtime" "memory" (memory 1))

  
  ;; sample program: (Num(4) + Num(6) => Num(10)).unwrap() => 10
  (func (export "main") (result i32)

    ;; Num(4) -> [0]
    i32.const 0
    i32.const 4
    call $num_i32
    ;; Num(6) -> [16]
    i32.const 16
    i32.const 6
    call $num_i32
    ;; for call to add:
    ;; out address = [32]
    i32.const 32
    ;; a
    i32.const 0
    ;; b
    i32.const 16
    call $add_num

    ;; now convert back into i32 for return
    i32.const 32
    call $num_as_i32
  )

)


