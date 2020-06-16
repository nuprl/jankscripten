# To build

```
cargo build --target wasm32-unknown-unknown
```

# To test on Wasm

```
cargo install wasm-bindgen-cli # just need to do this once
cargo test --target wasm32-unknown-unknown
```

note that output goes to [../target] because the super crate owns this one

more details on writing the runtime at [src/lib.rs] or `cargo doc --open`
