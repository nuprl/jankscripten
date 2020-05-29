# building

    cargo build --target wasm32-unknown-unknown --release

note that output goes to [../target] because the super crate owns this one

more details on writing the runtime at [src/lib.rs] or `cargo doc --open`
