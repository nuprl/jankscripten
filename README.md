Prerequisites
============


1. [Rust](https://rustup.rs/)

2. [Node](https://nodejs.org/en/) (for running tests and benchmarks)

3. `rustup target add wasm32-unknown-unknown` (Rust's WebAssembly backend)

4. `cargo install wasm-bindgen-cli` (allows Rust unit tests to run in WebAssembly)

Building
========

```
cargo build
```

Testing
=======

```
cargo test
(cd runtime && cargo test) # Runs tests using WebAssembly
```
