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
(cd runtime && cargo build)
```

Running
=======

To compile `filename.ext` to WebAssembly:

```
./bin/jankscripten compile filename.ext
```

*NOTE:* The supported extensions are .js and .notwasm.

To run a compiled WebAssembly program with the jankscripten runtime:

```
./bin/run-node filename.wasm
```

Testing
=======

```
cargo test
(cd runtime && cargo test) # Runs tests using WebAssembly
```

Debugging
=========

To debug or profile a compiled WebAssembly program:

```
node --inspect-brk bin/run FILENAME.wasm
```

See the Node [Debugging Guide](https://nodejs.org/en/docs/guides/debugging-getting-started/)
for more information.
