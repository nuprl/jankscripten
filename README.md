Prerequisites
============


1. [Rust](https://rustup.rs/)

2. [Node](https://nodejs.org/en/) (for running tests and benchmarks)

   We require Node 11 or higher.

3. `rustup target add wasm32-unknown-unknown` (Rust's WebAssembly backend)

4. `cargo install wasm-bindgen-cli` (allows Rust unit tests to run in WebAssembly)

## Auxiliary Dependencies

We depend on Rust packages that link with *libssl* and rely on *pkg-config*.
Those packages fail to build unless you have the libssl headers and the
pkg-config tool installed. On Ubuntu, run `sudo apt-get install libssl-dev
pkg-config` to install them.

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

The Chrome debugger uses source maps correctly to show the original Rust code.
You can use Visual Studio Code or Edge, but source maps do not appear to work
correctly. See the Node [Debugging
Guide](https://nodejs.org/en/docs/guides/debugging-getting-started/) for more
information.
