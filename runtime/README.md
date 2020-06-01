https://rustwasm.github.io/docs/book/introduction.html

# Setup

1. [`wasm-pack`](https://rustwasm.github.io/wasm-pack/installer/)

# Build

1. We will use `wasm-pack` to build everything. This ensures that 1) the correct
    stuff is installed, 2) `cargo` compiles to a `.wasm` binary, and 3)
    `wasm-bindgen` generates the corresponding JavaScript API.

    ```
    $ wasm-pack build
    ```