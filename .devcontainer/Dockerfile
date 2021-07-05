# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.177.0/containers/rust/.devcontainer/base.Dockerfile

FROM mcr.microsoft.com/vscode/devcontainers/rust:0-1

RUN rustup toolchain add 1.51.0
RUN rustup default 1.51.0
RUN rustup target add wasm32-unknown-unknown
RUN cargo install wasm-bindgen-cli

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    libssl-dev pkg-config libz3-dev