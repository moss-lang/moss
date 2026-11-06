//! Native WebAssembly optimization shared by the driver and integration tests.
pub mod binaryen;

#[cfg(test)]
#[allow(dead_code)]
mod wasmtime;
