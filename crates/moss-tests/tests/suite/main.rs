//! Moss's test suite: `cargo test -p moss-tests`.
//!
//! Needs a seed compiler and the oracle (see `moss_tests::seed` and
//! `moss_tests::oracle_path`) and the tools of the Nix dev shell. Each
//! module is one area; the comment at the top of each says what it holds
//! the compiler to.

mod agreement;
mod backend;
mod errors;
mod examples;
mod fixpoint;
mod float;
mod gensrc;
mod language;
mod selfhosted;
mod runtime_platform;
mod native_platform;
mod validate;
mod wasm_backend;

mod solved_merges;

mod bindings;
mod string_literals;
mod typed_reference_unions;
