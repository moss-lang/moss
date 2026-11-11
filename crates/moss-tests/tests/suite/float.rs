//! Floats (D67) as programs see them: the oracle's float instructions
//! against what Wasm specifies, held to the compiled programs.
//!
//! Ported from the Python bootstrap's `tests/test_float.py`. Its tests of
//! the Python interpreter's float helpers (`f32`, `fdiv`, `fround`,
//! `trunc_to_int`, ...) went with that interpreter; what is left is what
//! a program can observe.

use crate::wasm_backend::FLOAT_OUT;
use moss_tests::*;

/// What tests/wasi/newton.moss prints: the same report at f64, then at
/// f32.
const NEWTON_OUT: &str = "1500000000\n1416666667\n1414213562\n1414213562\n0\n333333333\n\
     -100000000\n16777217\n24-2-2\n2147483647\nnynnyy0\n\
     1500000000\n1416666752\n1414213504\n1414213504\n0\n333333344\n\
     -100000000\n16777216\n24-2-2\n2147483647\nnynnyy0\n";

/// NaN is unordered, through the instructions themselves, as a program
/// sees them; both runners agree.
#[test]
fn nan_comparisons() {
    let entry = source(
        r#"import "{root}/lib/wasm.moss" as w use Wasm;
import "{root}/lib/wasistd.moss" use Branch, i32_bool;
import "{root}/lib/wasi.moss" use WasiStd;
import "{root}/lib/wasip1.moss" use WasiCore;
assume Std, Wasm, Branch {
  fn yes(b: Bool) { if b { putchar(char::y) } else { putchar(char::n) } }
  fn run() {
    let nan = w::f64_div(w::f64_convert_i32_s(w::i32_zero), w::f64_convert_i32_s(w::i32_zero));
    let one = w::f64_convert_i32_s(w::i32_one);
    yes(i32_bool(w::f64_eq(nan, nan)));
    yes(i32_bool(w::f64_ne(nan, nan)));
    yes(i32_bool(w::f64_lt(nan, one)));
    yes(i32_bool(w::f64_gt(nan, one)));
    yes(i32_bool(w::f64_le(nan, nan)));
    yes(i32_bool(w::f64_ge(one, nan)));
    let nan32 = w::f32_demote_f64(nan);
    yes(i32_bool(w::f32_eq(nan32, nan32)));
    yes(i32_bool(w::f32_ne(nan32, nan32)));
    yes(i32_bool(w::f64_eq(w::f64_min(nan, one), w::f64_min(nan, one))));
  }
}
assume Wasm, WasiCore, Branch {
  fn main() {
    bind WasiStd;
    run();
  }
}
"#,
    );
    assert_eq!(run(&entry, &[]), "nynnnnnyn");
    assert_eq!(oracle_run(&entry, &[]), "nynnnnnyn");
}

/// tests/wasi/float.moss, which the compiler compiles
/// (`wasm_backend::f32_and_f64_are_scalar_types`), prints the same thing
/// under the oracle.
#[test]
fn the_interpreter_runs_the_float_program() {
    assert_eq!(oracle_run("tests/wasi/float.moss", &[]), FLOAT_OUT);
}

/// `Float` as `WasiStd` and `WasiFloats32` provide it — Moss over the
/// oracle's instructions, so f32 rounding here is the oracle's.
#[test]
fn the_interpreter_runs_float_over_wasi() {
    assert_eq!(oracle_run("tests/wasi/newton.moss", &[]), NEWTON_OUT);
}
