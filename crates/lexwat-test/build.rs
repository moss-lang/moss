//! Generate the merged / optimized `.wasm` artifacts from the committed `.wat`
//! sources, instead of checking compiled binaries into git.
//!
//! Uses binaryen's `wasm-merge` and `wasm-opt`, which the Nix dev shell provides
//! (see `flake.nix`).

use std::{env, path::PathBuf, process::Command};

fn run(tool: &str, args: &[&str]) {
    let status = Command::new(tool)
        .args(args)
        .status()
        .unwrap_or_else(|e| panic!("failed to spawn {tool} (is the nix dev shell active?): {e}"));
    assert!(status.success(), "{tool} {args:?} failed");
}

fn main() {
    for f in ["lex_nomem.wat", "driver.wat", "driver2.wat", "lex_selfsource.wat"] {
        println!("cargo:rerun-if-changed={f}");
    }

    let manifest = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let out = PathBuf::from(env::var("OUT_DIR").unwrap());
    let p = |f: &str| manifest.join(f).to_str().unwrap().to_owned();
    let o = |f: &str| out.join(f).to_str().unwrap().to_owned();

    // driver.wat exports next_byte -> internalize the byte fetch into the lexer.
    let (lex, drv, merged) = (p("lex_nomem.wat"), p("driver.wat"), o("merged.wasm"));
    run("wasm-merge", &[&lex, "lexer", &drv, "env", "-o", &merged]);

    // driver2.wat also imports next_token and loops it; two memories.
    let (drv2, merged2) = (p("driver2.wat"), o("merged2.wasm"));
    run("wasm-merge", &[&lex, "lexer", &drv2, "env", "--enable-multimemory", "-o", &merged2]);

    // wasm-opt -O3 the self-source lexer and the merged2 module.
    let (selfsrc, self_opt) = (p("lex_selfsource.wat"), o("self_opt.wasm"));
    run("wasm-opt", &["-O3", &selfsrc, "-o", &self_opt]);
    let merged2_opt = o("merged2_opt.wasm");
    run("wasm-opt", &["-O3", "--enable-multimemory", &merged2, "-o", &merged2_opt]);
}
