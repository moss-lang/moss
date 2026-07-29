use std::{env, path::Path};

fn main() {
    println!("cargo:rerun-if-env-changed=BINARYEN_LIB_DIR");
    println!("cargo:rerun-if-env-changed=BINARYEN_STATIC");
    println!("cargo:rerun-if-env-changed=BINARYEN_STATIC_STDCPP");

    if let Some(dir) = env::var_os("BINARYEN_LIB_DIR") {
        println!(
            "cargo:rustc-link-search=native={}",
            Path::new(&dir).display()
        );
    }
    let static_binaryen = env::var("BINARYEN_STATIC").is_ok_and(|value| value != "0");
    println!(
        "cargo:rustc-link-lib={}binaryen",
        if static_binaryen { "static=" } else { "" }
    );

    if env::var("CARGO_CFG_TARGET_ENV").as_deref() != Ok("msvc") {
        let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
        let cpp = if target_os == "macos" {
            "c++"
        } else {
            "stdc++"
        };
        let static_cpp = env::var("BINARYEN_STATIC_STDCPP").is_ok_and(|value| value != "0");
        println!(
            "cargo:rustc-link-lib={}{}",
            if static_cpp { "static=" } else { "" },
            cpp
        );
    }
}
