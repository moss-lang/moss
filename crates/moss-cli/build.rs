use std::{env, path::Path};

fn main() {
    println!("cargo:rustc-check-cfg=cfg(moss_embedded_compiler)");
    println!("cargo:rerun-if-env-changed=MOSS_COMPILER_CWASM");
    if env::var_os("MOSS_COMPILER_CWASM").is_some() {
        println!("cargo:rustc-cfg=moss_embedded_compiler");
    }
    println!("cargo:rerun-if-env-changed=BINARYEN_LIB_DIR");
    println!("cargo:rerun-if-env-changed=BINARYEN_STATIC");
    println!("cargo:rerun-if-env-changed=BINARYEN_STATIC_STDCPP");
    println!("cargo:rerun-if-env-changed=MCFGTHREAD_LIB_DIR");
    println!("cargo:rerun-if-env-changed=WASMTIME_LIB_DIR");
    println!("cargo:rerun-if-env-changed=WASMTIME_STATIC");

    let binaryen_lib_dir = env::var_os("BINARYEN_LIB_DIR");
    if let Some(dir) = &binaryen_lib_dir {
        println!(
            "cargo:rustc-link-search=native={}",
            Path::new(&dir).display()
        );
    }
    if let Some(dir) = env::var_os("MCFGTHREAD_LIB_DIR") {
        println!(
            "cargo:rustc-link-search=native={}",
            Path::new(&dir).display()
        );
    }
    let wasmtime_lib_dir = env::var_os("WASMTIME_LIB_DIR");
    if let Some(dir) = &wasmtime_lib_dir {
        println!(
            "cargo:rustc-link-search=native={}",
            Path::new(&dir).display()
        );
    }

    let static_binaryen = env::var("BINARYEN_STATIC").is_ok_and(|value| value != "0");
    let static_wasmtime = env::var("WASMTIME_STATIC").is_ok_and(|value| value != "0");
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if target_os == "macos" && !static_binaryen {
        if let Some(dir) = &binaryen_lib_dir {
            println!(
                "cargo:rustc-link-arg=-Wl,-rpath,{}",
                Path::new(dir).display()
            );
        }
    }
    if target_os == "macos" && !static_wasmtime {
        if let Some(dir) = &wasmtime_lib_dir {
            println!(
                "cargo:rustc-link-arg=-Wl,-rpath,{}",
                Path::new(dir).display()
            );
        }
    }
    println!(
        "cargo:rustc-link-lib={}binaryen",
        if static_binaryen { "static=" } else { "" }
    );
    println!(
        "cargo:rustc-link-lib={}wasmtime",
        if static_wasmtime { "static=" } else { "" }
    );
    if static_wasmtime && target_os == "windows" {
        // Wasmtime's static library is a Rust staticlib, which does not record
        // the system libraries its own dependencies need.
        for library in ["advapi32", "bcrypt", "ole32", "shell32"] {
            println!("cargo:rustc-link-lib={library}");
        }
    }

    if env::var("CARGO_CFG_TARGET_ENV").as_deref() != Ok("msvc") {
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
        if target_os == "windows" {
            println!("cargo:rustc-link-lib=static=mcfgthread");
            // GCC's unwinder also refers to mcfgthread. Native libraries named
            // with rustc-link-lib precede Rust's rlibs, so repeat it at the end
            // of the linker command to satisfy those later references.
            println!("cargo:rustc-link-arg=-lmcfgthread");
        }
    }
}
