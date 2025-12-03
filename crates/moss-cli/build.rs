use std::{env, path::Path};

fn main() {
    println!("cargo:rerun-if-env-changed=BINARYEN_LIB_DIR");
    println!("cargo:rerun-if-env-changed=BINARYEN_STATIC");
    println!("cargo:rerun-if-env-changed=BINARYEN_STATIC_STDCPP");
    println!("cargo:rerun-if-env-changed=MCFGTHREAD_LIB_DIR");
    println!("cargo:rerun-if-env-changed=TARGET");

    if let Some(dir) = env::var_os("BINARYEN_LIB_DIR") {
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

    let kind = match env::var("BINARYEN_STATIC").as_deref() {
        Ok("0") => "dylib",
        Ok(_) => "static",
        Err(_) => "dylib",
    };
    println!("cargo:rustc-link-lib={kind}=binaryen");

    if env::var("CARGO_CFG_TARGET_ENV").ok().as_deref() != Some("msvc") {
        let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
        let cpp_name = if target_os == "macos" {
            "c++"
        } else {
            "stdc++"
        };
        let static_cpp = env::var("BINARYEN_STATIC_STDCPP")
            .map(|v| v != "0")
            .unwrap_or_else(|_| {
                matches!(env::var("CARGO_CFG_TARGET_ENV").as_deref(), Ok("musl"))
                    || target_os == "windows"
            });
        let cpp_link = if static_cpp { "static" } else { "dylib" };
        println!("cargo:rustc-link-lib={cpp_link}={cpp_name}");
        if target_os == "windows" {
            println!("cargo:rustc-link-lib=static=mcfgthread");
        }
    }
}
