use std::{
    env, fs,
    path::{Path, PathBuf},
};

fn main() {
    println!("cargo:rerun-if-env-changed=WASMTIME_LIB_DIR");
    println!("cargo:rerun-if-env-changed=WASMTIME_LIB_PATH");
    println!("cargo:rerun-if-env-changed=MOSS_STATIC_WASMTIME");
    println!("cargo:rerun-if-env-changed=MOSS_BUNDLE_WASMTIME");

    let lib_dir = find_lib_dir().expect(
        "Failed to locate libwasmtime; set WASMTIME_LIB_DIR to a directory containing libwasmtime",
    );

    let static_link = env::var_os("MOSS_STATIC_WASMTIME").is_some();

    println!(
        "cargo:warning=linking against {}libwasmtime in {}",
        if static_link { "static " } else { "" },
        lib_dir.display()
    );
    println!("cargo:rustc-link-search=native={}", lib_dir.display());
    if static_link {
        println!("cargo:rustc-link-lib=static=wasmtime");
        // wasmtime's static archive brings its own Rust runtime symbols; allow duplicates.
        println!("cargo:rustc-link-arg=-Wl,--allow-multiple-definition");
    } else {
        println!("cargo:rustc-link-lib=wasmtime");
    }

    // Make sure the resulting binary can find libwasmtime without needing LD_LIBRARY_PATH.
    // If we're bundling the .so next to the binary, prefer a relative rpath.
    if !static_link && env::var_os("MOSS_BUNDLE_WASMTIME").is_some() {
        println!("cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN/../lib");
    } else if !static_link {
        println!("cargo:rustc-link-arg=-Wl,-rpath,{}", lib_dir.display());
    }
}

fn find_lib_dir() -> Option<PathBuf> {
    if let Some(dir) = env::var_os("WASMTIME_LIB_DIR") {
        let path = PathBuf::from(dir);
        if contains_lib(&path) {
            return Some(path);
        }
    }
    if let Some(path) = env::var_os("WASMTIME_LIB_PATH") {
        let path = PathBuf::from(path);
        if let Some(dir) = path.parent() {
            if contains_lib(dir) {
                return Some(dir.to_path_buf());
            }
        }
    }

    for dir in path_candidates_from_path() {
        if contains_lib(&dir) {
            return Some(dir);
        }
    }

    best_nix_store_candidate().or_else(|| {
        eprintln!("cargo:warning=no suitable libwasmtime found; searched /nix/store");
        None
    })
}

fn contains_lib(dir: &Path) -> bool {
    dir.join(lib_filename()).exists()
}

fn lib_filename() -> &'static str {
    if env::var_os("MOSS_STATIC_WASMTIME").is_some() {
        "libwasmtime.a"
    } else if cfg!(target_os = "macos") {
        "libwasmtime.dylib"
    } else if cfg!(target_os = "windows") {
        "wasmtime.dll"
    } else {
        "libwasmtime.so"
    }
}

fn path_candidates_from_path() -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    if let Ok(path_var) = env::var("PATH") {
        for entry in env::split_paths(&path_var) {
            if let Some(prefix) = entry.parent() {
                dirs.push(prefix.join("lib"));
                dirs.push(prefix.join("lib64"));
            }
        }
    }
    dirs
}

fn best_nix_store_candidate() -> Option<PathBuf> {
    let mut best: Option<(PathBuf, (u64, u64, u64))> = None;
    if let Ok(entries) = fs::read_dir("/nix/store") {
        for entry in entries.flatten() {
            let path = entry.path();
            let name = entry.file_name();
            let Some(name) = name.to_str() else { continue };
            if !name.contains("wasmtime") || !name.contains("lib") {
                continue;
            }
            for lib_dir in [path.join("lib"), path.join("lib64")] {
                if !contains_lib(&lib_dir) {
                    continue;
                }
                let version = version_tuple(name).unwrap_or((0, 0, 0));
                // Require the version to be >= 39 to match the headers we bind to.
                if version.0 < 39 {
                    continue;
                }
                let replace = match &best {
                    None => true,
                    Some((_, best_v)) => version > *best_v,
                };
                if replace {
                    best = Some((lib_dir, version));
                }
            }
        }
    }
    best.map(|(dir, _)| dir)
}

fn version_tuple(name: &str) -> Option<(u64, u64, u64)> {
    let (_, rest) = name.split_once("wasmtime-")?;
    let version_str = rest.split('-').next()?;
    let mut parts = version_str.split('.').filter_map(|p| p.parse::<u64>().ok());
    let major = parts.next()?;
    let minor = parts.next().unwrap_or(0);
    let patch = parts.next().unwrap_or(0);
    Some((major, minor, patch))
}
