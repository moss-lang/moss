//! Exercise oracle selection without rebuilding the actual interpreter.
#![cfg(unix)]

use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::process::{Command, Output};

#[test]
fn oracle_build_tracks_the_runtime_checkout() {
    // A child runs this same Rust artifact against a different checkout.
    // Environment changes stay in the child, outside other tests' processes.
    if let Some(root) = std::env::var_os("MOSS_WORKFLOW_ROOT") {
        assert_eq!(moss_tests::repo(), Path::new(&root).canonicalize().unwrap());
        let expected = std::env::var_os("MOSS_WORKFLOW_EXPECTED").unwrap();
        std::thread::scope(|scope| {
            let mut workers = Vec::new();
            for _ in 0..8 {
                workers.push(scope.spawn(moss_tests::oracle_path));
            }
            for worker in workers {
                assert_eq!(worker.join().unwrap(), Path::new(&expected));
            }
        });
        return;
    }

    let root = moss_tests::scratch().join("oracle workflow");
    for directory in [
        "crates/moss-tests",
        "src",
        "tools",
        "current-oracle/bin",
        "result-oracle/bin",
        "bootstrap/_build/default/run",
    ] {
        fs::create_dir_all(root.join(directory)).unwrap();
    }
    for file in [
        "crates/moss-tests/Cargo.toml",
        "src/main.moss",
        &format!("current-oracle/bin/{}", moss_tests::REFERENCE),
        &format!("result-oracle/bin/{}", moss_tests::REFERENCE),
        "bootstrap/_build/default/run/driver.exe",
        "custom-oracle",
    ] {
        fs::write(root.join(file), "").unwrap();
    }
    let nix = root.join("tools/nix");
    fs::write(
        &nix,
        r#"#!/bin/sh
printf '%s\n' "$@" >> "$MOSS_WORKFLOW_CALLS"
if [ "$MOSS_WORKFLOW_FAIL" = 1 ]; then exit 1; fi
printf '%s\n' "$MOSS_WORKFLOW_STORE"
"#,
    )
    .unwrap();
    fs::set_permissions(&nix, fs::Permissions::from_mode(0o755)).unwrap();

    let run = |fail: bool, explicit: bool| -> Output {
        let expected = if explicit {
            root.join("custom-oracle")
        } else {
            root.join("current-oracle/bin").join(moss_tests::REFERENCE)
        };
        let mut command = Command::new(std::env::current_exe().unwrap());
        command
            .args([
                "--exact",
                "oracle_build_tracks_the_runtime_checkout",
                "--nocapture",
            ])
            .current_dir(&root)
            .env("CARGO_MANIFEST_DIR", root.join("crates/moss-tests"))
            .env("MOSS_WORKFLOW_ROOT", &root)
            .env("MOSS_WORKFLOW_EXPECTED", expected)
            .env("MOSS_WORKFLOW_CALLS", root.join("calls"))
            .env("MOSS_WORKFLOW_STORE", root.join("current-oracle"))
            .env("MOSS_WORKFLOW_FAIL", if fail { "1" } else { "0" })
            .env("PATH", root.join("tools"))
            .env_remove("MOSS_ORACLE");
        if explicit {
            command.env("MOSS_ORACLE", root.join("custom-oracle"));
        }
        command.output().unwrap()
    };

    let out = run(false, false);
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let calls = fs::read_to_string(root.join("calls")).unwrap();
    assert_eq!(calls.lines().filter(|line| *line == "build").count(), 1);
    assert!(calls
        .lines()
        .any(|line| line == format!("{}#{}", root.display(), moss_tests::REFERENCE)));

    let out = run(true, false);
    assert!(!out.status.success());
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("building the current Rocq oracle failed")
    );
    let calls = fs::read_to_string(root.join("calls")).unwrap();
    assert_eq!(calls.lines().filter(|line| *line == "build").count(), 2);

    fs::remove_file(root.join("calls")).unwrap();
    let out = run(true, true);
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        !root.join("calls").exists(),
        "an explicit oracle should not invoke Nix"
    );
}
