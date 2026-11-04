//! The harness behind Moss's test suite (`tests/suite`).
//!
//! Three things run Moss programs here, and the suite holds them to each
//! other and to golden output:
//!
//! - **the compiler under test**: `src/main.moss`, compiled by a *seed*
//!   compiler (a previously built `mossc.wasm`, see [`seed`]) and optimized
//!   with `wasm-opt -O3`. The fixpoint test then checks that it compiles
//!   itself to exactly the module it is, so a seed from an older commit is
//!   fine as long as it still compiles the current source correctly;
//! - **Wasmtime**, which runs what that compiler writes, after
//!   `wasm-tools validate` has accepted it ([`wasm_file`]);
//! - **the oracle** (`bootstrap/`, see [`oracle_path`]): the Rocq front end,
//!   elaborator and interpreter, extracted to OCaml. It replaces the Python
//!   interpreter the suite used to have as its third runner, and its
//!   diagnostics are the Python bootstrap's messages, which is why the
//!   suite checks error messages against it.
//!
//! Every external tool is a hard requirement: a missing one is an
//! environment bug, not a reason to pass. The Nix dev shell provides
//! `wasmtime`, `wasm-opt`, `wasm-dis` and `wasm-tools`; CONTRIBUTING.md
//! says where the seed and the oracle come from.

pub mod gensrc;

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::OnceLock;

/// The repository root, which every path the compiler sees is relative to:
/// it is the directory Wasmtime preopens as `.`.
pub fn repo() -> &'static Path {
    static REPO: OnceLock<PathBuf> = OnceLock::new();
    REPO.get_or_init(|| {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("the repository root")
    })
}

/// A directory for the modules and sources this process writes. It is
/// inside the repository, because the compiler and the oracle only see
/// files under the directory Wasmtime preopens, and it is this process's
/// own, so that two runs do not trample each other.
pub fn scratch() -> &'static Path {
    static SCRATCH: OnceLock<PathBuf> = OnceLock::new();
    SCRATCH.get_or_init(|| {
        let dir = repo()
            .join("target/moss-tests")
            .join(std::process::id().to_string());
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).expect("create the scratch directory");
        dir
    })
}

static NAMES: AtomicUsize = AtomicUsize::new(0);

fn fresh(stem: &str) -> PathBuf {
    scratch().join(format!("{}-{stem}", NAMES.fetch_add(1, Ordering::Relaxed)))
}

/// `path` relative to the repository, as the compiler is given it.
pub fn rel(path: &Path) -> String {
    path.strip_prefix(repo())
        .expect("a path inside the repository")
        .to_str()
        .expect("a UTF-8 path")
        .to_string()
}

/// What a process did.
#[derive(Debug, Clone)]
pub struct Out {
    pub code: i32,
    pub stdout: Vec<u8>,
    pub stderr: String,
}

impl Out {
    pub fn text(&self) -> String {
        String::from_utf8_lossy(&self.stdout).into_owned()
    }

    pub fn is_module(&self) -> bool {
        self.stdout.starts_with(b"\0asm\x01\0\0\0")
    }
}

fn tool(name: &str) -> PathBuf {
    let found = std::env::var_os("PATH").and_then(|paths| {
        std::env::split_paths(&paths)
            .map(|dir| dir.join(name))
            .find(|candidate| candidate.is_file())
    });
    found.unwrap_or_else(|| {
        panic!("{name} not found on PATH; enter the Nix dev shell (`nix develop`), which provides it")
    })
}

/// Run `command`, feeding it nothing, with a generous timeout's worth of
/// patience (none: a hang is a test that never finishes, which `cargo test`
/// reports by name).
pub fn output(command: &mut Command) -> Out {
    let out = command
        .stdin(Stdio::null())
        .output()
        .unwrap_or_else(|e| panic!("cannot run {command:?}: {e}"));
    Out {
        code: out.status.code().unwrap_or(-1),
        stdout: out.stdout,
        stderr: String::from_utf8_lossy(&out.stderr).into_owned(),
    }
}

/// The Wasm features Binaryen may use, by their `wasm-opt --enable-` names:
/// the one list in `wasm-features.txt`, which the flake and the driver read
/// too.
pub fn wasm_features() -> Vec<String> {
    let text = fs::read_to_string(repo().join("wasm-features.txt")).expect("wasm-features.txt");
    text.lines()
        .map(str::trim)
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
        .map(String::from)
        .collect()
}

/// Each name in `wasm-features.txt` (Binaryen's) as wasmparser, and so
/// `wasm-tools validate`, names it. `crates/moss-cli/src/binaryen.rs` maps
/// the same names.
const VALIDATOR_FEATURES: &[(&str, &[&str])] = &[
    ("mutable-globals", &["mutable-global"]),
    ("nontrapping-float-to-int", &["saturating-float-to-int"]),
    ("sign-ext", &["sign-extension"]),
    ("multivalue", &["multi-value"]),
    ("bulk-memory", &["bulk-memory"]),
    ("bulk-memory-opt", &["bulk-memory-opt"]),
    ("reference-types", &["reference-types"]),
    ("call-indirect-overlong", &["call-indirect-overlong"]),
    ("simd", &["simd"]),
    ("tail-call", &["tail-call"]),
    ("extended-const", &["extended-const"]),
    ("multimemory", &["multi-memory"]),
    ("memory64", &["memory64"]),
    // Binaryen folds typed function references into its GC feature.
    ("gc", &["gc", "function-references"]),
    ("exception-handling", &["exceptions"]),
    ("relaxed-simd", &["relaxed-simd"]),
    ("threads", &["threads"]),
];

/// `wasm-features.txt` as a `wasm-tools validate --features` list: those
/// features and no others, as the driver's validator enables.
pub fn validator_features() -> String {
    let mut names = vec!["-all".to_string(), "mvp".to_string()];
    for feature in wasm_features() {
        let (_, mapped) = VALIDATOR_FEATURES
            .iter()
            .find(|(name, _)| *name == feature)
            .unwrap_or_else(|| {
                panic!("wasm-features.txt names a feature the tests cannot validate: {feature}")
            });
        names.extend(mapped.iter().map(|s| s.to_string()));
    }
    names.join(",")
}

/// `wasm-tools validate`, which is wasmparser, the validator Wasmtime uses:
/// it also enforces the limits engines impose, such as at most 1000
/// parameters per function type, which the specification does not.
pub fn validate(path: &Path) -> Result<(), String> {
    let out = output(
        Command::new(tool("wasm-tools"))
            .arg("validate")
            .arg(format!("--features={}", validator_features()))
            .arg(path),
    );
    if out.code == 0 {
        Ok(())
    } else {
        Err(format!("the compiler wrote an invalid module: {}", out.stderr.trim()))
    }
}

/// A module as a path Wasmtime can run, once it has validated.
///
/// Every module the suite runs, optimizes or disassembles is written here
/// first, so this is where a compiler that writes an invalid module is
/// caught: on its raw output, before `wasm-opt`, which reads one without
/// complaint and may quietly repair it at `-O0`..`-O2` (or abort at `-O3`),
/// and before Wasmtime, whose refusal reads like a runtime failure. The
/// driver checks the compiler's output too, with Wasmtime's validator; this
/// is the same check for the modules the suite makes without it.
pub fn wasm_file(module: &[u8]) -> PathBuf {
    try_wasm_file(module).unwrap_or_else(|e| panic!("{e}"))
}

pub fn try_wasm_file(module: &[u8]) -> Result<PathBuf, String> {
    let path = fresh("module.wasm");
    fs::write(&path, module).expect("write a module");
    if module.starts_with(b"\0asm") {
        validate(&path)?;
    }
    Ok(path)
}

/// The same program, many times faster to run: the self-hosted back end
/// emits straight-line code and leaves every call a call. A
/// semantics-preserving rewrite, which `fixpoint::optimizing_the_compiler_
/// does_not_change_it` asserts rather than assumes. Only ever applied to
/// the thing that *runs*, never to a module under test.
pub fn wasm_opt(module: &[u8]) -> Vec<u8> {
    let src = wasm_file(module);
    let dst = src.with_extension("opt.wasm");
    let mut command = Command::new(tool("wasm-opt"));
    command.arg("-mvp");
    for feature in wasm_features() {
        command.arg(format!("--enable-{feature}"));
    }
    let out = output(command.arg("-O3").arg("-o").arg(&dst).arg(&src));
    assert_eq!(out.code, 0, "wasm-opt failed: {}", out.stderr);
    fs::read(dst).expect("wasm-opt's output")
}

/// A module as Binaryen prints it, to look for instructions.
pub fn wasm_text(module: &[u8]) -> String {
    let mut command = Command::new(tool("wasm-dis"));
    for feature in wasm_features() {
        command.arg(format!("--enable-{feature}"));
    }
    let out = output(command.arg(wasm_file(module)));
    assert_eq!(out.code, 0, "wasm-dis failed: {}", out.stderr);
    String::from_utf8(out.stdout).expect("wasm-dis prints text")
}

/// Whether the type section's first entry is a `rec` group (0x4E).
pub fn starts_with_rec_group(module: &[u8]) -> bool {
    fn leb(module: &[u8], mut at: usize) -> (usize, usize) {
        let (mut value, mut shift) = (0usize, 0);
        loop {
            let byte = module[at];
            at += 1;
            value |= ((byte & 0x7F) as usize) << shift;
            shift += 7;
            if byte & 0x80 == 0 {
                return (value, at);
            }
        }
    }
    let mut at = 8;
    while at < module.len() {
        let id = module[at];
        let (size, body) = leb(module, at + 1);
        if id == 1 {
            let (_, first) = leb(module, body);
            return module[first] == 0x4E;
        }
        at = body + size;
    }
    false
}

/// Run a module under Wasmtime from the repository root, which it preopens
/// as `.` (what makes `Path` resolve: WASI paths are relative to a
/// preopened directory).
pub fn wasmtime(module: &Path, argv0: Option<&str>, args: &[&str]) -> Out {
    let mut command = Command::new(tool("wasmtime"));
    command.arg("run");
    if let Some(argv0) = argv0 {
        command.arg("--argv0").arg(argv0);
    }
    output(
        command
            .arg("--dir")
            .arg(".")
            .arg(module)
            .args(args)
            .current_dir(repo()),
    )
}

/// Run a module and return what it printed, requiring exit status `code`.
pub fn run_module_code(module: &[u8], args: &[&str], code: i32) -> String {
    let out = wasmtime(&wasm_file(module), None, args);
    assert_eq!(out.code, code, "wasmtime: {}", out.stderr);
    out.text()
}

pub fn run_module(module: &[u8], args: &[&str]) -> String {
    run_module_code(module, args, 0)
}

/// The seed: a previously built compiler, as a `.wasm`, from `MOSS_SEED`,
/// else `result-compiler` at the repository root (`nix build .#compiler -o
/// result-compiler`).
pub fn seed() -> PathBuf {
    let path = std::env::var_os("MOSS_SEED")
        .map(PathBuf::from)
        .unwrap_or_else(|| repo().join("result-compiler"));
    assert!(
        path.is_file(),
        "no seed compiler at {}: set MOSS_SEED to a previously built mossc.wasm, \
         or run `nix build .#compiler -o result-compiler` (CONTRIBUTING.md)",
        path.display()
    );
    path
}

/// One generation: `compiler` (a path to a module) run on the compiler's
/// own source. Returns the raw module it writes.
pub fn compile_self(compiler: &Path) -> Vec<u8> {
    let out = wasmtime(compiler, Some("lib/prelude.moss"), &["src/main.moss"]);
    assert_eq!(out.code, 0, "{}", out.stderr);
    assert!(
        out.is_module(),
        // A diagnostic is a letter and a name (D48), so the first lines of
        // a failure are the report, not a module.
        "not a module: {:?}",
        String::from_utf8_lossy(&out.stdout[..out.stdout.len().min(400)])
    );
    out.stdout
}

/// The compiler under test, as emitted: the seed's compile of `src/`.
pub fn compiler_raw() -> &'static [u8] {
    static RAW: OnceLock<Vec<u8>> = OnceLock::new();
    RAW.get_or_init(|| {
        let seed = fs::read(seed()).expect("read the seed");
        let seed = wasm_file(&seed);
        compile_self(&seed)
    })
}

/// The compiler under test, optimized, as a path to run.
pub fn compiler() -> &'static Path {
    static OPT: OnceLock<PathBuf> = OnceLock::new();
    OPT.get_or_init(|| wasm_file(&wasm_opt(compiler_raw())))
}

/// The self-hosted compiler on `entry` (relative to the repository), with
/// `prelude` as its argv0 (`""`: no prelude). Its stdout is a module or a
/// report, one line per diagnostic.
pub fn compile(entry: &str, prelude: &str) -> Out {
    let out = wasmtime(compiler(), Some(prelude), &[entry]);
    assert_eq!(out.code, 0, "the compiler failed on {entry}: {}", out.stderr);
    out
}

/// A module the compiler must write for `entry`, with the prelude.
pub fn build(entry: &str) -> Vec<u8> {
    build_with(entry, "lib/prelude.moss")
}

pub fn build_with(entry: &str, prelude: &str) -> Vec<u8> {
    let out = compile(entry, prelude);
    assert!(
        out.is_module(),
        "{entry} did not compile: {}",
        String::from_utf8_lossy(&out.stdout[..out.stdout.len().min(400)])
    );
    // Validated here, raw, before anything else reads it.
    wasm_file(&out.stdout);
    out.stdout
}

/// Compile `entry` and run it: what it printed, which must exit with 0.
pub fn run(entry: &str, args: &[&str]) -> String {
    run_module(&build(entry), args)
}

/// The report the compiler writes for `entry`, which must not be a module.
pub fn rejects(entry: &str) -> String {
    rejects_with(entry, "lib/prelude.moss")
}

pub fn rejects_with(entry: &str, prelude: &str) -> String {
    let out = compile(entry, prelude);
    assert!(!out.is_module(), "{entry} compiled, and should not have");
    out.text()
}

/// The oracle, from `MOSS_ORACLE`, else `result-oracle/bin/moss_m2` (`nix
/// build .#oracle -o result-oracle`), else
/// `bootstrap/_build/default/ocaml/moss_m2.exe` (`dune build` in `bootstrap`).
pub fn oracle_path() -> PathBuf {
    if let Some(path) = std::env::var_os("MOSS_ORACLE") {
        return PathBuf::from(path);
    }
    for candidate in [
        "result-oracle/bin/moss_m2",
        "bootstrap/_build/default/ocaml/moss_m2.exe",
    ] {
        let path = repo().join(candidate);
        if path.is_file() {
            return path;
        }
    }
    panic!(
        "no oracle: set MOSS_ORACLE, or run `nix build .#oracle -o result-oracle` \
         or `dune build` in `bootstrap` (CONTRIBUTING.md)"
    )
}

/// The oracle's exit statuses (`bootstrap/ocaml/moss_m2.ml`).
pub const ORACLE_FRONT_END: i32 = 63;
pub const ORACLE_REJECTED: i32 = 64;
pub const ORACLE_UNSUPPORTED: i32 = 65;
pub const TRAP: i32 = 134;

/// The oracle on `entry` (relative to the repository), from the repository
/// root, so the prelude is `lib/prelude.moss`: it parses, elaborates and
/// interprets the program, which writes to stdout as the compiled one would.
pub fn oracle(entry: &str, args: &[&str]) -> Out {
    let mut all = vec![entry];
    all.extend_from_slice(args);
    oracle_command(&all)
}

/// The oracle with these arguments, options first, from the repository.
/// The interpreter recurses as deeply as the program it runs, so the
/// oracle runs with as much stack as the system allows.
pub fn oracle_command(args: &[&str]) -> Out {
    output(
        Command::new("sh")
            .arg("-c")
            .arg(r#"ulimit -s unlimited 2>/dev/null || ulimit -s "$(ulimit -H -s)"; exec "$0" "$@""#)
            .arg(oracle_path())
            .env_remove("MOSS_LIB")
            .args(args)
            .current_dir(repo()),
    )
}

/// What the oracle prints for `entry`, which must run to completion.
pub fn oracle_run(entry: &str, args: &[&str]) -> String {
    let out = oracle(entry, args);
    assert_eq!(out.code, 0, "the oracle on {entry}: {}", out.stderr);
    out.text()
}

/// The oracle's message for `entry`, which its elaborator must reject (exit
/// 64; a front-end rejection is 63). Its messages are the Python
/// bootstrap's, which it was written to reproduce.
pub fn oracle_rejects(entry: &str) -> String {
    let out = oracle(entry, &[]);
    assert!(
        out.code == ORACLE_REJECTED || out.code == ORACLE_FRONT_END,
        "the oracle should reject {entry}, but exited with {}: {}",
        out.code,
        out.stderr
    );
    out.stderr
}

/// Write `files` (name, source) into a fresh directory under the scratch
/// directory and return that directory relative to the repository. In each
/// source, `{root}` stands for the relative path from the directory back
/// to the repository root, so a source can import `{root}/src/lex.moss`.
pub fn sources(files: &[(&str, &str)]) -> String {
    let dir = fresh("src");
    let root = {
        let depth = rel(&dir).split('/').count();
        vec![".."; depth].join("/")
    };
    for (name, text) in files {
        let path = dir.join(name);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        let mut file = fs::File::create(&path).unwrap();
        file.write_all(text.replace("{root}", &root).as_bytes()).unwrap();
    }
    rel(&dir)
}

/// One file, `main.moss`; returns its path relative to the repository.
pub fn source(text: &str) -> String {
    format!("{}/main.moss", sources(&[("main.moss", text)]))
}

/// The imports and `main` that make a program written against `Std` a
/// program the platform runs (D52): `main` assumes the primitive context
/// and applies `WasiStd`, then calls the program's own `run`.
pub const WASI_IMPORTS: &str = "import \"wasi.moss\" use WasiStd;\n\
     import \"wasistd.moss\" use Branch;\n\
     import \"wasm.moss\" use Wasm;\n\
     import \"wasip1.moss\" use Wasi;\n";

pub const WASI_MAIN: &str =
    "assume Wasm, Wasi, Branch {\n  fn main() {\n    bind WasiStd;\n    run();\n  }\n}\n";

/// A program over `Std`, as the Python bootstrap ran one with its native
/// `Std`: `imports`, then `assume Std { decls fn run() { body } }`, then
/// the platform's `main`.
pub fn std_program_with(imports: &str, decls: &str, body: &str) -> String {
    format!("{WASI_IMPORTS}{imports}assume Std {{\n{decls}\n  fn run() {{ {body} }}\n}}\n{WASI_MAIN}")
}

pub fn std_program(decls: &str, body: &str) -> String {
    std_program_with("", decls, body)
}

/// A whole program whose `main` assumes `Std` (the Python bootstrap's
/// native `Std` ran those), made one the platform runs: its `fn main()`
/// becomes `fn run()`, which the platform's `main` calls under `WasiStd`.
pub fn std_main(text: &str) -> String {
    assert!(text.contains("fn main()"), "no main in {text}");
    let (imports, rest): (Vec<&str>, Vec<&str>) =
        text.lines().partition(|line| line.starts_with("import "));
    let mut out = String::from(WASI_IMPORTS);
    for line in imports {
        out.push_str(line);
        out.push('\n');
    }
    out.push_str(&rest.join("\n").replace("fn main()", "fn run()"));
    out.push('\n');
    out.push_str(WASI_MAIN);
    out
}

/// Every example but `escape`, which is still written in the old language
/// (it needs string literals, D48). Listed from the directory, so a new
/// example cannot be forgotten and a deleted one cannot leave its golden
/// behind.
pub fn runnable_examples() -> Vec<String> {
    let mut names: Vec<String> = fs::read_dir(repo().join("examples"))
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|x| x == "moss"))
        .map(|p| p.file_stem().unwrap().to_str().unwrap().to_string())
        .filter(|name| name != "escape")
        .collect();
    names.sort();
    names
}

/// The `.moss` files of a directory of the repository, sorted, as paths
/// relative to it.
pub fn moss_files(dir: &str) -> Vec<String> {
    let mut files: Vec<String> = fs::read_dir(repo().join(dir))
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|x| x == "moss"))
        .map(|p| rel(&p))
        .collect();
    files.sort();
    files
}

pub fn read(rel: &str) -> String {
    fs::read_to_string(repo().join(rel)).unwrap_or_else(|e| panic!("{rel}: {e}"))
}
