mod binaryen;
mod validate;
mod wasmtime;

use std::{
    env, fs,
    io::{self, IsTerminal, Write},
    path::{Component, Path, PathBuf},
    process::ExitCode,
};

use anyhow::{anyhow, bail, Context, Result};
use binaryen::{optimize_wasm, parse_opt, Opt};
use clap::{Parser, Subcommand};
#[cfg(not(moss_embedded_compiler))]
use sha2::{Digest, Sha256};
use validate::{validate, Producer};
use wasmtime::{Access, Engine, Module, Wasi};

#[cfg(not(moss_embedded_compiler))]
const COMPILER_OPT: Opt = Opt::new(3, 0);

#[cfg(moss_embedded_compiler)]
static EMBEDDED_COMPILER: &[u8] = include_bytes!(env!("MOSS_COMPILER_CWASM"));

const URL: &str = concat!("https://github.com/moss-lang/moss/tree/", env!("MOSS_REV"));

enum CompilerModule {
    Wasm(Vec<u8>),
    #[cfg(moss_embedded_compiler)]
    Precompiled(&'static [u8]),
    /// A `.cwasm` installed next to the driver, which Wasmtime maps from disk.
    #[cfg(not(moss_embedded_compiler))]
    Precompiled(PathBuf),
}

#[derive(Parser)]
#[command(name = "moss", version = URL, about = "The Moss compiler and program runner")]
struct Cli {
    /// Binaryen optimization level: 0, 1, 2, 3, 4, s, or z
    #[arg(short = 'O', value_name = "LEVEL", value_parser = parse_opt, global = true)]
    optimization: Option<Opt>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile and run a Moss program
    Run {
        /// Entry module
        file: PathBuf,

        /// Arguments passed to the program
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        args: Vec<String>,
    },

    /// Compile a Moss program to WebAssembly
    Build {
        /// Entry module
        file: PathBuf,

        /// Write the module here instead of stdout
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

fn main() -> ExitCode {
    match real_main() {
        Ok(code) => ExitCode::from(code as u8),
        Err(error) => {
            eprintln!("{error:#}");
            ExitCode::FAILURE
        }
    }
}

fn real_main() -> Result<i32> {
    let cli = Cli::parse_from(normalized_args());
    let root = repository_root()?;
    let engine = Engine::new()?;

    match cli.command {
        Commands::Run { file, args } => {
            let wasm = compile(&engine, &root, &file, cli.optimization)?;
            run_program(&engine, &root, &file, &args, &wasm)
        }
        Commands::Build { file, output } => {
            let wasm = compile(&engine, &root, &file, cli.optimization)?;
            // The artifact, checked whoever wrote it last. `moss run` needs
            // no such check: Wasmtime validates as it compiles (see
            // `run_program`).
            let producer = match cli.optimization {
                Some(_) => Producer::Binaryen,
                None => Producer::Compiler,
            };
            validate(&engine, &wasm, producer)?;
            match output {
                Some(path) => fs::write(path, wasm)?,
                None => {
                    if io::stdout().is_terminal() {
                        bail!("refusing to write a WebAssembly binary to a terminal; use -o");
                    }
                    io::stdout().write_all(&wasm)?;
                }
            }
            Ok(0)
        }
    }
}

fn normalized_args() -> Vec<String> {
    normalize_args(env::args().collect())
}

fn normalize_args(mut args: Vec<String>) -> Vec<String> {
    let mut index = 1;
    while let Some(argument) = args.get(index) {
        match argument.as_str() {
            "-h" | "--help" | "-V" | "--version" => return args,
            "-O" => index += 2,
            argument if argument.starts_with("-O") && argument.len() > 2 => index += 1,
            _ => break,
        }
    }
    let has_subcommand = args
        .get(index)
        .is_some_and(|arg| matches!(arg.as_str(), "run" | "build" | "help"));
    if !has_subcommand && args.len() > 1 {
        args.insert(1, "run".into());
    }
    args
}

fn repository_root() -> Result<PathBuf> {
    if let Some(root) = env::var_os("MOSS_ROOT") {
        return Ok(PathBuf::from(root));
    }
    if let Ok(executable) = env::current_exe() {
        if let Some(prefix) = executable.parent().and_then(Path::parent) {
            let installed = prefix.join("share/moss");
            if installed.join("lib/prelude.moss").is_file() {
                return Ok(installed);
            }
        }
    }
    let cwd = env::current_dir()?;
    for candidate in cwd.ancestors() {
        if candidate.join("src/main.moss").is_file()
            && candidate.join("bootstrap/theories").is_dir()
            && candidate.join("lib/prelude.moss").is_file()
        {
            return Ok(candidate.to_path_buf());
        }
    }
    bail!("cannot find a Moss source tree; set MOSS_ROOT")
}

fn compile(
    engine: &Engine,
    root: &Path,
    file: &Path,
    optimization: Option<Opt>,
) -> Result<Vec<u8>> {
    let entry = absolute(file)?;
    let compiler = self_hosted_compiler(engine, root)?;
    let bytes = run_compiler(engine, root, &compiler, &entry)?;
    match optimization {
        Some(opt) => optimized(engine, bytes, opt),
        None => Ok(bytes),
    }
}

/// Binaryen's rewrite of a module, once the module has validated. Binaryen's
/// C API reads without validating, so an invalid module can come out of it
/// quietly "repaired" into a valid one that means something else, or abort
/// the whole process; only the compiler's own output says which bug it was.
fn optimized(engine: &Engine, bytes: Vec<u8>, opt: Opt) -> Result<Vec<u8>> {
    validate(engine, &bytes, Producer::Compiler)?;
    optimize_wasm(bytes, opt)
}

fn absolute(path: &Path) -> Result<PathBuf> {
    if path.is_absolute() {
        Ok(path.to_path_buf())
    } else {
        Ok(env::current_dir()?.join(path))
    }
}

/// A compiler built before, to compile the current source with: `MOSS_SEED`
/// (a `.wasm`), else the one this checkout last built, else
/// `result-compiler` (`nix build .#compiler -o result-compiler`, which
/// bootstraps one from nothing with the Rocq oracle in about 35 minutes).
/// Any compiler that compiles the current source correctly will do, since
/// what it writes is compiled once more by itself.
#[cfg(not(moss_embedded_compiler))]
fn seed_compiler(root: &Path, last: &Path) -> Result<Vec<u8>> {
    if let Some(path) = env::var_os("MOSS_SEED") {
        return fs::read(&path)
            .with_context(|| format!("cannot read MOSS_SEED={}", Path::new(&path).display()));
    }
    for candidate in [last.to_path_buf(), root.join("result-compiler")] {
        if candidate.is_file() {
            return Ok(fs::read(candidate)?);
        }
    }
    bail!(
        "no compiler to build the self-hosted compiler with: set MOSS_SEED to a \
         previously built mossc.wasm, or run `nix build .#compiler -o result-compiler` \
         (see CONTRIBUTING.md)"
    )
}

#[cfg(moss_embedded_compiler)]
fn self_hosted_compiler(_engine: &Engine, _root: &Path) -> Result<CompilerModule> {
    if let Some(path) = env::var_os("MOSS_COMPILER") {
        return Ok(CompilerModule::Wasm(fs::read(path)?));
    }
    Ok(CompilerModule::Precompiled(EMBEDDED_COMPILER))
}

#[cfg(not(moss_embedded_compiler))]
fn self_hosted_compiler(engine: &Engine, root: &Path) -> Result<CompilerModule> {
    if let Some(path) = env::var_os("MOSS_COMPILER") {
        return Ok(CompilerModule::Wasm(fs::read(path)?));
    }
    if let Some(precompiled) = installed_compiler() {
        return Ok(CompilerModule::Precompiled(precompiled));
    }
    let installed = root.join("mossc.wasm");
    if installed.is_file() {
        return Ok(CompilerModule::Wasm(fs::read(installed)?));
    }
    let target = root.join("target");
    let compiler = target.join("moss.wasm");
    let manifest = target.join("moss.inputs");
    let current = compiler_hash(root)?;
    if compiler.is_file() && fs::read_to_string(&manifest).ok().as_deref() == Some(&current) {
        return Ok(CompilerModule::Wasm(fs::read(compiler)?));
    }

    fs::create_dir_all(&target)?;
    eprintln!("Building the self-hosted Moss compiler...");
    // Two turns of the crank: the seed's compile of the source, and that
    // compiler's compile of it, which is what the source means however the
    // seed was built.
    let seed = seed_compiler(root, &compiler)?;
    let mut ready = seed;
    for _ in 0..2 {
        let next = run_compiler(
            engine,
            root,
            &CompilerModule::Wasm(ready),
            &root.join("src/main.moss"),
        )?;
        ready = optimized(engine, next, COMPILER_OPT)?;
    }

    let compiler_tmp = target.join(format!("moss.wasm.{}", std::process::id()));
    let manifest_tmp = target.join(format!("moss.inputs.{}", std::process::id()));
    fs::write(&compiler_tmp, &ready)?;
    fs::write(&manifest_tmp, &current)?;
    fs::rename(compiler_tmp, &compiler)?;
    fs::rename(manifest_tmp, manifest)?;
    Ok(CompilerModule::Wasm(ready))
}

/// The compiler's machine code, if a package build installed it beside this
/// executable.
///
/// The bundles carry those bytes inside the executable instead. Keeping them
/// in a file lets the Nix `default` package build the driver and the compiler
/// as separate derivations, so neither is rebuilt when only the other changes,
/// and lets Wasmtime map the code rather than copy it.
///
/// `libexec` rather than `share`, because this is machine code for one target:
/// it is the compiler proper, in the sense that `cc1` is to `gcc`.
#[cfg(not(moss_embedded_compiler))]
fn installed_compiler() -> Option<PathBuf> {
    let executable = env::current_exe().ok()?;
    let prefix = executable.parent()?.parent()?;
    let path = prefix.join("libexec/moss/compiler.cwasm");
    path.is_file().then_some(path)
}

#[cfg(not(moss_embedded_compiler))]
fn compiler_hash(root: &Path) -> Result<String> {
    use std::fmt::Write as _;

    let mut files = Vec::new();
    collect_files(&root.join("src"), "moss", &mut files)?;
    collect_files(&root.join("lib"), "moss", &mut files)?;
    files.sort();

    let mut digest = Sha256::new();
    for file in files {
        digest.update(file.strip_prefix(root)?.as_os_str().as_encoded_bytes());
        digest.update([0]);
        digest.update(fs::read(file)?);
        digest.update([0]);
    }
    let bytes = digest.finalize();
    let mut hex = String::with_capacity(bytes.len() * 2 + 1);
    for byte in bytes {
        write!(&mut hex, "{byte:02x}")?;
    }
    hex.push('\n');
    Ok(hex)
}

#[cfg(not(moss_embedded_compiler))]
fn collect_files(directory: &Path, extension: &str, files: &mut Vec<PathBuf>) -> Result<()> {
    for entry in fs::read_dir(directory)? {
        let path = entry?.path();
        if path.is_dir() {
            collect_files(&path, extension, files)?;
        } else if path.extension().is_some_and(|got| got == extension) {
            files.push(path);
        }
    }
    Ok(())
}

fn run_compiler(
    engine: &Engine,
    root: &Path,
    compiler: &CompilerModule,
    entry: &Path,
) -> Result<Vec<u8>> {
    let cwd = env::current_dir()?;
    // The library is the one beside the compiler: installed with it, or the
    // source tree it was built from. Never one found by walking up from the
    // entry file, which may sit in another checkout (a worktree, a branch)
    // whose `lib/` this compiler was not built against. Pairing a compiler
    // with another version's library made the shipped compiler disagree
    // with the one the tests run.
    let filesystem_root = common_ancestor(root, &cwd)
        .and_then(|common| common_ancestor(common, entry))
        .ok_or_else(|| anyhow!("compiler inputs do not share a filesystem root"))?;
    let prelude = root.join("lib/prelude.moss");
    let argv = vec![
        wasi_path(prelude.strip_prefix(filesystem_root)?)?,
        wasi_path(entry.strip_prefix(filesystem_root)?)?,
        "--diagnostics=human".to_owned(),
    ];
    let output = run_wasi_capture(engine, filesystem_root, compiler, &argv)?;
    valid_wasm(host_diagnostic_paths(output, filesystem_root, &cwd)?)
}

fn common_ancestor<'a>(left: &'a Path, right: &'a Path) -> Option<&'a Path> {
    left.ancestors()
        .find(|candidate| right.starts_with(candidate))
}

/// The compiler names files relative to its WASI preopen. Translate only
/// human diagnostic location lines back to host paths. Prefer paths below
/// cwd as relative paths; keep other paths absolute instead of adding `..`.
/// Preserve any :line:column suffix, including for related notes.
fn host_diagnostic_paths(output: Vec<u8>, filesystem_root: &Path, cwd: &Path) -> Result<Vec<u8>> {
    if !output.starts_with(b"error: ") {
        return Ok(output);
    }
    let root = wasi_path(filesystem_root)?;
    let root = root.trim_end_matches('/');
    let cwd = wasi_path(cwd)?;
    let mut rendered = String::with_capacity(output.len() + root.len());
    for line in String::from_utf8_lossy(&output).split_inclusive('\n') {
        if let Some(location) = line.strip_prefix("  --> ") {
            let absolute = format!("{root}/{location}");
            let path = Path::new(&absolute);
            let relative = path.strip_prefix(Path::new(&cwd)).ok().filter(|relative| {
                !relative
                    .components()
                    .any(|part| part == Component::ParentDir)
            });
            rendered.push_str("  --> ");
            rendered.push_str(&wasi_path(relative.unwrap_or(path))?);
        } else {
            rendered.push_str(line);
        }
    }
    Ok(rendered.into_bytes())
}

fn valid_wasm(bytes: Vec<u8>) -> Result<Vec<u8>> {
    if bytes.starts_with(b"\0asm\x01\0\0\0") {
        Ok(bytes)
    } else {
        let diagnostics = String::from_utf8_lossy(&bytes);
        if diagnostics.starts_with("error: ") {
            bail!("{}", diagnostics.trim_end());
        }
        bail!(
            "compiler did not write a WebAssembly module{}",
            if diagnostics.is_empty() {
                String::new()
            } else {
                format!(":\n{diagnostics}")
            }
        )
    }
}

fn run_wasi_capture(
    engine: &Engine,
    filesystem_root: &Path,
    compiler: &CompilerModule,
    argv: &[String],
) -> Result<Vec<u8>> {
    let mut wasi = Wasi::new(argv)?;
    wasi.preopen(filesystem_root, ".", Access::Read)?;
    wasi.inherit_stderr();
    let output = wasi.capture_stdout();
    let module = match compiler {
        CompilerModule::Wasm(bytes) => Module::new(engine, bytes)?,
        #[cfg(moss_embedded_compiler)]
        CompilerModule::Precompiled(bytes) => {
            // The package build generated these bytes with this exact Wasmtime
            // version, configuration, and target.
            unsafe { Module::deserialize(engine, bytes)? }
        }
        #[cfg(not(moss_embedded_compiler))]
        CompilerModule::Precompiled(path) => {
            // The package build generated this file with this exact Wasmtime
            // version, configuration, and target, and installed it in the same
            // store path as the library data this driver just found.
            unsafe { Module::deserialize_file(engine, path)? }
        }
    };
    let code = wasmtime::run(engine, &module, wasi)?;
    if code != 0 {
        bail!("compiler exited with status {code}");
    }
    Ok(output.into_bytes())
}

fn run_program(
    engine: &Engine,
    _root: &Path,
    file: &Path,
    args: &[String],
    bytes: &[u8],
) -> Result<i32> {
    let mut argv = vec![path_string(file)?];
    argv.extend_from_slice(args);
    let cwd = env::current_dir()?;
    let mut wasi = Wasi::new(&argv)?;
    wasi.preopen(&cwd, ".", Access::ReadWrite)?;
    wasi.inherit_stdin();
    wasi.inherit_stdout();
    wasi.inherit_stderr();
    // Wasmtime validates the module here, so an invalid one stops before it
    // runs; the compiler accepted the program, so that is the compiler's bug.
    let module = Module::new(engine, bytes).map_err(|error| {
        anyhow!(
            "Wasmtime refused the compiled module: {error:#}\n{}",
            validate::COMPILER_BUG
        )
    })?;
    wasmtime::run(engine, &module, wasi)
}

fn path_string(path: &Path) -> Result<String> {
    path.to_str()
        .map(str::to_owned)
        .ok_or_else(|| anyhow!("path is not valid UTF-8: {}", path.display()))
}

fn wasi_path(path: &Path) -> Result<String> {
    Ok(path_string(path)?.replace('\\', "/"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn optimization_requires_a_level() {
        assert!(Cli::try_parse_from(["moss", "run", "-O3", "main.moss"]).is_ok());
        assert!(Cli::try_parse_from(["moss", "run", "-Os", "main.moss"]).is_ok());
        assert!(Cli::try_parse_from(["moss", "run", "-Oz", "main.moss"]).is_ok());
        assert!(Cli::try_parse_from(["moss", "run", "-O", "main.moss"]).is_err());
    }

    #[test]
    fn a_file_implies_run_even_with_options() {
        let args = normalize_args(
            ["moss", "-O3", "examples/hello.moss", "--", "-x"]
                .map(str::to_owned)
                .to_vec(),
        );
        assert_eq!(args[1], "run");
        let cli = Cli::try_parse_from(args).unwrap();
        assert!(matches!(cli.command, Commands::Run { .. }));
    }

    #[test]
    fn source_diagnostics_are_reported_without_a_module_protocol_error() {
        let report = "error: this name is not in scope: `missing`\n  --> main.moss:3:5\n";
        assert_eq!(
            valid_wasm(report.as_bytes().to_vec())
                .unwrap_err()
                .to_string(),
            report.trim_end()
        );
        assert!(valid_wasm(b"unexpected compiler output".to_vec())
            .unwrap_err()
            .to_string()
            .contains("compiler did not write a WebAssembly module"));
    }

    #[test]
    fn diagnostic_paths_use_the_host_root_for_errors_and_related_notes() {
        let report = concat!(
            "error: imports bind the same name to different declarations: `Base`\n",
            "  --> Users/samueles/github/samestep/phd-thesis/examples/moss/doubledouble.moss\n",
            "error: imports bind the same name to different declarations: `Base`\n",
            "  --> Users/samueles/github/samestep/phd-thesis/examples/moss/shadow.moss:30:1\n",
            "   |\n",
            "30 | import \"./doubledouble.moss\" use Base, lift, BaseOps,\n",
            "   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n",
            "note: previous definition is here\n",
            "  --> Users/samueles/github/samestep/phd-thesis/examples/moss/shadow.moss:12:1\n",
        );
        let translated = host_diagnostic_paths(
            report.as_bytes().to_vec(),
            Path::new("/"),
            Path::new("/elsewhere"),
        )
        .unwrap();
        assert_eq!(
            String::from_utf8(translated).unwrap(),
            report.replace("  --> Users/", "  --> /Users/")
        );
    }

    #[test]
    fn diagnostic_paths_support_nested_preopens_and_windows_roots() {
        let report = b"error: this name is not in scope\n  --> project/main.moss:3:5\n";
        for (root, cwd, expected) in [
            ("/home/sam", "/home/sam/project", "main.moss:3:5"),
            ("/home/sam", "/home/sam", "project/main.moss:3:5"),
            ("/home/sam", "/home/sam/project/", "main.moss:3:5"),
            (
                "/home/sam",
                "/home/sam/proj",
                "/home/sam/project/main.moss:3:5",
            ),
            (
                "/home/sam",
                "/home/sam/project/subdir",
                "/home/sam/project/main.moss:3:5",
            ),
            (r"C:\", r"C:\project", "main.moss:3:5"),
            (r"C:\", r"C:\other", "C:/project/main.moss:3:5"),
            ("/", "/", "project/main.moss:3:5"),
        ] {
            let translated =
                host_diagnostic_paths(report.to_vec(), Path::new(root), Path::new(cwd)).unwrap();
            assert_eq!(
                String::from_utf8(translated).unwrap(),
                format!("error: this name is not in scope\n  --> {expected}\n")
            );
        }
    }

    #[test]
    fn diagnostic_paths_choose_relative_and_absolute_locations_in_one_report() {
        let report = concat!(
            "error: duplicate definition\n",
            "  --> Users/sam/project/examples/main.moss:30:1\n",
            "note: previous definition is here\n",
            "  --> Users/sam/shared/original.moss:12:1\n",
            "error: cannot read this source file\n",
            "  --> Users/sam/project/examples/missing.moss\n",
            "error: cannot read this source file\n",
            "  --> Users/sam/project-other/missing.moss\n",
        );
        let translated = host_diagnostic_paths(
            report.as_bytes().to_vec(),
            Path::new("/"),
            Path::new("/Users/sam/project"),
        )
        .unwrap();
        assert_eq!(
            String::from_utf8(translated).unwrap(),
            concat!(
                "error: duplicate definition\n",
                "  --> examples/main.moss:30:1\n",
                "note: previous definition is here\n",
                "  --> /Users/sam/shared/original.moss:12:1\n",
                "error: cannot read this source file\n",
                "  --> examples/missing.moss\n",
                "error: cannot read this source file\n",
                "  --> /Users/sam/project-other/missing.moss\n",
            )
        );
    }

    #[test]
    fn diagnostic_path_translation_preserves_modules_and_compact_reports() {
        for output in [b"\0asm\x01\0\0\0\xff".as_slice(), b"!B main.moss missing\n"] {
            assert_eq!(
                host_diagnostic_paths(
                    output.to_vec(),
                    Path::new("/home/sam"),
                    Path::new("/home/sam/project")
                )
                .unwrap(),
                output
            );
        }
    }

    #[test]
    fn wasi_paths_use_forward_slashes() {
        assert_eq!(
            wasi_path(Path::new(r"lib\prelude.moss")).unwrap(),
            "lib/prelude.moss"
        );
    }
}
