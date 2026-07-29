mod binaryen;
mod wasmtime;

use std::{
    env, fs,
    io::{self, IsTerminal, Write},
    path::{Path, PathBuf},
    process::{Command, ExitCode, Stdio},
};

use anyhow::{anyhow, bail, Context, Result};
use binaryen::{optimize_wasm, parse_opt, Opt};
use clap::{Parser, Subcommand, ValueEnum};
#[cfg(not(moss_embedded_compiler))]
use sha2::{Digest, Sha256};
use wasmtime::{Access, Engine, Module, Wasi};

#[cfg(not(moss_embedded_compiler))]
const COMPILER_OPT: Opt = Opt::new(3, 0);

#[cfg(moss_embedded_compiler)]
static EMBEDDED_COMPILER: &[u8] = include_bytes!(env!("MOSS_COMPILER_CWASM"));

enum CompilerModule {
    Wasm(Vec<u8>),
    #[cfg(moss_embedded_compiler)]
    Precompiled(&'static [u8]),
}

#[derive(Clone, Copy, Debug, ValueEnum)]
enum Compiler {
    SelfHosted,
    Bootstrap,
}

#[derive(Parser)]
#[command(name = "moss", version, about = "The Moss compiler and program runner")]
struct Cli {
    /// Compiler implementation to use
    #[arg(long, value_enum, default_value = "self-hosted", global = true)]
    compiler: Compiler,

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
            eprintln!("moss: {error:#}");
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
            let wasm = compile(&engine, &root, cli.compiler, &file, cli.optimization)?;
            run_program(&engine, &root, &file, &args, &wasm)
        }
        Commands::Build { file, output } => {
            let wasm = compile(&engine, &root, cli.compiler, &file, cli.optimization)?;
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
            "--compiler" | "-O" => index += 2,
            argument
                if argument.starts_with("--compiler=")
                    || (argument.starts_with("-O") && argument.len() > 2) =>
            {
                index += 1
            }
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
            && candidate.join("bootstrap/mossc").is_dir()
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
    compiler: Compiler,
    file: &Path,
    optimization: Option<Opt>,
) -> Result<Vec<u8>> {
    let entry = absolute(file)?;
    let bytes = match compiler {
        Compiler::Bootstrap => bootstrap_compile(root, &entry)?,
        Compiler::SelfHosted => {
            let compiler = self_hosted_compiler(engine, root)?;
            run_compiler(engine, root, &compiler, &entry)?
        }
    };
    match optimization {
        Some(opt) => optimize_wasm(bytes, opt),
        None => Ok(bytes),
    }
}

fn absolute(path: &Path) -> Result<PathBuf> {
    if path.is_absolute() {
        Ok(path.to_path_buf())
    } else {
        Ok(env::current_dir()?.join(path))
    }
}

fn bootstrap_compile(root: &Path, entry: &Path) -> Result<Vec<u8>> {
    let output = Command::new("python3")
        .args(["-m", "mossc"])
        .arg(entry)
        .current_dir(root)
        .env("PYTHONPATH", root.join("bootstrap"))
        .env("MOSS_LIB", root.join("lib"))
        .stdout(Stdio::piped())
        .spawn()
        .context("could not start the Python bootstrap compiler")?
        .wait_with_output()?;
    if !output.status.success() {
        bail!("bootstrap compiler exited with {}", output.status);
    }
    valid_wasm(output.stdout)
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
    let s0 = bootstrap_compile(root, &root.join("src/main.moss"))?;
    let s0 = optimize_wasm(s0, COMPILER_OPT)?;
    let s1 = run_compiler(
        engine,
        root,
        &CompilerModule::Wasm(s0),
        &root.join("src/main.moss"),
    )?;
    let ready = optimize_wasm(s1, COMPILER_OPT)?;

    let compiler_tmp = target.join(format!("moss.wasm.{}", std::process::id()));
    let manifest_tmp = target.join(format!("moss.inputs.{}", std::process::id()));
    fs::write(&compiler_tmp, &ready)?;
    fs::write(&manifest_tmp, &current)?;
    fs::rename(compiler_tmp, &compiler)?;
    fs::rename(manifest_tmp, manifest)?;
    Ok(CompilerModule::Wasm(ready))
}

#[cfg(not(moss_embedded_compiler))]
fn compiler_hash(root: &Path) -> Result<String> {
    let mut files = Vec::new();
    collect_files(&root.join("bootstrap/mossc"), "py", &mut files)?;
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
    Ok(format!("{:x}\n", digest.finalize()))
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
    let library_root = entry
        .ancestors()
        .find(|candidate| candidate.join("lib/prelude.moss").is_file())
        .unwrap_or(root);
    let filesystem_root = common_ancestor(library_root, &cwd)
        .ok_or_else(|| anyhow!("compiler inputs do not share a filesystem root"))?;
    let prelude = library_root.join("lib/prelude.moss");
    let argv = vec![
        wasi_path(prelude.strip_prefix(filesystem_root)?)?,
        wasi_path(entry.strip_prefix(filesystem_root)?)?,
    ];
    let output = run_wasi_capture(engine, filesystem_root, compiler, &argv)?;
    valid_wasm(output)
}

fn common_ancestor<'a>(left: &'a Path, right: &'a Path) -> Option<&'a Path> {
    left.ancestors()
        .find(|candidate| right.starts_with(candidate))
}

fn valid_wasm(bytes: Vec<u8>) -> Result<Vec<u8>> {
    if bytes.starts_with(b"\0asm\x01\0\0\0") {
        Ok(bytes)
    } else {
        let diagnostics = String::from_utf8_lossy(&bytes);
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
    let module = Module::new(engine, bytes)?;
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
    fn wasi_paths_use_forward_slashes() {
        assert_eq!(
            wasi_path(Path::new(r"lib\prelude.moss")).unwrap(),
            "lib/prelude.moss"
        );
    }
}
