use std::{
    ffi::CString,
    fs,
    io::{self, IsTerminal, Write},
    mem::MaybeUninit,
    os::raw::c_char,
    path::{Path, PathBuf},
    process::ExitCode,
    ptr,
};

use anyhow::{anyhow, bail};
use clap::{Parser, Subcommand};
use index_vec::Idx;
use line_index::{LineIndex, TextSize};
use moss_cli::util::err_fail;
use moss_cli::wasmtime_ffi as w;
use moss_core::{
    lex::{ByteIndex, LexError, lex},
    lower::lower,
    parse::{ParseError, parse},
    prelude::prelude,
    wasm::wasm,
};

struct Compiler<'a> {
    path: &'a str,
    source: &'a str,
}

impl Compiler<'_> {
    fn error(&self, byte: impl Idx, message: &str) -> anyhow::Error {
        // Build the `LineIndex` only on errors rather than always, since ideally we don't need it.
        let lines = LineIndex::new(self.source);
        let name = self.path;
        let line_col = lines.line_col(TextSize::new(byte.index() as u32));
        let line = line_col.line + 1;
        let col = line_col.col + 1;
        anyhow!("{name}:{line}:{col} {message}")
    }
}

fn compile(script: &str) -> anyhow::Result<Vec<u8>> {
    let source = fs::read_to_string(script)?;
    let compiler = Compiler {
        path: script,
        source: &source,
    };
    let (tokens, starts) = lex(&source).map_err(|err| match err {
        LexError::SourceTooLong => anyhow!("{script}:1:1 {}", err.message()),
        LexError::InvalidToken { start, end: _ } => compiler.error(start, err.message()),
    })?;
    let tree = parse(&tokens).map_err(|err| match err {
        ParseError::Expected { id, tokens: _ } => compiler.error(starts[id], &err.message()),
    })?;
    let (mut ir, mut names, lib) = prelude();
    let module = lower(
        &source,
        &starts,
        &tree,
        &mut ir,
        &mut names,
        lib.prelude,
        &[],
    )
    .map_err(|err| {
        let (tokens, message) = err.describe(&source, &starts, &tree, &ir, &names);
        let start = match tokens {
            Some(range) => starts[range.first],
            None => ByteIndex::new(0),
        };
        compiler.error(start, &message)
    })?;
    let start = names.fndefs[&(module, ir.strings.get_id("main").unwrap())];
    let bytes = wasm(&ir, &names, lib, start);
    Ok(bytes)
}

fn build(script: &str, output: Option<&Path>) -> anyhow::Result<()> {
    let bytes = compile(script)?;
    match output {
        Some(out) => {
            fs::write(out, bytes)?;
        }
        None => {
            let mut stdout = io::stdout();
            if stdout.is_terminal() {
                bail!("Can't print binary to terminal; redirect or use `-o`");
            }
            stdout.write_all(&bytes)?;
        }
    }
    Ok(())
}

fn run(script: &str, args: &[String]) -> anyhow::Result<i32> {
    unsafe {
        let bytes = compile(script)?;
        let mut handles = WasmtimeHandles::default();

        handles.engine = w::wasm_engine_new_with_config(w::wasm_config_new());
        if handles.engine.is_null() {
            bail!("Failed to create Wasmtime engine");
        }

        handles.store = w::wasmtime_store_new(handles.engine, ptr::null_mut(), None);
        if handles.store.is_null() {
            bail!("Failed to create Wasmtime store");
        }
        let context = w::wasmtime_store_context(handles.store);

        handles.linker = w::wasmtime_linker_new(handles.engine);
        if handles.linker.is_null() {
            bail!("Failed to create Wasmtime linker");
        }

        let wasi = build_wasi_config(script, args)?;
        let err = w::wasmtime_context_set_wasi(context, wasi);
        if !err.is_null() {
            return handle_error(err);
        }
        let err = w::wasmtime_linker_define_wasi(handles.linker);
        if !err.is_null() {
            return handle_error(err);
        }

        let mut module = ptr::null_mut();
        let err = w::wasmtime_module_new(handles.engine, bytes.as_ptr(), bytes.len(), &mut module);
        if !err.is_null() {
            return handle_error(err);
        }
        handles.module = module;

        let module_name = b"main";
        let err = w::wasmtime_linker_module(
            handles.linker,
            context,
            module_name.as_ptr().cast(),
            module_name.len(),
            module,
        );
        if !err.is_null() {
            return handle_error(err);
        }

        let mut start = MaybeUninit::<w::wasmtime_func_t>::uninit();
        let err = w::wasmtime_linker_get_default(
            handles.linker,
            context,
            module_name.as_ptr().cast(),
            module_name.len(),
            start.as_mut_ptr(),
        );
        if !err.is_null() {
            return handle_error(err);
        }
        let start = start.assume_init();

        let mut trap = ptr::null_mut();
        let err = w::wasmtime_func_call(
            context,
            &start,
            ptr::null(),
            0,
            ptr::null_mut(),
            0,
            &mut trap,
        );
        if !err.is_null() {
            return handle_error(err);
        }
        if !trap.is_null() {
            return handle_trap(trap);
        }
        Ok(0)
    }
}

#[derive(Default)]
struct WasmtimeHandles {
    engine: *mut w::wasm_engine_t,
    store: *mut w::wasmtime_store_t,
    linker: *mut w::wasmtime_linker_t,
    module: *mut w::wasmtime_module_t,
}

impl Drop for WasmtimeHandles {
    fn drop(&mut self) {
        // Leak engine state on drop; the process is short-lived and this avoids
        // shutdown ordering differences across Wasmtime builds.
    }
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn build_wasi_config(
    script: &str,
    args: &[String],
) -> anyhow::Result<*mut w::wasi_config_t> {
    let cfg = w::wasi_config_new();
    if cfg.is_null() {
        bail!("Failed to create WASI configuration");
    }

    let mut argv = Vec::with_capacity(args.len() + 1);
    argv.push(CString::new(script)?);
    for arg in args {
        argv.push(CString::new(arg.as_str())?);
    }
    let argv_ptrs: Vec<*const c_char> = argv.iter().map(|s| s.as_ptr()).collect();
    if !w::wasi_config_set_argv(cfg, argv_ptrs.len(), argv_ptrs.as_ptr()) {
        w::wasi_config_delete(cfg);
        bail!("Invalid argv entry for WASI (expected UTF-8 without null bytes)");
    }

    w::wasi_config_inherit_env(cfg);
    w::wasi_config_inherit_stdin(cfg);
    w::wasi_config_inherit_stdout(cfg);
    w::wasi_config_inherit_stderr(cfg);

    // Keep these raw pointers alive for the lifetime of the config to avoid any
    // use-after-free inside the WASI preopen handling.
    let host = CString::new(".")?.into_raw();
    let guest = CString::new(".")?.into_raw();
    if !w::wasi_config_preopen_dir(
        cfg,
        host,
        guest,
        w::WASMTIME_WASI_DIR_PERMS_READ | w::WASMTIME_WASI_DIR_PERMS_WRITE,
        w::WASMTIME_WASI_FILE_PERMS_READ | w::WASMTIME_WASI_FILE_PERMS_WRITE,
    ) {
        w::wasi_config_delete(cfg);
        bail!("Failed to preopen current directory for WASI");
    }

    Ok(cfg)
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn handle_error(err: *mut w::wasmtime_error_t) -> anyhow::Result<i32> {
    let mut status: i32 = 0;
    if w::wasmtime_error_exit_status(err, &mut status) {
        w::wasmtime_error_delete(err);
        return Ok(status);
    }
    let message = error_message(err);
    w::wasmtime_error_delete(err);
    Err(anyhow!(message))
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn handle_trap(trap: *mut w::wasm_trap_t) -> anyhow::Result<i32> {
    let message = trap_message(trap);
    w::wasm_trap_delete(trap);
    Err(anyhow!(message))
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn error_message(err: *mut w::wasmtime_error_t) -> String {
    let mut message = w::wasm_name_t {
        size: 0,
        data: ptr::null_mut(),
    };
    w::wasmtime_error_message(err, &mut message);
    byte_vec_to_string(&mut message)
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn trap_message(trap: *mut w::wasm_trap_t) -> String {
    let mut message = w::wasm_name_t {
        size: 0,
        data: ptr::null_mut(),
    };
    w::wasm_trap_message(trap, &mut message);
    byte_vec_to_string(&mut message)
}

#[allow(unsafe_op_in_unsafe_fn)]
unsafe fn byte_vec_to_string(vec: &mut w::wasm_name_t) -> String {
    let slice = std::slice::from_raw_parts(vec.data, vec.size);
    let msg = String::from_utf8_lossy(slice).to_string();
    w::wasm_byte_vec_delete(vec);
    msg
}

fn run_exit(script: &str, args: &[String]) -> ExitCode {
    match run(script, args) {
        Ok(exit_code) => ExitCode::from(exit_code as u8),
        Err(err) => err_fail(err),
    }
}

#[derive(Parser)]
#[command(name = "moss", version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Run {
        script: String,
        args: Vec<String>,
    },
    Build {
        script: String,

        #[arg(short)]
        output: Option<PathBuf>,
    },
    Lsp,
}

fn main() -> ExitCode {
    // Handle shebangs.
    if let Some((file, args)) = std::env::args().skip(1).collect::<Vec<_>>().split_first() {
        // Prevent collision with possible future subcommands.
        if file.contains("/") {
            return run_exit(file, args);
        }
    }
    let args = Cli::parse();
    match args.command {
        Commands::Run { script, args } => run_exit(&script, &args),
        Commands::Build { script, output } => match build(&script, output.as_deref()) {
            Ok(()) => ExitCode::SUCCESS,
            Err(err) => err_fail(err),
        },
        Commands::Lsp => moss_cli::lsp::language_server(),
    }
}
