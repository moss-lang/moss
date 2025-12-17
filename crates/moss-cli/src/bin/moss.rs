use std::{
    fs,
    io::{self, IsTerminal, Write},
    iter,
    path::{Path, PathBuf},
    process::{Command, ExitCode, Stdio},
};

use anyhow::{anyhow, bail};
use clap::{Parser, Subcommand, ValueEnum};
use index_vec::Idx;
use line_index::{LineIndex, TextSize};
use moss_cli::util::err_fail;
use moss_core::{
    lex::{ByteIndex, LexError, lex},
    lower::lower,
    parse::{ParseError, parse},
    prelude::prelude,
    wasm::wasm,
};
use wasmtime::{Engine, Linker, Module, Store};
use wasmtime_wasi::{DirPerms, FilePerms, WasiCtxBuilder, p1::WasiP1Ctx};

#[derive(Copy, Clone, Debug, ValueEnum)]
enum EngineChoice {
    Wasmtime,
    Node,
}

const NODE_WASI_BOOTSTRAP: &str = r#"const { WASI } = require('node:wasi');
const { stdin, env, exit } = require('node:process');
const args = JSON.parse(env.MOSS_WASI_ARGS || '[]');
const wasi = new WASI({
  version: 'preview1',
  args,
  env,
  preopens: { '.': '.' },
  returnOnExit: true,
});
(async () => {
  const chunks = [];
  for await (const chunk of stdin) chunks.push(chunk);
  const module = await WebAssembly.compile(Buffer.concat(chunks));
  const imports = typeof wasi.getImportObject === 'function'
    ? wasi.getImportObject()
    : { wasi_snapshot_preview1: wasi.wasiImport };
  const instance = await WebAssembly.instantiate(module, imports);
  const code = wasi.start(instance);
  if (typeof code === 'number') {
    process.exitCode = code;
  }
})().catch((err) => {
  console.error(err);
  exit(1);
});
"#;

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

fn run(script: &str, args: &[String], engine: EngineChoice) -> anyhow::Result<i32> {
    let bytes = compile(script)?;
    match engine {
        EngineChoice::Wasmtime => run_wasmtime(script, args, &bytes),
        EngineChoice::Node => run_node(script, args, &bytes),
    }
}

fn run_wasmtime(script: &str, args: &[String], bytes: &[u8]) -> anyhow::Result<i32> {
    let engine = Engine::default();
    let mut linker = Linker::new(&engine);
    wasmtime_wasi::p1::add_to_linker_sync(&mut linker, |ctx: &mut WasiP1Ctx| ctx)?;
    let argv: Vec<&str> = iter::once(script)
        .chain(args.iter().map(String::as_str))
        .collect();
    let wasi_p1 = WasiCtxBuilder::new()
        .args(&argv)
        .preopened_dir(".", ".", DirPerms::all(), FilePerms::all())?
        .inherit_stdout()
        .build_p1();
    let mut store = Store::new(&engine, wasi_p1);
    let module = Module::from_binary(&engine, bytes)?;
    let instance = linker.instantiate(&mut store, &module)?;
    let start = instance.get_typed_func::<(), ()>(&mut store, "_start")?;
    match start.call(&mut store, ()) {
        Ok(()) => Ok(0),
        Err(err) => match err.downcast_ref::<wasmtime_wasi::I32Exit>() {
            Some(exit_code) => Ok(exit_code.0),
            None => Err(err),
        },
    }
}

fn run_node(script: &str, args: &[String], bytes: &[u8]) -> anyhow::Result<i32> {
    let argv: Vec<&str> = iter::once(script)
        .chain(args.iter().map(String::as_str))
        .collect();
    let encoded_args = serde_json::to_string(&argv)?;
    let mut child = Command::new("node");
    // Disable V8 fast API to avoid crashes with multiple memories (V8 bug 14260).
    let mut child = child
        .arg("--no-wasm-fast-api")
        .arg("--no-turbo-fast-api-calls")
        .arg("--experimental-wasi-unstable-preview1")
        .arg("--no-warnings")
        .arg("-e")
        .arg(NODE_WASI_BOOTSTRAP)
        .env("MOSS_WASI_ARGS", encoded_args)
        .stdin(Stdio::piped())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|err| anyhow!("failed to spawn node: {err}"))?;
    if let Some(stdin) = child.stdin.as_mut() {
        stdin.write_all(bytes)?;
    }
    drop(child.stdin.take());
    let status = child.wait()?;
    Ok(status.code().unwrap_or(1))
}

fn run_exit(script: &str, args: &[String], engine: EngineChoice) -> ExitCode {
    match run(script, args, engine) {
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

        #[arg(long, value_enum, default_value_t = EngineChoice::Wasmtime)]
        engine: EngineChoice,
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
            return run_exit(file, args, EngineChoice::Wasmtime);
        }
    }
    let args = Cli::parse();
    match args.command {
        Commands::Run {
            script,
            args,
            engine,
        } => run_exit(&script, &args, engine),
        Commands::Build { script, output } => match build(&script, output.as_deref()) {
            Ok(()) => ExitCode::SUCCESS,
            Err(err) => err_fail(err),
        },
        Commands::Lsp => moss_cli::lsp::language_server(),
    }
}
