use std::{
    fs,
    io::{self, IsTerminal, Write},
    path::{Path, PathBuf},
    process::ExitCode,
};

use anyhow::{anyhow, bail};
use clap::{Parser, Subcommand};
use connie::{
    lex::lex,
    parse::{ParseError, parse},
    wasm::wasm,
};
use connie_cli::util::err_fail;
use line_index::{LineIndex, TextSize};
use wasmtime::{Engine, Linker, Module, Store};
use wasmtime_wasi::{WasiCtxBuilder, p1::WasiP1Ctx};

fn compile(script: &Path) -> anyhow::Result<Vec<u8>> {
    let source = fs::read_to_string(script)?;
    let lines = LineIndex::new(&source);
    let (tokens, starts) = lex(&source).map_err(|err| {
        let line_col = lines.line_col(TextSize::new(err.byte_range().start as u32));
        anyhow!(
            "{}:{}:{} {}",
            script.display(),
            line_col.line + 1,
            line_col.col + 1,
            err.message(),
        )
    })?;
    let tree = parse(&tokens).map_err(|err| match err {
        ParseError::Expected { id, tokens: _ } => {
            let start = starts[id];
            let line_col = lines.line_col(TextSize::new(start.raw()));
            anyhow!(
                "{}:{}:{} {}",
                script.display(),
                line_col.line + 1,
                line_col.col + 1,
                err.message(),
            )
        }
    })?;
    let bytes = wasm(&source, &starts, &tree);
    Ok(bytes)
}

fn build(script: &Path, output: Option<&Path>) -> anyhow::Result<()> {
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

fn run(script: &Path, _args: &[String]) -> anyhow::Result<i32> {
    let bytes = compile(script)?;
    let engine = Engine::default();
    let mut linker = Linker::new(&engine);
    wasmtime_wasi::p1::add_to_linker_sync(&mut linker, |ctx: &mut WasiP1Ctx| ctx)?;
    let wasi_p1 = WasiCtxBuilder::new().inherit_stdout().build_p1();
    let mut store = Store::new(&engine, wasi_p1);
    let module = Module::from_binary(&engine, &bytes)?;
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

fn run_exit(script: &Path, args: &[String]) -> ExitCode {
    match run(script, args) {
        Ok(exit_code) => ExitCode::from(exit_code as u8),
        Err(err) => err_fail(err),
    }
}

#[derive(Parser)]
#[command(name = "connie", version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Run {
        script: PathBuf,
        args: Vec<String>,
    },
    Build {
        script: PathBuf,

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
            return run_exit(Path::new(file), args);
        }
    }
    let args = Cli::parse();
    match args.command {
        Commands::Run { script, args } => run_exit(&script, &args),
        Commands::Build { script, output } => match build(&script, output.as_deref()) {
            Ok(()) => ExitCode::SUCCESS,
            Err(err) => err_fail(err),
        },
        Commands::Lsp => connie_cli::lsp::language_server(),
    }
}
