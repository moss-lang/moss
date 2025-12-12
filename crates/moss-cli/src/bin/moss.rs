use std::{
    fs,
    io::{self, IsTerminal, Write},
    iter,
    path::{Path, PathBuf},
    process::ExitCode,
};

use anyhow::{anyhow, bail};
use clap::{Parser, Subcommand};
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
#[path = "../binaryen.rs"]
mod binaryen;
use binaryen::{Opt, optimize_wasm, parse_opt};
use wasmtime::{Engine, Linker, Module, Store};
use wasmtime_wasi::{DirPerms, FilePerms, WasiCtxBuilder, p1::WasiP1Ctx};

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

fn compile(script: &str, opt: Option<Opt>) -> anyhow::Result<Vec<u8>> {
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
    match opt {
        Some(opt) => optimize_wasm(bytes, opt),
        None => Ok(bytes),
    }
}

fn build(script: &str, output: Option<&Path>, opt: Option<Opt>) -> anyhow::Result<()> {
    let bytes = compile(script, opt)?;
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

fn run(script: &str, args: &[String], opt: Option<Opt>) -> anyhow::Result<i32> {
    let bytes = compile(script, opt)?;
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

fn run_exit(script: &str, args: &[String], opt: Option<Opt>) -> ExitCode {
    match run(script, args, opt) {
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
        #[arg(
            short = 'O',
            long = "opt-level",
            value_name = "LEVEL",
            num_args = 0..=1,
            default_missing_value = "2",
            value_parser = parse_opt,
            help = "Binaryen optimization level (0,1,2,3,s,z)"
        )]
        opt: Option<Opt>,
    },
    Build {
        script: String,

        #[arg(short)]
        output: Option<PathBuf>,

        #[arg(
            short = 'O',
            long = "opt-level",
            value_name = "LEVEL",
            num_args = 0..=1,
            default_missing_value = "2",
            value_parser = parse_opt,
            help = "Binaryen optimization level (0,1,2,3,s,z)"
        )]
        opt: Option<Opt>,
    },
    Lsp,
}

fn main() -> ExitCode {
    // Handle shebangs.
    if let Some((file, args)) = std::env::args().skip(1).collect::<Vec<_>>().split_first() {
        // Prevent collision with possible future subcommands.
        if file.contains("/") {
            return run_exit(file, args, None);
        }
    }
    let args = Cli::parse();
    match args.command {
        Commands::Run { script, args, opt } => run_exit(&script, &args, opt),
        Commands::Build {
            script,
            output,
            opt,
        } => match build(&script, output.as_deref(), opt) {
            Ok(()) => ExitCode::SUCCESS,
            Err(err) => err_fail(err),
        },
        Commands::Lsp => moss_cli::lsp::language_server(),
    }
}
