use std::{
    fs,
    path::{Path, PathBuf},
    process::ExitCode,
};

use anyhow::anyhow;
use clap::{Parser, Subcommand};
use connie::{
    lex::lex,
    parse::{ParseError, parse},
};
use connie_cli::util::err_fail;
use line_index::{LineIndex, TextSize};

fn run(script: &Path, _args: &[String]) -> anyhow::Result<()> {
    let source = fs::read_to_string(script)?;
    let lines = LineIndex::new(&source);
    let (tokens, token_starts) = lex(&source).map_err(|err| {
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
            let start = token_starts[id];
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
    println!("{tree:?}");
    Ok(())
}

#[derive(Parser)]
#[command(name = "connie", version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Run { script: PathBuf, args: Vec<String> },
    Lsp,
}

fn main() -> ExitCode {
    // Handle shebangs.
    if let Some((file, args)) = std::env::args().skip(1).collect::<Vec<_>>().split_first() {
        // Prevent collision with possible future subcommands.
        if file.contains("/") {
            return match run(Path::new(file), args) {
                Ok(()) => ExitCode::SUCCESS,
                Err(err) => err_fail(err),
            };
        }
    }
    let args = Cli::parse();
    match args.command {
        Commands::Run { script, args } => match run(&script, &args) {
            Ok(()) => ExitCode::SUCCESS,
            Err(err) => err_fail(err),
        },
        Commands::Lsp => connie_cli::lsp::language_server(),
    }
}
