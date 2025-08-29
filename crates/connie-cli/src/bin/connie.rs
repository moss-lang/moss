use std::process::ExitCode;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "connie", version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Lsp,
}

fn main() -> ExitCode {
    let args = Cli::parse();
    match args.command {
        Commands::Lsp => connie_cli::lsp::language_server(),
    }
}
