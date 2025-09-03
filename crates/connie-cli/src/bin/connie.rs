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
    // Handle shebangs.
    if let Some((file, args)) = std::env::args().skip(1).collect::<Vec<_>>().split_first() {
        // Prevent collision with possible future subcommands.
        if file.contains("/") {
            println!("{file} {args:?}");
            return ExitCode::SUCCESS;
        }
    }
    let args = Cli::parse();
    match args.command {
        Commands::Lsp => connie_cli::lsp::language_server(),
    }
}
