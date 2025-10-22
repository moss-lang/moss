use std::{
    fs::{self, DirEntry},
    io,
    path::{Path, PathBuf},
    process::Command,
    rc::Rc,
};

use anyhow::{Context, bail};
use clap::{Parser, Subcommand};
use indexmap::IndexMap;

struct Tests {
    fix: bool,
    current: Option<Rc<str>>,
    failures: IndexMap<Rc<str>, Vec<PathBuf>>,
}

impl Tests {
    fn write(&mut self, path: PathBuf, contents: &[u8]) -> anyhow::Result<()> {
        if self.fix {
            if contents.is_empty() {
                match fs::remove_file(path) {
                    Ok(()) => Ok(()),
                    Err(err) => match err.kind() {
                        io::ErrorKind::NotFound => Ok(()),
                        _ => bail!(err),
                    },
                }
            } else {
                let Some(parent) = path.parent() else {
                    bail!("no parent");
                };
                fs::create_dir_all(parent)?;
                fs::write(path, contents)?;
                Ok(())
            }
        } else {
            let expected = match fs::read(&path) {
                Ok(actual) => actual,
                Err(err) => match err.kind() {
                    io::ErrorKind::NotFound => Vec::new(),
                    _ => bail!(err),
                },
            };
            if contents != expected {
                let Some(current) = &self.current else {
                    bail!("no current test");
                };
                self.failures
                    .entry(Rc::clone(current))
                    .or_default()
                    .push(path);
            }
            Ok(())
        }
    }

    fn example(&mut self, entry: &DirEntry) -> anyhow::Result<()> {
        let path = entry.path();
        let output = Command::new("connie").arg(&path).output()?;
        let Some(stem) = path.file_stem() else {
            bail!("no file stem");
        };
        self.write(
            Path::new("tests/examples/status")
                .join(stem)
                .with_extension("txt"),
            if output.status.success() {
                String::new()
            } else {
                format!("{}", output.status)
            }
            .as_bytes(),
        )?;
        self.write(
            Path::new("tests/examples/stdout")
                .join(stem)
                .with_extension("txt"),
            &output.stdout,
        )?;
        self.write(
            Path::new("tests/examples/stderr")
                .join(stem)
                .with_extension("txt"),
            &output.stderr,
        )?;
        Ok(())
    }

    fn test(mut self) -> anyhow::Result<()> {
        for result in fs::read_dir("examples")? {
            let entry = result?;
            self.current = Some(Rc::from(format!("{}", entry.path().display())));
            self.example(&entry)
                .with_context(|| format!("{}", entry.path().display()))?;
            self.current = None;
        }
        if self.failures.is_empty() {
            return Ok(());
        };
        for (test, outputs) in self.failures {
            eprintln!("{test}");
            for output in outputs {
                eprintln!("  {}", output.display());
            }
        }
        bail!("some tests failed; run `dev test --fix` to automatically fix them");
    }
}

/// Tools for developing the Connie compiler
#[derive(Parser)]
#[command(name = "dev")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run the tests
    Test {
        /// Overwrite expected output with actual output
        #[arg(long)]
        fix: bool,
    },
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();
    match args.command {
        Commands::Test { fix } => {
            if !Command::new("cargo").arg("test").status()?.success() {
                bail!("`cargo test` failed");
            }
            Tests {
                fix,
                current: None,
                failures: IndexMap::new(),
            }
            .test()?;
            Ok(())
        }
    }
}
