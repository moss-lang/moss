use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    fs, io,
    ops::Range,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};

use anyhow::{Context, bail};
use clap::{Parser, Subcommand};
use connie::{
    lex::{lex, relex},
    lower::lower,
    parse::{ParseError, parse},
    prelude::prelude,
};
use indexmap::IndexMap;
use line_index::{LineIndex, TextSize};

const PROFILE: &str = "release-with-debug";

fn get_errors(source: &str) -> Vec<(Option<Range<usize>>, String)> {
    let (tokens, starts) = match lex(source) {
        Ok(ok) => ok,
        Err(err) => return vec![(Some(err.byte_range()), err.message().to_string())],
    };
    let tree = match parse(&tokens) {
        Ok(ok) => ok,
        Err(err) => match err {
            ParseError::Expected { id, tokens: _ } => {
                return vec![(Some(relex(source, &starts, id)), err.message())];
            }
        },
    };
    let (mut ir, mut names, lib) = prelude();
    match lower(
        source,
        &starts,
        &tree,
        &mut ir,
        &mut names,
        lib.prelude,
        &[],
    ) {
        Ok(_) => {}
        Err(err) => {
            let (tokens, message) = err.describe(source, &starts, &tree, &ir, &names);
            let range = tokens.map(|range| {
                let start = starts[range.first].index();
                let end = relex(source, &starts, range.last).end;
                start..end
            });
            return vec![(range, message)];
        }
    };
    Vec::new()
}

struct Tests {
    binary: PathBuf,
    fix: bool,
    current: Option<Arc<str>>,
    failures: IndexMap<Arc<str>, Vec<PathBuf>>,
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
                    .entry(Arc::clone(current))
                    .or_default()
                    .push(path);
            }
            Ok(())
        }
    }

    fn example(&mut self, path: PathBuf) -> anyhow::Result<()> {
        let output = Command::new(&self.binary).arg(&path).output()?;
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

    fn errors(&mut self, path: PathBuf) -> anyhow::Result<()> {
        let source = itertools::join(
            fs::read_to_string(&path)?
                .lines()
                .filter(|line| !line.starts_with('#')),
            "\n",
        );
        let mut out = String::new();
        let mut errors = HashMap::new();
        let lines = LineIndex::new(&source);
        for (range, message) in get_errors(&source) {
            match range {
                None => writeln!(&mut out, "# {message}")?,
                Some(range) => {
                    let start = lines.line_col(TextSize::new(range.start as u32));
                    let end = lines.line_col(TextSize::new(range.end as u32));
                    if end.line != start.line {
                        bail!("error spanning multiple lines");
                    }
                    errors
                        .entry(start.line)
                        .or_insert_with(Vec::new)
                        .push((start.col, end.col, message));
                }
            }
        }
        for (i, line) in source.lines().enumerate() {
            writeln!(&mut out, "{line}")?;
            if let Some(errors) = errors.remove(&(i as u32)) {
                for (start, end, message) in errors {
                    let Some(spaces) = (start as usize).checked_sub(1) else {
                        bail!("error at very beginning of line");
                    };
                    let carets = (end - start) as usize;
                    writeln!(&mut out, "#{:spaces$}{:^<carets$} {message}", "", "")?;
                }
            }
        }
        if !errors.is_empty() {
            bail!("not all errors were successfully written back");
        }
        self.write(path, out.as_bytes())?;
        Ok(())
    }

    fn test(mut self, tests: HashSet<String>) -> anyhow::Result<()> {
        for result in fs::read_dir("examples")? {
            let path = result?.path();
            let name = Arc::<str>::from(format!("{}", path.display()));
            if !tests.is_empty() && !tests.contains(name.as_ref()) {
                continue;
            }
            self.current = Some(Arc::clone(&name));
            self.example(path).context(name)?;
            self.current = None;
        }
        for result in fs::read_dir("tests/errors")? {
            let path = result?.path();
            let name = Arc::<str>::from(format!("{}", path.display()));
            if !tests.is_empty() && !tests.contains(name.as_ref()) {
                continue;
            }
            self.current = Some(Arc::clone(&name));
            self.errors(path).context(name)?;
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

        /// Run only some tests instead of all
        test: Vec<String>,

        /// Skip `cargo test`
        #[arg(long)]
        skip_cargo_test: bool,

        /// Use an existing `connie` binary instead of rebuilding
        #[arg(long)]
        prebuilt: Option<PathBuf>,
    },
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();
    match args.command {
        Commands::Test {
            fix,
            test,
            skip_cargo_test,
            prebuilt,
        } => {
            if test.is_empty()
                && !skip_cargo_test
                && !Command::new("cargo").arg("test").status()?.success()
            {
                bail!("`cargo test` failed");
            }
            Tests {
                binary: match prebuilt {
                    Some(path) => path,
                    None => {
                        if !Command::new("cargo")
                            .args(["build", "--package=connie-cli", "--profile", PROFILE])
                            .status()?
                            .success()
                        {
                            bail!("`cargo build` failed");
                        }
                        PathBuf::from(format!("target/{PROFILE}/connie"))
                    }
                },
                fix,
                current: None,
                failures: IndexMap::new(),
            }
            .test(HashSet::from_iter(test))?;
            Ok(())
        }
    }
}
