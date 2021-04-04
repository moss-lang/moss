mod codegen;
mod db;
mod lsp;
mod parser;
mod text;

use crate::{codegen::Codegen, db::QueryGroup};
use std::{env, path::PathBuf, rc::Rc, sync::Arc};
use structopt::StructOpt;
use url::Url;

const ABOUT: &str = r#"
Here is an example Quench program:

    #!/usr/bin/env quench
    print("Hello, world!");

Save the above contents as hello.qn and run this command:

    quench run hello.qn

Or on a Unix-like system, you could instead run these two commands:

    chmod +x hello.qn
    ./hello.qn

Either way, you should see this output:

    Hello, world!
"#;

#[derive(StructOpt)]
#[structopt(about = ABOUT)]
enum Opt {
    /// Compiles a file to JavaScript
    Compile { file: PathBuf },

    /// Starts the language server
    Lsp,

    /// Runs a script
    Run {
        /// Source file to run as a script
        file: PathBuf,

        /// Arguments to pass to the script
        args: Vec<String>,
    },
}

fn compile(file: PathBuf) -> anyhow::Result<String> {
    let uri = Url::from_file_path(file.canonicalize()?).unwrap();
    let mut db = db::Database::default();
    db.open_document(uri.clone(), slurp::read_all_to_string(file)?)?;

    let diagnostics = db.diagnostics(uri.clone());
    if diagnostics.is_empty() {
        db.compile(uri)
            .and_then(|compiled| Codegen::new().gen(compiled))
            .ok_or(anyhow::anyhow!("Failed to compile."))
    } else {
        for lspower::lsp::Diagnostic {
            severity,
            range: lspower::lsp::Range { start, end },
            message,
            ..
        } in diagnostics
        {
            let loc = format!(
                "{}:{} to {}:{}",
                start.line, start.character, end.line, end.character
            );
            match severity {
                Some(severity) => {
                    eprintln!("{} {:?}: {}", loc, severity, message);
                }
                None => {
                    eprintln!("{} {}", loc, message);
                }
            }
        }
        Err(anyhow::anyhow!("Failed to parse."))
    }
}

const DENO_VERSION: &str = "1.8.3";

fn run(file: PathBuf, args: Vec<String>) -> anyhow::Result<()> {
    let js = compile(file)?;

    let options = deno_runtime::worker::WorkerOptions {
        apply_source_maps: false,
        args,
        debug_flag: false,
        unstable: false,
        ca_data: None,
        // https://github.com/denoland/deno/blob/v1.8.3/cli/version.rs
        user_agent: format!("Deno/{}", DENO_VERSION),
        seed: None,
        module_loader: Rc::new(deno_core::FsModuleLoader),
        create_web_worker_cb: Arc::new(|_| todo!("Quench does not yet support web workers")),
        js_error_create_fn: None,
        attach_inspector: false,
        maybe_inspector_server: None,
        should_break_on_first_statement: false,
        runtime_version: DENO_VERSION.to_string(),
        // Quench doesn't compile to TypeScript so this is just a placeholder that shouldn't matter
        // https://github.com/denoland/deno/blob/v1.8.3/runtime/examples/hello_runtime.rs#L38
        ts_version: "x".to_string(),
        no_color: no_color::is_no_color(),
        get_error_class_fn: None,
        location: None,
    };
    // https://github.com/denoland/deno/blob/v1.8.3/cli/main.rs#L482
    let main_module = deno_core::resolve_path("$deno$eval.js").unwrap();
    let mut worker = deno_runtime::worker::MainWorker::from_options(
        main_module,
        deno_runtime::permissions::Permissions::allow_all(),
        &options,
    );
    worker.bootstrap(&options);

    worker.execute(&js)
}

fn main() -> anyhow::Result<()> {
    // handle shebangs
    if let Some((file, args)) = env::args().skip(1).collect::<Vec<_>>().split_first() {
        // prevent collision with possible future subcommands
        if file.contains("/") {
            return run(PathBuf::from(file), args.to_vec());
        }
    }
    match Opt::from_args() {
        Opt::Compile { file } => {
            print!("{}", compile(file)?);
            Ok(())
        }
        Opt::Lsp => Ok(lsp::main()),
        Opt::Run { file, args } => run(file, args),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;
    use regex::Regex;

    #[test]
    fn test_deno_version() {
        let deno_core_version = slurp::read_all_to_string("Cargo.toml")
            .unwrap()
            .parse::<toml::Value>()
            .unwrap()
            .get("dependencies")
            .unwrap()
            .get("deno_core")
            .unwrap()
            .as_str()
            .unwrap()
            .to_string();
        let (_, minor, patch) = Regex::new(r"0.(\d)(\d)")
            .unwrap()
            .captures(&deno_core_version)
            .unwrap()
            .iter()
            .collect_tuple()
            .unwrap();
        // I don't know if this correspondence is documented, but that doesn't really matter here
        // anyway since the point here is just to avoid accidentally updating the deno_core version
        // in Cargo.toml without also updating DENO_VERSION here
        assert_eq!(
            DENO_VERSION,
            format!("1.{}.{}", minor.unwrap().as_str(), patch.unwrap().as_str())
        );
    }
}
