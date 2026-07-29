use std::{env, fs};

use anyhow::{bail, Context, Result};
use wasmtime::{Config, Engine};

fn main() -> Result<()> {
    let mut args = env::args_os().skip(1);
    let target = args.next().context("missing target triple")?;
    let input = args.next().context("missing input path")?;
    let output = args.next().context("missing output path")?;
    if args.next().is_some() {
        bail!("usage: moss-precompiler TARGET INPUT OUTPUT");
    }

    let mut config = Config::new();
    config.target(target.to_str().context("target triple is not UTF-8")?)?;
    let engine = Engine::new(&config)?;
    let compiled = engine.precompile_module(&fs::read(input)?)?;
    fs::write(output, compiled)?;
    Ok(())
}
