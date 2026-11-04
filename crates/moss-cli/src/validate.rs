//! Validation of the modules the driver hands on: to Binaryen, and to
//! whoever reads what `moss build` writes.
//!
//! An invalid module is never a user error: the compiler accepted the
//! program, so it owes a valid module for it. Left alone, `moss build` wrote
//! one with exit status 0, which every engine then refused to load. Binaryen
//! does not catch it: the driver uses its C API, which reads a module without
//! validating it and does not enforce the limits engines impose (it read a
//! function type of 4,477 parameters). Given an invalid module it may write a
//! valid one that means something else, or abort the process. So the
//! compiler's output is checked before Binaryen reads it, and the artifact
//! after Binaryen writes it.
//!
//! The validator is the Wasmtime engine the driver already links, so a module
//! passes exactly when this driver could run it, limits included (at most
//! 1000 parameters per function type, for one), at no cost in size. Its
//! message is all a user gets, and all a user could act on; `wasm-tools
//! validate` gives a developer more.

use anyhow::{bail, Result};

use crate::wasmtime::{self, Engine};

/// What to say about an invalid module the compiler is responsible for.
pub const COMPILER_BUG: &str = "This is a compiler bug, not a problem with your program; \
please report it at https://github.com/moss-lang/moss/issues with the program that triggered it.";

/// Which step wrote a module, for the message when it is invalid.
#[derive(Clone, Copy, Debug)]
pub enum Producer {
    /// The compiler's output, as it wrote it.
    Compiler,
    /// Binaryen's rewrite of the compiler's output, which had validated.
    Binaryen,
}

pub fn validate(engine: &Engine, bytes: &[u8], producer: Producer) -> Result<()> {
    let Err(error) = wasmtime::validate(engine, bytes) else {
        return Ok(());
    };
    match producer {
        Producer::Compiler => {
            bail!("the compiler wrote an invalid WebAssembly module: {error:#}\n{COMPILER_BUG}")
        }
        Producer::Binaryen => bail!(
            "Binaryen's optimizer turned a valid WebAssembly module into an invalid one: \
             {error:#}\nThis is a bug in Binaryen or in how moss drives it, not a problem \
             with your program; please report it at https://github.com/moss-lang/moss/issues \
             with the program and the -O level. Building without -O avoids it."
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A module of one function type with `params` i32 parameters and one
    /// function of that type whose body is `body` (without its locals).
    fn module(params: u32, body: &[u8]) -> Vec<u8> {
        fn leb(mut value: u32, out: &mut Vec<u8>) {
            loop {
                let byte = (value & 0x7f) as u8;
                value >>= 7;
                if value == 0 {
                    out.push(byte);
                    break;
                }
                out.push(byte | 0x80);
            }
        }
        fn section(id: u8, contents: &[u8], out: &mut Vec<u8>) {
            out.push(id);
            leb(contents.len() as u32, out);
            out.extend_from_slice(contents);
        }
        let mut wasm = b"\0asm\x01\0\0\0".to_vec();
        let mut types = vec![1, 0x60];
        leb(params, &mut types);
        types.extend(std::iter::repeat(0x7f).take(params as usize));
        types.push(0); // No results.
        section(1, &types, &mut wasm);
        section(3, &[1, 0], &mut wasm);
        let mut code = vec![1];
        let mut entry = vec![0]; // No locals.
        entry.extend_from_slice(body);
        leb(entry.len() as u32, &mut code);
        code.extend(entry);
        section(10, &code, &mut wasm);
        wasm
    }

    fn error(bytes: &[u8], producer: Producer) -> String {
        let engine = Engine::new().unwrap();
        validate(&engine, bytes, producer).unwrap_err().to_string()
    }

    #[test]
    fn a_valid_module_passes() {
        let engine = Engine::new().unwrap();
        validate(&engine, &module(1000, &[0x0b]), Producer::Compiler).unwrap();
    }

    /// The shadow example's first design: 4,477 parameters, which the
    /// specification allows and every engine refuses.
    #[test]
    fn too_many_parameters_fail() {
        let message = error(&module(4477, &[0x0b]), Producer::Compiler);
        assert!(
            message.contains("function params size is out of bounds"),
            "{message}"
        );
        assert!(message.contains("This is a compiler bug"), "{message}");
    }

    #[test]
    fn a_bad_body_fails() {
        // `i64.const 0` left on the stack of a function with no results.
        let message = error(&module(0, &[0x42, 0x00, 0x0b]), Producer::Compiler);
        assert!(message.contains("type mismatch"), "{message}");
    }

    #[test]
    fn binaryen_is_blamed_for_its_own_output() {
        let message = error(&module(4477, &[0x0b]), Producer::Binaryen);
        assert!(message.contains("Building without -O"), "{message}");
    }
}
