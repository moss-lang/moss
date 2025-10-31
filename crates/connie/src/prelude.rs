use indexmap::IndexMap;

use crate::{
    lex::lex,
    lower::{IR, Named, Path, lower},
    parse::parse,
};

const WASM: &str = include_str!("../../../lib/wasm.con");
const WASIP1: &str = include_str!("../../../lib/wasip1.con");
const PRELUDE: &str = include_str!("../../../lib/prelude.con");

pub fn prelude() -> (IR, IndexMap<Path, Named>) {
    let mut ir = IR::default();
    {
        let source = WASM;
        let (tokens, starts) = lex(source).unwrap();
        let tree = parse(&tokens).unwrap();
        ir = lower(source, &starts, &tree, ir).unwrap();
    }
    {
        let source = WASIP1;
        let (tokens, starts) = lex(source).unwrap();
        let tree = parse(&tokens).unwrap();
        ir = lower(source, &starts, &tree, ir).unwrap();
    }
    {
        let source = PRELUDE;
        let (tokens, starts) = lex(source).unwrap();
        let tree = parse(&tokens).unwrap();
        ir = lower(source, &starts, &tree, ir).unwrap();
    }
    drop(ir);
    todo!()
}
