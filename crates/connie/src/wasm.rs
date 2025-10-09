use itertools::Itertools;
use logos::Logos;
use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, ImportSection, MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::{
    lex::{Token, TokenId, TokenStarts, Tokens},
    parse::{Expr, Stmt, Tree},
};

const WASI_P1: &str = "wasi_snapshot_preview1";

pub fn wasm(source: &str, _: &Tokens, starts: &TokenStarts, tree: &Tree) -> Vec<u8> {
    let slice = |token: TokenId| {
        let start = starts[token].index();
        let (_, range) = Token::lexer(&source[start..]).spanned().next().unwrap();
        &source[(range.start + start)..(range.end + start)]
    };

    assert_eq!(tree.funcs.len(), 1);
    let main = tree.funcs[0];
    assert_eq!(slice(main.name), "main");
    assert_eq!(main.body.stmts.len(), 1);
    assert!(main.body.expr.is_none());
    let (stmt,) = main.body.stmts.into_iter().collect_tuple().unwrap();

    let mut section_type = TypeSection::new();
    let mut section_import = ImportSection::new();
    let mut section_function = FunctionSection::new();
    let mut section_memory = MemorySection::new();
    let mut section_export = ExportSection::new();
    let mut section_code = CodeSection::new();
    let mut section_data = DataSection::new();

    section_type.ty().function([], []);
    section_type.ty().function([ValType::I32], []);
    section_type.ty().function(
        [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
        [ValType::I32],
    );

    section_import
        .import(WASI_P1, "fd_write", EntityType::Function(2))
        .import(WASI_P1, "proc_exit", EntityType::Function(1));

    section_function.function(0);

    section_memory.memory(MemoryType {
        minimum: 1,
        maximum: Some(1),
        memory64: false,
        shared: false,
        page_size_log2: None,
    });

    section_export
        .export("_start", ExportKind::Func, 2)
        .export("memory", ExportKind::Memory, 0);

    let offset: u32 = 8;
    let (string, len) = match tree.stmts[stmt] {
        Stmt::Expr(expr) => match tree.exprs[expr] {
            Expr::String(_) => unimplemented!(),
            Expr::Call(name, arg) => {
                assert_eq!(slice(name), "println");
                match tree.exprs[arg] {
                    Expr::Call(_, _) => unimplemented!(),
                    Expr::String(string) => {
                        let with_quotes = slice(string);
                        let without_quotes = &with_quotes[1..with_quotes.len() - 1];
                        let with_newline = without_quotes.to_owned() + "\n";
                        let num_bytes = with_newline.len() as u32;
                        (with_newline, num_bytes)
                    }
                }
            }
        },
    };

    let mut func = Function::new([]);
    func.instructions()
        .i32_const(1)
        .i32_const(0)
        .i32_const(1)
        .i32_const((offset + len).next_multiple_of(4) as i32)
        .call(0)
        .drop()
        .i32_const(0)
        .call(1)
        .unreachable()
        .end();
    section_code.function(&func);

    section_data
        .active(0, &ConstExpr::i32_const(0), offset.to_le_bytes())
        .active(0, &ConstExpr::i32_const(4), len.to_le_bytes())
        .active(0, &ConstExpr::i32_const(8), string.bytes());

    let mut module = Module::new();
    module
        .section(&section_type)
        .section(&section_import)
        .section(&section_function)
        .section(&section_memory)
        .section(&section_export)
        .section(&section_code)
        .section(&section_data);
    module.finish()
}
