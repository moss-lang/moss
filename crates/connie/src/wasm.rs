use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, ImportSection, MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::{
    lex::{TokenStarts, Tokens},
    parse::Tree,
};

const WASI_P1: &str = "wasi_snapshot_preview1";

pub fn wasm(_: &str, _: &Tokens, _: &TokenStarts, _: &Tree) -> Vec<u8> {
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
    let string = "Hello, world!\n";
    let len = string.len() as u32;

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
