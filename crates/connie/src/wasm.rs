use index_vec::{IndexVec, define_index_type};
use indexmap::IndexSet;
use wasm_encoder::{
    CodeSection, DataSection, EntityType, ExportKind, ExportSection, FunctionSection,
    ImportSection, MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::{
    intern::StrId,
    lower::{self, IR, Type},
    util::IdRange,
};

define_index_type! {
    pub struct TypeId = u32;
}

const WASI_P1: &str = "wasi_snapshot_preview1";

struct Wasm<'a> {
    ir: &'a IR,
    strings: IndexSet<StrId>,
    types: IndexVec<TypeId, ValType>,
    layouts: IndexVec<lower::TypeId, IdRange<TypeId>>,
}

impl<'a> Wasm<'a> {
    fn program(mut self) -> Vec<u8> {
        let mut section_type = TypeSection::new();
        let mut section_import = ImportSection::new();
        let mut section_function = FunctionSection::new();
        let mut section_memory = MemorySection::new();
        let mut section_export = ExportSection::new();
        let mut section_code = CodeSection::new();
        let mut section_data = DataSection::new();

        section_memory.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        section_export.export("memory", ExportKind::Memory, 0);

        for (id, ty) in self.ir.types.iter_enumerated() {
            let layout = match ty {
                Type::String => IdRange::new(&mut self.types, vec![ValType::I32, ValType::I32]),
            };
            assert_eq!(self.layouts.push(layout), id);
        }

        let proc_exit = section_import.len();
        section_import.import(
            WASI_P1,
            "proc_exit",
            EntityType::Function(section_type.len()),
        );
        section_type.ty().function([ValType::I32], []);

        let fd_write = section_import.len();
        section_import.import(
            WASI_P1,
            "fd_write",
            EntityType::Function(section_type.len()),
        );
        section_type.ty().function(
            [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32],
        );

        for (id, func) in self.ir.funcs.iter_enumerated() {
            section_function.function(section_type.len());
            section_type.ty().function(
                self.layouts[func.param].get(&self.types).iter().copied(),
                self.layouts[func.result].get(&self.types).iter().copied(),
            );
        }

        if let Some(main) = self.ir.main {
            section_export.export(
                "_start",
                ExportKind::Func,
                section_import.len() + main.raw(),
            );
        }

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
}

pub fn wasm(ir: &IR) -> Vec<u8> {
    Wasm {
        ir,
        strings: Default::default(),
        types: Default::default(),
        layouts: Default::default(),
    }
    .program()
}
