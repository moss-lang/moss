use std::collections::HashMap;

use index_vec::{IndexVec, define_index_type};
use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, GlobalSection, GlobalType, ImportSection, InstructionSink, MemArg,
    MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::{
    intern::StrId,
    lower::{self, IR, Instr, InstrId, Type},
    util::IdRange,
};

define_index_type! {
    pub struct TypeId = u32;
}

define_index_type! {
    pub struct LocalId = u32;
}

const WASI_P1: &str = "wasi_snapshot_preview1";

trait AsInstructionSink {
    fn insn(&mut self) -> InstructionSink<'_>;
}

impl AsInstructionSink for Vec<u8> {
    fn insn(&mut self) -> InstructionSink<'_> {
        InstructionSink::new(self)
    }
}

struct Wasm<'a> {
    ir: &'a IR,

    data_offset: i32,
    strings: HashMap<StrId, i32>,
    types: IndexVec<TypeId, ValType>,
    layouts: IndexVec<lower::TypeId, IdRange<TypeId>>,

    section_type: TypeSection,
    section_import: ImportSection,
    section_function: FunctionSection,
    section_memory: MemorySection,
    section_global: GlobalSection,
    section_export: ExportSection,
    section_code: CodeSection,
    section_data: DataSection,

    /// Locals for the current function.
    locals: IndexVec<LocalId, ValType>,

    // Start of local range for each SSA value in the current function.
    variables: HashMap<InstrId, LocalId>,

    /// The current function body.
    body: Vec<u8>,
}

impl<'a> Wasm<'a> {
    fn mem_global(&self) -> u32 {
        0
    }

    fn func_println(&self) -> u32 {
        2
    }

    fn get(&mut self, instr: InstrId) {
        let start = self.variables[&instr];
        let end = LocalId::from_usize(self.layouts[self.ir.vals[instr]].len());
        for localidx in (IdRange { start, end }) {
            self.body.insn().local_get(localidx.raw());
        }
    }

    fn set(&mut self, instr: InstrId) {
        let prev = self.variables.insert(instr, self.locals.len_idx());
        assert!(prev.is_none());
        for &ty in self.layouts[self.ir.vals[instr]]
            .get(&self.types)
            .iter()
            .rev()
        {
            let localidx = self.locals.push(ty);
            self.body.insn().local_set(localidx.raw());
        }
    }

    fn instrs(&mut self, instrs: IdRange<InstrId>) {
        for instr in instrs {
            match self.ir.instrs[instr] {
                Instr::String(id) => {
                    let string = &self.ir.strings[id];
                    let len = string.len() as i32;
                    let &mut offset = self.strings.entry(id).or_insert_with(|| {
                        let offset = self.data_offset;
                        self.section_data
                            .active(0, &ConstExpr::i32_const(offset), string.bytes());
                        self.data_offset += len;
                        offset
                    });
                    self.body.insn().i32_const(offset).i32_const(len);
                }
                Instr::Println(string) => {
                    let println = self.func_println();
                    self.get(string);
                    self.body.insn().call(println);
                }
                Instr::Provide(_, _, _) => todo!(),
            }
            self.set(instr);
        }
    }

    fn program(mut self) -> Vec<u8> {
        self.section_memory.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        self.section_export.export("memory", ExportKind::Memory, 0);

        for (id, ty) in self.ir.types.iter_enumerated() {
            let layout = match ty {
                Type::Unit => IdRange::new(&mut self.types, Vec::new()),
                Type::String => IdRange::new(&mut self.types, vec![ValType::I32, ValType::I32]),
            };
            assert_eq!(self.layouts.push(layout), id);
        }

        self.data_offset = 1;
        self.section_data
            .active(0, &ConstExpr::i32_const(0), "\n".bytes());

        let proc_exit = self.section_import.len();
        self.section_import.import(
            WASI_P1,
            "proc_exit",
            EntityType::Function(self.section_type.len()),
        );
        self.section_type.ty().function([ValType::I32], []);

        let fd_write = self.section_import.len();
        self.section_import.import(
            WASI_P1,
            "fd_write",
            EntityType::Function(self.section_type.len()),
        );
        self.section_type.ty().function(
            [ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            [ValType::I32],
        );

        assert_eq!(self.section_import.len(), self.func_println());
        self.section_function.function(self.section_type.len());
        self.section_type
            .ty()
            .function([ValType::I32, ValType::I32], []);
        self.section_code.function(&{
            let iovec = 2;
            let mut f = Function::new([(1, ValType::I32)]);
            f.instructions()
                .global_get(self.mem_global())
                .local_tee(iovec)
                .local_get(0)
                .i32_store(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                })
                .local_get(iovec)
                .local_get(1)
                .i32_store(MemArg {
                    offset: 4,
                    align: 2,
                    memory_index: 0,
                })
                .local_get(iovec)
                .i32_const(0)
                .i32_store(MemArg {
                    offset: 8,
                    align: 2,
                    memory_index: 0,
                })
                .local_get(iovec)
                .i32_const(1)
                .i32_store(MemArg {
                    offset: 12,
                    align: 2,
                    memory_index: 0,
                })
                .i32_const(1)
                .local_get(iovec)
                .i32_const(2)
                .local_get(iovec)
                .i32_const(16)
                .i32_add()
                .call(fd_write)
                .drop()
                .end();
            f
        });

        let func_offset = self.section_import.len() + self.section_function.len();

        for (id, func) in self.ir.funcs.iter_enumerated() {
            let params = self.layouts[func.param];
            self.section_function.function(self.section_type.len());
            self.section_type.ty().function(
                params.get(&self.types).iter().copied(),
                self.layouts[func.result].get(&self.types).iter().copied(),
            );
            self.instrs(func.body);
            if self.ir.main == Some(id) {
                self.body.insn().i32_const(0).call(proc_exit).unreachable();
                self.section_export
                    .export("_start", ExportKind::Func, func_offset + id.raw());
            }
            self.body.insn().end();
            let mut f =
                Function::new_with_locals_types(self.locals.iter().skip(params.len()).copied());
            f.raw(self.body);
            self.section_code.function(&f);
            self.locals = Default::default();
            self.variables = Default::default();
            self.body = Default::default();
        }

        self.section_global.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: false,
                shared: false,
            },
            &ConstExpr::i32_const((self.data_offset + 7) / 8 * 8),
        );

        let mut module = Module::new();
        module
            .section(&self.section_type)
            .section(&self.section_import)
            .section(&self.section_function)
            .section(&self.section_memory)
            .section(&self.section_global)
            .section(&self.section_export)
            .section(&self.section_code)
            .section(&self.section_data);
        module.finish()
    }
}

pub fn wasm(ir: &IR) -> Vec<u8> {
    Wasm {
        ir,

        data_offset: Default::default(),
        strings: Default::default(),
        types: Default::default(),
        layouts: Default::default(),
        variables: Default::default(),

        section_type: Default::default(),
        section_import: Default::default(),
        section_function: Default::default(),
        section_memory: Default::default(),
        section_global: Default::default(),
        section_export: Default::default(),
        section_code: Default::default(),
        section_data: Default::default(),

        locals: Default::default(),
        body: Default::default(),
    }
    .program()
}
