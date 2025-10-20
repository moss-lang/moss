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
    pub struct GlobalId = u32;
}

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

    /// Start of global range for each var.
    vars: IndexVec<lower::VarId, GlobalId>,

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

    /// Start of local range for each SSA value in the current function.
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

    fn get_local(&mut self, ty: lower::TypeId, start: LocalId) {
        let len = self.layouts[ty].len();
        let end = LocalId::from_usize(start.index() + len);
        for localidx in (IdRange { start, end }) {
            self.body.insn().local_get(localidx.raw());
        }
    }

    fn get(&mut self, instr: InstrId) {
        self.get_local(self.ir.vals[instr], self.variables[&instr])
    }

    fn set(&mut self, instr: InstrId) {
        let start = self.locals.len_idx();
        let prev = self.variables.insert(instr, start);
        assert!(prev.is_none());
        let layout = self.layouts[self.ir.vals[instr]];
        for &ty in layout.get(&self.types) {
            self.locals.push(ty);
        }
        let end = LocalId::from_usize(start.index() + layout.len());
        for localidx in (IdRange { start, end }).into_iter().rev() {
            self.body.insn().local_set(localidx.raw());
        }
    }

    fn instrs(&mut self, param: lower::TypeId, mut instr: InstrId) {
        loop {
            match self.ir.instrs[instr] {
                Instr::Unit => {}
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
                Instr::Param => {
                    self.get_local(param, LocalId::from_raw(0));
                }
                Instr::Get(var) => {
                    let start = self.vars[var];
                    let len = self.layouts[self.ir.vars[var].ty].len();
                    let end = GlobalId::from_usize(start.index() + len);
                    for globalidx in (IdRange { start, end }) {
                        self.body.insn().global_get(globalidx.raw());
                    }
                }
                Instr::Provide { var, val, pop: _ } => {
                    self.get(val);
                    let start = self.vars[var];
                    let len = self.layouts[self.ir.vars[var].ty].len();
                    let end = GlobalId::from_usize(start.index() + len);
                    for globalidx in (IdRange { start, end }).into_iter().rev() {
                        self.body.insn().global_set(globalidx.raw());
                    }
                    // TODO: Reset it when the `pop` instruction is reached.
                }
                Instr::Call(func, val) => {
                    let funcidx_offset = self.func_println() + 1;
                    self.get(val);
                    self.body.insn().call(funcidx_offset + func.raw());
                }
                Instr::Println(string) => {
                    let println = self.func_println();
                    self.get(string);
                    self.body.insn().call(println);
                }
                Instr::Return(val) => {
                    self.get(val);
                    break;
                }
            }
            self.set(instr);
            instr = InstrId::from_raw(instr.raw() + 1);
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

        for (i, ty) in self.ir.types.iter().enumerate() {
            let id = lower::TypeId::from_usize(i);
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

        let mut global_offset = 1;
        for var in &self.ir.vars {
            self.vars.push(GlobalId::from_usize(global_offset));
            global_offset += self.layouts[var.ty].len();
        }

        for (id, func) in self.ir.funcs.iter_enumerated() {
            let params = self.layouts[func.param];
            self.section_function.function(self.section_type.len());
            self.section_type.ty().function(
                params.get(&self.types).iter().copied(),
                self.layouts[func.result].get(&self.types).iter().copied(),
            );
            self.instrs(func.param, self.ir.bodies[id]);
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

        assert_eq!(self.section_global.len(), self.mem_global());
        self.section_global.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: false,
                shared: false,
            },
            &ConstExpr::i32_const((self.data_offset + 7) / 8 * 8),
        );
        for var in &self.ir.vars {
            for ty in self.layouts[var.ty] {
                self.section_global.global(
                    GlobalType {
                        val_type: self.types[ty],
                        mutable: true,
                        shared: false,
                    },
                    &ConstExpr::i32_const(0),
                );
            }
        }

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
        vars: Default::default(),

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
