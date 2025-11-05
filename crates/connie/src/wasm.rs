use std::collections::HashMap;

use index_vec::{IndexVec, define_index_type};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection,
    Function, FunctionSection, GlobalSection, GlobalType, ImportSection, InstructionSink, MemArg,
    MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::{
    intern::StrId,
    lower::{self, FndefId, IR, Instr, Int32Arith, Int32Comp, Names, Type, ValdefId},
    prelude::Lib,
    tuples::TupleLoc,
    util::IdRange,
};

define_index_type! {
    struct GlobalId = u32;
}

define_index_type! {
    struct TypeId = u32;
}

define_index_type! {
    struct TypeOffset = u32;
}

define_index_type! {
    struct LocalId = u32;
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

#[derive(Clone, Copy)]
struct Layout {
    types: IdRange<TypeId>,
}

#[derive(Clone, Copy)]
struct Tmp {
    ty: lower::TypeId,
    start: LocalId,
}

struct Wasm<'a> {
    ir: &'a IR,
    names: &'a Names,
    lib: Lib,
    main: FndefId,

    data_offset: i32,
    strings: HashMap<StrId, i32>,
    types: IndexVec<TypeId, ValType>,
    layouts: IndexVec<lower::TypeId, Layout>,
    offsets: IndexVec<TupleLoc, TypeOffset>,

    /// Start of global range for each `val`.
    valdefs: IndexVec<ValdefId, GlobalId>,

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

    /// Start of local range for each IR local in the current function.
    variables: HashMap<lower::LocalId, LocalId>,

    /// The current function body.
    body: Vec<u8>,
}

impl<'a> Wasm<'a> {
    fn mem_global(&self) -> u32 {
        0
    }

    fn func_args(&self) -> u32 {
        4
    }

    fn func_println(&self) -> u32 {
        5
    }

    fn make_locals(&mut self, ty: lower::TypeId) -> LocalId {
        let start = self.locals.len_idx();
        let layout = self.layouts[ty];
        for &ty in layout.types.get(&self.types) {
            self.locals.push(ty);
        }
        start
    }

    fn get_locals(&mut self, ty: lower::TypeId, start: LocalId) {
        let len = self.layouts[ty].types.len();
        let end = LocalId::from_usize(start.index() + len);
        for localidx in (IdRange { start, end }) {
            self.body.insn().local_get(localidx.raw());
        }
    }

    fn set_locals(&mut self, ty: lower::TypeId, start: LocalId) {
        let layout = self.layouts[ty];
        let end = LocalId::from_usize(start.index() + layout.types.len());
        for localidx in (IdRange { start, end }).into_iter().rev() {
            self.body.insn().local_set(localidx.raw());
        }
    }

    fn get(&mut self, instr: lower::LocalId) {
        self.get_locals(self.ir.locals[instr], self.variables[&instr])
    }

    fn set(&mut self, instr: lower::LocalId) {
        let ty = self.ir.locals[instr];
        let start = self.make_locals(ty);
        let prev = self.variables.insert(instr, start);
        assert!(prev.is_none());
        self.set_locals(ty, start);
    }

    fn get_tmp(&mut self, tmp: Tmp) {
        self.get_locals(tmp.ty, tmp.start);
    }

    fn set_tmp(&mut self, ty: lower::TypeId) -> Tmp {
        let start = self.make_locals(ty);
        self.set_locals(ty, start);
        Tmp { ty, start }
    }

    fn get_val(&mut self, valdef: ValdefId) {
        let start = self.valdefs[valdef];
        let len = self.layouts[self.ir.valdefs[valdef].ty].types.len();
        let end = GlobalId::from_usize(start.index() + len);
        for globalidx in (IdRange { start, end }) {
            self.body.insn().global_get(globalidx.raw());
        }
    }

    fn set_val(&mut self, valdef: ValdefId) {
        let start = self.valdefs[valdef];
        let len = self.layouts[self.ir.valdefs[valdef].ty].types.len();
        let end = GlobalId::from_usize(start.index() + len);
        for globalidx in (IdRange { start, end }).into_iter().rev() {
            self.body.insn().global_set(globalidx.raw());
        }
    }

    fn instrs(&mut self, param: lower::TypeId, mut instr: lower::LocalId) -> lower::LocalId {
        loop {
            match self.ir.instrs[instr] {
                Instr::Int32(n) => {
                    self.body.insn().i32_const(n);
                }
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
                Instr::Tuple(tuple) => {
                    for &id in tuple.get(&self.ir.refs) {
                        self.get(id);
                    }
                }
                Instr::Elem(tuple, index) => {
                    let ty = self.ir.locals[tuple];
                    let range = self.ir.types[ty.index()].tuple();
                    let elem = TupleLoc::from_raw(range.start.raw() + index.raw());
                    let start =
                        LocalId::from_raw(self.variables[&tuple].raw() + self.offsets[elem].raw());
                    self.get_locals(self.ir.tuples[elem], start);
                }
                Instr::Int32Arith(a, op, b) => {
                    self.get(a);
                    self.get(b);
                    match op {
                        Int32Arith::Add => self.body.insn().i32_add(),
                        Int32Arith::Sub => self.body.insn().i32_sub(),
                        Int32Arith::Mul => self.body.insn().i32_mul(),
                        Int32Arith::Div => self.body.insn().i32_div_s(),
                    };
                }
                Instr::Int32Comp(a, op, b) => {
                    self.get(a);
                    self.get(b);
                    match op {
                        Int32Comp::Neq => self.body.insn().i32_ne(),
                        Int32Comp::Less => self.body.insn().i32_lt_s(),
                    };
                }
                Instr::Set(lhs, rhs) => {
                    self.get(rhs);
                    let ty = self.ir.locals[lhs];
                    let &start = self.variables.get(&lhs).unwrap();
                    self.set_locals(ty, start);
                }
                Instr::Param => {
                    self.get_locals(param, LocalId::from_raw(0));
                }
                Instr::Val(valdef) => self.get_val(valdef),
                Instr::Call(func, local) => {
                    let funcidx_offset = self.func_println() + 1;
                    self.get(local);
                    self.body.insn().call(funcidx_offset + func.raw());
                }
                Instr::BindVal(valdef, local) => {
                    self.get_val(valdef);
                    let tmp = self.set_tmp(self.ir.valdefs[valdef].ty);
                    self.get(local);
                    self.set_val(valdef);
                    instr = self.instrs(param, lower::LocalId::from_raw(instr.raw() + 1));
                    self.get_tmp(tmp);
                    self.set_val(valdef);
                }
                Instr::If(cond) => {
                    self.get(cond);
                    self.body.insn().if_(BlockType::Empty);
                    instr = self.instrs(param, lower::LocalId::from_raw(instr.raw() + 1));
                    self.body.insn().end();
                }
                Instr::Loop => {
                    self.body.insn().loop_(BlockType::Empty);
                    instr = self.instrs(param, lower::LocalId::from_raw(instr.raw() + 1));
                    self.body.insn().end();
                }
                Instr::Br(depth) => {
                    self.body.insn().br(depth.0);
                }
                Instr::End => break,
                Instr::Return(local) => {
                    self.get(local);
                    break;
                }
            }
            self.set(instr);
            instr = lower::LocalId::from_raw(instr.raw() + 1);
        }
        instr
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

        for (i, &ty) in self.ir.types.iter().enumerate() {
            let id = lower::TypeId::from_usize(i);
            let types = match ty {
                Type::String => IdRange::new(&mut self.types, vec![ValType::I32, ValType::I32]),
                Type::Bool => IdRange::new(&mut self.types, vec![ValType::I32]),
                Type::Int32 => IdRange::new(&mut self.types, vec![ValType::I32]),
                Type::Tuple(elems) => {
                    let mut types = Vec::new();
                    for &elem in &self.ir.tuples[elems] {
                        types.extend_from_slice(self.layouts[elem].types.get(&self.types));
                    }
                    IdRange::new(&mut self.types, types)
                }
            };
            let layout = Layout { types };
            assert_eq!(self.layouts.push(layout), id);
        }
        for range in self.ir.tuples.ranges() {
            let mut offset = TypeOffset::new(0);
            for &ty in &self.ir.tuples[range] {
                self.offsets.push(offset);
                offset = TypeOffset::from_usize(offset.index() + self.layouts[ty].types.len());
            }
        }
        assert_eq!(self.offsets.len(), self.ir.tuples.count());

        self.data_offset = 1;
        self.section_data
            .active(0, &ConstExpr::i32_const(0), "\n".bytes());

        let args_get = self.section_import.len();
        self.section_import.import(
            WASI_P1,
            "args_get",
            EntityType::Function(self.section_type.len()),
        );
        self.section_type
            .ty()
            .function([ValType::I32, ValType::I32], [ValType::I32]);

        let args_sizes_get = self.section_import.len();
        self.section_import.import(
            WASI_P1,
            "args_sizes_get",
            EntityType::Function(self.section_type.len()),
        );
        self.section_type
            .ty()
            .function([ValType::I32, ValType::I32], [ValType::I32]);

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

        let proc_exit = self.section_import.len();
        self.section_import.import(
            WASI_P1,
            "proc_exit",
            EntityType::Function(self.section_type.len()),
        );
        self.section_type.ty().function([ValType::I32], []);

        assert_eq!(self.section_import.len(), self.func_args());
        self.section_function.function(self.section_type.len());
        self.section_type
            .ty()
            .function([], [ValType::I32, ValType::I32]);
        self.section_code.function(&{
            let pointer = 0;
            let argc = 1;
            let size = 2;
            let argv = 3;
            let i = 4;
            let prev = 5;
            let curr = 6;
            let write = 7;
            let mut f = Function::new([(8, ValType::I32)]);
            f.instructions()
                .global_get(self.mem_global())
                .local_tee(pointer)
                .local_get(pointer)
                .i32_const(4)
                .i32_add()
                .call(args_sizes_get)
                .if_(BlockType::Empty)
                .unreachable()
                .end()
                .local_get(pointer)
                .i32_load(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                })
                .local_set(argc)
                .local_get(pointer)
                .i32_load(MemArg {
                    offset: 4,
                    align: 2,
                    memory_index: 0,
                })
                .local_set(size)
                .local_get(pointer)
                .local_get(size)
                .i32_add()
                .i32_const(3)
                .i32_add()
                .i32_const(2)
                .i32_shr_u()
                .i32_const(2)
                .i32_shl()
                .local_set(argv)
                .local_get(argv)
                .local_get(argc)
                .i32_const(3)
                .i32_shl()
                .i32_add()
                .global_set(self.mem_global())
                .local_get(argv)
                .local_get(pointer)
                .call(args_get)
                .if_(BlockType::Empty)
                .unreachable()
                .end()
                .local_get(argc)
                .local_set(i)
                .local_get(pointer)
                .local_get(size)
                .i32_add()
                .local_set(prev)
                .loop_(BlockType::Empty)
                .local_get(i)
                .if_(BlockType::Empty)
                .local_get(i)
                .i32_const(1)
                .i32_sub()
                .local_set(i)
                .local_get(argv)
                .local_get(i)
                .i32_const(2)
                .i32_shl()
                .i32_add()
                .i32_load(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                })
                .local_set(curr)
                .local_get(argv)
                .local_get(i)
                .i32_const(3)
                .i32_shl()
                .i32_add()
                .local_set(write)
                .local_get(write)
                .local_get(prev)
                .i32_const(1)
                // These strings are null-terminated.
                .i32_sub()
                .local_get(curr)
                .i32_sub()
                .i32_store(MemArg {
                    offset: 4,
                    align: 2,
                    memory_index: 0,
                })
                .local_get(write)
                .local_get(curr)
                .i32_store(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                })
                .local_get(curr)
                .local_set(prev)
                .br(1)
                .end()
                .end()
                .local_get(argv)
                .local_get(argc)
                .end();
            f
        });

        assert_eq!(
            self.section_import.len() + self.section_function.len(),
            self.func_println(),
        );
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
                .i32_const(1)
                .local_get(iovec)
                .i32_const(1)
                .local_get(iovec)
                .i32_const(8)
                .i32_add()
                .call(fd_write)
                .if_(BlockType::Empty)
                .unreachable()
                .end()
                .local_get(iovec)
                .i32_const(0)
                .i32_store(MemArg {
                    offset: 0,
                    align: 2,
                    memory_index: 0,
                })
                .local_get(iovec)
                .i32_const(1)
                .i32_store(MemArg {
                    offset: 4,
                    align: 2,
                    memory_index: 0,
                })
                .i32_const(1)
                .local_get(iovec)
                .i32_const(1)
                .local_get(iovec)
                .i32_const(8)
                .i32_add()
                .call(fd_write)
                .if_(BlockType::Empty)
                .unreachable()
                .end()
                .end();
            f
        });

        let func_offset = self.section_import.len() + self.section_function.len();

        let mut global_offset = 1;
        for valdef in &self.ir.valdefs {
            self.valdefs.push(GlobalId::from_usize(global_offset));
            global_offset += self.layouts[valdef.ty].types.len();
        }

        for (id, fndef) in self.ir.fndefs.iter_enumerated() {
            let params = self.layouts[fndef.param];
            self.section_function.function(self.section_type.len());
            self.section_type.ty().function(
                params.types.get(&self.types).iter().copied(),
                self.layouts[fndef.result]
                    .types
                    .get(&self.types)
                    .iter()
                    .copied(),
            );
            self.instrs(fndef.param, self.ir.bodies[id]);
            self.body.insn().i32_const(0).call(proc_exit).unreachable();
            self.section_export
                .export("_start", ExportKind::Func, func_offset + self.main.raw());
            self.body.insn().end();
            let mut f = Function::new_with_locals_types(
                self.locals.iter().skip(params.types.len()).copied(),
            );
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
                mutable: true,
                shared: false,
            },
            &ConstExpr::i32_const((self.data_offset + 7) / 8 * 8),
        );
        for valdef in &self.ir.valdefs {
            for ty in self.layouts[valdef.ty].types {
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

pub fn wasm(ir: &IR, names: &Names, lib: Lib, main: FndefId) -> Vec<u8> {
    Wasm {
        ir,
        names,
        lib,
        main,

        data_offset: Default::default(),
        strings: Default::default(),
        types: Default::default(),
        layouts: Default::default(),
        offsets: Default::default(),
        valdefs: Default::default(),

        section_type: Default::default(),
        section_import: Default::default(),
        section_function: Default::default(),
        section_memory: Default::default(),
        section_global: Default::default(),
        section_export: Default::default(),
        section_code: Default::default(),
        section_data: Default::default(),

        locals: Default::default(),
        variables: Default::default(),
        body: Default::default(),
    }
    .program()
}
