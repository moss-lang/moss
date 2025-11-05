use std::{collections::HashMap, mem::take};

use index_vec::{IndexVec, define_index_type};
use strum::{EnumIter, IntoEnumIterator, IntoStaticStr};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection,
    Function, FunctionSection, GlobalSection, ImportSection, InstructionSink, MemorySection,
    MemoryType, Module, TypeSection, ValType,
};

use crate::{
    context::{Cache, ValId},
    intern::StrId,
    lower::{self, Fndef, FndefId, IR, Instr, Int32Arith, Int32Comp, Names, Type, ValdefId},
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

#[derive(Clone, Copy, EnumIter, IntoStaticStr)]
#[strum(serialize_all = "snake_case")]
enum Instruction {
    Unreachable,

    I32Load,
    I32Load8S,
    I32Load8U,
    I32Load16S,
    I32Load16U,
    I32Store,
    I32Store8,
    I32Store16,
    MemorySize,
    MemoryGrow,
    MemoryCopy,
    MemoryFill,

    I32Eqz,
    I32Eq,
    I32Ne,
    I32LtS,
    I32LtU,
    I32GtS,
    I32GtU,
    I32LeS,
    I32LeU,
    I32GeS,
    I32GeU,
    I32Clz,
    I32Ctz,
    I32Popcnt,
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32DivU,
    I32RemS,
    I32RemU,
    I32And,
    I32Or,
    I32Xor,
    I32Shl,
    I32ShrS,
    I32ShrU,
    I32Rotl,
    I32Rotr,
    I32Extend8S,
    I32Extend16S,
}

const WASIP1: &str = "wasi_snapshot_preview1";

const WASIP1_IMPORTS: &[&str] = &["args_get", "args_sizes_get", "fd_write", "proc_exit"];

trait AsInstructionSink {
    fn insn(&mut self) -> InstructionSink<'_>;
}

impl AsInstructionSink for Vec<u8> {
    fn insn(&mut self) -> InstructionSink<'_> {
        InstructionSink::new(self)
    }
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
    instructions: HashMap<FndefId, Instruction>,
    cache: Cache<'a>,

    data_offset: i32,
    strings: HashMap<StrId, i32>,
    offsets: IndexVec<TupleLoc, TypeOffset>,

    /// Start of global range for each `val`.
    valdefs: IndexVec<ValId, GlobalId>,

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
    /// Execute `f` for each Wasm type needed to represent `ty` in the current context.
    fn layout(&mut self, ty: lower::TypeId, mut f: impl FnMut(ValType)) {
        match self.ir.types[ty.index()] {
            Type::String => {
                f(ValType::I32);
                f(ValType::I32);
            }
            Type::Bool => {
                f(ValType::I32);
            }
            Type::Int32 => {
                f(ValType::I32);
            }
            Type::Tuple(elems) => {
                for &elem in &self.ir.tuples[elems] {
                    self.layout(elem, &mut f);
                }
            }
            Type::Tydef(_) => todo!(),
            Type::Structdef(structdef) => {
                for &(_, field) in &self.ir.fields[self.ir.structdefs[structdef].fields] {
                    self.layout(field, &mut f);
                }
            }
        }
    }

    /// Get the number of Wasm types needed to represent `ty` in the current context.
    ///
    /// Linear time.
    fn layout_len(&mut self, ty: lower::TypeId) -> usize {
        let mut len = 0;
        self.layout(ty, |_| len += 1);
        len
    }

    fn make_locals(&mut self, ty: lower::TypeId) -> LocalId {
        let start = self.locals.len_idx();
        let mut locals = Vec::new();
        self.layout(ty, |t| locals.push(t));
        self.locals.append(&mut IndexVec::from_vec(locals));
        start
    }

    fn get_locals(&mut self, ty: lower::TypeId, start: LocalId) {
        let len = self.layout_len(ty);
        let end = LocalId::from_usize(start.index() + len);
        for localidx in (IdRange { start, end }) {
            self.body.insn().local_get(localidx.raw());
        }
    }

    fn set_locals(&mut self, ty: lower::TypeId, start: LocalId) {
        let len = self.layout_len(ty);
        let end = LocalId::from_usize(start.index() + len);
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
        let len = self.layout_len(self.ir.valdefs[valdef].ty);
        let end = GlobalId::from_usize(start.index() + len);
        for globalidx in (IdRange { start, end }) {
            self.body.insn().global_get(globalidx.raw());
        }
    }

    fn set_val(&mut self, valdef: ValdefId) {
        let start = self.valdefs[valdef];
        let len = self.layout_len(self.ir.valdefs[valdef].ty);
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

    fn fndef(&mut self, fndef: FndefId) {
        let Fndef {
            needs: _,
            param,
            result,
        } = self.ir.fndefs[fndef];
        let mut params = Vec::new();
        let mut results = Vec::new();
        self.layout(param, |t| params.push(t));
        self.layout(result, |t| results.push(t));
        self.instrs(param, self.ir.bodies[fndef]);
        self.body.insn().end();
        let mut f = Function::new_with_locals_types(self.locals.iter().skip(params.len()).copied());
        f.raw(take(&mut self.body));
        self.locals = Default::default();
        self.variables = Default::default();
        self.section_code.function(&f);
        self.section_function.function(self.section_type.len());
        self.section_type.ty().function(params, results);
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

        for string in WASIP1_IMPORTS {
            let name = self.ir.strings.get_id(string).unwrap();
            let fndef = self.names.fndefs[&(self.lib.wasip1, name)];
            let Fndef {
                needs: _,
                param,
                result,
            } = self.ir.fndefs[fndef];
            let params = self.ir.tuples[self.ir.types[param.index()].tuple()]
                .iter()
                .map(|ty| match self.ir.types[ty.index()] {
                    Type::Int32 => ValType::I32,
                    _ => panic!(),
                });
            let results: &[ValType] = match self.ir.types[result.index()] {
                Type::Int32 => &[ValType::I32],
                Type::Tuple(elems) => {
                    assert!(elems.is_empty());
                    &[]
                }
                _ => panic!(),
            };
            self.section_import.import(
                WASIP1,
                string,
                EntityType::Function(self.section_type.len()),
            );
            self.section_type
                .ty()
                .function(params, results.iter().copied());
        }

        let start = self.section_import.len() + self.section_function.len();
        self.section_function.function(self.section_type.len());
        self.section_type.ty().function([], []);
        let mut f = Function::new([]);
        f.instructions()
            .call(self.section_import.len() + self.section_function.len())
            .i32_const(0)
            .call(
                WASIP1_IMPORTS
                    .iter()
                    .position(|&name| name == "proc_exit")
                    .unwrap() as u32,
            )
            .end();
        self.section_code.function(&f);
        self.section_export
            .export("_start", ExportKind::Func, start);
        self.fndef(self.main);

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
    let instructions = Instruction::iter()
        .map(|instruction| {
            let name = ir.strings.get_id(<&str>::from(instruction)).unwrap();
            let fndef = names.fndefs[&(lib.wasm, name)];
            (fndef, instruction)
        })
        .collect();
    let cache = Cache::new(ir);
    Wasm {
        ir,
        names,
        lib,
        main,
        instructions,
        cache,

        data_offset: Default::default(),
        strings: Default::default(),
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
