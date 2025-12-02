use std::{collections::HashMap, mem::take};

use index_vec::{IndexVec, define_index_type};
use indexmap::IndexSet;
use strum::{EnumIter, IntoEnumIterator, IntoStaticStr};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection,
    Function, FunctionSection, GlobalSection, GlobalType, ImportSection, InstructionSink, MemArg,
    MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::{
    context::{BuiltinId, Cache, ContextId, Fn, FnId, Ty, TyId, Val, ValId},
    intern::StrId,
    lower::{
        self, ElemId, FieldId, Fndef, FndefId, IR, Instr, Int32Arith, Int32Comp, Names, ValdefId,
    },
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
    I64Load,
    I32Load8S,
    I32Load8U,
    I32Load16S,
    I32Load16U,
    I64Load8S,
    I64Load8U,
    I64Load16S,
    I64Load16U,
    I64Load32S,
    I64Load32U,
    I32Store,
    I64Store,
    I32Store8,
    I32Store16,
    I64Store8,
    I64Store16,
    I64Store32,
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
    I64Eqz,
    I64Eq,
    I64Ne,
    I64LtS,
    I64LtU,
    I64GtS,
    I64GtU,
    I64LeS,
    I64LeU,
    I64GeS,
    I64GeU,
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
    I64Clz,
    I64Ctz,
    I64Popcnt,
    I64Add,
    I64Sub,
    I64Mul,
    I64DivS,
    I64DivU,
    I64RemS,
    I64RemU,
    I64And,
    I64Or,
    I64Xor,
    I64Shl,
    I64ShrS,
    I64ShrU,
    I64Rotl,
    I64Rotr,
    I32WrapI64,
    #[strum(serialize = "i64_extend_i32_s")]
    I64ExtendI32S,
    #[strum(serialize = "i64_extend_i32_u")]
    I64ExtendI32U,
    I32Extend8S,
    I32Extend16S,
    I64Extend8S,
    I64Extend16S,
    I64Extend32S,
}

const WASIP1: &str = "wasi_snapshot_preview1";

enum Builtin {
    Instruction(Instruction),
    Function(u32),
}

trait AsInstructionSink {
    fn insn(&mut self) -> InstructionSink<'_>;
}

impl AsInstructionSink for Vec<u8> {
    fn insn(&mut self) -> InstructionSink<'_> {
        InstructionSink::new(self)
    }
}

#[derive(Clone, Copy)]
struct Local {
    ctx: ContextId,
    start: LocalId,
}

#[derive(Clone, Copy)]
struct Tmp {
    ty: TyId,
    start: LocalId,
}

struct Wasm<'a> {
    ir: &'a IR,
    names: &'a Names,
    lib: Lib,
    main: FndefId,
    val_memidx: ValdefId,
    val_dst: ValdefId,
    val_src: ValdefId,
    val_align: ValdefId,
    val_offset: ValdefId,
    cache: Cache<'a>,
    builtins: IndexVec<BuiltinId, Builtin>,
    funcs: IndexVec<FnId, Option<u32>>,
    next_funcidx: u32,

    data_offset: i32,
    strings: HashMap<StrId, i32>,

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

    /// The current context.
    ctx: ContextId,

    /// Locals for the current function.
    locals: IndexVec<LocalId, ValType>,

    /// Start of local range for each IR local in the current function.
    variables: HashMap<lower::LocalId, Local>,

    /// Compile-time values computed inside static blocks.
    statics: HashMap<lower::LocalId, ValId>,

    /// The current function body.
    body: Vec<u8>,
}

impl<'a> Wasm<'a> {
    /// Execute `f` for each Wasm type needed to represent `ty` in the current context.
    fn layout(&self, ty: TyId, f: &mut impl FnMut(ValType)) {
        match self.cache[ty] {
            Ty::String => {
                f(ValType::I32);
                f(ValType::I32);
            }
            Ty::Bool => {
                f(ValType::I32);
            }
            Ty::Int32 => {
                f(ValType::I32);
            }
            Ty::Int64 => {
                f(ValType::I64);
            }
            Ty::Tuple(elems) => {
                for &elem in &self.cache[elems] {
                    self.layout(elem, f);
                }
            }
            Ty::Structdef(_, fields) => {
                for &field in &self.cache[fields] {
                    self.layout(field, f);
                }
            }
        }
    }

    /// Get the number of Wasm types needed to represent `ty` in the current context.
    ///
    /// Linear time.
    fn layout_len(&mut self, ty: TyId) -> usize {
        let mut len = 0;
        self.layout(ty, &mut |_| len += 1);
        len
    }

    /// Get a [`Vec`] of the Wasm types needed to represent `ty` in the current context.
    fn layout_vec(&mut self, ty: TyId) -> Vec<ValType> {
        let mut types = Vec::new();
        self.layout(ty, &mut |t| types.push(t));
        types
    }

    fn make_locals(&mut self, ty: TyId) -> LocalId {
        let start = self.locals.len_idx();
        let locals = self.layout_vec(ty);
        self.locals.append(&mut IndexVec::from_vec(locals));
        start
    }

    fn get_locals(&mut self, ty: TyId, start: LocalId) {
        let len = self.layout_len(ty);
        let end = LocalId::from_usize(start.index() + len);
        for localidx in (IdRange { start, end }) {
            self.body.insn().local_get(localidx.raw());
        }
    }

    fn set_locals(&mut self, ty: TyId, start: LocalId) {
        let len = self.layout_len(ty);
        let end = LocalId::from_usize(start.index() + len);
        for localidx in (IdRange { start, end }).into_iter().rev() {
            self.body.insn().local_set(localidx.raw());
        }
    }

    fn get(&mut self, instr: lower::LocalId) {
        let Local { ctx, start } = self.variables[&instr];
        let ty = self.cache.ty(ctx, self.ir.locals[instr]);
        self.get_locals(ty, start)
    }

    fn set(&mut self, instr: lower::LocalId) {
        let ctx = self.ctx;
        let ty = self.cache.ty(ctx, self.ir.locals[instr]);
        let start = self.make_locals(ty);
        let prev = self.variables.insert(instr, Local { ctx, start });
        assert!(prev.is_none());
        self.set_locals(ty, start);
    }

    fn get_tmp(&mut self, tmp: Tmp) {
        self.get_locals(tmp.ty, tmp.start);
    }

    fn set_tmp(&mut self, ty: TyId) -> Tmp {
        let start = self.make_locals(ty);
        self.set_locals(ty, start);
        Tmp { ty, start }
    }

    fn string(&mut self, id: StrId) {
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

    fn get_val(&mut self, valdef: ValdefId) {
        let val = self.cache.valdef(self.ctx, valdef);
        match self.cache[val] {
            Val::Bool(b) => {
                self.body.insn().i32_const(if b { 1 } else { 0 });
            }
            Val::Int32(n) => {
                self.body.insn().i32_const(n as i32);
            }
            Val::String(id) => {
                self.string(id);
            }
            Val::Dynamic(_, ty) => {
                let start = self.valdefs[val];
                let len = self.layout_len(ty);
                let end = GlobalId::from_usize(start.index() + len);
                for globalidx in (IdRange { start, end }) {
                    self.body.insn().global_get(globalidx.raw());
                }
            }
        }
    }

    fn set_val(&mut self, valdef: ValdefId) {
        let val = self.cache.valdef(self.ctx, valdef);
        match self.cache[val] {
            // We don't need to restore anything for static bindings.
            Val::Bool(_) | Val::Int32(_) | Val::String(_) => {}
            Val::Dynamic(_, ty) => {
                let start = self.valdefs[val];
                let len = self.layout_len(ty);
                let end = GlobalId::from_usize(start.index() + len);
                for globalidx in (IdRange { start, end }).into_iter().rev() {
                    self.body.insn().global_set(globalidx.raw());
                }
            }
        }
    }

    fn val_i32_as_u32(&mut self, valdef: ValdefId) -> u32 {
        let val = self.cache.valdef(self.ctx, valdef);
        let Val::Int32(n) = self.cache[val] else {
            panic!();
        };
        n
    }

    fn val_i32_as_u64(&mut self, valdef: ValdefId) -> u64 {
        let val = self.cache.valdef(self.ctx, valdef);
        let Val::Int32(n) = self.cache[val] else {
            panic!();
        };
        n as u64
    }

    fn memarg(&mut self) -> MemArg {
        MemArg {
            offset: self.val_i32_as_u64(self.val_offset),
            align: self.val_i32_as_u32(self.val_align),
            memory_index: self.val_i32_as_u32(self.val_memidx),
        }
    }

    fn instrs(&mut self, param: TyId, mut instr: lower::LocalId) -> lower::LocalId {
        loop {
            match self.ir.instrs[instr] {
                Instr::Static => {
                    instr += 1;
                    loop {
                        match self.ir.instrs[instr] {
                            Instr::EndStatic => break,
                            Instr::Val(valdef) => {
                                let val = self.cache.valdef(self.ctx, valdef);
                                self.statics.insert(instr, val);
                            }
                            Instr::Int32(n) => {
                                let val = self.cache.val_int32(n);
                                self.statics.insert(instr, val);
                            }
                            Instr::String(s) => {
                                let val = self.cache.val_string(s);
                                self.statics.insert(instr, val);
                            }
                            _ => panic!(),
                        }
                        instr += 1;
                    }
                }
                Instr::EndStatic => panic!(),
                Instr::BindVal(valdef, local) => {
                    if let Some(&val) = self.statics.get(&local) {
                        let ctx = self.ctx;
                        let mut context = self.cache[self.ctx].clone();
                        context.set_val(valdef, val);
                        self.ctx = self.cache.make_ctx(context);
                        instr = self.instrs(param, instr + 1);
                        self.ctx = ctx;
                    } else {
                        self.get_val(valdef);
                        let ty = {
                            let val = self.cache.valdef(self.ctx, valdef);
                            match self.cache[val] {
                                // We don't need any temporary locals to restore static bindings.
                                Val::Bool(_) | Val::Int32(_) | Val::String(_) => {
                                    self.cache.ty_unit()
                                }
                                Val::Dynamic(_, ty) => ty,
                            }
                        };
                        let tmp = self.set_tmp(ty);
                        self.get(local);
                        let ctx = self.ctx;
                        let context = {
                            let mut context = self.cache[self.ctx].clone();
                            let Local { ctx, start: _ } = self.variables[&local];
                            let ty = self.cache.ty(ctx, self.ir.locals[local]);
                            let val = self.cache.val_dynamic(valdef, ty);
                            context.set_val(valdef, val);
                            context
                        };
                        self.ctx = self.cache.make_ctx(context);
                        self.set_val(valdef);
                        instr = self.instrs(param, instr + 1);
                        self.ctx = ctx;
                        self.get_tmp(tmp);
                        self.set_val(valdef);
                    }
                }
                Instr::EndBind => break,
                Instr::Val(valdef) => self.get_val(valdef),
                Instr::Param => {
                    self.get_locals(param, LocalId::from_raw(0));
                }
                Instr::Copy(local) => {
                    self.get(local);
                }
                Instr::Set(lhs, rhs) => {
                    self.get(rhs);
                    let Local { ctx, start } = self.variables[&lhs];
                    let ty = self.cache.ty(ctx, self.ir.locals[lhs]);
                    self.set_locals(ty, start);
                }
                Instr::Int32(n) => {
                    self.body.insn().i32_const(n as i32);
                }
                Instr::String(id) => {
                    self.string(id);
                }
                Instr::Tuple(locals) => {
                    for &id in locals.get(&self.ir.refs) {
                        self.get(id);
                    }
                }
                Instr::Struct(_, locals) => {
                    for &id in locals.get(&self.ir.refs) {
                        self.get(id);
                    }
                }
                Instr::Elem(tuple, index) => {
                    let Local { ctx, mut start } = self.variables[&tuple];
                    let ty = self.cache.ty(ctx, self.ir.locals[tuple]);
                    let range = self.cache[ty].tuple();
                    // TODO: Make this not be linear time.
                    for i in (IdRange {
                        start: ElemId::new(0),
                        end: index,
                    }) {
                        start += self.layout_len(
                            self.cache[TupleLoc::from_raw(range.start.raw() + i.raw())],
                        );
                    }
                    let elem = TupleLoc::from_raw(range.start.raw() + index.raw());
                    self.get_locals(self.cache[elem], start);
                }
                Instr::Field(record, index) => {
                    let Local { ctx, mut start } = self.variables[&record];
                    let ty = self.cache.ty(ctx, self.ir.locals[record]);
                    let range = self.cache[ty].structdef();
                    // TODO: Make this not be linear time.
                    for i in (IdRange {
                        start: FieldId::new(0),
                        end: index,
                    }) {
                        start += self.layout_len(
                            self.cache[TupleLoc::from_raw(range.start.raw() + i.raw())],
                        );
                    }
                    let field = TupleLoc::from_raw(range.start.raw() + index.raw());
                    self.get_locals(self.cache[field], start);
                }
                Instr::Int32Arith(a, op, b) => {
                    self.get(a);
                    self.get(b);
                    match op {
                        Int32Arith::Add => self.body.insn().i32_add(),
                        Int32Arith::Sub => self.body.insn().i32_sub(),
                        Int32Arith::Mul => self.body.insn().i32_mul(),
                        Int32Arith::Div => self.body.insn().i32_div_s(),
                        Int32Arith::Rem => self.body.insn().i32_rem_s(),
                    };
                }
                Instr::Int32Comp(a, op, b) => {
                    self.get(a);
                    self.get(b);
                    match op {
                        Int32Comp::Eq => self.body.insn().i32_eq(),
                        Int32Comp::Neq => self.body.insn().i32_ne(),
                        Int32Comp::Lt => self.body.insn().i32_lt_s(),
                        Int32Comp::Gt => self.body.insn().i32_gt_s(),
                        Int32Comp::Leq => self.body.insn().i32_le_s(),
                        Int32Comp::Geq => self.body.insn().i32_ge_s(),
                    };
                }
                Instr::Call(fndef, local) => {
                    self.get(local);
                    let f = self.cache.fndef(self.ctx, fndef);
                    match self.cache[f] {
                        Fn::Builtin(builtin) => match self.builtins[builtin] {
                            Builtin::Instruction(instruction) => {
                                match instruction {
                                    Instruction::Unreachable => {
                                        self.body.insn().unreachable();
                                    }

                                    Instruction::I32Load => {
                                        let memarg = self.memarg();
                                        self.body.insn().i32_load(memarg);
                                    }
                                    Instruction::I64Load => {
                                        let memarg = self.memarg();
                                        self.body.insn().i64_load(memarg);
                                    }
                                    Instruction::I32Load8S => {
                                        let memarg = self.memarg();
                                        self.body.insn().i32_load8_s(memarg);
                                    }
                                    Instruction::I32Load8U => {
                                        let memarg = self.memarg();
                                        self.body.insn().i32_load8_u(memarg);
                                    }
                                    Instruction::I32Load16S => {
                                        let memarg = self.memarg();
                                        self.body.insn().i32_load16_s(memarg);
                                    }
                                    Instruction::I32Load16U => {
                                        let memarg = self.memarg();
                                        self.body.insn().i32_load16_u(memarg);
                                    }
                                    Instruction::I64Load8S => {
                                        let memarg = self.memarg();
                                        self.body.insn().i64_load8_s(memarg);
                                    }
                                    Instruction::I64Load8U => {
                                        let memarg = self.memarg();
                                        self.body.insn().i64_load8_u(memarg);
                                    }
                                    Instruction::I64Load16S => {
                                        let memarg = self.memarg();
                                        self.body.insn().i64_load16_s(memarg);
                                    }
                                    Instruction::I64Load16U => {
                                        let memarg = self.memarg();
                                        self.body.insn().i64_load16_u(memarg);
                                    }
                                    Instruction::I64Load32S => {
                                        let memarg = self.memarg();
                                        self.body.insn().i64_load32_s(memarg);
                                    }
                                    Instruction::I64Load32U => {
                                        let memarg = self.memarg();
                                        self.body.insn().i64_load32_u(memarg);
                                    }
                                    Instruction::I32Store => {
                                        let memarg = self.memarg();
                                        self.body.insn().i32_store(memarg);
                                    }
                                    Instruction::I64Store => {
                                        let memarg = self.memarg();
                                        self.body.insn().i64_store(memarg);
                                    }
                                    Instruction::I32Store8 => {
                                        let memarg = self.memarg();
                                        self.body.insn().i32_store8(memarg);
                                    }
                                    Instruction::I32Store16 => {
                                        let memarg = self.memarg();
                                        self.body.insn().i32_store16(memarg);
                                    }
                                    Instruction::I64Store8 => {
                                        let memarg = self.memarg();
                                        self.body.insn().i64_store8(memarg);
                                    }
                                    Instruction::I64Store16 => {
                                        let memarg = self.memarg();
                                        self.body.insn().i64_store16(memarg);
                                    }
                                    Instruction::I64Store32 => {
                                        let memarg = self.memarg();
                                        self.body.insn().i64_store32(memarg);
                                    }
                                    Instruction::MemorySize => {
                                        let memidx = self.val_i32_as_u32(self.val_memidx);
                                        self.body.insn().memory_size(memidx);
                                    }
                                    Instruction::MemoryGrow => {
                                        let memidx = self.val_i32_as_u32(self.val_memidx);
                                        self.body.insn().memory_grow(memidx);
                                    }
                                    Instruction::MemoryCopy => {
                                        let dst = self.val_i32_as_u32(self.val_dst);
                                        let src = self.val_i32_as_u32(self.val_src);
                                        self.body.insn().memory_copy(dst, src);
                                    }
                                    Instruction::MemoryFill => {
                                        let memidx = self.val_i32_as_u32(self.val_memidx);
                                        self.body.insn().memory_fill(memidx);
                                    }

                                    Instruction::I32Eqz => {
                                        self.body.insn().i32_eqz();
                                    }
                                    Instruction::I32Eq => {
                                        self.body.insn().i32_eq();
                                    }
                                    Instruction::I32Ne => {
                                        self.body.insn().i32_ne();
                                    }
                                    Instruction::I32LtS => {
                                        self.body.insn().i32_lt_s();
                                    }
                                    Instruction::I32LtU => {
                                        self.body.insn().i32_lt_u();
                                    }
                                    Instruction::I32GtS => {
                                        self.body.insn().i32_gt_s();
                                    }
                                    Instruction::I32GtU => {
                                        self.body.insn().i32_gt_u();
                                    }
                                    Instruction::I32LeS => {
                                        self.body.insn().i32_le_s();
                                    }
                                    Instruction::I32LeU => {
                                        self.body.insn().i32_le_u();
                                    }
                                    Instruction::I32GeS => {
                                        self.body.insn().i32_ge_s();
                                    }
                                    Instruction::I32GeU => {
                                        self.body.insn().i32_ge_u();
                                    }
                                    Instruction::I64Eqz => {
                                        self.body.insn().i64_eqz();
                                    }
                                    Instruction::I64Eq => {
                                        self.body.insn().i64_eq();
                                    }
                                    Instruction::I64Ne => {
                                        self.body.insn().i64_ne();
                                    }
                                    Instruction::I64LtS => {
                                        self.body.insn().i64_lt_s();
                                    }
                                    Instruction::I64LtU => {
                                        self.body.insn().i64_lt_u();
                                    }
                                    Instruction::I64GtS => {
                                        self.body.insn().i64_gt_s();
                                    }
                                    Instruction::I64GtU => {
                                        self.body.insn().i64_gt_u();
                                    }
                                    Instruction::I64LeS => {
                                        self.body.insn().i64_le_s();
                                    }
                                    Instruction::I64LeU => {
                                        self.body.insn().i64_le_u();
                                    }
                                    Instruction::I64GeS => {
                                        self.body.insn().i64_ge_s();
                                    }
                                    Instruction::I64GeU => {
                                        self.body.insn().i64_ge_u();
                                    }
                                    Instruction::I32Clz => {
                                        self.body.insn().i32_clz();
                                    }
                                    Instruction::I32Ctz => {
                                        self.body.insn().i32_ctz();
                                    }
                                    Instruction::I32Popcnt => {
                                        self.body.insn().i32_popcnt();
                                    }
                                    Instruction::I32Add => {
                                        self.body.insn().i32_add();
                                    }
                                    Instruction::I32Sub => {
                                        self.body.insn().i32_sub();
                                    }
                                    Instruction::I32Mul => {
                                        self.body.insn().i32_mul();
                                    }
                                    Instruction::I32DivS => {
                                        self.body.insn().i32_div_s();
                                    }
                                    Instruction::I32DivU => {
                                        self.body.insn().i32_div_u();
                                    }
                                    Instruction::I32RemS => {
                                        self.body.insn().i32_rem_s();
                                    }
                                    Instruction::I32RemU => {
                                        self.body.insn().i32_rem_u();
                                    }
                                    Instruction::I32And => {
                                        self.body.insn().i32_and();
                                    }
                                    Instruction::I32Or => {
                                        self.body.insn().i32_or();
                                    }
                                    Instruction::I32Xor => {
                                        self.body.insn().i32_xor();
                                    }
                                    Instruction::I32Shl => {
                                        self.body.insn().i32_shl();
                                    }
                                    Instruction::I32ShrS => {
                                        self.body.insn().i32_shr_s();
                                    }
                                    Instruction::I32ShrU => {
                                        self.body.insn().i32_shr_u();
                                    }
                                    Instruction::I32Rotl => {
                                        self.body.insn().i32_rotl();
                                    }
                                    Instruction::I32Rotr => {
                                        self.body.insn().i32_rotr();
                                    }
                                    Instruction::I64Clz => {
                                        self.body.insn().i64_clz();
                                    }
                                    Instruction::I64Ctz => {
                                        self.body.insn().i64_ctz();
                                    }
                                    Instruction::I64Popcnt => {
                                        self.body.insn().i64_popcnt();
                                    }
                                    Instruction::I64Add => {
                                        self.body.insn().i64_add();
                                    }
                                    Instruction::I64Sub => {
                                        self.body.insn().i64_sub();
                                    }
                                    Instruction::I64Mul => {
                                        self.body.insn().i64_mul();
                                    }
                                    Instruction::I64DivS => {
                                        self.body.insn().i64_div_s();
                                    }
                                    Instruction::I64DivU => {
                                        self.body.insn().i64_div_u();
                                    }
                                    Instruction::I64RemS => {
                                        self.body.insn().i64_rem_s();
                                    }
                                    Instruction::I64RemU => {
                                        self.body.insn().i64_rem_u();
                                    }
                                    Instruction::I64And => {
                                        self.body.insn().i64_and();
                                    }
                                    Instruction::I64Or => {
                                        self.body.insn().i64_or();
                                    }
                                    Instruction::I64Xor => {
                                        self.body.insn().i64_xor();
                                    }
                                    Instruction::I64Shl => {
                                        self.body.insn().i64_shl();
                                    }
                                    Instruction::I64ShrS => {
                                        self.body.insn().i64_shr_s();
                                    }
                                    Instruction::I64ShrU => {
                                        self.body.insn().i64_shr_u();
                                    }
                                    Instruction::I64Rotl => {
                                        self.body.insn().i64_rotl();
                                    }
                                    Instruction::I64Rotr => {
                                        self.body.insn().i64_rotr();
                                    }
                                    Instruction::I32WrapI64 => {
                                        self.body.insn().i32_wrap_i64();
                                    }
                                    Instruction::I64ExtendI32S => {
                                        self.body.insn().i64_extend_i32_s();
                                    }
                                    Instruction::I64ExtendI32U => {
                                        self.body.insn().i64_extend_i32_u();
                                    }
                                    Instruction::I32Extend8S => {
                                        self.body.insn().i32_extend8_s();
                                    }
                                    Instruction::I32Extend16S => {
                                        self.body.insn().i32_extend16_s();
                                    }
                                    Instruction::I64Extend8S => {
                                        self.body.insn().i64_extend8_s();
                                    }
                                    Instruction::I64Extend16S => {
                                        self.body.insn().i64_extend16_s();
                                    }
                                    Instruction::I64Extend32S => {
                                        self.body.insn().i64_extend32_s();
                                    }
                                };
                            }
                            Builtin::Function(funcidx) => {
                                self.body.insn().call(funcidx);
                            }
                        },
                        Fn::Fndef(_, _) => {
                            let funcidx = self.funcs.get(f).copied().unwrap_or_else(|| {
                                assert_eq!(f, self.funcs.len_idx());
                                let funcidx = Some(self.next_funcidx);
                                self.funcs.push(funcidx);
                                self.next_funcidx += 1;
                                funcidx
                            });
                            self.body.insn().call(funcidx.unwrap());
                        }
                    }
                }
                Instr::If(cond, ty) => {
                    let Local { ctx, start: _ } = self.variables[&cond];
                    let ty = self.cache.ty(ctx, ty);
                    let layout = self.layout_vec(ty);
                    let typeidx = self.section_type.len();
                    self.section_type.ty().function([], layout);
                    self.get(cond);
                    self.body.insn().if_(BlockType::FunctionType(typeidx));
                }
                Instr::Else(local) => {
                    self.get(local);
                    self.body.insn().else_();
                    instr += 1;
                    continue;
                }
                Instr::EndIf(local) => {
                    self.get(local);
                    self.body.insn().end();
                }
                Instr::Loop => {
                    self.body.insn().loop_(BlockType::Empty);
                }
                Instr::EndLoop => {
                    self.body.insn().end();
                }
                Instr::Br(depth) => {
                    self.body.insn().br(depth.0);
                }
                Instr::Return(local) => {
                    self.get(local);
                    self.body.insn().end();
                    break;
                }
            }
            self.set(instr);
            instr += 1;
        }
        instr
    }

    fn funcidx(&self) -> u32 {
        // TODO: Handle the case where non-function imports have also been added.
        self.section_import.len() + self.section_function.len()
    }

    fn func(&mut self, f: FnId) {
        match self.cache[f] {
            Fn::Builtin(_) => panic!(),
            Fn::Fndef(ctx, fndef) => {
                self.ctx = ctx;
                let Fndef {
                    needs: _,
                    param,
                    result,
                } = self.ir.fndefs[fndef];
                let param = self.cache.ty(self.ctx, param);
                let result = self.cache.ty(self.ctx, result);
                let params = self.layout_vec(param);
                let results = self.layout_vec(result);
                self.instrs(param, self.ir.bodies[fndef].unwrap());
                let mut f =
                    Function::new_with_locals_types(self.locals.iter().skip(params.len()).copied());
                f.raw(take(&mut self.body));
                self.locals = Default::default();
                self.variables = Default::default();
                self.statics = Default::default();
                self.section_code.function(&f);
                self.section_function.function(self.section_type.len());
                self.section_type.ty().function(params, results);
            }
        }
    }

    fn program(mut self) -> Vec<u8> {
        let mut context = self.cache[self.ctx].clone();
        let val_false =
            self.names.valdefs[&(self.lib.bool, self.ir.strings.get_id("false").unwrap())];
        let val_true =
            self.names.valdefs[&(self.lib.bool, self.ir.strings.get_id("true").unwrap())];
        context.set_val(val_false, self.cache.val_bool(false));
        context.set_val(val_true, self.cache.val_bool(true));

        let ty_memidx =
            self.names.tydefs[&(self.lib.wasm, self.ir.strings.get_id("MemIdx").unwrap())];
        context.set_ty(ty_memidx, self.cache.ty_int32());
        let memidx_wasi = 0;
        context.set_val(self.val_memidx, self.cache.val_int32(memidx_wasi));
        let memidx_valdefs: Vec<ValdefId> = self
            .cache
            .fndef_valdefs(self.main)
            .filter(|&valdef| {
                // Skip the special WASI memory here, because we always want it to be index zero.
                valdef != self.val_memidx
                    && self.ir.types[self.ir.valdefs[valdef].ty.index()]
                        == lower::Type::Tydef(ty_memidx)
            })
            .collect();
        for (i, &valdef) in memidx_valdefs.iter().enumerate() {
            context.set_val(valdef, self.cache.val_int32((i + 1) as u32));
        }

        for instruction in Instruction::iter() {
            let builtin = self.builtins.push(Builtin::Instruction(instruction));
            let f = self.cache.fn_builtin(builtin);
            assert_eq!(self.funcs.push(None), f);
            let name = self.ir.strings.get_id(<&str>::from(instruction)).unwrap();
            let fndef = self.names.fndefs[&(self.lib.wasm, name)];
            context.set_fn(fndef, f);
        }

        // We use the ordering from the `context` definition instead of, for instance, just using
        // the iteration order of `self.names`, since that would be nondeterministic.
        let wasip1_fndefs: IndexSet<FndefId> = self.ir.ctxdefs
            [self.names.ctxdefs[&(self.lib.wasip1, self.ir.strings.get_id("wasip1").unwrap())]]
            .def
            .fns
            .get(&self.ir.need_fns)
            .iter()
            .map(|need| need.id)
            .collect();
        // It isn't ideal to iterate through every name binding from every module just to filter out
        // all the ones except from the one module we care about, but it's fine for now.
        let wasip1_names: HashMap<FndefId, StrId> = self
            .names
            .fndefs
            .iter()
            .filter(|&(&(module, _), &fndef)| {
                module == self.lib.wasip1 && wasip1_fndefs.contains(&fndef)
            })
            .map(|(&(_, name), &fndef)| (fndef, name))
            .collect();
        for &fndef in &wasip1_fndefs {
            let builtin = self.builtins.push(Builtin::Function(self.funcidx()));
            let f = self.cache.fn_builtin(builtin);
            assert_eq!(self.funcs.push(None), f);
            context.set_fn(fndef, f);
            let Fndef {
                needs: _,
                param,
                result,
            } = self.ir.fndefs[fndef];
            let param = self.cache.ty(self.ctx, param);
            let result = self.cache.ty(self.ctx, result);
            let params =
                self.cache[self.cache[param].tuple()]
                    .iter()
                    .map(|&ty| match self.cache[ty] {
                        Ty::Int32 => ValType::I32,
                        Ty::Int64 => ValType::I64,
                        _ => panic!(),
                    });
            let results: &[ValType] = match self.cache[result] {
                Ty::Int32 => &[ValType::I32],
                Ty::Tuple(elems) => {
                    assert!(elems.is_empty());
                    &[]
                }
                _ => panic!(),
            };
            self.section_import.import(
                WASIP1,
                &self.ir.strings[wasip1_names[&fndef]],
                EntityType::Function(self.section_type.len()),
            );
            self.section_type
                .ty()
                .function(params, results.iter().copied());
        }

        let global_stack = self.section_global.len();

        let builtin_stack = self.builtins.push(Builtin::Function(self.funcidx()));
        let f_stack = self.cache.fn_builtin(builtin_stack);
        assert_eq!(self.funcs.push(None), f_stack);
        let fndef_stack =
            self.names.fndefs[&(self.lib.wasi, self.ir.strings.get_id("stack").unwrap())];
        context.set_fn(fndef_stack, f_stack);
        self.section_function.function(self.section_type.len());
        self.section_type.ty().function([], [ValType::I32]);
        self.section_code.function(&{
            let mut f = Function::new([]);
            f.instructions().global_get(global_stack).end();
            f
        });

        let builtin_reserve = self.builtins.push(Builtin::Function(self.funcidx()));
        let f_reserve = self.cache.fn_builtin(builtin_reserve);
        assert_eq!(self.funcs.push(None), f_reserve);
        let fndef_reserve =
            self.names.fndefs[&(self.lib.wasi, self.ir.strings.get_id("reserve").unwrap())];
        context.set_fn(fndef_reserve, f_reserve);
        self.section_function.function(self.section_type.len());
        self.section_type.ty().function([ValType::I32], []);
        self.section_code.function(&{
            let mut f = Function::new([]);
            f.instructions()
                .global_get(global_stack)
                .i64_extend_i32_u()
                .local_get(0)
                .i64_extend_i32_u()
                .i64_add()
                .i64_const(u16::MAX as i64)
                .i64_add()
                .i64_const(16)
                .i64_shr_u()
                .i32_wrap_i64()
                .memory_size(memidx_wasi)
                .i32_sub()
                .memory_grow(memidx_wasi)
                .drop()
                .end();
            f
        });

        let builtin_claim = self.builtins.push(Builtin::Function(self.funcidx()));
        let f_claim = self.cache.fn_builtin(builtin_claim);
        assert_eq!(self.funcs.push(None), f_claim);
        let fndef_claim =
            self.names.fndefs[&(self.lib.wasi, self.ir.strings.get_id("claim").unwrap())];
        context.set_fn(fndef_claim, f_claim);
        self.section_function.function(self.section_type.len());
        self.section_type.ty().function([ValType::I32], []);
        self.section_code.function(&{
            let mut f = Function::new([(1, ValType::I64)]);
            f.instructions()
                .global_get(global_stack)
                .i64_extend_i32_u()
                .local_get(0)
                .i64_extend_i32_u()
                .i64_add()
                .i64_const(15)
                .i64_add()
                .i64_const(4)
                .i64_shr_u()
                .i64_const(4)
                .i64_shl()
                .local_tee(1)
                .i64_const(u32::MAX as i64)
                .i64_gt_u()
                .if_(BlockType::Empty)
                .unreachable()
                .end()
                .local_get(1)
                .i32_wrap_i64()
                .global_set(global_stack)
                .end();
            f
        });

        self.section_global.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            // We'll set this dynamically when we codegen the `_start` function, since at that point
            // we'll know the total length of all the active data segments.
            &ConstExpr::i32_const(0),
        );

        let ctx = self.cache.make_ctx(context);
        let main = self.cache.fndef(ctx, self.main);
        let main_funcidx = self.funcidx();
        assert_eq!(self.funcs.push(Some(main_funcidx)), main);
        self.next_funcidx = main_funcidx + 1;
        let mut next_fn = main;
        while next_fn < self.cache.next_fn() {
            self.ctx = ctx;
            self.func(next_fn);
            next_fn += 1;
        }

        self.section_export
            .export("memory", ExportKind::Memory, memidx_wasi);
        self.section_memory.memory(MemoryType {
            minimum: ((self.data_offset + 65535) / 65536) as u64,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        for _ in memidx_valdefs {
            self.section_memory.memory(MemoryType {
                minimum: 0,
                maximum: None,
                memory64: false,
                shared: false,
                page_size_log2: None,
            });
        }

        let start = self.funcidx();
        self.section_function.function(self.section_type.len());
        self.section_type.ty().function([], []);
        self.section_code.function(&{
            let mut f = Function::new([]);
            f.instructions()
                .i32_const((self.data_offset + 15) / 16 * 16)
                .global_set(global_stack)
                .call(self.funcs[main].unwrap())
                .i32_const(0)
                .call(
                    wasip1_fndefs
                        .iter()
                        .position(|fndef| &self.ir.strings[wasip1_names[fndef]] == "proc_exit")
                        .unwrap() as u32,
                )
                .end();
            f
        });
        self.section_export
            .export("_start", ExportKind::Func, start);

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
    let cache = Cache::new(ir);
    let ctx = cache.empty();
    let val_memidx = names.valdefs[&(lib.wasm, ir.strings.get_id("memidx").unwrap())];
    let val_dst = names.valdefs[&(lib.wasm, ir.strings.get_id("dst").unwrap())];
    let val_src = names.valdefs[&(lib.wasm, ir.strings.get_id("src").unwrap())];
    let val_align = names.valdefs[&(lib.wasm, ir.strings.get_id("align").unwrap())];
    let val_offset = names.valdefs[&(lib.wasm, ir.strings.get_id("offset").unwrap())];
    Wasm {
        ir,
        names,
        lib,
        main,
        val_memidx,
        val_dst,
        val_src,
        val_align,
        val_offset,
        cache,
        builtins: IndexVec::new(),
        funcs: IndexVec::new(),
        next_funcidx: 0,

        data_offset: Default::default(),
        strings: Default::default(),
        valdefs: Default::default(),

        section_type: Default::default(),
        section_import: Default::default(),
        section_function: Default::default(),
        section_memory: Default::default(),
        section_global: Default::default(),
        section_export: Default::default(),
        section_code: Default::default(),
        section_data: Default::default(),

        ctx,
        locals: Default::default(),
        variables: Default::default(),
        statics: Default::default(),
        body: Default::default(),
    }
    .program()
}
