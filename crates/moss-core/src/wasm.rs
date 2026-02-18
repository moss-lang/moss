use std::{collections::HashMap, mem::take};

use index_vec::{IndexVec, define_index_type};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use strum::{EnumIter, IntoStaticStr};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, ExportKind, ExportSection, Function,
    FunctionSection, GlobalSection, GlobalType, ImportSection, InstructionSink, MemArg,
    MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::{
    intern::StrId,
    lower::{
        self, Body, ElemId, FieldId, FndefId, IR, Instr, ModuleId, Named, Names, Sigdef, SigdefId,
        ValdefId,
    },
    prelude::Lib,
    tuples::{TupleLoc, TupleRange, Tuples},
    util::IdRange,
};

define_index_type! {
    /// The index of an [`Object`] in the `objects` field during [`Wasm`] codegen.
    struct ObjectId = u32;
}

define_index_type! {
    /// The index of a `u32` Wasm local in the `locals` field during [`Wasm`] codegen.
    struct LocalId = u32;
}

#[derive(Clone, Copy, Debug, EnumIter, Eq, Hash, IntoStaticStr, PartialEq)]
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

/// The range within which a literal integer falls.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum IntLitIn {
    Uint31,
    Uint32,
    Int32,
    Uint63,
    Uint64,
    Int64,
    Uint,
    Int,
}

/// The type of an input literal.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum IntLitOut {
    Uint32,
    Int32,
    Uint64,
    Int64,
    Uint,
    Int,
}

/// A static object.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Object {
    /// The type of integer literals of a specific kind.
    TyLitInt(IntLitIn),

    /// The type of character literals.
    TyLitChar,

    /// The type of string literals.
    TyLitString,

    /// The type of a Wasm `memidx`.
    TyMemIdx,

    /// The Wasm `i32` type.
    TyI32,

    /// The Wasm `i64` type.
    TyI64,

    /// A function signature, with parameter and result types.
    Sig(ObjectId, ObjectId),

    /// The function to process a specific kind of integer literal.
    FnInt(IntLitIn, IntLitOut),

    /// The function to process a character literal.
    FnChar,

    /// The function to process a string literal.
    FnString,

    /// A Wasm instruction.
    FnInstr(Instruction),

    /// The Wasm `funcidx` of an imported WASI P1 function.
    FnWasi(u32),

    /// A defined function in a specific context.
    FnDef(FndefId, ObjectId),

    /// A 32-bit unsigned integer constant.
    ValU32(u32),

    /// A dynamic value with a type, stored in Wasm locals starting at a given index.
    ValDyn(ObjectId, LocalId),

    /// A context with some output slots.
    Ctx(TupleRange),
}

trait AsInstructionSink {
    fn insn(&mut self) -> InstructionSink<'_>;
}

impl AsInstructionSink for Vec<u8> {
    fn insn(&mut self) -> InstructionSink<'_> {
        InstructionSink::new(self)
    }
}

const MEMIDX_WASI: u32 = 0;

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
    data_offset: i32,
    strings: HashMap<StrId, i32>,
    objects: IndexSet<Object>,
    tuples: Tuples<ObjectId>,

    /// Wasm `funcidx` for each [`Object::FnWasi`] and [`Object::FnDef`].
    funcidxs: IndexSet<Object>,

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

    /// Type and start of Wasm local range for each IR variable in the current function.
    variables: HashMap<lower::InstrId, ObjectId>,

    /// The current function body.
    body: Vec<u8>,
}

impl<'a> Wasm<'a> {
    fn named(&self, module: ModuleId, string: &str) -> Named {
        self.names.names[&(module, self.ir.strings.get_id(string).unwrap())]
    }

    fn wasip1_sigdefs(&self) -> IndexMap<StrId, SigdefId> {
        // It isn't ideal to iterate through every name binding from every module just to filter
        // out all the ones except from the one module we care about, but it's fine for now. We
        // sort by name in order to achieve determinism.
        self.names
            .names
            .iter()
            .filter_map(|(&(module, name), &named)| match named {
                Named::Sigdef(fndef) if module == self.lib.wasip1 => Some((name, fndef)),
                _ => None,
            })
            .sorted_by_key(|&(name, _)| &self.ir.strings[name])
            .collect()
    }

    fn mkobj(&mut self, object: Object) -> ObjectId {
        let (i, _) = self.objects.insert_full(object);
        ObjectId::from_usize(i)
    }

    fn mkctx(&mut self, slots: &[ObjectId]) -> ObjectId {
        let tuple = self.tuples.make(slots);
        self.mkobj(Object::Ctx(tuple))
    }

    fn obj(&self, id: ObjectId) -> Object {
        self.objects[id.index()]
    }

    fn wasi_ctx(&mut self, wasip1_sigdefs: &IndexMap<StrId, SigdefId>) -> ObjectId {
        let mut slots = Vec::new();
        let ctxdef_wasi = self.named(self.lib.wasi, "Wasi").ctxdef();
        for instr in self.ir.ctxdefs[ctxdef_wasi].def.body {
            match self.ir.instrs[instr] {
                Instr::Param => unimplemented!(),
                Instr::Open => unimplemented!(),
                Instr::Ctx { slots } => unimplemented!(),
                Instr::NeedTydef { def, params } => unimplemented!(),
                Instr::NeedSigdef { def, params } => unimplemented!(),
                Instr::NeedValdef { def, params } => unimplemented!(),
                Instr::NeedCtxdef { def, params } => unimplemented!(),
                Instr::Tagdef { def, params } => unimplemented!(),
                Instr::Tuple { elems } => unimplemented!(),
                Instr::Record { fields } => unimplemented!(),
                Instr::Context => unimplemented!(),
                Instr::Get { ctx, slot } => unimplemented!(),
                Instr::BindTydef {
                    def,
                    params,
                    bind,
                    args,
                } => unimplemented!(),
                Instr::BindTagdef {
                    def,
                    params,
                    bind,
                    args,
                } => unimplemented!(),
                Instr::BindAliasdef {
                    def,
                    params,
                    bind,
                    args,
                } => unimplemented!(),
                Instr::BindSigdef {
                    def,
                    params,
                    bind,
                    args,
                } => unimplemented!(),
                Instr::BindFndef {
                    def,
                    params,
                    bind,
                    args,
                } => unimplemented!(),
                Instr::BindValdef {
                    def,
                    params,
                    bind,
                    args,
                } => unimplemented!(),
                Instr::BindLit { def, params, bind } => unimplemented!(),
                Instr::BindCtxdef {
                    def,
                    params,
                    bind,
                    args,
                } => todo!(),
                Instr::Sig { param, result } => unimplemented!(),
                Instr::Set { lhs, rhs } => unimplemented!(),
                Instr::If { cond } => unimplemented!(),
                Instr::Else { result } => unimplemented!(),
                Instr::EndIf { result } => unimplemented!(),
                Instr::Loop => unimplemented!(),
                Instr::EndLoop => unimplemented!(),
                Instr::Br { depth } => unimplemented!(),
                Instr::Expr { ty, expr } => unimplemented!(),
            }
        }
        self.mkctx(&slots)
    }

    fn next_funcidx(&self) -> u32 {
        self.funcidxs.len() as u32
    }

    fn get_func(&self, funcidx: u32) -> Object {
        self.funcidxs[funcidx as usize]
    }

    fn get_funcidx(&self, fndef: FndefId, ctx: ObjectId) -> Option<u32> {
        Some(self.funcidxs.get_index_of(&Object::FnDef(fndef, ctx))? as u32)
    }

    fn push_func_wasi(&mut self) -> u32 {
        let funcidx = self.next_funcidx();
        self.funcidxs.insert(Object::FnWasi(funcidx));
        funcidx
    }

    fn insert_func(&mut self, func: Object) -> u32 {
        let (i, _) = self.funcidxs.insert_full(func);
        i as u32
    }

    /// Execute `f` for each Wasm type needed to represent `ty` in the current context.
    fn layout(&self, ty: ObjectId, f: &mut impl FnMut(ValType)) {
        match self.obj(ty) {
            Object::TyI32 => f(ValType::I32),
            Object::TyI64 => f(ValType::I64),
            Object::TyLitInt(_)
            | Object::TyLitChar
            | Object::TyLitString
            | Object::TyMemIdx
            | Object::Sig(_, _)
            | Object::FnInt(_, _)
            | Object::FnChar
            | Object::FnString
            | Object::FnInstr(_)
            | Object::FnWasi(_)
            | Object::FnDef(_, _)
            | Object::ValU32(_)
            | Object::ValDyn(_, _)
            | Object::Ctx(_) => panic!(),
        }
    }

    /// Get the number of Wasm types needed to represent `ty` in the current context.
    ///
    /// Linear time.
    fn layout_len(&mut self, ty: ObjectId) -> usize {
        let mut len = 0;
        self.layout(ty, &mut |_| len += 1);
        len
    }

    /// Get a [`Vec`] of the Wasm types needed to represent `ty` in the current context.
    fn layout_vec(&mut self, ty: ObjectId) -> Vec<ValType> {
        let mut types = Vec::new();
        self.layout(ty, &mut |t| types.push(t));
        types
    }

    fn make_locals(&mut self, ty: ObjectId) -> LocalId {
        let start = self.locals.len_idx();
        let locals = self.layout_vec(ty);
        self.locals.append(&mut IndexVec::from_vec(locals));
        start
    }

    fn get_locals(&mut self, ty: ObjectId, start: LocalId) {
        let len = self.layout_len(ty);
        let end = LocalId::from_usize(start.index() + len);
        for localidx in (IdRange { start, end }) {
            self.body.insn().local_get(localidx.raw());
        }
    }

    fn set_locals(&mut self, ty: ObjectId, start: LocalId) {
        let len = self.layout_len(ty);
        let end = LocalId::from_usize(start.index() + len);
        for localidx in (IdRange { start, end }).into_iter().rev() {
            self.body.insn().local_set(localidx.raw());
        }
    }

    fn get(&mut self, instr: lower::InstrId) {
        let Object::ValDyn(ty, start) = self.obj(self.variables[&instr]) else {
            panic!()
        };
        self.get_locals(ty, start)
    }

    fn set(&mut self, ty: ObjectId, instr: lower::InstrId) {
        let start = self.make_locals(ty);
        let obj = self.mkobj(Object::ValDyn(ty, start));
        let prev = self.variables.insert(instr, obj);
        assert!(prev.is_none());
        self.set_locals(ty, start);
    }

    fn string(&mut self, id: StrId) {
        let string = &self.ir.strings[id];
        let len = string.len() as i32;
        let &mut offset = self.strings.entry(id).or_insert_with(|| {
            let offset = self.data_offset;
            self.section_data
                .active(MEMIDX_WASI, &ConstExpr::i32_const(offset), string.bytes());
            self.data_offset += len;
            offset
        });
        self.body.insn().i32_const(offset).i32_const(len);
    }

    fn val_u32(&mut self, fill: Fill, valdef: ValdefId) -> u32 {
        let context = self.ir.ctx(fill.ctx);
        // TODO: Pass the right context to `index_val`.
        match self.slots[fill.slots][context.index_val(valdef, fill.ctx)] {
            Slot::ValU32(n) => n,
            _ => panic!(),
        }
    }

    fn memarg(&mut self, fill: Fill) -> MemArg {
        MemArg {
            offset: self.val_u32(fill, self.val_offset).into(),
            align: self.val_u32(fill, self.val_align),
            memory_index: self.val_u32(fill, self.val_memidx),
        }
    }

    fn wasm_instruction(&mut self, fill: Fill, instruction: Instruction) {
        match instruction {
            Instruction::Unreachable => {
                self.body.insn().unreachable();
            }

            Instruction::I32Load => {
                let memarg = self.memarg(fill);
                self.body.insn().i32_load(memarg);
            }
            Instruction::I64Load => {
                let memarg = self.memarg(fill);
                self.body.insn().i64_load(memarg);
            }
            Instruction::I32Load8S => {
                let memarg = self.memarg(fill);
                self.body.insn().i32_load8_s(memarg);
            }
            Instruction::I32Load8U => {
                let memarg = self.memarg(fill);
                self.body.insn().i32_load8_u(memarg);
            }
            Instruction::I32Load16S => {
                let memarg = self.memarg(fill);
                self.body.insn().i32_load16_s(memarg);
            }
            Instruction::I32Load16U => {
                let memarg = self.memarg(fill);
                self.body.insn().i32_load16_u(memarg);
            }
            Instruction::I64Load8S => {
                let memarg = self.memarg(fill);
                self.body.insn().i64_load8_s(memarg);
            }
            Instruction::I64Load8U => {
                let memarg = self.memarg(fill);
                self.body.insn().i64_load8_u(memarg);
            }
            Instruction::I64Load16S => {
                let memarg = self.memarg(fill);
                self.body.insn().i64_load16_s(memarg);
            }
            Instruction::I64Load16U => {
                let memarg = self.memarg(fill);
                self.body.insn().i64_load16_u(memarg);
            }
            Instruction::I64Load32S => {
                let memarg = self.memarg(fill);
                self.body.insn().i64_load32_s(memarg);
            }
            Instruction::I64Load32U => {
                let memarg = self.memarg(fill);
                self.body.insn().i64_load32_u(memarg);
            }
            Instruction::I32Store => {
                let memarg = self.memarg(fill);
                self.body.insn().i32_store(memarg);
            }
            Instruction::I64Store => {
                let memarg = self.memarg(fill);
                self.body.insn().i64_store(memarg);
            }
            Instruction::I32Store8 => {
                let memarg = self.memarg(fill);
                self.body.insn().i32_store8(memarg);
            }
            Instruction::I32Store16 => {
                let memarg = self.memarg(fill);
                self.body.insn().i32_store16(memarg);
            }
            Instruction::I64Store8 => {
                let memarg = self.memarg(fill);
                self.body.insn().i64_store8(memarg);
            }
            Instruction::I64Store16 => {
                let memarg = self.memarg(fill);
                self.body.insn().i64_store16(memarg);
            }
            Instruction::I64Store32 => {
                let memarg = self.memarg(fill);
                self.body.insn().i64_store32(memarg);
            }
            Instruction::MemorySize => {
                let memidx = self.val_u32(fill, self.val_memidx);
                self.body.insn().memory_size(memidx);
            }
            Instruction::MemoryGrow => {
                let memidx = self.val_u32(fill, self.val_memidx);
                self.body.insn().memory_grow(memidx);
            }
            Instruction::MemoryCopy => {
                let dst = self.val_u32(fill, self.val_dst);
                let src = self.val_u32(fill, self.val_src);
                self.body.insn().memory_copy(dst, src);
            }
            Instruction::MemoryFill => {
                let memidx = self.val_u32(fill, self.val_memidx);
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

    fn instrs(&mut self, fill: Fill, param: TypeId, mut instr: lower::LocalId) -> lower::LocalId {
        loop {
            match self.ir.instrs[instr] {
                Instr::BindCall(fndef, local) => todo!(),
                Instr::EndBind => break,
                Instr::Val(valdef, ctx) => todo!(),
                Instr::Param => {
                    self.get_locals(fill, param, LocalId::from_raw(0));
                }
                Instr::Copy(local) => {
                    self.get(local);
                }
                Instr::Set(lhs, rhs) => {
                    self.get(rhs);
                    let Local { fill, start } = self.variables[&lhs];
                    self.set_locals(fill, self.ir.locals[lhs], start);
                }
                Instr::Nominal(tagdef, local) => todo!(),
                Instr::Tuple(locals) => {
                    for &id in locals.get(&self.ir.refs) {
                        self.get(id);
                    }
                }
                Instr::Record(_, locals) => {
                    for &id in locals.get(&self.ir.refs) {
                        self.get(id);
                    }
                }
                Instr::Elem(tuple, index) => {
                    let Local { fill, mut start } = self.variables[&tuple];
                    let ty = self.ir.locals[tuple];
                    let range = self.ir.ty(ty).tuple(); // TODO: Handle type aliases and bindings.
                    // TODO: Make this not be linear time.
                    for i in (IdRange {
                        start: ElemId::new(0),
                        end: index,
                    }) {
                        start += self.layout_len(
                            fill,
                            self.ir.tuples[TupleLoc::from_raw(range.start.raw() + i.raw())],
                        );
                    }
                    let elem = TupleLoc::from_raw(range.start.raw() + index.raw());
                    self.get_locals(fill, self.ir.tuples[elem], start);
                }
                Instr::Field(record, index) => {
                    let Local { fill, mut start } = self.variables[&record];
                    let ty = self.ir.locals[record];
                    let range = self.ir.ty(ty).record(); // TODO: Handle type aliases and bindings.
                    // TODO: Make this not be linear time.
                    for i in (IdRange {
                        start: FieldId::new(0),
                        end: index,
                    }) {
                        let (_, field) =
                            self.ir.records[TupleLoc::from_raw(range.start.raw() + i.raw())];
                        start += self.layout_len(fill, field);
                    }
                    let loc = TupleLoc::from_raw(range.start.raw() + index.raw());
                    let (_, field) = self.ir.records[loc];
                    self.get_locals(fill, field, start);
                }
                Instr::Call(fndef, local) => {
                    self.get(local);
                    let slot = match self.ir.bodies[fndef] {
                        Some(_) => {
                            // TODO: Pass the correct fill to `Slot::FnDef`.
                            Slot::FnDef(fndef, fill)
                        }
                        None => {
                            let context = self.ir.ctx(fill.ctx);
                            // TODO: Pass the correct context to `index_fn`.
                            self.slots[fill.slots][context.index_fn(fndef, fill.ctx)]
                        }
                    };
                    match slot {
                        Slot::FnInstr(instruction) => self.wasm_instruction(fill, instruction),
                        _ => {
                            let funcidx = self.insert_func(slot);
                            self.body.insn().call(funcidx);
                        }
                    }
                }
                Instr::If(cond, ty) => {
                    let Local { fill, start: _ } = self.variables[&cond];
                    let layout = self.layout_vec(fill, ty);
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
                Instr::ReturnBind(ctx) => todo!(),
            }
            self.set(fill, instr);
            instr += 1;
        }
        instr
    }

    fn interp(&mut self, ctx: ObjectId, body: Body) -> ObjectId {
        for instr in body.body {
            let result = match self.ir.instrs[instr] {
                Instr::Param => ctx,
                Instr::Open => todo!(),
                Instr::Ctx { slots } => todo!(),
                Instr::NeedTydef { def, params } => todo!(),
                Instr::NeedSigdef { def, params } => todo!(),
                Instr::NeedValdef { def, params } => todo!(),
                Instr::NeedCtxdef { def, params } => todo!(),
                Instr::Tagdef { def, params } => todo!(),
                Instr::Tuple { elems } => todo!(),
                Instr::Record { fields } => todo!(),
                Instr::Context => todo!(),
                Instr::Get { ctx, slot } => todo!(),
                Instr::BindTydef {
                    def,
                    params,
                    bind,
                    args,
                } => todo!(),
                Instr::BindTagdef {
                    def,
                    params,
                    bind,
                    args,
                } => todo!(),
                Instr::BindAliasdef {
                    def,
                    params,
                    bind,
                    args,
                } => todo!(),
                Instr::BindSigdef {
                    def,
                    params,
                    bind,
                    args,
                } => todo!(),
                Instr::BindFndef {
                    def,
                    params,
                    bind,
                    args,
                } => todo!(),
                Instr::BindValdef {
                    def,
                    params,
                    bind,
                    args,
                } => todo!(),
                Instr::BindLit { def, params, bind } => todo!(),
                Instr::BindCtxdef {
                    def,
                    params,
                    bind,
                    args,
                } => todo!(),
                Instr::Sig { param, result } => todo!(),
                Instr::Set { lhs, rhs } => todo!(),
                Instr::If { cond } => todo!(),
                Instr::Else { result } => todo!(),
                Instr::EndIf { result } => todo!(),
                Instr::Loop => todo!(),
                Instr::EndLoop => todo!(),
                Instr::Br { depth } => todo!(),
                Instr::Expr { ty, expr } => todo!(),
            };
            self.variables.insert(instr, result);
        }
        todo!()
    }

    fn funcidx(&self) -> u32 {
        // TODO: Handle the case where non-function imports have also been added.
        self.section_import.len() + self.section_function.len()
    }

    fn func(&mut self, funcidx: u32) {
        match self.get_func(funcidx) {
            Object::FnDef(fndef, ctx) => {
                let Sigdef { ctx: _, sig } = self.ir.fndefs[fndef];
                let Object::Sig(param, result) = self.obj(self.interp(ctx, sig)) else {
                    panic!()
                };
                let params = self.layout_vec(param);
                let results = self.layout_vec(result);
                self.instrs(ctx, param, self.ir.bodies[fndef]);
                let mut f =
                    Function::new_with_locals_types(self.locals.iter().skip(params.len()).copied());
                f.raw(take(&mut self.body));
                self.locals = Default::default();
                self.variables = Default::default();
                self.section_code.function(&f);
                self.section_function.function(self.section_type.len());
                self.section_type.ty().function(params, results);
            }
            _ => panic!(),
        }
    }

    fn program(mut self) -> Vec<u8> {
        let wasip1_sigdefs = self.wasip1_sigdefs();
        let wasi_ctx = self.wasi_ctx(&wasip1_sigdefs);

        let main_funcidx = self.funcidx();
        let mut next_fn = main_funcidx;
        while next_fn < self.next_funcidx() {
            self.func(next_fn);
            next_fn += 1;
        }

        self.section_global.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: false,
                shared: false,
            },
            &ConstExpr::i32_const(self.data_offset),
        );

        assert_eq!(self.section_memory.len(), MEMIDX_WASI);
        self.section_memory.memory(MemoryType {
            minimum: ((self.data_offset + 65535) / 65536) as u64,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        self.section_export
            .export("memory", ExportKind::Memory, MEMIDX_WASI);

        let start = self.funcidx();
        self.section_function.function(self.section_type.len());
        self.section_type.ty().function([], []);
        self.section_code.function(&{
            let mut f = Function::new([]);
            f.instructions()
                .call(self.get_funcidx(self.main, wasi_ctx).unwrap())
                .i32_const(0)
                .call(wasip1_sigdefs[&self.ir.strings.get_id("proc_exit").unwrap()].raw())
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

pub fn wasm(ir: &mut IR, names: &Names, lib: Lib, main: FndefId) -> Vec<u8> {
    Wasm {
        ir,
        names,
        lib,
        main,
        val_memidx: names.names[&(lib.wasm, ir.strings.get_id("memidx").unwrap())].valdef(),
        val_dst: names.names[&(lib.wasm, ir.strings.get_id("dst").unwrap())].valdef(),
        val_src: names.names[&(lib.wasm, ir.strings.get_id("src").unwrap())].valdef(),
        val_align: names.names[&(lib.wasm, ir.strings.get_id("align").unwrap())].valdef(),
        val_offset: names.names[&(lib.wasm, ir.strings.get_id("offset").unwrap())].valdef(),
        data_offset: Default::default(),
        strings: Default::default(),
        objects: Default::default(),
        tuples: Default::default(),

        funcidxs: Default::default(),

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
