use std::{collections::HashMap, mem::take};

use index_vec::{IndexVec, define_index_type};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use strum::{EnumIter, IntoStaticStr};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, GlobalSection, GlobalType, ImportSection, InstructionSink, MemArg,
    MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::{
    intern::StrId,
    lower::{
        self, Body, ElemId, Expr, FieldId, FndefId, IR, Instr, InstrId, InstrList, ModuleId, Named,
        Names, Sigdef, SigdefId, ValdefId,
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
    /// An open slot.
    Open,

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

    /// A tuple or record type.
    TyTuple(TupleRange),

    /// A "type" that represents a context instead of an actual value.
    TyCtx,

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

    /// The "result" of executing a statement that only has side effects.
    ValStmt,

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

    /// Wasm `funcidx` of each imported WASI P1 function, by name.
    wasi_funcidx: HashMap<StrId, u32>,

    section_type: TypeSection,
    section_import: ImportSection,
    section_function: FunctionSection,
    section_memory: MemorySection,
    section_global: GlobalSection,
    section_export: ExportSection,
    section_code: CodeSection,
    section_data: DataSection,

    /// Static [`Object`] for each IR value in the current [`Body`].
    variables: HashMap<lower::InstrId, ObjectId>,

    /// Locals for the current function.
    locals: IndexVec<LocalId, ValType>,

    /// The current function body.
    body: Vec<u8>,
}

impl<'a> Wasm<'a> {
    /// The empty context.
    fn empty(&mut self) -> ObjectId {
        self.mkctx(&[])
    }

    /// Monomorphize a static IR `node` against a concrete context `ctx`, producing the
    /// corresponding codegen [`Object`]. This is the backend analogue of lowering's
    /// `reduce`/`invoke`: it resolves contextual references, reduces applications, and unfolds
    /// transparent definitions, but bottoms out in target [`Object`]s instead of IR nodes.
    fn eval(&mut self, node: lower::NodeId, ctx: ObjectId) -> ObjectId {
        match self.ir.nodes[node.index()] {
            lower::Node::Sig { param, result } => {
                let param = self.eval(param, ctx);
                let result = self.eval(result, ctx);
                self.mkobj(Object::Sig(param, result))
            }
            lower::Node::Tuple { elems } => {
                let items = self.ir.lists[elems].to_vec();
                let objs: Vec<ObjectId> = items.into_iter().map(|e| self.eval(e, ctx)).collect();
                let tuple = self.tuples.make(&objs);
                self.mkobj(Object::TyTuple(tuple))
            }
            // A definition's body is a lambda over its contextual needs. Instantiating it against
            // `ctx` means binding those needs and evaluating the result. (Milestone B: bind the
            // needs from `ctx`; for now only the no-need case is exercised.)
            lower::Node::Lambda { needs, result, .. } => {
                if self.ir.lists[needs].is_empty() {
                    self.eval(result, ctx)
                } else {
                    todo!("eval: Lambda with needs (milestone B)")
                }
            }
            other => todo!("eval: {other:?} (milestone B)"),
        }
    }

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

    fn list(&self, items: InstrList) -> Vec<ObjectId> {
        Vec::from_iter(
            items
                .get(&self.ir.items)
                .iter()
                .map(|item| self.variables[item]),
        )
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

    fn local(&self, x: InstrId) -> (ObjectId, LocalId) {
        match self.obj(self.variables[&x]) {
            Object::ValDyn(ty, start) => (ty, start),
            _ => panic!(),
        }
    }

    fn wasi_ctx(&mut self, wasip1_sigdefs: &IndexMap<StrId, SigdefId>) -> ObjectId {
        // MILESTONE A: minimal root context. The empty `main` ignores the context entirely, so we
        // only need to (a) import `proc_exit` so `_start` can call it, and (b) return *some* Ctx
        // to key `FnDef(main, ctx)`.
        //
        // TODO(milestone B): build the full `Wasi` context as an `Object::Ctx`, in `lib/wasi.moss`
        // member order: I32/I64/MemIdx -> Ty*; the ten `lit::Literal*` -> TyLit*; the realizers ->
        // FnInt/FnChar/FnString; `Numerals[Number=I32/I64]` -> nested Ctx of digit/radix ValU32;
        // `wasm::Wasm[Base]` -> nested Ctx of FnInstr (+ memidx/align/offset ValU32); and
        // `wasip1::WasiP1[Base]` -> nested Ctx of FnWasi, importing *every* wasip1 function with
        // its eval'd signature. Then `eval` must resolve `Need*`/`Get` against this Ctx.
        for (&name, _) in wasip1_sigdefs {
            if &self.ir.strings[name] == "proc_exit" {
                // `proc_exit : (i32) -> ()`.
                let typeidx = self.section_type.len();
                self.section_type.ty().function([ValType::I32], []);
                let funcidx = self.push_func_wasi();
                self.section_import
                    .import(WASIP1, "proc_exit", EntityType::Function(typeidx));
                self.wasi_funcidx.insert(name, funcidx);
            }
        }
        self.empty()
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
            Object::TyTuple(elems) => {
                for &elem in &self.tuples[elems] {
                    self.layout(elem, f);
                }
            }
            Object::Open
            | Object::TyLitInt(_)
            | Object::TyLitChar
            | Object::TyLitString
            | Object::TyMemIdx
            | Object::TyCtx
            | Object::Sig(_, _)
            | Object::FnInt(_, _)
            | Object::FnChar
            | Object::FnString
            | Object::FnInstr(_)
            | Object::FnWasi(_)
            | Object::FnDef(_, _)
            | Object::ValU32(_)
            | Object::ValDyn(_, _)
            | Object::ValStmt
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

    fn set(&mut self, ty: ObjectId) -> ObjectId {
        let start = self.make_locals(ty);
        self.set_locals(ty, start);
        self.mkobj(Object::ValDyn(ty, start))
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

    fn val_u32(&mut self, ctx: ObjectId, valdef: ValdefId) -> u32 {
        todo!()
    }

    fn memarg(&mut self, ctx: ObjectId) -> MemArg {
        MemArg {
            offset: self.val_u32(ctx, self.val_offset).into(),
            align: self.val_u32(ctx, self.val_align),
            memory_index: self.val_u32(ctx, self.val_memidx),
        }
    }

    fn wasm_instruction(&mut self, ctx: ObjectId, instruction: Instruction) {
        match instruction {
            Instruction::Unreachable => {
                self.body.insn().unreachable();
            }

            Instruction::I32Load => {
                let memarg = self.memarg(ctx);
                self.body.insn().i32_load(memarg);
            }
            Instruction::I64Load => {
                let memarg = self.memarg(ctx);
                self.body.insn().i64_load(memarg);
            }
            Instruction::I32Load8S => {
                let memarg = self.memarg(ctx);
                self.body.insn().i32_load8_s(memarg);
            }
            Instruction::I32Load8U => {
                let memarg = self.memarg(ctx);
                self.body.insn().i32_load8_u(memarg);
            }
            Instruction::I32Load16S => {
                let memarg = self.memarg(ctx);
                self.body.insn().i32_load16_s(memarg);
            }
            Instruction::I32Load16U => {
                let memarg = self.memarg(ctx);
                self.body.insn().i32_load16_u(memarg);
            }
            Instruction::I64Load8S => {
                let memarg = self.memarg(ctx);
                self.body.insn().i64_load8_s(memarg);
            }
            Instruction::I64Load8U => {
                let memarg = self.memarg(ctx);
                self.body.insn().i64_load8_u(memarg);
            }
            Instruction::I64Load16S => {
                let memarg = self.memarg(ctx);
                self.body.insn().i64_load16_s(memarg);
            }
            Instruction::I64Load16U => {
                let memarg = self.memarg(ctx);
                self.body.insn().i64_load16_u(memarg);
            }
            Instruction::I64Load32S => {
                let memarg = self.memarg(ctx);
                self.body.insn().i64_load32_s(memarg);
            }
            Instruction::I64Load32U => {
                let memarg = self.memarg(ctx);
                self.body.insn().i64_load32_u(memarg);
            }
            Instruction::I32Store => {
                let memarg = self.memarg(ctx);
                self.body.insn().i32_store(memarg);
            }
            Instruction::I64Store => {
                let memarg = self.memarg(ctx);
                self.body.insn().i64_store(memarg);
            }
            Instruction::I32Store8 => {
                let memarg = self.memarg(ctx);
                self.body.insn().i32_store8(memarg);
            }
            Instruction::I32Store16 => {
                let memarg = self.memarg(ctx);
                self.body.insn().i32_store16(memarg);
            }
            Instruction::I64Store8 => {
                let memarg = self.memarg(ctx);
                self.body.insn().i64_store8(memarg);
            }
            Instruction::I64Store16 => {
                let memarg = self.memarg(ctx);
                self.body.insn().i64_store16(memarg);
            }
            Instruction::I64Store32 => {
                let memarg = self.memarg(ctx);
                self.body.insn().i64_store32(memarg);
            }
            Instruction::MemorySize => {
                let memidx = self.val_u32(ctx, self.val_memidx);
                self.body.insn().memory_size(memidx);
            }
            Instruction::MemoryGrow => {
                let memidx = self.val_u32(ctx, self.val_memidx);
                self.body.insn().memory_grow(memidx);
            }
            Instruction::MemoryCopy => {
                let dst = self.val_u32(ctx, self.val_dst);
                let src = self.val_u32(ctx, self.val_src);
                self.body.insn().memory_copy(dst, src);
            }
            Instruction::MemoryFill => {
                let memidx = self.val_u32(ctx, self.val_memidx);
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

    fn expr(&mut self, ctx: ObjectId, expr: Expr) {
        match expr {
            Expr::Param { ty } => {
                // The function parameter occupies the first locals, `[0, layout_len(param))`.
                let ty = self.eval(ty, ctx);
                self.get_locals(ty, LocalId::from_usize(0));
            }
            Expr::Copy { value } => {
                self.get(value);
            }
            Expr::Nominal { ty, inner } => todo!("expr: Nominal (milestone B)"),
            Expr::Tuple { elems } => {
                for &id in elems.get(&self.ir.items) {
                    self.get(id);
                }
            }
            Expr::Record { fields } => {
                for &(_, id) in fields.get(&self.ir.records) {
                    self.get(id);
                }
            }
            Expr::Elem { tuple, index } => {
                let (ty, mut start) = self.local(tuple);
                let Object::TyTuple(range) = self.obj(ty) else {
                    panic!()
                };
                // TODO: Make this not be linear time.
                for i in (IdRange {
                    start: ElemId::new(0),
                    end: index,
                }) {
                    start += self
                        .layout_len(self.tuples[TupleLoc::from_raw(range.start.raw() + i.raw())]);
                }
                let elem = TupleLoc::from_raw(range.start.raw() + index.raw());
                self.get_locals(self.tuples[elem], start);
            }
            Expr::Field { record, index } => {
                let (ty, mut start) = self.local(record);
                let Object::TyTuple(range) = self.obj(ty) else {
                    panic!()
                };
                // TODO: Make this not be linear time.
                for i in (IdRange {
                    start: FieldId::new(0),
                    end: index,
                }) {
                    start += self
                        .layout_len(self.tuples[TupleLoc::from_raw(range.start.raw() + i.raw())]);
                }
                let elem = TupleLoc::from_raw(range.start.raw() + index.raw());
                self.get_locals(self.tuples[elem], start);
            }
            Expr::Val { val } => todo!("expr: Val (milestone B)"),
            Expr::Call { func, arg } => todo!("expr: Call (milestone B)"),
            Expr::Bind { ctx } => todo!("expr: Bind (milestone B)"),
        }
    }

    fn interp(&mut self, ctx: ObjectId, inputs: &[ObjectId], body: Body) -> ObjectId {
        for instr in body.body {
            let result = match self.ir.instrs[instr] {
                Instr::Set { lhs, rhs } => {
                    let (ty, start) = self.local(lhs);
                    self.get(rhs);
                    self.set_locals(ty, start);
                    self.mkobj(Object::ValStmt)
                }
                Instr::If { ty, cond } => {
                    let ty = self.eval(ty, ctx);
                    let layout = self.layout_vec(ty);
                    let typeidx = self.section_type.len();
                    self.section_type.ty().function([], layout);
                    self.get(cond);
                    self.body.insn().if_(BlockType::FunctionType(typeidx));
                    self.mkobj(Object::ValStmt)
                }
                Instr::Else { result } => {
                    self.get(result);
                    self.body.insn().else_();
                    self.mkobj(Object::ValStmt)
                }
                Instr::EndIf { result } => {
                    self.get(result);
                    self.body.insn().end();
                    // TODO: Properly handle the value returned from `if`/`else`.
                    self.mkobj(Object::ValStmt)
                }
                Instr::Loop => {
                    self.body.insn().loop_(BlockType::Empty);
                    self.mkobj(Object::ValStmt)
                }
                Instr::EndLoop => {
                    self.body.insn().end();
                    self.mkobj(Object::ValStmt)
                }
                Instr::Br { depth } => {
                    self.body.insn().br(depth.0);
                    self.mkobj(Object::ValStmt)
                }
                Instr::Expr { ty, expr } => {
                    let ty = self.eval(ty, ctx);
                    self.expr(ctx, expr);
                    self.set(ty)
                }
            };
            self.variables.insert(instr, result);
        }
        self.variables[&body.result()]
    }

    fn funcidx(&self) -> u32 {
        // TODO: Handle the case where non-function imports have also been added.
        self.section_import.len() + self.section_function.len()
    }

    fn func(&mut self, funcidx: u32) {
        match self.get_func(funcidx) {
            Object::FnDef(fndef, ctx) => {
                let Sigdef(sig) = self.ir.fndefs[fndef];
                // Monomorphize the signature against the context to lay out params and results.
                let signature = self.eval(sig, ctx);
                let Object::Sig(param, result) = self.obj(signature) else {
                    panic!()
                };
                let params = self.layout_vec(param);
                let results = self.layout_vec(result);
                // The parameter occupies the first locals; body instructions add more after it.
                self.make_locals(param);
                let body = self.ir.bodies[fndef];
                self.interp(ctx, &[], body);
                // Leave the body's result value on the operand stack, then close the function.
                self.get(body.result());
                self.body.insn().end();
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

    fn dbg_node(&self, tag: &str, node: lower::NodeId, depth: usize) {
        if depth > 6 {
            return;
        }
        let n = self.ir.nodes[node.index()];
        eprintln!("{}{tag} %{} = {n:?}", "  ".repeat(depth), node.index());
        use lower::Node::*;
        let kids: Vec<lower::NodeId> = match n {
            Lambda { needs, result, .. } => self.ir.lists[needs]
                .iter()
                .copied()
                .chain([result])
                .collect(),
            Apply { lambda, args } => [lambda]
                .into_iter()
                .chain(self.ir.lists[args].iter().copied())
                .collect(),
            List { items } | Tuple { elems: items } => self.ir.lists[items].to_vec(),
            NeedTydef { param, .. }
            | NeedSigdef { param, .. }
            | NeedValdef { param, .. }
            | NeedCtxdef { param, .. } => vec![param],
            Get { ctx, .. } => vec![ctx],
            Bind { args, bind } => self.ir.lists[args].iter().copied().chain([bind]).collect(),
            BindTydef { bind, .. }
            | BindSigdef { bind, .. }
            | BindValdef { bind, .. }
            | BindCtxdef { bind, .. } => vec![bind],
            Sig { param, result } => vec![param, result],
            _ => vec![],
        };
        for k in kids {
            self.dbg_node("", k, depth + 1);
        }
    }

    fn program(mut self) -> Vec<u8> {
        if std::env::var("MOSS_DBG").is_ok() {
            let Sigdef(sig) = self.ir.fndefs[self.main];
            eprintln!("=== main sig node ===");
            self.dbg_node("sig", sig, 0);
            eprintln!("=== main body ===");
            let body = self.ir.bodies[self.main];
            for instr in body.body {
                eprintln!("i{} = {:?}", instr.index(), self.ir.instrs[instr]);
                if let Instr::Expr { ty, expr } = self.ir.instrs[instr] {
                    self.dbg_node("  ty", ty, 1);
                    match expr {
                        Expr::Call { func, .. } => self.dbg_node("  func", func, 1),
                        Expr::Val { val } => self.dbg_node("  val", val, 1),
                        Expr::Bind { ctx } => self.dbg_node("  ctx", ctx, 1),
                        _ => {}
                    }
                }
            }
        }
        let wasip1_sigdefs = self.wasip1_sigdefs();
        let wasi_ctx = self.wasi_ctx(&wasip1_sigdefs);

        // Register `main` itself as the first defined function, monomorphized against the root
        // context. The worklist loop below then emits it (and anything it transitively calls).
        self.insert_func(Object::FnDef(self.main, wasi_ctx));

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
        let main_funcidx = self.get_funcidx(self.main, wasi_ctx).unwrap();
        let proc_exit = self.wasi_funcidx[&self.ir.strings.get_id("proc_exit").unwrap()];
        self.section_function.function(self.section_type.len());
        self.section_type.ty().function([], []);
        self.section_code.function(&{
            let mut f = Function::new([]);
            f.instructions()
                .call(main_funcidx)
                .i32_const(0)
                .call(proc_exit)
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
        wasi_funcidx: Default::default(),

        section_type: Default::default(),
        section_import: Default::default(),
        section_function: Default::default(),
        section_memory: Default::default(),
        section_global: Default::default(),
        section_export: Default::default(),
        section_code: Default::default(),
        section_data: Default::default(),

        variables: Default::default(),
        locals: Default::default(),
        body: Default::default(),
    }
    .program()
}
