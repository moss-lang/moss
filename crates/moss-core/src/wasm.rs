use std::{cmp::Reverse, collections::HashMap, mem::take};

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
        self, AliasdefId, Body, CtxdefId, ElemId, Expr, FieldId, FndefId, IR, Instr, InstrId,
        InstrList, ModuleId, Named, Names, Sigdef, SigdefId, TagdefId, TydefId, Val, ValdefId,
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

define_index_type! {
    /// The index of a compile-time closure.
    struct ClosureId = u32;
}

define_index_type! {
    /// The index of a compile-time keyed context.
    struct CtxId = u32;
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum SlotKey {
    Tydef(TydefId),
    Sigdef(SigdefId),
    Valdef(ValdefId),
    Ctxdef(CtxdefId),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Slot {
    key: Option<SlotKey>,
    value: ObjectId,
}

#[derive(Clone, Debug)]
struct Closure {
    env: HashMap<InstrId, ObjectId>,
    start: InstrId,
    end: InstrId,
    result: InstrId,
}

#[derive(Clone, Debug)]
struct Ctx {
    slots: Vec<Slot>,
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
const GLOBAL_HEAP: u32 = 0;
const RUNTIME_MEMORY_SLACK: i32 = 2 * 1024 * 1024;

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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum IntFormat {
    U32,
    I32,
    U64,
    I64,
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

    /// A specific kind of integer literal realization awaiting specialization.
    FnIntTemplate(IntLitIn, IntLitOut),

    /// A character literal realization awaiting specialization.
    FnCharTemplate,

    /// A string literal realization awaiting specialization.
    FnStringTemplate,

    /// A concrete literal realization in a specific context.
    FnInt(IntLitIn, IntLitOut, ObjectId),

    /// A concrete character realization in a specific context.
    FnChar(ObjectId),

    /// A concrete string realization in a specific context.
    FnString(ObjectId),

    /// A Wasm instruction awaiting specialization.
    FnInstrTemplate(Instruction),

    /// A concrete Wasm instruction in a specific context.
    FnInstr(Instruction, ObjectId),

    /// The Wasm `funcidx` of an imported WASI P1 function.
    FnWasi(u32),

    /// Print a string to stdout.
    FnPrint,

    /// Format the method receiver as a string.
    FnToStringThis(IntFormat, ObjectId),

    /// Format the explicit argument as a string.
    FnToStringArg(IntFormat),

    /// Get the process command-line arguments.
    FnArgs,

    /// Read a file into a string.
    FnRead,

    /// Get the length of an `Args` value.
    FnArgsLen(ObjectId),

    /// Get one argument from an `Args` value.
    FnArgsGet(ObjectId),

    /// A contextual alias awaiting specialization.
    Alias(AliasdefId),

    /// A contextual nominal type awaiting specialization.
    Tag(TagdefId),

    /// A defined function awaiting specialization.
    FnTemplate(FndefId),

    /// A compile-time closure.
    Lambda(ClosureId),

    /// The result of a `bind for ... using ...`.
    Binding(ObjectId, ObjectId),

    /// A defined function in a specific context.
    FnDef(FndefId, ObjectId),

    /// A 32-bit unsigned integer constant.
    ValU32(u32),

    /// A compile-time literal value.
    ValLit(Val),

    /// A dynamic value with a type, stored in Wasm locals starting at a given index.
    ValDyn(ObjectId, LocalId),

    /// The "result" of executing a statement that only has side effects.
    ValStmt,

    /// A context with some output slots.
    Ctx(CtxId),
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
    fd_read: u32,
    fd_close: u32,
    path_open: u32,
    args_get: u32,
    args_sizes_get: u32,
    fd_write: u32,
    print_scratch: Option<i32>,
    data_offset: i32,
    strings: HashMap<StrId, i32>,
    objects: IndexSet<Object>,
    tuples: Tuples<ObjectId>,
    closures: IndexVec<ClosureId, Closure>,
    contexts: IndexVec<CtxId, Ctx>,

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
        self.push_ctx(Vec::new())
    }

    fn named(&self, module: ModuleId, string: &str) -> Named {
        self.names.names[&(module, self.ir.strings.get_id(string).unwrap())]
    }

    fn named_detached(&self, module: ModuleId, string: &str) -> Named {
        self.names.detached[&(module, self.ir.strings.get_id(string).unwrap())].into()
    }

    fn specialize_open_sig(&mut self, available: ObjectId, def: SigdefId, ctx: ObjectId) -> Option<ObjectId> {
        let ty_lhs = self.named(self.lib.ops, "Lhs").tydef();
        let ty_rhs = self.named(self.lib.ops, "Rhs").tydef();
        let lhs = self.lookup_in_ctx(ctx, SlotKey::Tydef(ty_lhs));
        let rhs = self.lookup_in_ctx(ctx, SlotKey::Tydef(ty_rhs));
        let ty_this = self.named(self.lib.prelude, "This").tydef();
        let val_this = self.named(self.lib.prelude, "this").valdef();
        let this_ty = self.lookup_in_ctx(available, SlotKey::Tydef(ty_this));
        let this_val = self.lookup_in_ctx(available, SlotKey::Valdef(val_this));

        let ty_uint32 = self.lookup_in_ctx(available, SlotKey::Tydef(self.named(self.lib.int, "Uint32").tydef()))?;
        let ty_int32 = self.lookup_in_ctx(available, SlotKey::Tydef(self.named(self.lib.int, "Int32").tydef()))?;
        let ty_uint64 = self.lookup_in_ctx(available, SlotKey::Tydef(self.named(self.lib.int, "Uint64").tydef()))?;
        let ty_int64 = self.lookup_in_ctx(available, SlotKey::Tydef(self.named(self.lib.int, "Int64").tydef()))?;
        let ty_uint = self.lookup_in_ctx(available, SlotKey::Tydef(self.named(self.lib.int, "Uint").tydef()))?;
        let ty_int = self.lookup_in_ctx(available, SlotKey::Tydef(self.named(self.lib.int, "Int").tydef()))?;
        let ty_args = self.lookup_in_ctx(available, SlotKey::Tydef(self.named(self.lib.prelude, "Args").tydef()));

        let binary = |this: &mut Self, i32_u, i32_s, i64_u, i64_s| {
            let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
                return None;
            };
            let instruction = if lhs == ty_uint32 && rhs == ty_uint32 {
                i32_u
            } else if lhs == ty_int32 && rhs == ty_int32 {
                i32_s
            } else if lhs == ty_uint64 && rhs == ty_uint64 {
                i64_u
            } else if lhs == ty_int64 && rhs == ty_int64 {
                i64_s
            } else if lhs == ty_uint && rhs == ty_uint {
                i32_u
            } else if lhs == ty_int && rhs == ty_int {
                i32_s
            } else {
                return None;
            };
            Some(this.mkobj(Object::FnInstrTemplate(instruction)))
        };

        let same_width = |this: &mut Self, i32, i64| {
            let (Some(lhs), Some(rhs)) = (lhs, rhs) else {
                return None;
            };
            let instruction = if (lhs == ty_uint32 || lhs == ty_int32 || lhs == ty_uint || lhs == ty_int)
                && lhs == rhs
            {
                i32
            } else if (lhs == ty_uint64 || lhs == ty_int64) && lhs == rhs {
                i64
            } else {
                return None;
            };
            Some(this.mkobj(Object::FnInstrTemplate(instruction)))
        };

        let this_ctx = match (this_ty, this_val) {
            (Some(this_ty), Some(this_val)) => Some(self.push_ctx(vec![
                Slot {
                    key: Some(SlotKey::Tydef(ty_this)),
                    value: this_ty,
                },
                Slot {
                    key: Some(SlotKey::Valdef(val_this)),
                    value: this_val,
                },
            ])),
            _ => None,
        };

        Some(if def == self.named(self.lib.ops, "eq").sigdef() {
            binary(self, Instruction::I32Eq, Instruction::I32Eq, Instruction::I64Eq, Instruction::I64Eq)?
        } else if def == self.named(self.lib.ops, "ne").sigdef() {
            binary(self, Instruction::I32Ne, Instruction::I32Ne, Instruction::I64Ne, Instruction::I64Ne)?
        } else if def == self.named(self.lib.ops, "lt").sigdef() {
            binary(self, Instruction::I32LtU, Instruction::I32LtS, Instruction::I64LtU, Instruction::I64LtS)?
        } else if def == self.named(self.lib.ops, "gt").sigdef() {
            binary(self, Instruction::I32GtU, Instruction::I32GtS, Instruction::I64GtU, Instruction::I64GtS)?
        } else if def == self.named(self.lib.ops, "le").sigdef() {
            binary(self, Instruction::I32LeU, Instruction::I32LeS, Instruction::I64LeU, Instruction::I64LeS)?
        } else if def == self.named(self.lib.ops, "ge").sigdef() {
            binary(self, Instruction::I32GeU, Instruction::I32GeS, Instruction::I64GeU, Instruction::I64GeS)?
        } else if def == self.named(self.lib.ops, "add").sigdef() {
            same_width(self, Instruction::I32Add, Instruction::I64Add)?
        } else if def == self.named(self.lib.ops, "sub").sigdef() {
            same_width(self, Instruction::I32Sub, Instruction::I64Sub)?
        } else if def == self.named(self.lib.ops, "mul").sigdef() {
            same_width(self, Instruction::I32Mul, Instruction::I64Mul)?
        } else if def == self.named(self.lib.ops, "div").sigdef() {
            binary(self, Instruction::I32DivU, Instruction::I32DivS, Instruction::I64DivU, Instruction::I64DivS)?
        } else if def == self.named(self.lib.ops, "rem").sigdef() {
            binary(self, Instruction::I32RemU, Instruction::I32RemS, Instruction::I64RemU, Instruction::I64RemS)?
        } else if def == self.named(self.lib.ops, "and").sigdef() {
            same_width(self, Instruction::I32And, Instruction::I64And)?
        } else if def == self.named(self.lib.ops, "or").sigdef() {
            same_width(self, Instruction::I32Or, Instruction::I64Or)?
        } else if def == self.named(self.lib.ops, "xor").sigdef() {
            same_width(self, Instruction::I32Xor, Instruction::I64Xor)?
        } else if def == self.named(self.lib.ops, "shl").sigdef() {
            same_width(self, Instruction::I32Shl, Instruction::I64Shl)?
        } else if def == self.named(self.lib.ops, "shr").sigdef() {
            binary(self, Instruction::I32ShrU, Instruction::I32ShrS, Instruction::I64ShrU, Instruction::I64ShrS)?
        } else if def == self.named_detached(self.lib.string, "to_string").sigdef() {
            let this_ty = this_ty?;
            let format = if this_ty == ty_uint32 || this_ty == ty_uint {
                IntFormat::U32
            } else if this_ty == ty_int32 || this_ty == ty_int {
                IntFormat::I32
            } else if this_ty == ty_uint64 {
                IntFormat::U64
            } else if this_ty == ty_int64 {
                IntFormat::I64
            } else {
                return None;
            };
            self.mkobj(Object::FnToStringThis(format, this_ctx?))
        } else if def == self.named_detached(self.lib.prelude, "len").sigdef() {
            if this_ty == ty_args {
                self.mkobj(Object::FnArgsLen(this_ctx?))
            } else {
                return None;
            }
        } else if def == self.named_detached(self.lib.prelude, "get").sigdef() {
            if this_ty == ty_args {
                self.mkobj(Object::FnArgsGet(this_ctx?))
            } else {
                return None;
            }
        } else {
            return None;
        })
    }

    fn key_name(&self, key: SlotKey) -> String {
        match key {
            SlotKey::Tydef(def) => self
                .names
                .names
                .iter()
                .find_map(|(&(_, name), &named)| match named {
                    Named::Tydef(id) if id == def => Some(self.ir.strings[name].to_owned()),
                    _ => None,
                })
                .unwrap_or_else(|| format!("tydef#{:?}", def)),
            SlotKey::Sigdef(def) => self
                .names
                .names
                .iter()
                .find_map(|(&(_, name), &named)| match named {
                    Named::Sigdef(id) if id == def => Some(self.ir.strings[name].to_owned()),
                    _ => None,
                })
                .or_else(|| {
                    self.names.detached.iter().find_map(|(&(_, name), &named)| match named {
                        lower::NamedFn::Sigdef(id) if id == def => Some(self.ir.strings[name].to_owned()),
                        _ => None,
                    })
                })
                .unwrap_or_else(|| format!("sigdef#{:?}", def)),
            SlotKey::Valdef(def) => self
                .names
                .names
                .iter()
                .find_map(|(&(_, name), &named)| match named {
                    Named::Valdef(id) if id == def => Some(self.ir.strings[name].to_owned()),
                    _ => None,
                })
                .unwrap_or_else(|| format!("valdef#{:?}", def)),
            SlotKey::Ctxdef(def) => self
                .names
                .names
                .iter()
                .find_map(|(&(_, name), &named)| match named {
                    Named::Ctxdef(id) if id == def => Some(self.ir.strings[name].to_owned()),
                    _ => None,
                })
                .unwrap_or_else(|| format!("ctxdef#{:?}", def)),
        }
    }

    fn slot_name(&self, slot: Slot) -> String {
        match slot.key {
            Some(key) => format!("{}={:?}", self.key_name(key), self.obj(slot.value)),
            None => format!("_={:?}", self.obj(slot.value)),
        }
    }

    fn ctx_name(&self, ctx: ObjectId) -> String {
        self.ctx_slots(ctx).iter().copied().map(|slot| self.slot_name(slot)).join(", ")
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

    fn push_ctx(&mut self, slots: Vec<Slot>) -> ObjectId {
        let id = self.contexts.push(Ctx { slots });
        self.mkobj(Object::Ctx(id))
    }

    fn obj(&self, id: ObjectId) -> Object {
        self.objects[id.index()]
    }

    fn ctx_id(&self, id: ObjectId) -> CtxId {
        match self.obj(id) {
            Object::Ctx(ctx) => ctx,
            _ => panic!(),
        }
    }

    fn ctx_slots(&self, id: ObjectId) -> &[Slot] {
        &self.contexts[self.ctx_id(id)].slots
    }

    fn ctx_slot(&self, id: ObjectId, slot: usize) -> Slot {
        self.ctx_slots(id)[slot]
    }

    fn push_closure(
        &mut self,
        env: HashMap<InstrId, ObjectId>,
        start: InstrId,
        end: InstrId,
        result: InstrId,
    ) -> ObjectId {
        let id = self.closures.push(Closure {
            env,
            start,
            end,
            result,
        });
        self.mkobj(Object::Lambda(id))
    }

    fn find_end_lambda(&self, start: InstrId) -> InstrId {
        let mut instr = start;
        loop {
            instr += 1;
            if let Instr::EndLambda {
                start: this_start,
                result: _,
            } = self.ir.instrs[instr]
                && this_start == start
            {
                return instr;
            }
        }
    }

    fn slot_key(&self, instr: InstrId) -> Option<SlotKey> {
        match self.ir.instrs[instr] {
            Instr::NeedTydef { def, .. } | Instr::BindTydef { def, .. } => Some(SlotKey::Tydef(def)),
            Instr::NeedSigdef { def, .. } | Instr::BindSigdef { def, .. } => Some(SlotKey::Sigdef(def)),
            Instr::NeedValdef { def, .. } | Instr::BindValdef { def, .. } => Some(SlotKey::Valdef(def)),
            Instr::NeedCtxdef { def, .. } | Instr::BindCtxdef { def, .. } => Some(SlotKey::Ctxdef(def)),
            _ => None,
        }
    }

    fn ctx_from_items(
        &mut self,
        ambient: ObjectId,
        env: &HashMap<InstrId, ObjectId>,
        items: InstrList,
    ) -> ObjectId {
        let slots = items
            .get(&self.ir.items)
            .iter()
            .map(|&item| Slot {
                key: self.slot_key(item),
                value: self.static_value(ambient, env, item),
            })
            .collect();
        self.push_ctx(slots)
    }

    fn merge_ctxs(&mut self, left: ObjectId, right: ObjectId) -> ObjectId {
        let mut slots = self.ctx_slots(left).to_vec();
        slots.extend_from_slice(self.ctx_slots(right));
        self.push_ctx(slots)
    }

    fn available_ctx(&mut self, ambient: ObjectId, env: &HashMap<InstrId, ObjectId>) -> ObjectId {
        let mut locals = env
            .iter()
            .filter_map(|(&instr, &value)| {
                Some((
                    Reverse(instr),
                    Slot {
                        key: Some(self.slot_key(instr)?),
                        value,
                    },
                ))
            })
            .collect::<Vec<_>>();
        locals.sort_by_key(|&(instr, _)| instr);

        let mut slots = Vec::with_capacity(locals.len() + self.ctx_slots(ambient).len());
        for (_, slot) in locals {
            slots.push(slot);
        }
        slots.extend_from_slice(self.ctx_slots(ambient));
        self.push_ctx(slots)
    }

    fn lookup_in_ctx(&mut self, ctx: ObjectId, key: SlotKey) -> Option<ObjectId> {
        let slots = self.ctx_slots(ctx).to_vec();
        for slot in &slots {
            if slot.key == Some(key) {
                return Some(slot.value);
            }
        }
        for slot in slots {
            if let Object::Ctx(_) = self.obj(slot.value)
                && let Some(found) = self.lookup_in_ctx(slot.value, key)
            {
                return Some(found);
            }
        }
        None
    }

    fn direct_lookup_in_ctx(&self, ctx: ObjectId, key: SlotKey) -> Option<ObjectId> {
        self.ctx_slots(ctx)
            .iter()
            .find(|slot| slot.key == Some(key))
            .map(|slot| slot.value)
    }

    fn local(&self, x: InstrId) -> (ObjectId, LocalId) {
        match self.obj(self.variables[&x]) {
            Object::ValDyn(ty, start) => (ty, start),
            _ => panic!(),
        }
    }

    fn body_keys(&self, body: Body, limit: Option<usize>) -> Vec<SlotKey> {
        self.range_keys(body.body.start, body.body.end, limit)
    }

    fn range_keys(&self, start: InstrId, end: InstrId, limit: Option<usize>) -> Vec<SlotKey> {
        let mut keys = Vec::new();
        let mut instr = start;
        while instr < end {
            match self.ir.instrs[instr] {
                Instr::Lambda => {
                    instr = self.find_end_lambda(instr);
                }
                Instr::NeedTydef { def, .. } => keys.push(SlotKey::Tydef(def)),
                Instr::NeedSigdef { def, .. } => keys.push(SlotKey::Sigdef(def)),
                Instr::NeedValdef { def, .. } => keys.push(SlotKey::Valdef(def)),
                Instr::NeedCtxdef { def, .. } => keys.push(SlotKey::Ctxdef(def)),
                _ => {}
            }
            if let Some(limit) = limit
                && keys.len() == limit
            {
                break;
            }
            instr += 1;
        }
        keys
    }

    fn ctx_from_keys(&mut self, keys: &[SlotKey], args: &[ObjectId]) -> ObjectId {
        assert_eq!(keys.len(), args.len());
        let slots = keys
            .iter()
            .copied()
            .zip(args.iter().copied())
            .map(|(key, value)| Slot {
                key: Some(key),
                value,
            })
            .collect();
        self.push_ctx(slots)
    }

    fn specialize_sig(&mut self, obj: ObjectId, ctx: ObjectId) -> ObjectId {
        match self.obj(obj) {
            Object::FnIntTemplate(input, output) => self.mkobj(Object::FnInt(input, output, ctx)),
            Object::FnInt(input, output, _) => self.mkobj(Object::FnInt(input, output, ctx)),
            Object::FnCharTemplate | Object::FnChar(_) => self.mkobj(Object::FnChar(ctx)),
            Object::FnStringTemplate | Object::FnString(_) => self.mkobj(Object::FnString(ctx)),
            Object::FnInstrTemplate(instruction) | Object::FnInstr(instruction, _) => {
                self.mkobj(Object::FnInstr(instruction, ctx))
            }
            Object::FnTemplate(def) | Object::FnDef(def, _) => {
                let func = Object::FnDef(def, ctx);
                self.insert_func(func);
                self.mkobj(func)
            }
            _ => obj,
        }
    }

    fn apply(&mut self, ambient: ObjectId, lambda: ObjectId, args: &[ObjectId]) -> ObjectId {
        match self.obj(lambda) {
            Object::Lambda(closure) => self.apply_closure(closure, ambient, args),
            Object::Alias(def) => {
                let keys = {
                    let lower::Aliasdef(body) = self.ir.aliasdefs[def];
                    self.body_keys(body, None)
                };
                let args = self.ctx_from_keys(&keys, args);
                let ctx = self.merge_ctxs(args, ambient);
                let lower::Aliasdef(body) = self.ir.aliasdefs[def];
                self.eval_static_body(ctx, body)
            }
            Object::Tag(def) => {
                let keys = {
                    let lower::Tagdef(body) = self.ir.tagdefs[def];
                    self.body_keys(body, None)
                };
                let args = self.ctx_from_keys(&keys, args);
                let ctx = self.merge_ctxs(args, ambient);
                let lower::Tagdef(body) = self.ir.tagdefs[def];
                self.eval_static_body(ctx, body)
            }
            Object::FnTemplate(def) => {
                let keys = {
                    let Sigdef(body) = self.ir.fndefs[def];
                    self.body_keys(body, Some(self.ir.fn_arity[def]))
                };
                let args = self.ctx_from_keys(&keys, args);
                let ctx = self.merge_ctxs(args, ambient);
                let func = Object::FnDef(def, ctx);
                self.insert_func(func);
                self.mkobj(func)
            }
            Object::FnInt(..)
            | Object::FnChar(_)
            | Object::FnString(_)
            | Object::FnInstr(..)
            | Object::FnWasi(_)
            | Object::FnPrint
            | Object::FnToStringThis(..)
            | Object::FnToStringArg(_)
            | Object::FnArgs
            | Object::FnRead
            | Object::FnArgsLen(_)
            | Object::FnArgsGet(_)
            | Object::FnDef(_, _) => lambda,
            Object::TyLitInt(_)
            | Object::TyLitChar
            | Object::TyLitString
            | Object::TyMemIdx
            | Object::TyI32
            | Object::TyI64
            | Object::TyTuple(_)
            | Object::TyCtx
            | Object::Sig(_, _) => lambda,
            Object::Open => {
                if !args.is_empty() {
                    eprintln!("apply default obj=Open args={args:?}");
                }
                panic!();
            }
            _ => {
                if !args.is_empty() {
                    eprintln!(
                        "apply default obj={:?} args={args:?}",
                        self.obj(lambda)
                    );
                }
                assert!(args.is_empty());
                lambda
            }
        }
    }

    fn apply_closure(&mut self, closure: ClosureId, ambient: ObjectId, args: &[ObjectId]) -> ObjectId {
        let Closure {
            env,
            start,
            end,
            result,
        } = self.closures[closure].clone();
        let mut env = env;
        let used = self.eval_static_range(&mut env, ambient, start + 1, end, Some(args));
        assert_eq!(used, args.len());
        env[&result]
    }

    fn eval_static_lambda_auto(
        &mut self,
        lambda: InstrId,
        ambient: ObjectId,
        env: &HashMap<InstrId, ObjectId>,
    ) -> ObjectId {
        let lambda = self.static_value(ambient, env, lambda);
        self.apply(ambient, lambda, &[])
    }

    fn eval_static_need(
        &mut self,
        ambient: ObjectId,
        env: &HashMap<InstrId, ObjectId>,
        key: SlotKey,
        param: InstrId,
    ) -> ObjectId {
        let available = self.available_ctx(ambient, env);
        let ctx = self.eval_static_lambda_auto(param, ambient, env);
        match key {
            SlotKey::Tydef(_) | SlotKey::Sigdef(_) | SlotKey::Valdef(_) => {
                let value = self.lookup_in_ctx(available, key).unwrap_or_else(|| {
                    eprintln!(
                        "missing key={key:?} ({}) in available=[{}] param_ctx=[{}]",
                        self.key_name(key),
                        self.ctx_name(available),
                        self.ctx_name(ctx)
                    );
                    panic!()
                });
                if let (SlotKey::Sigdef(def), Object::Open) = (key, self.obj(value))
                    && let Some(value) = self.specialize_open_sig(available, def, ctx)
                {
                    return self.specialize_sig(value, ctx);
                }
                self.specialize_sig(value, ctx)
            }
            SlotKey::Ctxdef(def) => match self.direct_lookup_in_ctx(ctx, key) {
                Some(value) => match self.obj(value) {
                    Object::Lambda(_) => self.apply(ctx, value, &[]),
                    _ => value,
                },
                None => {
                    let lower::Ctxdef(body) = self.ir.ctxdefs[def];
                    self.eval_static_body(ctx, body)
                }
            },
        }
    }

    fn eval_static_bind(&mut self, ambient: ObjectId, env: &HashMap<InstrId, ObjectId>, bind: InstrId) -> ObjectId {
        let binding = self.eval_static_lambda_auto(bind, ambient, env);
        let Object::Binding(_, value) = self.obj(binding) else {
            panic!()
        };
        value
    }

    fn eval_static_body(&mut self, ambient: ObjectId, body: Body) -> ObjectId {
        let mut env = HashMap::new();
        let used = self.eval_static_range(&mut env, ambient, body.body.start, body.body.end, None);
        assert_eq!(used, 0);
        env[&body.result()]
    }

    fn static_value(
        &mut self,
        ambient: ObjectId,
        env: &HashMap<InstrId, ObjectId>,
        instr: InstrId,
    ) -> ObjectId {
        if let Some(&value) = env.get(&instr) {
            return value;
        }
        match self.ir.instrs[instr] {
            Instr::Lambda => {
                let end = self.find_end_lambda(instr);
                let Instr::EndLambda { result, .. } = self.ir.instrs[end] else {
                    panic!()
                };
                let closure = self.push_closure(env.clone(), instr, end, result);
                closure
            }
            Instr::EndLambda { start, result } => self.push_closure(env.clone(), start, instr, result),
            Instr::Apply { lambda, args } => {
                let lambda = self.static_value(ambient, env, lambda);
                let args = args
                    .into_iter()
                    .map(|item| self.static_value(ambient, env, self.ir.items[item]))
                    .collect::<Vec<_>>();
                self.apply(ambient, lambda, &args)
            }
            Instr::Stack { items } => {
                let slots = items
                    .get(&self.ir.items)
                    .iter()
                    .map(|&item| Slot {
                        key: self.slot_key(item),
                        value: self.static_value(ambient, env, item),
                    })
                    .collect();
                self.push_ctx(slots)
            }
            Instr::NeedTydef { def, param } => self.eval_static_need(ambient, env, SlotKey::Tydef(def), param),
            Instr::NeedSigdef { def, param } => self.eval_static_need(ambient, env, SlotKey::Sigdef(def), param),
            Instr::NeedValdef { def, param } => self.eval_static_need(ambient, env, SlotKey::Valdef(def), param),
            Instr::NeedCtxdef { def, param } => self.eval_static_need(ambient, env, SlotKey::Ctxdef(def), param),
            Instr::Tagdef { def } => self.mkobj(Object::Tag(def)),
            Instr::Aliasdef { def } => self.mkobj(Object::Alias(def)),
            Instr::Tuple { elems } => {
                let elems = elems
                    .into_iter()
                    .map(|item| self.static_value(ambient, env, self.ir.items[item]))
                    .collect::<Vec<_>>();
                let tuple = self.tuples.make(&elems);
                self.mkobj(Object::TyTuple(tuple))
            }
            Instr::Record { fields } => {
                let fields = fields
                    .get(&self.ir.records)
                    .iter()
                    .map(|&(_, field)| self.static_value(ambient, env, field))
                    .collect::<Vec<_>>();
                let tuple = self.tuples.make(&fields);
                self.mkobj(Object::TyTuple(tuple))
            }
            Instr::Context => self.mkobj(Object::TyCtx),
            Instr::Fndef { def } => self.mkobj(Object::FnTemplate(def)),
            Instr::Get { ctx, slot } => {
                let ctx = self.static_value(ambient, env, ctx);
                self.ctx_slot(ctx, slot.index()).value
            }
            Instr::Lit { val } => self.mkobj(Object::ValLit(val)),
            Instr::Bind { args, bind } => {
                let slots = args
                    .get(&self.ir.items)
                    .iter()
                    .map(|&item| Slot {
                        key: self.slot_key(item),
                        value: self.static_value(ambient, env, item),
                    })
                    .collect();
                let args = self.push_ctx(slots);
                let bind = self.static_value(ambient, env, bind);
                self.mkobj(Object::Binding(args, bind))
            }
            Instr::BindTydef { bind, .. }
            | Instr::BindSigdef { bind, .. }
            | Instr::BindValdef { bind, .. } => self.eval_static_bind(ambient, env, bind),
            Instr::BindCtxdef { bind, .. } => {
                let value = self.eval_static_bind(ambient, env, bind);
                match self.obj(value) {
                    Object::Lambda(_) => self.apply(ambient, value, &[]),
                    _ => value,
                }
            }
            Instr::Sig { param, result } => {
                let param = self.static_value(ambient, env, param);
                let result = self.static_value(ambient, env, result);
                self.mkobj(Object::Sig(param, result))
            }
            Instr::Set { .. }
            | Instr::If { .. }
            | Instr::Else { .. }
            | Instr::EndIf { .. }
            | Instr::Loop
            | Instr::EndLoop
            | Instr::Br { .. }
            | Instr::Expr { .. } => panic!(),
        }
    }

    fn eval_static_range(
        &mut self,
        env: &mut HashMap<InstrId, ObjectId>,
        ambient: ObjectId,
        start: InstrId,
        end: InstrId,
        args: Option<&[ObjectId]>,
    ) -> usize {
        let mut next_arg = 0;
        let mut taking_args = args.is_some();
        let mut instr = start;
        while instr < end {
            let param = matches!(
                self.ir.instrs[instr],
                Instr::NeedTydef { .. }
                    | Instr::NeedSigdef { .. }
                    | Instr::NeedValdef { .. }
                    | Instr::NeedCtxdef { .. }
                    | Instr::BindTydef { .. }
                    | Instr::BindSigdef { .. }
                    | Instr::BindValdef { .. }
                    | Instr::BindCtxdef { .. }
            );
            if !param {
                taking_args = false;
            }
            let object = match self.ir.instrs[instr] {
                Instr::Lambda => {
                    let lambda_end = self.find_end_lambda(instr);
                    let Instr::EndLambda { result, .. } = self.ir.instrs[lambda_end] else {
                        panic!()
                    };
                    let closure = self.push_closure(env.clone(), instr, lambda_end, result);
                    env.insert(lambda_end, closure);
                    instr = lambda_end;
                    instr += 1;
                    continue;
                }
                Instr::EndLambda { .. } => {
                    instr += 1;
                    continue;
                }
                Instr::Apply { lambda, args } => {
                    let lambda = self.static_value(ambient, env, lambda);
                    let args = args
                        .into_iter()
                        .map(|item| self.static_value(ambient, env, self.ir.items[item]))
                        .collect::<Vec<_>>();
                    self.apply(ambient, lambda, &args)
                }
                Instr::Stack { items } => self.ctx_from_items(ambient, env, items),
                Instr::NeedTydef { def, param } => match (args, taking_args.then_some(())) {
                    (Some(args), Some(())) if next_arg < args.len() => {
                        let value = args[next_arg];
                        next_arg += 1;
                        value
                    }
                    _ => {
                        taking_args = false;
                        self.eval_static_need(ambient, env, SlotKey::Tydef(def), param)
                    }
                },
                Instr::NeedSigdef { def, param } => match (args, taking_args.then_some(())) {
                    (Some(args), Some(())) if next_arg < args.len() => {
                        let value = args[next_arg];
                        next_arg += 1;
                        value
                    }
                    _ => {
                        taking_args = false;
                        self.eval_static_need(ambient, env, SlotKey::Sigdef(def), param)
                    }
                },
                Instr::NeedValdef { def, param } => match (args, taking_args.then_some(())) {
                    (Some(args), Some(())) if next_arg < args.len() => {
                        let value = args[next_arg];
                        next_arg += 1;
                        value
                    }
                    _ => {
                        taking_args = false;
                        self.eval_static_need(ambient, env, SlotKey::Valdef(def), param)
                    }
                },
                Instr::NeedCtxdef { def, param } => match (args, taking_args.then_some(())) {
                    (Some(args), Some(())) if next_arg < args.len() => {
                        let value = args[next_arg];
                        next_arg += 1;
                        value
                    }
                    _ => {
                        taking_args = false;
                        self.eval_static_need(ambient, env, SlotKey::Ctxdef(def), param)
                    }
                },
                Instr::Tagdef { def } => self.mkobj(Object::Tag(def)),
                Instr::Aliasdef { def } => self.mkobj(Object::Alias(def)),
                Instr::Tuple { elems } => {
                    let tuple = self.tuples.make(
                        &elems
                            .into_iter()
                            .map(|item| env[&self.ir.items[item]])
                            .collect::<Vec<_>>(),
                    );
                    self.mkobj(Object::TyTuple(tuple))
                }
                Instr::Record { fields } => {
                    let tuple = self.tuples.make(
                        &fields
                            .get(&self.ir.records)
                            .iter()
                            .map(|&(_, field)| env[&field])
                            .collect::<Vec<_>>(),
                    );
                    self.mkobj(Object::TyTuple(tuple))
                }
                Instr::Context => self.mkobj(Object::TyCtx),
                Instr::Fndef { def } => self.mkobj(Object::FnTemplate(def)),
                Instr::Get { ctx, slot } => self.ctx_slot(env[&ctx], slot.index()).value,
                Instr::Lit { val } => self.mkobj(Object::ValLit(val)),
                Instr::Bind { args, bind } => {
                    let args = self.ctx_from_items(ambient, env, args);
                    self.mkobj(Object::Binding(args, env[&bind]))
                }
                Instr::BindTydef { bind, .. }
                | Instr::BindSigdef { bind, .. }
                | Instr::BindValdef { bind, .. }
                | Instr::BindCtxdef { bind, .. } => {
                    if taking_args
                        && let Some(args) = args
                        && next_arg < args.len()
                    {
                        let value = args[next_arg];
                        next_arg += 1;
                        value
                    } else {
                        taking_args = false;
                        self.eval_static_bind(ambient, env, bind)
                    }
                }
                Instr::Sig { param, result } => self.mkobj(Object::Sig(env[&param], env[&result])),
                Instr::Set { .. }
                | Instr::If { .. }
                | Instr::Else { .. }
                | Instr::EndIf { .. }
                | Instr::Loop
                | Instr::EndLoop
                | Instr::Br { .. }
                | Instr::Expr { .. } => panic!(),
            };
            env.insert(instr, object);
            instr += 1;
        }
        next_arg
    }

    fn wasi_ctx(&mut self, wasip1_sigdefs: &IndexMap<StrId, SigdefId>) -> ObjectId {
        let ty_i32 = self.mkobj(Object::TyI32);
        let ty_i64 = self.mkobj(Object::TyI64);
        let ty_memidx = self.mkobj(Object::TyMemIdx);
        let ty_bool = self.mkobj(Object::TyI32);
        let ty_lit_uint31 = self.mkobj(Object::TyLitInt(IntLitIn::Uint31));
        let ty_lit_uint32 = self.mkobj(Object::TyLitInt(IntLitIn::Uint32));
        let ty_lit_int32 = self.mkobj(Object::TyLitInt(IntLitIn::Int32));
        let ty_lit_uint63 = self.mkobj(Object::TyLitInt(IntLitIn::Uint63));
        let ty_lit_uint64 = self.mkobj(Object::TyLitInt(IntLitIn::Uint64));
        let ty_lit_int64 = self.mkobj(Object::TyLitInt(IntLitIn::Int64));
        let ty_lit_uint = self.mkobj(Object::TyLitInt(IntLitIn::Uint));
        let ty_lit_int = self.mkobj(Object::TyLitInt(IntLitIn::Int));
        let ty_lit_char = self.mkobj(Object::TyLitChar);
        let ty_lit_string = self.mkobj(Object::TyLitString);
        let ty_uint32 = ty_i32;
        let ty_int32 = ty_i32;
        let ty_uint64 = ty_i64;
        let ty_int64 = ty_i64;
        let ty_uint = ty_i32;
        let ty_int = ty_i32;
        let ty_char = ty_i32;
        let ty_string = {
            let tuple = self.tuples.make(&[ty_i32, ty_i32]);
            self.mkobj(Object::TyTuple(tuple))
        };
        let ty_args = {
            let tuple = self.tuples.make(&[ty_i32, ty_i32, ty_i32, ty_i32]);
            self.mkobj(Object::TyTuple(tuple))
        };

        let wasip1_base = self.push_ctx(vec![
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.types, "I32").tydef())),
                value: ty_i32,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.types, "I64").tydef())),
                value: ty_i64,
            },
        ]);

        let mut wasip1_slots = Vec::new();
        for (&name, &sigdef) in wasip1_sigdefs {
            let Sigdef(body) = self.ir.sigdefs[sigdef];
            let signature = self.eval_static_body(wasip1_base, body);
            let Object::Sig(param, result) = self.obj(signature) else {
                panic!()
            };
            let funcidx = self.push_func_wasi();
            match self.ir.strings[name].as_ref() {
                "fd_write" => self.fd_write = funcidx,
                "fd_read" => self.fd_read = funcidx,
                "fd_close" => self.fd_close = funcidx,
                "path_open" => self.path_open = funcidx,
                "args_get" => self.args_get = funcidx,
                "args_sizes_get" => self.args_sizes_get = funcidx,
                _ => {}
            }
            let params = self.layout_vec(param);
            let results = self.layout_vec(result);
            self.section_import.import(
                WASIP1,
                &self.ir.strings[name],
                wasm_encoder::EntityType::Function(self.section_type.len()),
            );
            self.section_type.ty().function(params, results);
            wasip1_slots.push(Slot {
                key: Some(SlotKey::Sigdef(sigdef)),
                value: self.mkobj(Object::FnWasi(funcidx)),
            });
        }
        let wasip1_ctx = self.push_ctx(wasip1_slots);

        let fn_uint31_realize_uint32 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint31, IntLitOut::Uint32));
        let fn_uint32_realize_uint32 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint32, IntLitOut::Uint32));
        let fn_uint31_realize_int32 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint31, IntLitOut::Int32));
        let fn_int32_realize_int32 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Int32, IntLitOut::Int32));
        let fn_uint31_realize_uint64 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint31, IntLitOut::Uint64));
        let fn_uint32_realize_uint64 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint32, IntLitOut::Uint64));
        let fn_uint63_realize_uint64 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint63, IntLitOut::Uint64));
        let fn_uint64_realize_uint64 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint64, IntLitOut::Uint64));
        let fn_uint31_realize_int64 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint31, IntLitOut::Int64));
        let fn_uint32_realize_int64 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint32, IntLitOut::Int64));
        let fn_int32_realize_int64 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Int32, IntLitOut::Int64));
        let fn_uint63_realize_int64 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint63, IntLitOut::Int64));
        let fn_int64_realize_int64 =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Int64, IntLitOut::Int64));
        let fn_uint31_realize_uint =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint31, IntLitOut::Uint));
        let fn_uint32_realize_uint =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint32, IntLitOut::Uint));
        let fn_uint63_realize_uint =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint63, IntLitOut::Uint));
        let fn_uint64_realize_uint =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint64, IntLitOut::Uint));
        let fn_uint_realize_uint =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint, IntLitOut::Uint));
        let fn_uint31_realize_int =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint31, IntLitOut::Int));
        let fn_uint32_realize_int =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint32, IntLitOut::Int));
        let fn_int32_realize_int =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Int32, IntLitOut::Int));
        let fn_uint63_realize_int =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint63, IntLitOut::Int));
        let fn_uint64_realize_int =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint64, IntLitOut::Int));
        let fn_int64_realize_int =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Int64, IntLitOut::Int));
        let fn_uint_realize_int =
            self.mkobj(Object::FnIntTemplate(IntLitIn::Uint, IntLitOut::Int));
        let fn_int_realize_int = self.mkobj(Object::FnIntTemplate(IntLitIn::Int, IntLitOut::Int));
        let fn_char = self.mkobj(Object::FnCharTemplate);
        let fn_string = self.mkobj(Object::FnStringTemplate);
        let val_false = self.mkobj(Object::ValU32(0));
        let val_true = self.mkobj(Object::ValU32(1));
        let open = self.mkobj(Object::Open);
        let fn_print = self.mkobj(Object::FnPrint);
        let fn_args = self.mkobj(Object::FnArgs);
        let fn_read = self.mkobj(Object::FnRead);
        let fn_uint32_to_string = self.mkobj(Object::FnToStringArg(IntFormat::U32));
        let fn_uint64_to_string = self.mkobj(Object::FnToStringArg(IntFormat::U64));
        let std_ctx = self.push_ctx(vec![
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.literal, "LiteralUint31").tydef())),
                value: ty_lit_uint31,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.literal, "LiteralUint32").tydef())),
                value: ty_lit_uint32,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.literal, "LiteralInt32").tydef())),
                value: ty_lit_int32,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.literal, "LiteralUint63").tydef())),
                value: ty_lit_uint63,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.literal, "LiteralUint64").tydef())),
                value: ty_lit_uint64,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.literal, "LiteralInt64").tydef())),
                value: ty_lit_int64,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.literal, "LiteralUint").tydef())),
                value: ty_lit_uint,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.literal, "LiteralInt").tydef())),
                value: ty_lit_int,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.literal, "LiteralChar").tydef())),
                value: ty_lit_char,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.literal, "LiteralString").tydef())),
                value: ty_lit_string,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.literal, "literal_uint31").valdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.literal, "literal_uint32").valdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.literal, "literal_int32").valdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.literal, "literal_uint63").valdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.literal, "literal_uint64").valdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.literal, "literal_int64").valdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.literal, "literal_uint").valdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.literal, "literal_int").valdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.literal, "literal_char").valdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.literal, "literal_string").valdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.int, "Uint32").tydef())),
                value: ty_uint32,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.int, "Int32").tydef())),
                value: ty_int32,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.int, "Uint64").tydef())),
                value: ty_uint64,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.int, "Int64").tydef())),
                value: ty_int64,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.int, "Uint").tydef())),
                value: ty_uint,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.int, "Int").tydef())),
                value: ty_int,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.char, "Char").tydef())),
                value: ty_char,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.string, "String").tydef())),
                value: ty_string,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.prelude, "Args").tydef())),
                value: ty_args,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint31_realize_uint32").sigdef())),
                value: fn_uint31_realize_uint32,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint32_realize_uint32").sigdef())),
                value: fn_uint32_realize_uint32,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint31_realize_int32").sigdef())),
                value: fn_uint31_realize_int32,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "int32_realize_int32").sigdef())),
                value: fn_int32_realize_int32,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint31_realize_uint64").sigdef())),
                value: fn_uint31_realize_uint64,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint32_realize_uint64").sigdef())),
                value: fn_uint32_realize_uint64,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint63_realize_uint64").sigdef())),
                value: fn_uint63_realize_uint64,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint64_realize_uint64").sigdef())),
                value: fn_uint64_realize_uint64,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint31_realize_int64").sigdef())),
                value: fn_uint31_realize_int64,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint32_realize_int64").sigdef())),
                value: fn_uint32_realize_int64,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "int32_realize_int64").sigdef())),
                value: fn_int32_realize_int64,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint63_realize_int64").sigdef())),
                value: fn_uint63_realize_int64,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "int64_realize_int64").sigdef())),
                value: fn_int64_realize_int64,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint31_realize_uint").sigdef())),
                value: fn_uint31_realize_uint,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint32_realize_uint").sigdef())),
                value: fn_uint32_realize_uint,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint63_realize_uint").sigdef())),
                value: fn_uint63_realize_uint,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint64_realize_uint").sigdef())),
                value: fn_uint64_realize_uint,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint_realize_uint").sigdef())),
                value: fn_uint_realize_uint,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint31_realize_int").sigdef())),
                value: fn_uint31_realize_int,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint32_realize_int").sigdef())),
                value: fn_uint32_realize_int,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "int32_realize_int").sigdef())),
                value: fn_int32_realize_int,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint63_realize_int").sigdef())),
                value: fn_uint63_realize_int,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint64_realize_int").sigdef())),
                value: fn_uint64_realize_int,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "int64_realize_int").sigdef())),
                value: fn_int64_realize_int,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "uint_realize_int").sigdef())),
                value: fn_uint_realize_int,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "int_realize_int").sigdef())),
                value: fn_int_realize_int,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "char_realize").sigdef())),
                value: fn_char,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.literal, "string_realize").sigdef())),
                value: fn_string,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named_detached(self.lib.string, "to_string").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.prelude, "Bool").tydef())),
                value: ty_bool,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.prelude, "false").valdef())),
                value: val_false,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.named(self.lib.prelude, "true").valdef())),
                value: val_true,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.prelude, "print").sigdef())),
                value: fn_print,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.prelude, "args").sigdef())),
                value: fn_args,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.prelude, "read").sigdef())),
                value: fn_read,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.prelude, "uint32_to_string").sigdef())),
                value: fn_uint32_to_string,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.prelude, "uint64_to_string").sigdef())),
                value: fn_uint64_to_string,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named_detached(self.lib.prelude, "len").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named_detached(self.lib.prelude, "get").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "eq").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "ne").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "lt").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "gt").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "le").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "ge").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "neg").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "not").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "add").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "sub").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "mul").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "div").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "rem").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "shl").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "shr").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "and").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "or").sigdef())),
                value: open,
            },
            Slot {
                key: Some(SlotKey::Sigdef(self.named(self.lib.ops, "xor").sigdef())),
                value: open,
            },
        ]);

        let default_memidx = self.mkobj(Object::ValU32(MEMIDX_WASI));
        self.push_ctx(vec![
            Slot {
                key: Some(SlotKey::Ctxdef(self.named(self.lib.wasip1, "WasiP1").ctxdef())),
                value: wasip1_ctx,
            },
            Slot {
                key: Some(SlotKey::Ctxdef(self.named(self.lib.prelude, "Std").ctxdef())),
                value: std_ctx,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.types, "I32").tydef())),
                value: ty_i32,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.types, "I64").tydef())),
                value: ty_i64,
            },
            Slot {
                key: Some(SlotKey::Tydef(self.named(self.lib.types, "MemIdx").tydef())),
                value: ty_memidx,
            },
            Slot {
                key: Some(SlotKey::Valdef(self.val_memidx)),
                value: default_memidx,
            },
        ])
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
            | Object::FnIntTemplate(_, _)
            | Object::FnCharTemplate
            | Object::FnStringTemplate
            | Object::FnInt(..)
            | Object::FnChar(_)
            | Object::FnString(_)
            | Object::FnInstrTemplate(_)
            | Object::FnInstr(..)
            | Object::FnWasi(_)
            | Object::FnPrint
            | Object::FnToStringThis(..)
            | Object::FnToStringArg(_)
            | Object::FnArgs
            | Object::FnRead
            | Object::FnArgsLen(_)
            | Object::FnArgsGet(_)
            | Object::Alias(_)
            | Object::Tag(_)
            | Object::FnTemplate(_)
            | Object::Lambda(_)
            | Object::Binding(_, _)
            | Object::FnDef(_, _)
            | Object::ValU32(_)
            | Object::ValLit(_)
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
        let value = self.variables[&instr];
        match self.obj(value) {
            Object::ValStmt => {}
            _ => self.emit_value_object(value),
        }
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

    fn print_scratch(&mut self) -> i32 {
        *self.print_scratch.get_or_insert_with(|| {
            let offset = (self.data_offset + 3) & !3;
            self.data_offset = offset + 12;
            offset
        })
    }

    fn literal_valdef(&self, input: IntLitIn) -> ValdefId {
        match input {
            IntLitIn::Uint31 => self.named(self.lib.literal, "literal_uint31").valdef(),
            IntLitIn::Uint32 => self.named(self.lib.literal, "literal_uint32").valdef(),
            IntLitIn::Int32 => self.named(self.lib.literal, "literal_int32").valdef(),
            IntLitIn::Uint63 => self.named(self.lib.literal, "literal_uint63").valdef(),
            IntLitIn::Uint64 => self.named(self.lib.literal, "literal_uint64").valdef(),
            IntLitIn::Int64 => self.named(self.lib.literal, "literal_int64").valdef(),
            IntLitIn::Uint => self.named(self.lib.literal, "literal_uint").valdef(),
            IntLitIn::Int => self.named(self.lib.literal, "literal_int").valdef(),
        }
    }

    fn literal_value(&mut self, ctx: ObjectId, valdef: ValdefId) -> Val {
        let value = self.lookup_in_ctx(ctx, SlotKey::Valdef(valdef)).unwrap();
        let Object::ValLit(val) = self.obj(value) else {
            panic!()
        };
        val
    }

    fn val_u32(&mut self, ctx: ObjectId, valdef: ValdefId) -> u32 {
        let value = self.lookup_in_ctx(ctx, SlotKey::Valdef(valdef)).unwrap();
        match self.obj(value) {
            Object::ValU32(value) => value,
            Object::ValLit(Val::Uint31(value)) | Object::ValLit(Val::Uint32(value)) => value,
            Object::ValLit(Val::Int32(value)) => value as u32,
            _ => panic!(),
        }
    }

    fn print(&mut self, arg: InstrId) {
        let ty_i32 = self.mkobj(Object::TyI32);
        let ptr = self.make_locals(ty_i32);
        let len = self.make_locals(ty_i32);
        let scratch = self.print_scratch();
        let memarg = MemArg {
            offset: 0,
            align: 2,
            memory_index: MEMIDX_WASI,
        };

        self.get(arg);
        self.body.insn().local_set(len.raw()).local_set(ptr.raw());

        self.body
            .insn()
            .i32_const(scratch)
            .local_get(ptr.raw())
            .i32_store(memarg);
        self.body
            .insn()
            .i32_const(scratch + 4)
            .local_get(len.raw())
            .i32_store(memarg);
        self.body
            .insn()
            .i32_const(1)
            .i32_const(scratch)
            .i32_const(1)
            .i32_const(scratch + 8)
            .call(self.fd_write)
            .drop();
    }

    fn memarg(&self, align: u32) -> MemArg {
        MemArg {
            offset: 0,
            align,
            memory_index: MEMIDX_WASI,
        }
    }

    fn bound_value(&mut self, ctx: ObjectId, def: ValdefId) -> ObjectId {
        self.lookup_in_ctx(ctx, SlotKey::Valdef(def)).unwrap()
    }

    fn emit_value_object(&mut self, value: ObjectId) {
        match self.obj(value) {
            Object::ValDyn(ty, start) => self.get_locals(ty, start),
            Object::ValU32(value) => {
                self.body.insn().i32_const(value as i32);
            }
            Object::ValLit(Val::Uint31(value)) | Object::ValLit(Val::Uint32(value)) => {
                self.body.insn().i32_const(value as i32);
            }
            Object::ValLit(Val::Int32(value)) => {
                self.body.insn().i32_const(value);
            }
            Object::ValLit(Val::Uint63(value)) | Object::ValLit(Val::Uint64(value)) => {
                self.body.insn().i64_const(value as i64);
            }
            Object::ValLit(Val::Int64(value)) => {
                self.body.insn().i64_const(value);
            }
            Object::ValLit(Val::Char(value)) => {
                self.body.insn().i32_const(value as i32);
            }
            Object::ValLit(Val::String(value)) => {
                self.string(value);
            }
            _ => panic!(),
        }
    }

    fn alloc_const(&mut self, size: i32) -> LocalId {
        let ty_i32 = self.mkobj(Object::TyI32);
        let ptr = self.make_locals(ty_i32);
        self.body
            .insn()
            .global_get(GLOBAL_HEAP)
            .i32_const(3)
            .i32_add()
            .i32_const(!3)
            .i32_and()
            .local_tee(ptr.raw())
            .i32_const(size)
            .i32_add()
            .global_set(GLOBAL_HEAP);
        ptr
    }

    fn alloc_local(&mut self, size: LocalId) -> LocalId {
        let ty_i32 = self.mkobj(Object::TyI32);
        let ptr = self.make_locals(ty_i32);
        self.body
            .insn()
            .global_get(GLOBAL_HEAP)
            .i32_const(3)
            .i32_add()
            .i32_const(!3)
            .i32_and()
            .local_tee(ptr.raw())
            .local_get(size.raw())
            .i32_add()
            .global_set(GLOBAL_HEAP);
        ptr
    }

    fn trap_if_nonzero(&mut self) {
        self.body.insn().if_(BlockType::Empty).unreachable().end();
    }

    fn emit_i32_to_string(&mut self, signed: bool) {
        let ty_i32 = self.mkobj(Object::TyI32);
        let value = self.make_locals(ty_i32);
        let digits = self.make_locals(ty_i32);
        let index = self.make_locals(ty_i32);
        let negative = signed.then(|| self.make_locals(ty_i32));
        let max = if signed { 11 } else { 10 };
        let start = self.alloc_const(max);
        let memarg = self.memarg(0);

        self.body.insn().local_set(value.raw());
        if let Some(negative) = negative {
            self.body
                .insn()
                .local_get(value.raw())
                .i32_const(0)
                .i32_lt_s()
                .local_set(negative.raw());
            self.body.insn().local_get(negative.raw()).if_(BlockType::Empty);
            self.body
                .insn()
                .i32_const(0)
                .local_get(value.raw())
                .i32_sub()
                .local_set(digits.raw());
            self.body.insn().else_();
            self.body
                .insn()
                .local_get(value.raw())
                .local_set(digits.raw());
            self.body.insn().end();
        } else {
            self.body
                .insn()
                .local_get(value.raw())
                .local_set(digits.raw());
        }

        self.body
            .insn()
            .i32_const(max - 1)
            .local_set(index.raw())
            .loop_(BlockType::Empty)
            .local_get(start.raw())
            .local_get(index.raw())
            .i32_add()
            .local_get(digits.raw())
            .i32_const(10)
            .i32_rem_u()
            .i32_const(48)
            .i32_add()
            .i32_store8(memarg)
            .local_get(digits.raw())
            .i32_const(10)
            .i32_div_u()
            .local_tee(digits.raw())
            .i32_eqz()
            .if_(BlockType::Empty)
            .else_()
            .local_get(index.raw())
            .i32_const(1)
            .i32_sub()
            .local_set(index.raw())
            .br(1)
            .end()
            .end();

        if let Some(negative) = negative {
            self.body.insn().local_get(negative.raw()).if_(BlockType::Empty);
            self.body
                .insn()
                .local_get(index.raw())
                .i32_const(1)
                .i32_sub()
                .local_tee(index.raw())
                .local_set(index.raw())
                .local_get(start.raw())
                .local_get(index.raw())
                .i32_add()
                .i32_const('-' as i32)
                .i32_store8(memarg);
            self.body.insn().end();
        }

        self.body
            .insn()
            .local_get(start.raw())
            .local_get(index.raw())
            .i32_add()
            .i32_const(max)
            .local_get(index.raw())
            .i32_sub();
    }

    fn emit_i64_to_string(&mut self, signed: bool) {
        let ty_i32 = self.mkobj(Object::TyI32);
        let ty_i64 = self.mkobj(Object::TyI64);
        let value = self.make_locals(ty_i64);
        let digits = self.make_locals(ty_i64);
        let index = self.make_locals(ty_i32);
        let negative = signed.then(|| self.make_locals(ty_i32));
        let max = if signed { 20 } else { 20 };
        let start = self.alloc_const(max);
        let memarg = self.memarg(0);

        self.body.insn().local_set(value.raw());
        if let Some(negative) = negative {
            self.body
                .insn()
                .local_get(value.raw())
                .i64_const(0)
                .i64_lt_s()
                .local_set(negative.raw());
            self.body.insn().local_get(negative.raw()).if_(BlockType::Empty);
            self.body
                .insn()
                .i64_const(0)
                .local_get(value.raw())
                .i64_sub()
                .local_set(digits.raw());
            self.body.insn().else_();
            self.body
                .insn()
                .local_get(value.raw())
                .local_set(digits.raw());
            self.body.insn().end();
        } else {
            self.body
                .insn()
                .local_get(value.raw())
                .local_set(digits.raw());
        }

        self.body
            .insn()
            .i32_const(max - 1)
            .local_set(index.raw())
            .loop_(BlockType::Empty)
            .local_get(start.raw())
            .local_get(index.raw())
            .i32_add()
            .local_get(digits.raw())
            .i64_const(10)
            .i64_rem_u()
            .i32_wrap_i64()
            .i32_const(48)
            .i32_add()
            .i32_store8(memarg)
            .local_get(digits.raw())
            .i64_const(10)
            .i64_div_u()
            .local_tee(digits.raw())
            .i64_eqz()
            .if_(BlockType::Empty)
            .else_()
            .local_get(index.raw())
            .i32_const(1)
            .i32_sub()
            .local_set(index.raw())
            .br(1)
            .end()
            .end();

        if let Some(negative) = negative {
            self.body.insn().local_get(negative.raw()).if_(BlockType::Empty);
            self.body
                .insn()
                .local_get(index.raw())
                .i32_const(1)
                .i32_sub()
                .local_tee(index.raw())
                .local_set(index.raw())
                .local_get(start.raw())
                .local_get(index.raw())
                .i32_add()
                .i32_const('-' as i32)
                .i32_store8(memarg);
            self.body.insn().end();
        }

        self.body
            .insn()
            .local_get(start.raw())
            .local_get(index.raw())
            .i32_add()
            .i32_const(max)
            .local_get(index.raw())
            .i32_sub();
    }

    fn emit_to_string(&mut self, format: IntFormat) {
        match format {
            IntFormat::U32 => self.emit_i32_to_string(false),
            IntFormat::I32 => self.emit_i32_to_string(true),
            IntFormat::U64 => self.emit_i64_to_string(false),
            IntFormat::I64 => self.emit_i64_to_string(true),
        }
    }

    fn emit_args(&mut self) {
        let ty_i32 = self.mkobj(Object::TyI32);
        let memarg = self.memarg(2);
        let scratch = self.alloc_const(8);
        let argc = self.make_locals(ty_i32);
        let size = self.make_locals(ty_i32);
        let size_argv = self.make_locals(ty_i32);
        let total = self.make_locals(ty_i32);
        let argv = self.make_locals(ty_i32);
        let buffer = self.make_locals(ty_i32);

        self.body
            .insn()
            .local_get(scratch.raw())
            .local_get(scratch.raw())
            .i32_const(4)
            .i32_add()
            .call(self.args_sizes_get);
        self.trap_if_nonzero();

        self.body
            .insn()
            .local_get(scratch.raw())
            .i32_load(memarg)
            .local_set(argc.raw())
            .local_get(scratch.raw())
            .i32_const(4)
            .i32_add()
            .i32_load(memarg)
            .local_set(size.raw())
            .local_get(argc.raw())
            .i32_const(4)
            .i32_mul()
            .local_set(size_argv.raw())
            .local_get(size_argv.raw())
            .local_get(size.raw())
            .i32_add()
            .local_set(total.raw());

        let allocated = self.alloc_local(total);
        self.body
            .insn()
            .local_get(allocated.raw())
            .local_set(argv.raw());

        self.body
            .insn()
            .local_get(argv.raw())
            .local_get(size_argv.raw())
            .i32_add()
            .local_set(buffer.raw())
            .local_get(argv.raw())
            .local_get(buffer.raw())
            .call(self.args_get);
        self.trap_if_nonzero();

        self.body
            .insn()
            .local_get(argc.raw())
            .local_get(size.raw())
            .local_get(argv.raw())
            .local_get(buffer.raw());
    }

    fn emit_args_len(&mut self, ctx: ObjectId) {
        let this = self.bound_value(ctx, self.named(self.lib.prelude, "this").valdef());
        let Object::ValDyn(_, start) = self.obj(this) else {
            panic!()
        };
        self.body.insn().local_get(start.raw());
    }

    fn emit_args_get(&mut self, ctx: ObjectId, arg: InstrId) {
        let ty_i32 = self.mkobj(Object::TyI32);
        let memarg = self.memarg(2);
        let this = self.bound_value(ctx, self.named(self.lib.prelude, "this").valdef());
        let Object::ValDyn(_, start) = self.obj(this) else {
            panic!()
        };
        let argc = start;
        let size = LocalId::from_raw(start.raw() + 1);
        let argv = LocalId::from_raw(start.raw() + 2);
        let buffer = LocalId::from_raw(start.raw() + 3);
        let index = self.make_locals(ty_i32);
        let next = self.make_locals(ty_i32);
        let pointer = self.make_locals(ty_i32);
        let end = self.make_locals(ty_i32);

        self.get(arg);
        self.body.insn().local_set(index.raw());
        self.body
            .insn()
            .local_get(index.raw())
            .local_get(argc.raw())
            .i32_ge_u()
            .if_(BlockType::Empty)
            .unreachable()
            .end()
            .local_get(argv.raw())
            .local_get(index.raw())
            .i32_const(4)
            .i32_mul()
            .i32_add()
            .i32_load(memarg)
            .local_set(pointer.raw())
            .local_get(index.raw())
            .i32_const(1)
            .i32_add()
            .local_tee(next.raw())
            .local_get(argc.raw())
            .i32_lt_u()
            .if_(BlockType::Empty)
            .local_get(argv.raw())
            .local_get(next.raw())
            .i32_const(4)
            .i32_mul()
            .i32_add()
            .i32_load(memarg)
            .local_set(end.raw())
            .else_()
            .local_get(buffer.raw())
            .local_get(size.raw())
            .i32_add()
            .local_set(end.raw())
            .end()
            .local_get(pointer.raw())
            .local_get(end.raw())
            .i32_const(1)
            .i32_sub()
            .local_get(pointer.raw())
            .i32_sub();
    }

    fn emit_read(&mut self, arg: InstrId) {
        const READ_CHUNK: i32 = 65536;
        const READ_CAPACITY: i32 = 1024 * 1024;

        let ty_i32 = self.mkobj(Object::TyI32);
        let memarg = self.memarg(2);
        let path_ptr = self.make_locals(ty_i32);
        let path_len = self.make_locals(ty_i32);
        let fd = self.make_locals(ty_i32);
        let length = self.make_locals(ty_i32);
        let size = self.make_locals(ty_i32);
        let retptr = self.alloc_const(4);
        let iovs = self.alloc_const(12);
        let buffer = self.alloc_const(READ_CAPACITY);

        self.get(arg);
        self.body
            .insn()
            .local_set(path_len.raw())
            .local_set(path_ptr.raw())
            .i32_const(3)
            .i32_const(0)
            .local_get(path_ptr.raw())
            .local_get(path_len.raw())
            .i32_const(0)
            .i64_const(0)
            .i64_const(0)
            .i32_const(0)
            .local_get(retptr.raw())
            .call(self.path_open);
        self.trap_if_nonzero();

        self.body
            .insn()
            .local_get(retptr.raw())
            .i32_load(memarg)
            .local_set(fd.raw())
            .i32_const(0)
            .local_set(length.raw())
            .loop_(BlockType::Empty)
            .local_get(iovs.raw())
            .local_get(buffer.raw())
            .local_get(length.raw())
            .i32_add()
            .i32_store(memarg)
            .local_get(iovs.raw())
            .i32_const(4)
            .i32_add()
            .i32_const(READ_CHUNK)
            .i32_store(memarg)
            .local_get(fd.raw())
            .local_get(iovs.raw())
            .i32_const(1)
            .local_get(iovs.raw())
            .i32_const(8)
            .i32_add()
            .call(self.fd_read);
        self.trap_if_nonzero();

        self.body
            .insn()
            .local_get(iovs.raw())
            .i32_const(8)
            .i32_add()
            .i32_load(memarg)
            .local_tee(size.raw())
            .drop()
            .local_get(length.raw())
            .local_get(size.raw())
            .i32_add()
            .local_set(length.raw())
            .local_get(size.raw())
            .i32_const(READ_CHUNK)
            .i32_eq()
            .if_(BlockType::Empty)
            .br(1)
            .end()
            .end()
            .local_get(fd.raw())
            .call(self.fd_close);
        self.trap_if_nonzero();

        self.body
            .insn()
            .local_get(buffer.raw())
            .local_get(length.raw());
    }

    fn int_const(&mut self, input: IntLitIn, output: IntLitOut, ctx: ObjectId) {
        match (self.literal_value(ctx, self.literal_valdef(input)), output) {
            (Val::Uint31(value), IntLitOut::Uint32 | IntLitOut::Int32)
            | (Val::Uint32(value), IntLitOut::Uint32 | IntLitOut::Int32) => {
                self.body.insn().i32_const(value as i32);
            }
            (Val::Int32(value), IntLitOut::Uint32 | IntLitOut::Int32) => {
                self.body.insn().i32_const(value);
            }
            (Val::Uint31(value), IntLitOut::Uint64 | IntLitOut::Int64)
            | (Val::Uint32(value), IntLitOut::Uint64 | IntLitOut::Int64) => {
                self.body.insn().i64_const(value as i64);
            }
            (Val::Uint63(value), IntLitOut::Uint64 | IntLitOut::Int64)
            | (Val::Uint64(value), IntLitOut::Uint64 | IntLitOut::Int64) => {
                self.body.insn().i64_const(value as i64);
            }
            (Val::Int32(value), IntLitOut::Uint64 | IntLitOut::Int64) => {
                self.body.insn().i64_const(value as i64);
            }
            (Val::Int64(value), IntLitOut::Uint64 | IntLitOut::Int64) => {
                self.body.insn().i64_const(value);
            }
            (Val::Uint31(value), IntLitOut::Uint | IntLitOut::Int)
            | (Val::Uint32(value), IntLitOut::Uint | IntLitOut::Int) => {
                self.body.insn().i32_const(value as i32);
            }
            (Val::Uint63(value), IntLitOut::Uint | IntLitOut::Int)
            | (Val::Uint64(value), IntLitOut::Uint | IntLitOut::Int) => {
                self.body.insn().i32_const(value as i32);
            }
            (Val::Int32(value), IntLitOut::Uint | IntLitOut::Int) => {
                self.body.insn().i32_const(value);
            }
            (Val::Int64(value), IntLitOut::Uint | IntLitOut::Int) => {
                self.body.insn().i32_const(value as i32);
            }
            (Val::Uint(value), IntLitOut::Uint | IntLitOut::Int) => {
                self.body.insn().i32_const(self.ir.strings[value].parse::<u32>().unwrap() as i32);
            }
            (Val::Int(value), IntLitOut::Uint | IntLitOut::Int) => {
                self.body.insn().i32_const(self.ir.strings[value].parse::<i32>().unwrap());
            }
            _ => panic!(),
        }
    }

    fn memarg_ctx(&mut self, ctx: ObjectId) -> MemArg {
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
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i32_load(memarg);
            }
            Instruction::I64Load => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i64_load(memarg);
            }
            Instruction::I32Load8S => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i32_load8_s(memarg);
            }
            Instruction::I32Load8U => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i32_load8_u(memarg);
            }
            Instruction::I32Load16S => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i32_load16_s(memarg);
            }
            Instruction::I32Load16U => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i32_load16_u(memarg);
            }
            Instruction::I64Load8S => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i64_load8_s(memarg);
            }
            Instruction::I64Load8U => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i64_load8_u(memarg);
            }
            Instruction::I64Load16S => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i64_load16_s(memarg);
            }
            Instruction::I64Load16U => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i64_load16_u(memarg);
            }
            Instruction::I64Load32S => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i64_load32_s(memarg);
            }
            Instruction::I64Load32U => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i64_load32_u(memarg);
            }
            Instruction::I32Store => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i32_store(memarg);
            }
            Instruction::I64Store => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i64_store(memarg);
            }
            Instruction::I32Store8 => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i32_store8(memarg);
            }
            Instruction::I32Store16 => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i32_store16(memarg);
            }
            Instruction::I64Store8 => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i64_store8(memarg);
            }
            Instruction::I64Store16 => {
                let memarg = self.memarg_ctx(ctx);
                self.body.insn().i64_store16(memarg);
            }
            Instruction::I64Store32 => {
                let memarg = self.memarg_ctx(ctx);
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

    fn expr(&mut self, expr: Expr) {
        match expr {
            Expr::Param { ty } => {
                let ty = self.variables[&ty];
                self.get_locals(ty, LocalId::from_raw(0));
            }
            Expr::Copy { value } => {
                self.get(value);
            }
            Expr::Nominal { ty, inner } => todo!(),
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
            Expr::Val { val } => match self.obj(self.variables[&val]) {
                Object::ValDyn(ty, start) => self.get_locals(ty, start),
                Object::ValU32(value) => {
                    self.body.insn().i32_const(value as i32);
                }
                Object::ValLit(Val::Char(value)) => {
                    self.body.insn().i32_const(value as i32);
                }
                Object::ValLit(Val::String(value)) => {
                    self.string(value);
                }
                _ => panic!(),
            },
            Expr::Call { func, arg } => match self.obj(self.variables[&func]) {
                Object::FnWasi(funcidx) => {
                    self.get(arg);
                    self.body.insn().call(funcidx);
                }
                Object::FnDef(fndef, ctx) => {
                    let funcidx = self
                        .get_funcidx(fndef, ctx)
                        .unwrap_or_else(|| self.insert_func(Object::FnDef(fndef, ctx)));
                    self.get(arg);
                    self.body.insn().call(funcidx);
                }
                Object::FnInstr(instruction, ctx) => {
                    self.get(arg);
                    self.wasm_instruction(ctx, instruction);
                }
                Object::FnInt(input, output, ctx) => {
                    self.get(arg);
                    self.int_const(input, output, ctx);
                }
                Object::FnChar(ctx) => {
                    self.get(arg);
                    let Val::Char(value) =
                        self.literal_value(ctx, self.named(self.lib.literal, "literal_char").valdef())
                    else {
                        panic!()
                    };
                    self.body.insn().i32_const(value as i32);
                }
                Object::FnString(ctx) => {
                    self.get(arg);
                    let Val::String(value) =
                        self.literal_value(ctx, self.named(self.lib.literal, "literal_string").valdef())
                    else {
                        panic!()
                    };
                    self.string(value);
                }
                Object::FnPrint => {
                    self.print(arg);
                }
                Object::FnToStringThis(format, ctx) => {
                    let value = self.bound_value(ctx, self.named(self.lib.prelude, "this").valdef());
                    self.emit_value_object(value);
                    self.emit_to_string(format);
                }
                Object::FnToStringArg(format) => {
                    self.get(arg);
                    self.emit_to_string(format);
                }
                Object::FnArgs => {
                    self.emit_args();
                }
                Object::FnRead => {
                    self.emit_read(arg);
                }
                Object::FnArgsLen(ctx) => {
                    self.emit_args_len(ctx);
                }
                Object::FnArgsGet(ctx) => {
                    self.emit_args_get(ctx, arg);
                }
                _ => panic!(),
            },
        }
    }

    fn interp(&mut self, ctx: ObjectId, inputs: &[ObjectId], body: Body) -> ObjectId {
        if std::env::var_os("MOSS_TRACE_WASM").is_some() {
            eprintln!("wasm:interp start={:?} end={:?}", body.body.start, body.body.end);
        }
        let mut instr = body.body.start;
        while instr < body.body.end {
            let result = match self.ir.instrs[instr] {
                Instr::Lambda => {
                    let lambda_end = self.find_end_lambda(instr);
                    let Instr::EndLambda { result, .. } = self.ir.instrs[lambda_end] else {
                        panic!()
                    };
                    let closure = self.push_closure(self.variables.clone(), instr, lambda_end, result);
                    self.variables.insert(lambda_end, closure);
                    instr = lambda_end + 1;
                    continue;
                }
                Instr::EndLambda { .. } => {
                    instr += 1;
                    continue;
                }
                Instr::Apply { lambda, args } => {
                    let vars = self.variables.clone();
                    let lambda = vars
                        .get(&lambda)
                        .copied()
                        .unwrap_or_else(|| self.static_value(ctx, &vars, lambda));
                    let args = args
                        .into_iter()
                        .map(|item| {
                            let item = self.ir.items[item];
                            vars.get(&item)
                                .copied()
                                .unwrap_or_else(|| self.static_value(ctx, &vars, item))
                        })
                        .collect::<Vec<_>>();
                    self.apply(ctx, lambda, &args)
                }
                Instr::Stack { items } => {
                    let vars = self.variables.clone();
                    self.ctx_from_items(ctx, &vars, items)
                }
                Instr::NeedTydef { def, param } => {
                    let vars = self.variables.clone();
                    self.eval_static_need(ctx, &vars, SlotKey::Tydef(def), param)
                }
                Instr::NeedSigdef { def, param } => {
                    let vars = self.variables.clone();
                    self.eval_static_need(ctx, &vars, SlotKey::Sigdef(def), param)
                }
                Instr::NeedValdef { def, param } => {
                    let vars = self.variables.clone();
                    self.eval_static_need(ctx, &vars, SlotKey::Valdef(def), param)
                }
                Instr::NeedCtxdef { def, param } => {
                    let vars = self.variables.clone();
                    self.eval_static_need(ctx, &vars, SlotKey::Ctxdef(def), param)
                }
                Instr::Tagdef { def } => self.mkobj(Object::Tag(def)),
                Instr::Aliasdef { def } => self.mkobj(Object::Alias(def)),
                Instr::Tuple { elems } => {
                    let tuple = self.tuples.make(&self.list(elems));
                    self.mkobj(Object::TyTuple(tuple))
                }
                Instr::Record { fields } => {
                    let objs = Vec::from_iter(
                        fields
                            .get(&self.ir.records)
                            .iter()
                            .map(|&(_, field)| self.variables[&field]),
                    );
                    let tuple = self.tuples.make(&objs);
                    self.mkobj(Object::TyTuple(tuple))
                }
                Instr::Context => self.mkobj(Object::TyCtx),
                Instr::Fndef { def } => self.mkobj(Object::FnTemplate(def)),
                Instr::Get { ctx, slot } => self.ctx_slot(self.variables[&ctx], slot.index()).value,
                Instr::Lit { val } => self.mkobj(Object::ValLit(val)),
                Instr::Bind { args, bind } => {
                    let vars = self.variables.clone();
                    let args = self.ctx_from_items(ctx, &vars, args);
                    self.mkobj(Object::Binding(args, self.variables[&bind]))
                }
                Instr::BindTydef { bind, .. }
                | Instr::BindSigdef { bind, .. }
                | Instr::BindValdef { bind, .. }
                | Instr::BindCtxdef { bind, .. } => {
                    let vars = self.variables.clone();
                    self.eval_static_bind(ctx, &vars, bind)
                }
                Instr::Sig { param, result } => {
                    let obj_param = self.variables[&param];
                    let obj_result = self.variables[&result];
                    self.mkobj(Object::Sig(obj_param, obj_result))
                }
                Instr::Set { lhs, rhs } => {
                    let (ty, start) = self.local(lhs);
                    self.get(rhs);
                    self.set_locals(ty, start);
                    self.mkobj(Object::ValStmt)
                }
                Instr::If { ty, cond } => {
                    let vars = self.variables.clone();
                    let ty = self.static_value(ctx, &vars, ty);
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
                    let vars = self.variables.clone();
                    let ty = self.static_value(ctx, &vars, ty);
                    self.expr(expr);
                    self.set(ty)
                }
            };
            self.variables.insert(instr, result);
            instr += 1;
        }
        self.variables[&body.result()]
    }

    fn funcidx(&self) -> u32 {
        // TODO: Handle the case where non-function imports have also been added.
        self.section_import.len() + self.section_function.len()
    }

    fn func(&mut self, funcidx: u32) {
        if std::env::var_os("MOSS_TRACE_WASM").is_some() {
            eprintln!("wasm:func {funcidx}");
        }
        match self.get_func(funcidx) {
            Object::FnDef(fndef, ctx) => {
                let Sigdef(sig) = self.ir.fndefs[fndef];
                let signature = self.interp(ctx, &[], sig);
                let Object::Sig(param, result) = self.obj(signature) else {
                    panic!()
                };
                let params = self.layout_vec(param);
                let results = self.layout_vec(result);
                let Some(body) = self.ir.bodies[fndef] else {
                    panic!("missing lowered body for {fndef:?}");
                };
                self.interp(ctx, &[], body);
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

    fn program(mut self) -> Vec<u8> {
        if std::env::var_os("MOSS_TRACE_WASM").is_some() {
            eprintln!("wasm:program start");
        }
        let wasip1_sigdefs = self.wasip1_sigdefs();
        if std::env::var_os("MOSS_TRACE_WASM").is_some() {
            eprintln!("wasm:program imports={}", wasip1_sigdefs.len());
        }
        let wasi_ctx = self.wasi_ctx(&wasip1_sigdefs);
        self.insert_func(Object::FnDef(self.main, wasi_ctx));
        if std::env::var_os("MOSS_TRACE_WASM").is_some() {
            eprintln!("wasm:program wasi_ctx={wasi_ctx:?}");
        }

        let main_funcidx = self.funcidx();
        let mut next_fn = main_funcidx;
        while next_fn < self.next_funcidx() {
            self.func(next_fn);
            next_fn += 1;
        }

        self.section_global.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i32_const(self.data_offset),
        );

        assert_eq!(self.section_memory.len(), MEMIDX_WASI);
        self.section_memory.memory(MemoryType {
            minimum: ((self.data_offset + RUNTIME_MEMORY_SLACK + 65535) / 65536) as u64,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        self.section_export
            .export("memory", ExportKind::Memory, MEMIDX_WASI);

        let proc_exit_sigdef = wasip1_sigdefs[&self.ir.strings.get_id("proc_exit").unwrap()];
        let proc_exit = self
            .lookup_in_ctx(wasi_ctx, SlotKey::Sigdef(proc_exit_sigdef))
            .unwrap();
        let Object::FnWasi(proc_exit) = self.obj(proc_exit) else {
            panic!()
        };
        let start = self.funcidx();
        self.section_function.function(self.section_type.len());
        self.section_type.ty().function([], []);
        self.section_code.function(&{
            let mut f = Function::new([]);
            f.instructions()
                .call(self.get_funcidx(self.main, wasi_ctx).unwrap())
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
        fd_read: Default::default(),
        fd_close: Default::default(),
        path_open: Default::default(),
        args_get: Default::default(),
        args_sizes_get: Default::default(),
        fd_write: Default::default(),
        print_scratch: Default::default(),
        data_offset: Default::default(),
        strings: Default::default(),
        objects: Default::default(),
        tuples: Default::default(),
        closures: Default::default(),
        contexts: Default::default(),

        funcidxs: Default::default(),

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
