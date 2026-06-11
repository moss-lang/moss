use std::{collections::HashMap, mem::take};

use index_vec::{IndexVec, define_index_type};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools;
use strum::{EnumIter, IntoEnumIterator, IntoStaticStr};
use wasm_encoder::{
    BlockType, CodeSection, ConstExpr, DataSection, EntityType, ExportKind, ExportSection, Function,
    FunctionSection, GlobalSection, GlobalType, ImportSection, InstructionSink, MemArg,
    MemorySection, MemoryType, Module, TypeSection, ValType,
};

use crate::{
    intern::StrId,
    lower::{
        self, Body, ElemId, Expr, FieldId, FndefId, IR, Instr, InstrId, ModuleId, Named,
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

define_index_type! {
    /// The index of an interned environment in the `envs` field during [`Wasm`] codegen.
    struct EnvId = u32;
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

/// A backend-implemented contextual function (prototype intrinsics for the string/print surface).
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum Intrinsic {
    /// `string_builder(size: Uint) -> StringBuilder` — bump-allocate `size` bytes.
    BuilderNew,
    /// `set_char(b, i: Uint, c: Char) -> StringBuilder` — `i32.store8(b.ptr + i, c)`; return `b`.
    SetChar,
    /// `build(b) -> String` — `b` already laid out as `(ptr, size)`; identity.
    Build,
    /// `char_from_codepoint(cp: Uint) -> Char` — identity (ASCII, `Char` = i32).
    CharFromCp,
    /// `print(s: String)` — write `s`'s `(ptr,len)` as an iovec and `fd_write` to stdout.
    Print,
}

/// A backend-implemented contextual *type* (the string/char surface). Like [`Intrinsic`], these
/// are declared in `lib` but left unbound (no provider in the `bootstrap` bridge), so the backend
/// supplies their concrete Wasm representation directly when `eval` reaches the abstract reference.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
enum IntrinsicTy {
    /// `Char` — a Unicode code point, represented as `i32`.
    Char,
    /// `StringBuilder` — `(i32 ptr, i32 size)`, same layout as `String`.
    StringBuilder,
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

    /// The function to process a string literal.
    FnString,

    /// A Wasm instruction.
    FnInstr(Instruction),

    /// The Wasm `funcidx` of an imported WASI P1 function.
    FnWasi(u32),

    /// A backend intrinsic: a contextual function (`string_builder`/`set_char`/`build`/
    /// `char_from_codepoint`/`print`) the prototype implements directly in codegen.
    FnIntrinsic(Intrinsic),

    /// A defined function with its needs bound by an environment.
    FnDef(FndefId, EnvId),

    /// A deferred `eval(node, env)` — a lazy context slot, forced on projection.
    Thunk(lower::NodeId, EnvId),

    /// A lambda value (an un-applied `Node::Lambda`) capturing its defining environment.
    Closure(lower::NodeId, EnvId),

    /// A 32-bit unsigned integer constant.
    ValU32(u32),

    /// A static string value, materialized on demand by copying its bytes to the heap and
    /// leaving the `(ptr, len)` pair (the `String`/`StringBuilder` layout) on the stack.
    ValStr(StrId),

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

    /// Interned environments: each binds a function/lambda's need-nodes to construct [`Object`]s.
    envs: IndexSet<Box<[(lower::NodeId, ObjectId)]>>,

    /// Wasm `funcidx` for each [`Object::FnWasi`] and [`Object::FnDef`].
    funcidxs: IndexSet<Object>,

    /// Wasm `funcidx` of each imported WASI P1 function, by name.
    wasi_funcidx: HashMap<StrId, u32>,

    /// Sigdefs the backend implements as [`Intrinsic`]s (the string/print surface).
    intrinsics: HashMap<SigdefId, Intrinsic>,

    /// Tydefs the backend implements directly (the string/char surface); see [`IntrinsicTy`].
    intrinsic_tys: HashMap<lower::TydefId, IntrinsicTy>,

    /// Index of the mutable global used as the bump-allocation heap pointer.
    heap_global: u32,

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
    /// Intern an environment binding need-nodes to construct [`Object`]s.
    fn mkenv(&mut self, pairs: &[(lower::NodeId, ObjectId)]) -> EnvId {
        let (i, _) = self.envs.insert_full(pairs.to_vec().into_boxed_slice());
        EnvId::from_usize(i)
    }

    /// The empty environment (binds nothing).
    fn env_empty(&mut self) -> EnvId {
        self.mkenv(&[])
    }

    /// The contextual-definition identity of a `Need*` node: its kind and `def`. Two references to
    /// the same contextual definition that differ only in de Bruijn level or via-`param` are
    /// distinct nodes but share this key.
    fn need_key(&self, node: lower::NodeId) -> Option<(u8, usize)> {
        Some(match self.ir.nodes[node.index()] {
            lower::Node::Need { def: lower::DefKind::Ty(def), .. } => (0, def.index()),
            lower::Node::Need { def: lower::DefKind::Sig(def), .. } => (1, def.index()),
            lower::Node::Need { def: lower::DefKind::Val(def), .. } => (2, def.index()),
            lower::Node::Need { def: lower::DefKind::Ctx(def), .. } => (3, def.index()),
            _ => return None,
        })
    }

    /// Look up a need-node in an environment. First by exact node; then, since the same contextual
    /// definition is referenced by distinct nodes at different de Bruijn levels (the env binds it
    /// once), fall back to matching by contextual-definition identity `(kind, def)`.
    fn env_get(&self, env: EnvId, node: lower::NodeId) -> Option<ObjectId> {
        if let Some(o) = self.envs[env.index()]
            .iter()
            .find(|(n, _)| *n == node)
            .map(|(_, o)| *o)
        {
            return Some(o);
        }
        let key = self.need_key(node)?;
        self.envs[env.index()]
            .iter()
            .find(|(n, _)| self.need_key(*n) == Some(key))
            .map(|(_, o)| *o)
    }

    /// Build a child environment by binding `needs` (a function/lambda's need list) positionally to
    /// `args`, on top of the bindings already in `base`.
    fn extend_env(&mut self, base: EnvId, needs: lower::NodeList, args: &[ObjectId]) -> EnvId {
        let need_nodes: Vec<lower::NodeId> = self.ir.lists[needs].to_vec();
        assert_eq!(need_nodes.len(), args.len());
        let mut pairs: Vec<(lower::NodeId, ObjectId)> = self.envs[base.index()].to_vec();
        for (n, a) in need_nodes.into_iter().zip(args.iter().copied()) {
            pairs.push((n, a));
        }
        self.mkenv(&pairs)
    }

    /// Monomorphize a static IR `node` against an environment `env` (binding the enclosing
    /// function's needs), producing the corresponding codegen [`Object`]. The backend analogue of
    /// lowering's `reduce`: β-reduce `Apply`, project `Get`, unfold transparent defs, fold literals,
    /// and resolve `Need*` from `env`. Contexts are built lazily (slots are [`Object::Thunk`]s).
    fn eval(&mut self, node: lower::NodeId, env: EnvId) -> ObjectId {
        match self.ir.nodes[node.index()] {
            lower::Node::Sig { param, result } => {
                let param = self.eval(param, env);
                let result = self.eval(result, env);
                self.mkobj(Object::Sig(param, result))
            }
            lower::Node::Tuple { elems } => {
                let items = self.ir.lists[elems].to_vec();
                let objs: Vec<ObjectId> = items.into_iter().map(|e| self.eval(e, env)).collect();
                let tuple = self.tuples.make(&objs);
                self.mkobj(Object::TyTuple(tuple))
            }
            // A bare lambda is a value: a closure capturing its defining environment. Applying it
            // (see `apply`) binds its needs and evaluates its result.
            lower::Node::Lambda { .. } => self.mkobj(Object::Closure(node, env)),
            // An unbound `sig` for one of the prototype's backend-implemented contextual functions
            // resolves to its intrinsic (the bridge left these inline, with no provider).
            lower::Node::Need { def: lower::DefKind::Sig(def), .. }
                if self.intrinsics.contains_key(&def) =>
            {
                let intr = self.intrinsics[&def];
                self.mkobj(Object::FnIntrinsic(intr))
            }
            // An unbound reference to a backend-implemented type resolves to its concrete Wasm
            // representation (the bridge left these inline, with no provider).
            lower::Node::Need { def: lower::DefKind::Ty(def), .. }
                if self.intrinsic_tys.contains_key(&def) =>
            {
                match self.intrinsic_tys[&def] {
                    IntrinsicTy::Char => self.mkobj(Object::TyI32),
                    IntrinsicTy::StringBuilder => {
                        let i32 = self.mkobj(Object::TyI32);
                        let tuple = self.tuples.make(&[i32, i32]);
                        self.mkobj(Object::TyTuple(tuple))
                    }
                }
            }
            // A need resolves to whatever the enclosing function was given for it.
            lower::Node::Need { .. } => self
                .env_get(env, node)
                .unwrap_or_else(|| panic!("eval: unbound need %{}", node.index())),
            lower::Node::Apply { lambda, args } => {
                let head = self.eval(lambda, env);
                let args = self.ir.lists[args].to_vec();
                let argobjs: Vec<ObjectId> = args.into_iter().map(|a| self.eval(a, env)).collect();
                self.apply(head, &argobjs)
            }
            // A context is a tuple of slots, each a deferred eval — built lazily so projecting one
            // slot (e.g. `println`) does not force its siblings (e.g. the arithmetic with `if`s).
            lower::Node::List { items } => {
                let item_nodes: Vec<lower::NodeId> = self.ir.lists[items].to_vec();
                let slots: Vec<ObjectId> = item_nodes
                    .into_iter()
                    .map(|n| self.mkobj(Object::Thunk(n, env)))
                    .collect();
                self.mkctx(&slots)
            }
            lower::Node::Get { ctx, slot } => {
                let ctx = self.eval(ctx, env);
                let Object::Ctx(range) = self.obj(ctx) else {
                    panic!("eval: Get on non-context {:?}", self.obj(ctx))
                };
                let len = range.end.raw() - range.start.raw();
                assert!(
                    slot.raw() < len,
                    "eval: Get slot {} out of bounds (ctx has {} slots)",
                    slot.raw(),
                    len
                );
                let slot_obj = self.tuples[TupleLoc::from_raw(range.start.raw() + slot.raw())];
                self.force(slot_obj)
            }
            lower::Node::Fndef { def } => self.mkobj(Object::FnDef(def, env)),
            // Transparent definitions unfold to their bodies.
            lower::Node::Aliasdef { def } => {
                let body = self.ir.aliasdefs[def].0;
                self.eval(body, env)
            }
            lower::Node::Context => self.mkobj(Object::TyCtx),
            lower::Node::Nothing => self.mkobj(Object::Open),
            lower::Node::Lit { val } => self.fold_lit(val),
            // A `Bind { args, bind }` (the body of a binding's lambda) projects to its bound value.
            lower::Node::Bind { bind, .. } => self.eval(bind, env),
            // Bindings construct context entries; as a value, a binding is a closure over its `bind`
            // lambda, projected when applied (see `apply`).
            lower::Node::BindDef { bind, .. } => self.mkobj(Object::Closure(bind, env)),
            other => todo!("eval: {other:?} (milestone B2)"),
        }
    }

    /// Force a slot object: if it is a deferred [`Object::Thunk`], evaluate it; otherwise return it.
    fn force(&mut self, obj: ObjectId) -> ObjectId {
        match self.obj(obj) {
            Object::Thunk(node, env) => self.eval(node, env),
            _ => obj,
        }
    }

    /// Apply a head [`Object`] to already-evaluated arguments, mirroring `reduce`'s `Apply` case.
    fn apply(&mut self, head: ObjectId, args: &[ObjectId]) -> ObjectId {
        match self.obj(head) {
            // Inline a lambda value: bind its needs to the args atop its captured environment.
            Object::Closure(lnode, capt) => {
                let lower::Node::Lambda { needs, result, .. } = self.ir.nodes[lnode.index()] else {
                    panic!("apply: closure over non-lambda")
                };
                let childenv = self.extend_env(capt, needs, args);
                self.eval(result, childenv)
            }
            // A `Fndef` applied to its context construct binds the function's needs to those args.
            // A context-returning function (e.g. `bootstrap`) is a context *constructor*: evaluate
            // its body to the context it builds. A value-returning function stays an `FnDef` to call.
            Object::FnDef(def, _) if !args.is_empty() => {
                let Sigdef(sig) = self.ir.fndefs[def];
                let lower::Node::Lambda { needs, .. } = self.ir.nodes[sig.index()] else {
                    panic!("apply: fndef sig is not a lambda")
                };
                let base = self.env_empty();
                let env = self.extend_env(base, needs, args);
                if self.returns_ctx(def) {
                    self.eval_ctx_body(def, env)
                } else {
                    self.mkobj(Object::FnDef(def, env))
                }
            }
            // A function-like leaf (intrinsic, Wasm instruction, or imported WASI function) ignores
            // its (static) context construct; its runtime arguments arrive at the `Call` site, where
            // emission consumes them.
            Object::FnIntrinsic(_) | Object::FnInstr(_) | Object::FnWasi(_) => head,
            // Already a value; applying to no further context arguments is the identity.
            _ if args.is_empty() => head,
            other => todo!("apply: {other:?} to {} args", args.len()),
        }
    }

    /// Fold a literal value to a codegen [`Object`].
    fn fold_lit(&mut self, val: lower::Val) -> ObjectId {
        match val {
            lower::Val::Uint31(n) | lower::Val::Uint32(n) => self.mkobj(Object::ValU32(n)),
            // A char is its code point (`Char` = i32, ASCII for now), like `char_from_codepoint`.
            lower::Val::Char(c) => self.mkobj(Object::ValU32(c as u32)),
            lower::Val::String(s) => self.mkobj(Object::ValStr(s)),
            other => todo!("fold_lit: {other:?} (milestone B2)"),
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

    /// Build the root `Wasi` context as an `Object::Ctx`, in `lib/wasi.moss`'s `Wasi` member order
    /// (which is the slot order lowering used). All leaves are concrete target Objects.
    ///
    /// B1 builds every slot that `hello`'s resolution reaches (types, literal types, realizers,
    /// `Numerals`, `Wasm` instructions, `memidx`). The `WasiP1` sub-context is a placeholder for now
    /// (B2: import every wasip1 function with its eval'd signature, in declaration order). `proc_exit`
    /// is still imported separately for `_start`.
    fn wasi_ctx(&mut self, wasip1_sigdefs: &IndexMap<StrId, SigdefId>) -> ObjectId {
        for (&name, _) in wasip1_sigdefs {
            let sig: Option<(&str, &[ValType], &[ValType])> = match &self.ir.strings[name] {
                "proc_exit" => Some(("proc_exit", &[ValType::I32], &[])),
                "fd_write" => Some((
                    "fd_write",
                    &[ValType::I32, ValType::I32, ValType::I32, ValType::I32],
                    &[ValType::I32],
                )),
                _ => None,
            };
            if let Some((import_name, params, results)) = sig {
                let typeidx = self.section_type.len();
                self.section_type
                    .ty()
                    .function(params.iter().copied(), results.iter().copied());
                let funcidx = self.push_func_wasi();
                self.section_import
                    .import(WASIP1, import_name, EntityType::Function(typeidx));
                self.wasi_funcidx.insert(name, funcidx);
            }
        }

        use IntLitIn::*;
        use IntLitOut as O;
        let leaves: &[Object] = &[
            // types
            Object::TyI32,
            Object::TyI64,
            Object::TyMemIdx,
            // literal types
            Object::TyLitInt(Uint31),
            Object::TyLitInt(Uint32),
            Object::TyLitInt(Int32),
            Object::TyLitInt(Uint63),
            Object::TyLitInt(Uint64),
            Object::TyLitInt(Int64),
            Object::TyLitInt(Uint),
            Object::TyLitInt(Int),
            Object::TyLitChar,
            Object::TyLitString,
            // realizers (in `wasi.moss` order; out-types per the `=I32`/`=I64` bindings there)
            Object::FnInt(Uint31, O::Uint32),
            Object::FnInt(Uint32, O::Uint32),
            Object::FnInt(Uint31, O::Int32),
            Object::FnInt(Int32, O::Int32),
            Object::FnInt(Uint31, O::Uint64),
            Object::FnInt(Uint32, O::Uint64),
            Object::FnInt(Uint63, O::Uint64),
            Object::FnInt(Uint64, O::Uint64),
            Object::FnInt(Uint31, O::Int64),
            Object::FnInt(Uint32, O::Int64),
            Object::FnInt(Int32, O::Int64),
            Object::FnInt(Uint63, O::Int64),
            Object::FnInt(Int64, O::Int64),
            Object::FnInt(Uint31, O::Uint),
            Object::FnInt(Uint32, O::Uint),
            Object::FnInt(Uint31, O::Int),
            Object::FnInt(Int32, O::Int),
            Object::FnString,
        ];
        let mut slots: Vec<ObjectId> = leaves.iter().map(|&o| self.mkobj(o)).collect();

        // `Numerals[Number=I32]` and `Numerals[Number=I64]`: nested Ctx of `digit0..digit9, radix`.
        for _ in 0..2 {
            let digits: Vec<ObjectId> =
                (0..=10u32).map(|n| self.mkobj(Object::ValU32(n))).collect();
            let nums = self.mkctx(&digits);
            slots.push(nums);
        }

        // `wasm::Wasm[Base]`: nested Ctx of `FnInstr`, in `Instruction` order (== `wasm.moss`'s
        // `Wasm` member order).
        let instrs: Vec<ObjectId> = Instruction::iter()
            .map(|i| self.mkobj(Object::FnInstr(i)))
            .collect();
        let wasm_ctx = self.mkctx(&instrs);
        slots.push(wasm_ctx);

        // `wasm::memidx[Base]`.
        let memidx = self.mkobj(Object::ValU32(MEMIDX_WASI));
        slots.push(memidx);

        // `wasip1::WasiP1[Base]`: placeholder (B2 builds the imports).
        let wasip1 = self.mkobj(Object::Open);
        slots.push(wasip1);

        self.mkctx(&slots)
    }

    fn next_funcidx(&self) -> u32 {
        self.funcidxs.len() as u32
    }

    fn get_func(&self, funcidx: u32) -> Object {
        self.funcidxs[funcidx as usize]
    }

    fn get_funcidx(&self, fndef: FndefId, env: EnvId) -> Option<u32> {
        Some(self.funcidxs.get_index_of(&Object::FnDef(fndef, env))? as u32)
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
            | Object::FnString
            | Object::FnInstr(_)
            | Object::FnWasi(_)
            | Object::FnIntrinsic(_)
            | Object::FnDef(_, _)
            | Object::Thunk(_, _)
            | Object::Closure(_, _)
            | Object::ValU32(_)
            | Object::ValStr(_)
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

    fn val_u32(&mut self, _ctx: ObjectId, _valdef: ValdefId) -> u32 {
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

    /// Push a value [`Object`]'s runtime representation onto the operand stack.
    fn materialize(&mut self, obj: ObjectId) {
        match self.obj(obj) {
            Object::ValU32(n) => {
                self.body.insn().i32_const(n as i32);
            }
            // A static string: emit (and intern) its bytes as a data segment, leaving the
            // `(ptr, len)` pair on the stack.
            Object::ValStr(s) => self.string(s),
            Object::ValDyn(ty, start) => self.get_locals(ty, start),
            other => todo!("materialize: {other:?}"),
        }
    }

    /// Append a fresh i32 local and return its index (scratch for intrinsics).
    fn tmp(&mut self) -> u32 {
        let id = self.locals.len_idx();
        self.locals.push(ValType::I32);
        id.raw()
    }

    /// Emit a call given the already-evaluated callee head, with its runtime argument(s) already on
    /// the operand stack.
    fn emit_call(&mut self, head: ObjectId) {
        match self.obj(head) {
            Object::FnDef(def, fenv) => {
                let funcidx = self.insert_func(Object::FnDef(def, fenv));
                self.body.insn().call(funcidx);
            }
            // Numeric instructions ignore `ctx`; memory instructions would need `val_u32` (milestone
            // C). `hello`'s arithmetic is numeric-only, so a placeholder context suffices.
            Object::FnInstr(instr) => {
                let ctx = self.mkobj(Object::Open);
                self.wasm_instruction(ctx, instr);
            }
            Object::FnWasi(funcidx) => {
                self.body.insn().call(funcidx);
            }
            Object::FnIntrinsic(intr) => self.emit_intrinsic(intr),
            other => todo!("emit_call: {other:?}"),
        }
    }

    /// Emit a backend intrinsic, consuming its runtime argument(s) from the operand stack and
    /// leaving its result there.
    fn emit_intrinsic(&mut self, intr: Intrinsic) {
        let store = MemArg { offset: 0, align: 2, memory_index: MEMIDX_WASI };
        let store8 = MemArg { offset: 0, align: 0, memory_index: MEMIDX_WASI };
        match intr {
            // `build(b)` / `char_from_codepoint(cp)` are identities on the layout already on the
            // stack (`(ptr,size)` / the codepoint).
            Intrinsic::Build | Intrinsic::CharFromCp => {}
            // `string_builder(size)`: ptr = heap; heap += size; result `(ptr, size)`.
            Intrinsic::BuilderNew => {
                let size = self.tmp();
                let ptr = self.tmp();
                self.body.insn().local_set(size);
                self.body.insn().global_get(self.heap_global).local_set(ptr);
                self.body
                    .insn()
                    .global_get(self.heap_global)
                    .local_get(size)
                    .i32_add()
                    .global_set(self.heap_global);
                self.body.insn().local_get(ptr).local_get(size);
            }
            // `set_char(b=(ptr,size), index, char)`: `store8(ptr+index, char)`; result `(ptr,size)`.
            Intrinsic::SetChar => {
                let c = self.tmp();
                let i = self.tmp();
                let sz = self.tmp();
                let p = self.tmp();
                self.body.insn().local_set(c).local_set(i).local_set(sz).local_set(p);
                self.body
                    .insn()
                    .local_get(p)
                    .local_get(i)
                    .i32_add()
                    .local_get(c)
                    .i32_store8(store8);
                self.body.insn().local_get(p).local_get(sz);
            }
            // `print(s=(ptr,len))`: write iovec `{ptr,len}` at `[0,8)`, `fd_write(1,0,1,8)`, drop.
            Intrinsic::Print => {
                let fd_write = self.wasi_funcidx[&self.ir.strings.get_id("fd_write").unwrap()];
                let len = self.tmp();
                let ptr = self.tmp();
                self.body.insn().local_set(len).local_set(ptr);
                self.body.insn().i32_const(0).local_get(ptr).i32_store(store); // iovec.buf
                self.body.insn().i32_const(4).local_get(len).i32_store(store); // iovec.buf_len
                self.body
                    .insn()
                    .i32_const(1) // fd = stdout
                    .i32_const(0) // iovs ptr
                    .i32_const(1) // iovs_len
                    .i32_const(8) // retptr0
                    .call(fd_write)
                    .drop();
            }
        }
    }

    fn expr(&mut self, env: EnvId, expr: Expr) {
        match expr {
            Expr::Param { ty } => {
                // The function parameter occupies the first locals, `[0, layout_len(param))`.
                let ty = self.eval(ty, env);
                self.get_locals(ty, LocalId::from_usize(0));
            }
            Expr::Copy { value } => {
                self.get(value);
            }
            Expr::Nominal { .. } => todo!("expr: Nominal (milestone B)"),
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
            Expr::Val { val } => {
                let mut obj = self.eval(val, env);
                // A contextual value can surface as a closure over its binding's lambda (e.g. a
                // `BindDef` member like `true=1` reached through a context projection). With no
                // needs left to supply, applying it to nothing projects out the bound value.
                while let Object::Closure(lnode, _) = self.obj(obj) {
                    let lower::Node::Lambda { needs, .. } = self.ir.nodes[lnode.index()] else {
                        break;
                    };
                    if !needs.is_empty() {
                        break;
                    }
                    obj = self.apply(obj, &[]);
                }
                self.materialize(obj);
            }
            Expr::Call { func, arg } => {
                let head = self.eval(func, env);
                // Push the runtime argument, then emit the call/instruction/intrinsic.
                self.get(arg);
                self.emit_call(head);
            }
            Expr::Bind { .. } => todo!("expr: Bind (milestone C)"),
        }
    }

    fn interp(&mut self, env: EnvId, body: Body) -> ObjectId {
        for instr in body.body {
            let result = match self.ir.instrs[instr] {
                Instr::Set { lhs, rhs } => {
                    let (ty, start) = self.local(lhs);
                    self.get(rhs);
                    self.set_locals(ty, start);
                    self.mkobj(Object::ValStmt)
                }
                Instr::If { ty, cond } => {
                    let ty = self.eval(ty, env);
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
                    let ty = self.eval(ty, env);
                    self.expr(env, expr);
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
            Object::FnDef(fndef, env) => {
                let Sigdef(sig) = self.ir.fndefs[fndef];
                // Monomorphize the signature against the environment (which binds the fndef's
                // needs) to lay out params and results. `env` already binds the sig lambda's needs,
                // so evaluate its *result* directly rather than re-applying the lambda.
                let lower::Node::Lambda { result: sig_result, .. } = self.ir.nodes[sig.index()]
                else {
                    panic!("fndef sig is not a lambda")
                };
                let signature = self.eval(sig_result, env);
                let Object::Sig(param, result) = self.obj(signature) else {
                    panic!()
                };
                let params = self.layout_vec(param);
                let results = self.layout_vec(result);
                // The parameter occupies the first locals; body instructions add more after it.
                self.make_locals(param);
                let body = self.ir.bodies[fndef];
                self.interp(env, body);
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

    /// The root environment for `main`: its needs bound to the concrete root context. `main`'s
    /// sole need (if any) is its `Std`, built from the root `Wasi` via the `std` bridge.
    fn root_env(&mut self, wasi_ctx: ObjectId) -> EnvId {
        let Sigdef(sig) = self.ir.fndefs[self.main];
        let lower::Node::Lambda { needs, .. } = self.ir.nodes[sig.index()] else {
            panic!("main sig is not a lambda")
        };
        let need_nodes: Vec<lower::NodeId> = self.ir.lists[needs].to_vec();
        if need_nodes.is_empty() {
            return self.env_empty();
        }
        let std_ctx = self.build_std(wasi_ctx);
        let pairs: Vec<(lower::NodeId, ObjectId)> =
            need_nodes.into_iter().map(|n| (n, std_ctx)).collect();
        self.mkenv(&pairs)
    }

    /// Does `def`'s signature return a context (`bind ...`) rather than a value? Such a function is
    /// a context *constructor*; applying it statically yields the context its body builds.
    fn returns_ctx(&self, def: FndefId) -> bool {
        // The fndef body is `Lambda { result: Sig { param, result: <return type> } }`. A `bind`
        // return type was lowered to a `Lambda` (returning the context's slot list); a value return
        // type is an ordinary type node.
        let Sigdef(sig) = self.ir.fndefs[def];
        let lower::Node::Lambda { result: sigresult, .. } = self.ir.nodes[sig.index()] else {
            return false;
        };
        let lower::Node::Sig { result, .. } = self.ir.nodes[sigresult.index()] else {
            return false;
        };
        matches!(self.ir.nodes[result.index()], lower::Node::Lambda { .. })
    }

    /// Evaluate the context built by a context-constructor `def`'s body, under `env` (which binds
    /// the def's needs). The body's result is a `Bind { ctx }`, possibly reached through a `Copy`.
    fn eval_ctx_body(&mut self, def: FndefId, env: EnvId) -> ObjectId {
        let body = self.ir.bodies[def];
        let mut instr = body.result();
        let ctx = loop {
            match self.ir.instrs[instr] {
                Instr::Expr {
                    expr: Expr::Bind { ctx },
                    ..
                } => break ctx,
                Instr::Expr {
                    expr: Expr::Copy { value },
                    ..
                } => instr = value,
                other => panic!("ctx-ctor body result is not a `bind`: {other:?}"),
            }
        };
        self.eval(ctx, env)
    }

    /// Construct the `Std` context Object via the `std` bridge (`fn std[Wasi](): bind Std`) against
    /// the root `Wasi` context. `std`'s body is `bind bootstrap(...)`, a single-frame context that
    /// *forwards* `bootstrap`'s `Std`; unwrap that frame to expose `Std`'s members directly.
    fn build_std(&mut self, wasi_ctx: ObjectId) -> ObjectId {
        let std_def = self.named(self.lib.wasi, "std").fndef();
        let Sigdef(sig) = self.ir.fndefs[std_def];
        let lower::Node::Lambda { needs, .. } = self.ir.nodes[sig.index()] else {
            panic!("std sig is not a lambda")
        };
        let wasi_need = self.ir.lists[needs].to_vec()[0];
        let std_env = self.mkenv(&[(wasi_need, wasi_ctx)]);
        let wrapped = self.eval_ctx_body(std_def, std_env);
        let Object::Ctx(range) = self.obj(wrapped) else {
            panic!("std bridge did not produce a context")
        };
        if range.end.raw() - range.start.raw() == 1 {
            let slot0 = self.tuples[range.start];
            self.force(slot0)
        } else {
            wrapped
        }
    }

    /// B1 validation: evaluate each `Val`/`Call` projection in `main`'s body against `root_env` and
    /// log the resulting [`Object`], confirming context resolution works without emitting code.
    fn validate(&mut self, root_env: EnvId) {
        eprintln!("=== B1 validation: main's projections ===");
        let body = self.ir.bodies[self.main];
        let instrs: Vec<InstrId> = body.body.into_iter().collect();
        for instr in instrs {
            // Evaluate each instruction's *type* node — these are the `Get{Std, slot}` projections,
            // which test ctxdef-order resolution without forcing the B2 value desugar.
            if let Instr::Expr { ty, .. } = self.ir.instrs[instr] {
                let o = self.eval(ty, root_env);
                eprintln!("  i{} ty %{} -> {:?}", instr.index(), ty.index(), self.obj(o));
            }
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
            Need { param, .. } => vec![param],
            Get { ctx, .. } => vec![ctx],
            Bind { args, bind } => self.ir.lists[args].iter().copied().chain([bind]).collect(),
            BindDef { bind, .. } => vec![bind],
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

        // The runtime hands `main` its `Std`; the backend constructs that `Std` from the root
        // `Wasi` via the `std` bridge, and binds it as `main`'s sole need.
        let root_env = self.root_env(wasi_ctx);

        // B1 validation: check that `main`'s context projections resolve to sane Objects, without
        // emitting any code. (`MOSS_B1=1` stops here; the emission path is milestone B2.)
        if std::env::var("MOSS_B1").is_ok() {
            self.validate(root_env);
            return Vec::new();
        }

        // Register `main` itself as the first defined function, monomorphized against the root
        // context. The worklist loop below then emits it (and anything it transitively calls).
        self.insert_func(Object::FnDef(self.main, root_env));

        let main_funcidx = self.funcidx();
        let mut next_fn = main_funcidx;
        while next_fn < self.next_funcidx() {
            self.func(next_fn);
            next_fn += 1;
        }

        // Global 0 is the mutable bump-allocation heap pointer (`heap_global`), starting past the
        // static data (which itself starts past the reserved `[0,16)` print scratch).
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
            minimum: ((self.data_offset + 65535) / 65536) as u64,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        self.section_export
            .export("memory", ExportKind::Memory, MEMIDX_WASI);

        let start = self.funcidx();
        let main_funcidx = self.get_funcidx(self.main, root_env).unwrap();
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
        // Reserve `[0,16)` of linear memory as print scratch (iovec + retptr); static data and the
        // heap start at 16.
        data_offset: 16,
        strings: Default::default(),
        objects: Default::default(),
        tuples: Default::default(),
        envs: Default::default(),

        funcidxs: Default::default(),
        wasi_funcidx: Default::default(),
        intrinsics: {
            let mut m = HashMap::new();
            for (&(_, s), &named) in &names.names {
                if let Named::Sigdef(d) = named {
                    let intr = match &ir.strings[s] {
                        "string_builder" => Some(Intrinsic::BuilderNew),
                        "set_char" => Some(Intrinsic::SetChar),
                        "build" => Some(Intrinsic::Build),
                        "char_from_codepoint" => Some(Intrinsic::CharFromCp),
                        "print" => Some(Intrinsic::Print),
                        _ => None,
                    };
                    if let Some(intr) = intr {
                        m.insert(d, intr);
                    }
                }
            }
            m
        },
        intrinsic_tys: {
            let mut m = HashMap::new();
            for (&(_, s), &named) in &names.names {
                if let Named::Tydef(d) = named {
                    let ty = match &ir.strings[s] {
                        "Char" => Some(IntrinsicTy::Char),
                        "StringBuilder" => Some(IntrinsicTy::StringBuilder),
                        _ => None,
                    };
                    if let Some(ty) = ty {
                        m.insert(d, ty);
                    }
                }
            }
            m
        },
        heap_global: 0,

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
