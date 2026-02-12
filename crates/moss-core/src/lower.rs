use std::{
    backtrace::Backtrace,
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    mem::take,
    ops::Index,
};

use index_vec::{IndexSlice, IndexVec, define_index_type};
use indexmap::IndexSet;

use crate::{
    intern::{StrId, Strings},
    lex::{TokenId, TokenStarts, relex, string},
    parse::{self, Binop, Block, Expr, ExprId, Field, Path, Spec, Stmt, StmtId, Tree, Unop},
    prelude::Base,
    range::{Inclusive, expr_range, path_range, single},
    util::IdRange,
};

define_index_type! {
    /// The unique ID of a module, i.e. a source file, in the `modules` field of the [`IR`].
    pub struct ModuleId = u32;
}

define_index_type! {
    /// The index of a [`StaticInstr`] in the `statics` field of the [`IR`].
    pub struct StaticId = u32;
}

define_index_type! {
    /// The index of a [`StaticId`] in the `items` field of the [`IR`].
    pub struct ItemId = u32;
}

define_index_type! {
    /// The index of a [`StrId`] and [`StaticId`] in the `records` field of the [`IR`].
    pub struct RecordId = u32;
}

define_index_type! {
    /// The index of an output slot on a context.
    pub struct SlotId = u32;
}

define_index_type! {
    /// The index of an element in a tuple.
    pub struct ElemId = u32;
}

define_index_type! {
    /// The index of a [`Tydef`] in the `tydefs` field of the [`IR`].
    pub struct TydefId = u32;
}

define_index_type! {
    /// The index of a [`Tagdef`] in the `tagdefs` field of the [`IR`].
    pub struct TagdefId = u32;
}

define_index_type! {
    /// The index of an [`Aliasdef`] in the `aliasdefs` field of the [`IR`].
    pub struct AliasdefId = u32;
}

define_index_type! {
    /// The index of a [`Fndef`] in the `fndefs` field of the [`IR`].
    pub struct FndefId = u32;
}

define_index_type! {
    /// The index of a [`Valdef`] in the `valdefs` field of the [`IR`].
    pub struct ValdefId = u32;
}

define_index_type! {
    /// The index of a [`Ctxdef`] in the `ctxdefs` field of the [`IR`].
    pub struct CtxdefId = u32;
}

define_index_type! {
    /// The index of a field in a record.
    pub struct FieldId = u32;
}

define_index_type! {
    /// The index of a [`TypeId`]/[`Instr`] in the `locals`/`instrs` field of the [`IR`].
    pub struct LocalId = u32;
}

define_index_type! {
    /// The index of a [`LocalId`] in the `refs` field of the [`IR`].
    pub struct RefId = u32;
}

define_index_type! {
    /// The index of a [`Drill`] in the `drills` field of the [`IR`].
    pub struct DrillId = u32;
}

/// An instruction in the static IR.
///
/// A sequence of static instructions is used to represent every `type` declaration, every `context`
/// definition, every `fn` signature, and every `val` type. It is also used to represent the
/// "contextual needs" that go in square brackets for each of those items.
///
/// For types, functions, and values, the static IR snippet will start with `Param`, use `Get` to
/// access some slots from that context, perhaps use `Ctx` to construct intermediate contexts as
/// necessary, and eventually return an item that it either extracted or perhaps constructed using
/// `Tagdef`, `Tuple`, or `Record`.
///
/// For contexts, ...
#[derive(Clone, Copy, Debug)]
pub enum StaticInstr {
    /// The parameter context.
    Param,

    /// An open slot.
    Open,

    /// A new context providing some number of slots.
    Ctx {
        /// Statics used to construct the output slots of the new context.
        slots: IdRange<ItemId>,
    },

    /// Need a contextual type parametrized by a specific context.
    NeedTydef {
        /// The contextual type declaration.
        def: TydefId,

        /// Statics destructured to satisfy the input slots of the parameter context.
        params: IdRange<ItemId>,
    },

    /// Need a contextual function parametrized by a specific context.
    NeedFndef {
        /// The contextual function declaration.
        def: FndefId,

        /// Statics destructured to satisfy the input slots of the parameter context.
        params: IdRange<ItemId>,
    },

    /// Need a contextual value parametrized by a specific context.
    NeedValdef {
        /// The contextual value declaration.
        def: ValdefId,

        /// Statics destructured to satisfy the input slots of the parameter context.
        params: IdRange<ItemId>,
    },

    /// Need a composite context parametrized by a specific context.
    NeedCtxdef {
        /// The context definition.
        def: CtxdefId,

        /// Statics destructured to satisfy the input slots of the parameter context.
        params: IdRange<ItemId>,
    },

    /// A nominal type parametrized by a specific context.
    Tagdef {
        /// The nominal type definition.
        def: TagdefId,

        /// Statics destructured to satisfy the input slots of the parameter context.
        params: IdRange<ItemId>,
    },

    /// A type alias parametrized by a specific context.
    Aliasdef {
        /// The nominal type definition.
        def: AliasdefId,

        /// Statics destructured to satisfy the input slots of the parameter context.
        params: IdRange<ItemId>,
    },

    /// A structural tuple of other types.
    Tuple {
        /// The types of the tuple elements.
        elems: IdRange<ItemId>,
    },

    /// A structural record of other types.
    Record {
        /// The types and field names of the record elements, in lexicographical order.
        fields: IdRange<RecordId>,
    },

    /// A literal value.
    Lit {
        /// The literal.
        val: Val,
    },

    /// A slot from a context.
    Get {
        /// The context.
        ctx: StaticId,

        /// The output slot index.
        slot: SlotId,
    },

    /// Provide a contextual type parametrized by a specific context.
    BindTydef {
        /// The contextual type declaration.
        def: TydefId,

        /// Statics for the input slots of the left-hand parameter context.
        params: IdRange<ItemId>,

        /// The type being provided.
        bind: StaticId,

        /// Statics for the input slots of the right-hand parameter context.
        args: IdRange<ItemId>,
    },

    /// Provide a contextual function parametrized by a specific context.
    BindFndef {
        /// The contextual function declaration.
        def: FndefId,

        /// Statics for the input slots of the left-hand parameter context.
        params: IdRange<ItemId>,

        /// The function being provided.
        bind: StaticId,

        /// Statics for the input slots of the right-hand parameter context.
        args: IdRange<ItemId>,
    },

    /// Provide a contextual value parametrized by a specific context.
    BindValdef {
        /// The contextual value declaration.
        def: ValdefId,

        /// Statics for the input slots of the left-hand parameter context.
        params: IdRange<ItemId>,

        /// The value being provided.
        bind: StaticId,

        /// Statics for the input slots of the right-hand parameter context.
        args: IdRange<ItemId>,
    },

    /// Provide a composite context parametrized by a specific context.
    BindCtxdef {
        /// The composite context definition.
        def: CtxdefId,

        /// Statics for the input slots of the left-hand parameter context.
        params: IdRange<ItemId>,

        /// The context being provided.
        bind: StaticId,

        /// Statics for the input slots of the right-hand parameter context.
        args: IdRange<ItemId>,
    },
}

#[derive(Clone, Copy, Debug)]
pub struct Static {
    pub body: IdRange<StaticId>,
}

impl Static {
    fn new(body: IdRange<StaticId>, result: StaticId) -> Self {
        assert_eq!(body.last(), Some(result));
        Self { body }
    }

    fn result(self) -> StaticId {
        self.body.last().unwrap()
    }
}

enum Query<T> {
    Missing,
    Ambiguous,
    Unique(T),
}

impl<T> Query<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> Query<U> {
        match self {
            Query::Missing => Query::Missing,
            Query::Ambiguous => Query::Ambiguous,
            Query::Unique(x) => Query::Unique(f(x)),
        }
    }
}

impl<T: Copy> Query<T> {
    fn combine(self, other: Self, f: impl FnOnce(T, T) -> Option<Ordering>) -> Self {
        match (self, other) {
            (Query::Missing, Query::Missing) => Query::Missing,
            // TODO: This is actually incorrect, since even if one part of a query turns up
            // ambiguous, another part may turn up an answer that's strictly better than all the
            // possible choices from the first part; but we don't track enough information to know
            // whether or not that's the case. Is the right solution to track more information from
            // ambiguous queries so that we can handle this case with as much precision as possible?
            // Or is the right solution to instead disallow ambiguous entries from coexisting within
            // the same context at all, being stricter to simplify this query implementation?
            (Query::Ambiguous, _) | (_, Query::Ambiguous) => Query::Ambiguous,
            (Query::Unique(x), Query::Missing) | (Query::Missing, Query::Unique(x)) => {
                Query::Unique(x)
            }
            (Query::Unique(x), Query::Unique(y)) => match f(x, y) {
                None => Query::Ambiguous,
                Some(Ordering::Greater | Ordering::Equal) => Query::Unique(x),
                Some(Ordering::Less) => Query::Unique(y),
            },
        }
    }
}

/// A path from one context downward into a context it contains.
type Drill = Option<(SlotId, DrillId)>;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Func {
    pub ctx: CtxId,
    pub def: FndefId,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Val {
    Opaque(CtxId, ValdefId),
    Uint31(u32),
    Uint32(u32),
    Int32(i32),
    Uint63(u64),
    Uint64(u64),
    Int64(i64),
    Uint(StrId),
    Int(StrId),
    Char(char),
    String(StrId),
}

#[derive(Clone, Copy, Debug)]
pub enum IntType {
    Uint32,
    Int32,
    Uint64,
    Int64,
    Uint,
    Int,
}

#[derive(Clone, Copy, Debug)]
pub struct Tydef {
    pub ctx: Static,
}

#[derive(Clone, Copy, Debug)]
pub struct Tagdef {
    pub ctx: Static,
    pub inner: Static,
}

#[derive(Clone, Copy, Debug)]
pub struct Aliasdef {
    pub ctx: Static,
    pub def: Static,
}

#[derive(Clone, Copy, Debug)]
pub struct Fndef {
    pub ctx: Static,
    pub param: Static,
    pub result: Static,
}

#[derive(Clone, Copy, Debug)]
pub struct Valdef {
    pub ctx: Static,
    pub ty: Static,
}

#[derive(Clone, Copy, Debug)]
pub struct Ctxdef {
    pub ctx: Static,
    pub def: Static,
}

#[derive(Clone, Copy, Debug)]
pub struct Depth(pub u32);

/// When executed, each instruction implicitly defines a mutable local variable.
#[derive(Clone, Copy, Debug)]
pub enum Instr {
    /// Bind a contextual value until the next [`Instr::EndBind`].
    ///
    /// Type: unit.
    BindCall(FndefId, LocalId),

    /// End the most recent binding.
    ///
    /// Type: unit.
    EndBind,

    /// Get a contextual value.
    ///
    /// Type: that of the given value.
    Val(ValdefId, CtxId),

    /// Get the value of the parameter to this function.
    ///
    /// Type: this function's parameter type.
    Param,

    /// Copy the value of another local into a fresh local.
    ///
    /// Type: same as the given local.
    Copy(LocalId),

    /// Set the left-hand local to the value of the right-hand local.
    ///
    /// Type: unit.
    Set(LocalId, LocalId),

    /// Construct a value of a nominal type given a value of its inner type.
    ///
    /// Type: the given nominal type.
    Nominal(TagdefId, LocalId),

    /// Construct a tuple.
    ///
    /// Type: [`Type::Tuple`] with the given element types.
    Tuple(IdRange<RefId>),

    /// Construct a record.
    ///
    /// Type: the given record type.
    Record(TypeId, IdRange<RefId>),

    /// Get an element of a tuple.
    ///
    /// Type: the element type.
    Elem(LocalId, ElemId),

    /// Get a field of a record.
    ///
    /// Type: the field type.
    Field(LocalId, FieldId),

    /// Call a function.
    ///
    /// Type: the function's result type.
    Call(FndefId, LocalId),

    /// Start a block only if the given condition is true.
    ///
    /// Type: unit.
    If(LocalId, TypeId),

    /// Start a block only if the preceding [`Instr::If`] condition was false.
    ///
    /// Type: same as the specified local from the true branch.
    Else(LocalId),

    /// End the current [`Instr::If`] or [`Instr::Else`] block.
    ///
    /// Type: same as the specified local from the false branch.
    EndIf(LocalId),

    /// Start a loop block.
    ///
    /// Type: unit.
    Loop,

    /// End the current [`Instr::Loop`] block.
    ///
    /// Type: unit.
    EndLoop,

    /// Branch to the end of the block with the given depth, or the beginning if it's a loop block.
    ///
    /// Type: unit.
    Br(Depth),

    /// Return from this function.
    ///
    /// Type: unit.
    Return(LocalId),

    /// Return bindings from this function.
    ///
    /// Type: unit.
    ReturnBind(CtxId),
}

#[derive(Debug)]
pub struct IR {
    pub modules: IndexVec<ModuleId, ()>,
    pub strings: Strings,
    pub statics: IndexVec<StaticId, StaticInstr>,
    pub items: IndexVec<ItemId, StaticId>,
    pub records: IndexVec<RecordId, (StrId, StaticId)>,
    pub empty: Static,
    pub tydefs: IndexVec<TydefId, Tydef>,
    pub tagdefs: IndexVec<TagdefId, Tagdef>,
    pub aliasdefs: IndexVec<AliasdefId, Aliasdef>,
    pub fndefs: IndexVec<FndefId, Fndef>,
    pub valdefs: IndexVec<ValdefId, Valdef>,
    pub ctxdefs: IndexVec<CtxdefId, Ctxdef>,
    pub locals: IndexVec<LocalId, TypeId>,
    pub instrs: IndexVec<LocalId, Instr>,
    pub refs: IndexVec<RefId, LocalId>,
    pub bodies: IndexVec<FndefId, Option<LocalId>>,
}

impl Default for IR {
    fn default() -> Self {
        let mut ir = Self {
            modules: Default::default(),
            strings: Default::default(),
            statics: Default::default(),
            items: Default::default(),
            records: Default::default(),
            tydefs: Default::default(),
            tagdefs: Default::default(),
            aliasdefs: Default::default(),
            fndefs: Default::default(),
            valdefs: Default::default(),
            ctxdefs: Default::default(),
            locals: Default::default(),
            instrs: Default::default(),
            refs: Default::default(),
            bodies: Default::default(),
        };
        let empty = ir.statics.push(StaticInstr::Ctx {
            slots: IdRange::new(&mut ir.items, Vec::new()),
        });
        let body = IdRange {
            start: empty,
            end: ir.statics.len_idx(),
        };
        ir.empty = Static::new(body, empty);
        ir
    }
}

impl IR {
    pub fn empty(&self) -> Static {
        self.empty
    }
}

type ModuleNames<T> = HashMap<(ModuleId, StrId), T>;

fn get_name<T: Copy>(
    prelude: ModuleId,
    current: ModuleId,
    names: &ModuleNames<T>,
    key: (ModuleId, StrId),
) -> Option<T> {
    let (module, name) = key;
    if let Some(&id) = names.get(&(module, name)) {
        Some(id)
    } else if module == current
        && let Some(&id) = names.get(&(prelude, name))
    {
        Some(id)
    } else {
        None
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Named {
    Module(ModuleId),
    Tydef(TydefId),
    Tagdef(TagdefId),
    Aliasdef(AliasdefId),
    Fndef(FndefId),
    Valdef(ValdefId),
    Ctxdef(CtxdefId),
}

impl Named {
    pub fn module(self) -> ModuleId {
        match self {
            Named::Module(id) => id,
            _ => panic!(),
        }
    }

    pub fn tydef(self) -> TydefId {
        match self {
            Named::Tydef(id) => id,
            _ => panic!(),
        }
    }

    pub fn tagdef(self) -> TagdefId {
        match self {
            Named::Tagdef(id) => id,
            _ => panic!(),
        }
    }

    pub fn aliasdef(self) -> AliasdefId {
        match self {
            Named::Aliasdef(id) => id,
            _ => panic!(),
        }
    }

    pub fn fndef(self) -> FndefId {
        match self {
            Named::Fndef(id) => id,
            _ => panic!(),
        }
    }

    pub fn valdef(self) -> ValdefId {
        match self {
            Named::Valdef(id) => id,
            _ => panic!(),
        }
    }

    pub fn ctxdef(self) -> CtxdefId {
        match self {
            Named::Ctxdef(id) => id,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Default)]
pub struct Names {
    pub names: HashMap<(ModuleId, StrId), Named>,
    pub attached: HashMap<(TagdefId, StrId), FndefId>,
    pub detached: HashMap<(ModuleId, StrId), FndefId>,
}

struct ErrorCtx<'a> {
    tree: &'a Tree,
    ir: &'a IR,
}

impl ErrorCtx<'_> {
    fn path(&self, id: Path) -> Inclusive {
        path_range(self.tree, id)
    }

    fn bind(&self, id: parse::BindId) -> Inclusive {
        todo!()
    }

    fn expr(&self, id: ExprId) -> Inclusive {
        expr_range(self.tree, id)
    }
}

#[derive(Debug)]
pub enum LowerError {
    Todo(TokenId, Backtrace),
    Undefined(TokenId),
    NotModule(TokenId),
    NotType(TokenId),
    NotNominal(TokenId),
    NotFn(TokenId),
    NotVal(TokenId),
    NotContext(TokenId),
    BindContext(parse::BindId),
    BindModule(parse::BindId),
    BindNominal(parse::BindId),
    BindAlias(parse::BindId),
    BindMismatch(parse::BindId),
    LitNotVal(TokenId),
    Uint32High(TokenId),
    Int32Low(TokenId),
    Int32High(TokenId),
    Uint64High(TokenId),
    Int64Low(TokenId),
    Int64High(TokenId),
    ThisNotMethod(ExprId),
    Overflow(ExprId),
    ArgCount(ExprId),
    BindMissing(parse::BindId),
}

impl LowerError {
    pub fn describe(
        self,
        _: &str,
        _: &TokenStarts,
        tree: &Tree,
        ir: &IR,
        _: &Names,
    ) -> (Option<Inclusive>, String) {
        let ctx = ErrorCtx { tree, ir };
        match self {
            LowerError::Todo(token, backtrace) => {
                (Some(single(token)), format!("TODO\n{backtrace}"))
            }
            LowerError::Undefined(token) => (Some(single(token)), "undefined".to_owned()),
            LowerError::NotModule(token) => (Some(single(token)), "not a module".to_owned()),
            LowerError::NotType(token) => (Some(single(token)), "not a type".to_owned()),
            LowerError::NotNominal(token) => (Some(single(token)), "not a nominal type".to_owned()),
            LowerError::NotVal(token) => (Some(single(token)), "not a value".to_owned()),
            LowerError::NotFn(token) => (Some(single(token)), "not a function".to_owned()),
            LowerError::NotContext(token) => {
                (Some(single(token)), "not a piece of context".to_owned())
            }
            LowerError::BindContext(bind) => (
                Some(ctx.bind(bind)),
                "cannot rebind a composite context".to_owned(),
            ),
            LowerError::BindModule(bind) => {
                (Some(ctx.bind(bind)), "cannot bind a module".to_owned())
            }
            LowerError::BindNominal(bind) => (
                Some(ctx.bind(bind)),
                "cannot bind a nominal type".to_owned(),
            ),
            LowerError::BindAlias(bind) => (
                Some(ctx.bind(bind)),
                "cannot rebind a type alias".to_owned(),
            ),
            LowerError::BindMismatch(bind) => (Some(ctx.bind(bind)), "mismatched kind".to_owned()),
            LowerError::LitNotVal(token) => (
                Some(single(token)),
                "cannot bind a literal for something other than a `val`".to_owned(),
            ),
            LowerError::Uint32High(token) => (
                Some(single(token)),
                "too high to fit in 32-bit unsigned integer".to_owned(),
            ),
            LowerError::Int32Low(token) => (
                Some(single(token)),
                "too low to fit in 32-bit signed integer".to_owned(),
            ),
            LowerError::Int32High(token) => (
                Some(single(token)),
                "too high to fit in 32-bit signed integer".to_owned(),
            ),
            LowerError::Uint64High(token) => (
                Some(single(token)),
                "too high to fit in 64-bit unsigned integer".to_owned(),
            ),
            LowerError::Int64Low(token) => (
                Some(single(token)),
                "too low to fit in 64-bit signed integer".to_owned(),
            ),
            LowerError::Int64High(token) => (
                Some(single(token)),
                "too high to fit in 64-bit signed integer".to_owned(),
            ),
            LowerError::ThisNotMethod(expr) => (
                Some(ctx.expr(expr)),
                "cannot use `this` in a function that is not a method".to_owned(),
            ),
            LowerError::Overflow(expr) => (Some(ctx.expr(expr)), "integer too large".to_owned()),
            LowerError::ArgCount(expr) => {
                (Some(ctx.expr(expr)), "wrong number of arguments".to_owned())
            }
            LowerError::BindMissing(bind) => (
                Some(ctx.bind(bind)),
                "`bind` missing an actual binding".to_owned(),
            ),
        }
    }
}

type LowerResult<T> = Result<T, LowerError>;

struct Lower<'a> {
    source: &'a str,
    starts: &'a TokenStarts,
    tree: &'a Tree,
    ir: &'a mut IR,
    names: &'a mut Names,
    base: Option<Base>,
    prelude: ModuleId,
    module: ModuleId,
    imports: &'a [ModuleId],
    tydefs: IndexVec<parse::TydefId, TydefId>,
    tagdefs: IndexVec<parse::TagdefId, TagdefId>,
    aliasdefs: IndexVec<parse::AliasdefId, AliasdefId>,
    funcdefs: IndexVec<parse::FuncdefId, FndefId>,
    attachdefs: IndexVec<parse::AttachdefId, (TagdefId, FndefId)>,
    detachdefs: IndexVec<parse::DetachdefId, FndefId>,
    valdefs: IndexVec<parse::ValdefId, ValdefId>,
    ctxdefs: IndexVec<parse::CtxdefId, CtxdefId>,
    funcs: Vec<(parse::FuncdefId, FndefId)>,
    attaches: Vec<(parse::AttachdefId, TagdefId, FndefId)>,
    detaches: Vec<(parse::DetachdefId, FndefId)>,
    drills: IndexSet<Drill>,
}

impl<'a> Index<DrillId> for Lower<'a> {
    type Output = Drill;

    fn index(&self, index: DrillId) -> &Self::Output {
        &self.drills[index.index()]
    }
}

impl<'a> Lower<'a> {
    fn todo(&self, token: TokenId) -> LowerError {
        LowerError::Todo(token, Backtrace::capture())
    }

    fn slice(&self, token: TokenId) -> &'a str {
        &self.source[relex(self.source, self.starts, token)]
    }

    fn name(&mut self, token: TokenId) -> StrId {
        self.ir.strings.make_id(self.slice(token))
    }

    fn lit(&mut self, token: TokenId) -> LowerResult<(Val, Option<IntType>)> {
        let slice = self.slice(token);
        match slice.chars().next().unwrap() {
            '0'..='9' => {
                let (digits, int_type) = if let Some(digits) = slice.strip_suffix("u32") {
                    (digits, IntType::Uint32)
                } else if let Some(digits) = slice.strip_suffix("i32") {
                    (digits, IntType::Int32)
                } else if let Some(digits) = slice.strip_suffix("u64") {
                    (digits, IntType::Uint64)
                } else if let Some(digits) = slice.strip_suffix("i64") {
                    (digits, IntType::Int64)
                } else if let Some(digits) = slice.strip_suffix("u") {
                    (digits, IntType::Uint)
                } else {
                    (slice, IntType::Int)
                };
                let val = match (digits.parse::<u32>(), digits.parse::<i32>()) {
                    (Ok(n), Ok(_)) => Val::Uint31(n),
                    (Ok(n), Err(_)) => Val::Uint32(n),
                    (Err(_), Ok(n)) => Val::Int32(n),
                    (Err(_), Err(_)) => match (digits.parse::<u64>(), digits.parse::<i64>()) {
                        (Ok(n), Ok(_)) => Val::Uint63(n),
                        (Ok(n), Err(_)) => Val::Uint64(n),
                        (Err(_), Ok(n)) => Val::Int64(n),
                        (Err(_), Err(_)) => {
                            // TODO: Check that it's actually all digits.
                            let string = self.ir.strings.make_id(digits);
                            if digits.starts_with('-') {
                                Val::Int(string)
                            } else {
                                Val::Uint(string)
                            }
                        }
                    },
                };
                Ok((val, Some(int_type)))
            }
            '"' => {
                let escaped = string(self.source, self.starts, token);
                let string = self.ir.strings.make_id(&escaped);
                Ok((Val::String(string), None))
            }
            _ => unreachable!(),
        }
    }

    fn empty_drill(&self) -> DrillId {
        DrillId::from_usize(self.drills.get_index_of(&None).unwrap())
    }

    fn drill(&mut self, drill: Drill) -> DrillId {
        let (i, _) = self.drills.insert_full(drill);
        DrillId::from_usize(i)
    }

    fn resolve_prefix(&mut self, path: Path) -> LowerResult<(ModuleId, StrId)> {
        let mut module = self.module;
        for name in path.prefix {
            let token = self.tree.names[name];
            let string = self.name(token);
            match get_name(
                self.prelude,
                self.module,
                &self.names.names,
                (module, string),
            ) {
                Some(Named::Module(next)) => module = next,
                Some(_) => return Err(LowerError::NotModule(token)),
                None => return Err(LowerError::Undefined(token)),
            }
        }
        let name = self.name(path.last);
        Ok((module, name))
    }

    fn path(&mut self, path: Path) -> LowerResult<Named> {
        let (module, name) = self.resolve_prefix(path)?;
        get_name(self.prelude, self.module, &self.names.names, (module, name))
            .ok_or(LowerError::Undefined(path.last))
    }

    fn detached(&mut self, path: Path) -> LowerResult<FndefId> {
        let (module, name) = self.resolve_prefix(path)?;
        get_name(
            self.prelude,
            self.module,
            &self.names.detached,
            (module, name),
        )
        .ok_or(LowerError::Undefined(path.last))
    }

    fn method(&mut self, ty: TypeId, name: StrId) -> Option<FndefId> {
        if let Type::Nominal(tagdef, _) = self.ir.types[ty.index()]
            && let Some(&fndef) = self.names.attached.get(&(tagdef, name))
        {
            Some(fndef)
        } else if let Some(&fndef) = self.names.detached.get(&(self.module, name)) {
            Some(fndef)
        } else if let Some(&fndef) = self.names.detached.get(&(self.prelude, name)) {
            Some(fndef)
        } else {
            None
        }
    }

    fn imports(&mut self) -> LowerResult<()> {
        for (import, &module) in self.tree.imports.iter().zip(self.imports) {
            if let Some(token) = import.name {
                let name = self.name(token);
                let named = Named::Module(module);
                self.names.names.insert((self.module, name), named);
            }
            for using in import.names {
                let token = self.tree.names[using];
                let name = self.name(token);
                let key = (self.module, name);
                match self.names.names.get(&(module, name)) {
                    None => return Err(LowerError::Undefined(token)),
                    Some(&Named::Module(module)) => {
                        self.names.names.insert(key, Named::Module(module));
                    }
                    Some(&Named::Tydef(tydef)) => {
                        self.names.names.insert(key, Named::Tydef(tydef));
                    }
                    Some(&Named::Tagdef(tagdef)) => {
                        self.names.names.insert(key, Named::Tagdef(tagdef));
                    }
                    Some(&Named::Aliasdef(aliasdef)) => {
                        self.names.names.insert(key, Named::Aliasdef(aliasdef));
                    }
                    Some(&Named::Fndef(fndef)) => {
                        self.names.names.insert(key, Named::Fndef(fndef));
                    }
                    Some(&Named::Valdef(valdef)) => {
                        self.names.names.insert(key, Named::Valdef(valdef));
                    }
                    Some(&Named::Ctxdef(ctxdef)) => {
                        self.names.names.insert(key, Named::Ctxdef(ctxdef));
                    }
                }
            }
            for using in import.methods {
                let token = self.tree.names[using];
                let name = self.name(token);
                match self.names.detached.get(&(module, name)) {
                    None => return Err(LowerError::Undefined(token)),
                    Some(&fndef) => {
                        self.names.detached.insert((self.module, name), fndef);
                    }
                }
            }
        }
        Ok(())
    }

    fn names(&mut self) -> LowerResult<()> {
        let mut next_ty = self.ir.tydefs.next_idx();
        let mut next_tag = self.ir.tagdefs.next_idx();
        let mut next_alias = self.ir.aliasdefs.next_idx();
        let mut next_fn = self.ir.fndefs.next_idx();
        let mut next_val = self.ir.valdefs.next_idx();
        let mut next_ctx = self.ir.ctxdefs.next_idx();
        // We must process nominal type names before we can attach methods to them.
        self.tagdefs = (self.tree.tagdefs.iter())
            .map(|item| {
                let id = next_tag;
                next_tag += 1;
                let name = self.name(item.name);
                self.names
                    .names
                    .insert((self.module, name), Named::Tagdef(id));
                id
            })
            .collect();
        self.attachdefs = (self.tree.attachdefs.iter())
            .map(|item| {
                let id = next_fn;
                next_fn += 1;
                let fn_name = self.name(item.fndef.name);
                let ty_name = self.name(item.ty);
                // TODO: Prevent attaching methods to nominal types defined in other modules.
                match self.names.names.get(&(self.module, ty_name)) {
                    Some(&Named::Tagdef(tagdef)) => {
                        self.names.attached.insert((tagdef, fn_name), id);
                        Ok((tagdef, id))
                    }
                    Some(_) => Err(LowerError::NotNominal(item.ty)),
                    None => Err(LowerError::Undefined(item.ty)),
                }
            })
            .collect::<LowerResult<_>>()?;
        // Other kinds of things can happen in arbitrary order.
        self.tydefs = (self.tree.tydefs.iter())
            .map(|item| {
                let id = next_ty;
                next_ty += 1;
                let name = self.name(item.name);
                self.names
                    .names
                    .insert((self.module, name), Named::Tydef(id));
                id
            })
            .collect();
        self.aliasdefs = (self.tree.aliasdefs.iter())
            .map(|item| {
                let id = next_alias;
                next_alias += 1;
                let name = self.name(item.name);
                self.names
                    .names
                    .insert((self.module, name), Named::Aliasdef(id));
                id
            })
            .collect();
        self.funcdefs = (self.tree.funcdefs.iter())
            .map(|item| {
                let id = next_fn;
                next_fn += 1;
                let name = self.name(item.fndef.name);
                self.names
                    .names
                    .insert((self.module, name), Named::Fndef(id));
                id
            })
            .collect();
        self.detachdefs = (self.tree.detachdefs.iter())
            .map(|item| {
                let id = next_fn;
                next_fn += 1;
                let name = self.name(item.fndef.name);
                self.names.detached.insert((self.module, name), id);
                id
            })
            .collect();
        self.valdefs = (self.tree.valdefs.iter())
            .map(|item| {
                let id = next_val;
                next_val += 1;
                let name = self.name(item.name);
                self.names
                    .names
                    .insert((self.module, name), Named::Valdef(id));
                id
            })
            .collect();
        self.ctxdefs = (self.tree.ctxdefs.iter())
            .map(|item| {
                let id = next_ctx;
                next_ctx += 1;
                let name = self.name(item.name);
                self.names
                    .names
                    .insert((self.module, name), Named::Ctxdef(id));
                id
            })
            .collect();
        Ok(())
    }

    fn emit(&mut self, stat: StaticInstr) -> StaticId {
        self.ir.statics.push(stat)
    }

    fn invoke_open(
        &mut self,
        param: Static,
        destruct: &[StaticId],
        target: Static,
    ) -> LowerResult<Vec<StaticId>> {
        todo!()
    }

    fn extract_ty(
        &mut self,
        param: Static,
        destruct: &[StaticId],
        def: TydefId,
        construct: &[StaticId],
    ) -> LowerResult<StaticId> {
        todo!()
    }

    fn extract_fn(
        &mut self,
        param: Static,
        destruct: &[StaticId],
        def: FndefId,
        construct: &[StaticId],
    ) -> LowerResult<StaticId> {
        todo!()
    }

    fn extract_val(
        &mut self,
        param: Static,
        destruct: &[StaticId],
        def: ValdefId,
        construct: &[StaticId],
    ) -> LowerResult<StaticId> {
        todo!()
    }

    fn extract_ctx(
        &mut self,
        param: Static,
        destruct: &[StaticId],
        def: CtxdefId,
        construct: &[StaticId],
    ) -> LowerResult<StaticId> {
        todo!()
    }

    /// Resolve the path of a spec and synthesize each of its attached bindings.
    ///
    /// The `param` gives the meaning of [`StaticInstr::Param`] in this context.
    ///
    /// The `destruct` list gives the set of entrypoints that can be used to synthesize bindings.
    fn spec(
        &mut self,
        param: Static,
        destruct: &[StaticId],
        spec: parse::Spec,
    ) -> LowerResult<(Named, Vec<StaticId>)> {
        let Spec { dot, path, binds } = spec;
        let named = if dot {
            Named::Fndef(self.detached(path)?)
        } else {
            self.path(path)?
        };
        let construct = binds
            .into_iter()
            .map(|bind| self.bind(param, &destruct, bind))
            .collect::<LowerResult<Vec<StaticId>>>()?;
        Ok((named, construct))
    }

    fn bind(
        &mut self,
        param: Static,
        slots: &[StaticId],
        bind: parse::BindId,
    ) -> LowerResult<StaticId> {
        let parse::Bind { key, val } = self.tree.binds[bind];
        let (lhs, destruct) = self.spec(param, slots, key)?;
        match val {
            None => match lhs {
                Named::Tydef(def) => {
                    let target = self.ir.tydefs[def].ctx;
                    let construct = self.invoke_open(param, &destruct, target)?;
                    let bind = self.extract_ty(param, &slots, def, &construct)?;
                    let params = IdRange::new(&mut self.ir.items, construct);
                    let args = params;
                    Ok(self.emit(StaticInstr::BindTydef {
                        def,
                        params,
                        bind,
                        args,
                    }))
                }
                Named::Fndef(def) => {
                    let target = self.ir.fndefs[def].ctx;
                    let construct = self.invoke_open(param, &destruct, target)?;
                    let bind = self.extract_fn(param, &slots, def, &construct)?;
                    let params = IdRange::new(&mut self.ir.items, construct);
                    let args = params;
                    Ok(self.emit(StaticInstr::BindFndef {
                        def,
                        params,
                        bind,
                        args,
                    }))
                }
                Named::Valdef(def) => {
                    let target = self.ir.valdefs[def].ctx;
                    let construct = self.invoke_open(param, &destruct, target)?;
                    let bind = self.extract_val(param, &slots, def, &construct)?;
                    let params = IdRange::new(&mut self.ir.items, construct);
                    let args = params;
                    Ok(self.emit(StaticInstr::BindValdef {
                        def,
                        params,
                        bind,
                        args,
                    }))
                }
                Named::Ctxdef(def) => {
                    let target = self.ir.ctxdefs[def].ctx;
                    let construct = self.invoke_open(param, &destruct, target)?;
                    let bind = self.extract_ctx(param, &slots, def, &construct)?;
                    let params = IdRange::new(&mut self.ir.items, construct);
                    let args = params;
                    Ok(self.emit(StaticInstr::BindCtxdef {
                        def,
                        params,
                        bind,
                        args,
                    }))
                }
                Named::Module(_) => Err(LowerError::BindModule(bind)),
                Named::Tagdef(_) => Err(LowerError::BindNominal(bind)),
                Named::Aliasdef(_) => Err(LowerError::BindAlias(bind)),
            },
            Some(parse::Entry::Lit(token)) => match lhs {
                Named::Valdef(valdef) => {
                    let (val, _) = self.lit(token)?;
                    let lit = self.emit(StaticInstr::Lit { val });
                    Ok(self.emit(StaticInstr::BindValdef {
                        def: valdef,
                        ctx: ctx1,
                        bind: lit,
                    }))
                }
                _ => Err(LowerError::LitNotVal(token)),
            },
            Some(parse::Entry::Ref(spec)) => {
                let (rhs, ctx2) = self.spec(spec)?;
                match (lhs, rhs) {
                    (Named::Tydef(tydef), Named::Tydef(def)) => {
                        let ty = self.ty(Type::Opaque(def, ctx2));
                        ctx.tys.entry(tydef).or_default().insert(ctx1, Some(ty));
                    }
                    (Named::Tydef(tydef), Named::Tagdef(def)) => {
                        let ty = self.ty(Type::Nominal(def, ctx2));
                        ctx.tys.entry(tydef).or_default().insert(ctx1, Some(ty));
                    }
                    (Named::Tydef(tydef), Named::Aliasdef(def)) => {
                        let ty = self.ty(Type::Alias(def, ctx2));
                        ctx.tys.entry(tydef).or_default().insert(ctx1, Some(ty));
                    }
                    (Named::Fndef(fndef), Named::Fndef(def)) => {
                        let f = Func { ctx: ctx2, def };
                        ctx.fns.entry(fndef).or_default().insert(ctx1, Some(f));
                    }
                    (Named::Valdef(valdef), Named::Valdef(def)) => {
                        let val = Val::Opaque(ctx2, def);
                        ctx.vals.entry(valdef).or_default().insert(ctx1, Some(val));
                    }
                    (Named::Ctxdef(_), _) => Err(LowerError::BindContext(bind)),
                    (Named::Module(_), _) => Err(LowerError::BindModule(bind)),
                    (Named::Tagdef(_), _) => Err(LowerError::BindNominal(bind)),
                    (Named::Aliasdef(_), _) => Err(LowerError::BindAlias(bind)),
                    _ => Err(LowerError::BindMismatch(bind)),
                }
            }
        }
    }

    fn needs(&mut self, param: Static, needs: IdRange<parse::NeedId>) -> LowerResult<Static> {
        let start = self.ir.statics.next_idx();
        let mut slots = Vec::new();
        for need in needs {
            let parse::Need { kind: _, bind } = self.tree.needs[need];
            // TODO: Handle `kind`.
            let parse::Bind { key, val } = self.tree.binds[bind];
            let slot = match val {
                Some(_) => self.bind(param, &slots, bind)?,
                None => {
                    let (lhs, destruct) = self.spec(param, &slots, key)?;
                    match lhs {
                        Named::Tydef(def) => {
                            let target = self.ir.tydefs[def].ctx;
                            let construct = self.invoke_open(param, &destruct, target)?;
                            let params = IdRange::new(&mut self.ir.items, construct);
                            self.emit(StaticInstr::NeedTydef { def, params })
                        }
                        Named::Fndef(def) => {
                            let target = self.ir.fndefs[def].ctx;
                            let construct = self.invoke_open(param, &destruct, target)?;
                            let params = IdRange::new(&mut self.ir.items, construct);
                            self.emit(StaticInstr::NeedFndef { def, params })
                        }
                        Named::Valdef(def) => {
                            let target = self.ir.valdefs[def].ctx;
                            let construct = self.invoke_open(param, &destruct, target)?;
                            let params = IdRange::new(&mut self.ir.items, construct);
                            self.emit(StaticInstr::NeedValdef { def, params })
                        }
                        Named::Ctxdef(def) => {
                            let target = self.ir.ctxdefs[def].ctx;
                            let construct = self.invoke_open(param, &destruct, target)?;
                            let params = IdRange::new(&mut self.ir.items, construct);
                            self.emit(StaticInstr::NeedCtxdef { def, params })
                        }
                        Named::Module(_) => return Err(LowerError::BindModule(bind)),
                        Named::Tagdef(_) => return Err(LowerError::BindNominal(bind)),
                        Named::Aliasdef(_) => return Err(LowerError::BindAlias(bind)),
                    }
                }
            };
            slots.push(slot);
        }
        let slots = IdRange::new(&mut self.ir.items, slots);
        let ctx = self.emit(StaticInstr::Ctx { slots });
        let end = self.ir.statics.next_idx();
        Ok(Static::new(IdRange { start, end }, ctx))
    }

    fn parse_ty(&mut self, ty: parse::TypeId) -> LowerResult<TypeId> {
        match self.tree.types[ty] {
            parse::Type::Spec(spec) => {
                let (named, ctx) = self.spec(spec)?;
                let ty = match named {
                    Named::Tydef(tydef) => Type::Opaque(tydef, ctx),
                    Named::Tagdef(tagdef) => Type::Nominal(tagdef, ctx),
                    Named::Aliasdef(aliasdef) => Type::Alias(aliasdef, ctx),
                    _ => return Err(LowerError::NotType(spec.path.last)),
                };
                Ok(self.ty(ty))
            }
            parse::Type::Tuple(elems) => {
                let lowered = elems
                    .into_iter()
                    .map(|elem| self.parse_ty(elem))
                    .collect::<LowerResult<Vec<TypeId>>>()?;
                Ok(self.ty_tuple(&lowered))
            }
            parse::Type::Record(members) => todo!(),
        }
    }

    fn parse_fndef(&mut self, fndef: parse::Fndef) -> LowerResult<Fndef> {
        let types = (fndef.params.into_iter())
            .map(|param| self.parse_ty(self.tree.params[param].ty))
            .collect::<LowerResult<Vec<TypeId>>>()?;
        Ok(Fndef {
            ctx: self.needs(fndef.needs)?,
            param: self.ty_tuple(&types),
            result: match fndef.result {
                parse::Return::Unit => self.ty_unit(),
                parse::Return::Type(ty) => self.parse_ty(ty)?,
                parse::Return::Bind(needs) => {
                    let ctx = self.needs(needs)?;
                    self.ty(Type::Bind(ctx))
                }
            },
        })
    }

    fn decls(&mut self) -> LowerResult<()> {
        let empty = self.ir.empty();
        for &decl in &self.tree.decls {
            match decl {
                parse::Decl::Tydef(id) => {
                    let parse::Tydef { name, needs } = self.tree.tydefs[id];
                    let tydef = Tydef {
                        ctx: self.needs(empty, needs)?,
                    };
                    let lowered = self.ir.tydefs.push(tydef);
                    let string = self.name(name);
                    self.names
                        .names
                        .insert((self.module, string), Named::Tydef(lowered));
                }
                parse::Decl::Tagdef(id) => todo!(),
                parse::Decl::Aliasdef(id) => todo!(),
                parse::Decl::Funcdef(id) => todo!(),
                parse::Decl::Attachdef(id) => todo!(),
                parse::Decl::Detachdef(id) => todo!(),
                parse::Decl::Valdef(id) => todo!(),
                parse::Decl::Ctxdef(id) => todo!(),
            }
        }
        for (id, &parse::Tydef { name, needs }) in self.tree.tydefs.iter_enumerated() {
            let id = self.tydefs[id];
            drop(name);
            let tydef = Tydef {
                ctx: self.needs(needs)?,
            };
            assert_eq!(self.ir.tydefs.push(tydef), id);
        }
        for (id, &parse::Tagdef { name, needs, def }) in self.tree.tagdefs.iter_enumerated() {
            let id = self.tagdefs[id];
            drop(name);
            let tagdef = Tagdef {
                ctx: self.needs(needs)?,
                inner: self.parse_ty(def)?,
            };
            assert_eq!(self.ir.tagdefs.push(tagdef), id);
        }
        for (id, &parse::Aliasdef { name, needs, def }) in self.tree.aliasdefs.iter_enumerated() {
            let id = self.aliasdefs[id];
            drop(name);
            let aliasdef = Aliasdef {
                ctx: self.needs(needs)?,
                def: self.parse_ty(def)?,
            };
            assert_eq!(self.ir.aliasdefs.push(aliasdef), id);
        }
        for (parse_id, &parse::Funcdef { fndef }) in self.tree.funcdefs.iter_enumerated() {
            let id = self.funcdefs[parse_id];
            self.funcs.push((parse_id, id));
            let fndef = self.parse_fndef(fndef)?;
            assert_eq!(self.ir.fndefs.push(fndef), id);
        }
        for (parse_id, &parse::Attachdef { ty, fndef }) in self.tree.attachdefs.iter_enumerated() {
            drop(ty);
            let (tagdef, id) = self.attachdefs[parse_id];
            self.attaches.push((parse_id, tagdef, id));
            let fndef = self.parse_fndef(fndef)?;
            assert_eq!(self.ir.fndefs.push(fndef), id);
        }
        for (parse_id, &parse::Detachdef { fndef }) in self.tree.detachdefs.iter_enumerated() {
            let id = self.detachdefs[parse_id];
            self.detaches.push((parse_id, id));
            let fndef = self.parse_fndef(fndef)?;
            assert_eq!(self.ir.fndefs.push(fndef), id);
        }
        for (id, &parse::Valdef { name, needs, ty }) in self.tree.valdefs.iter_enumerated() {
            let id = self.valdefs[id];
            drop(name);
            let valdef = Valdef {
                ctx: self.needs(needs)?,
                ty: self.parse_ty(ty)?,
            };
            assert_eq!(self.ir.valdefs.push(valdef), id);
        }
        for (id, &parse::Ctxdef { name, needs, def }) in self.tree.ctxdefs.iter_enumerated() {
            let id = self.ctxdefs[id];
            drop(name);
            let ctxdef = Ctxdef {
                ctx: self.needs(needs)?,
                def: self.needs(def)?,
            };
            assert_eq!(self.ir.ctxdefs.push(ctxdef), id);
        }
        Ok(())
    }

    fn bodies(&mut self) -> LowerResult<()> {
        let ctx = self.ctx(Ctx::default()); // This will get overwritten when we call `body`.
        for (funcdef, id_decl) in take(&mut self.funcs) {
            let parse::Funcdef { fndef } = self.tree.funcdefs[funcdef];
            let start = Body {
                x: self,
                locals: HashMap::new(),
                ctx,
            }
            .body(fndef, id_decl)?;
            let id_body = self.ir.bodies.push(start);
            assert_eq!(id_decl, id_body);
        }
        for (attachdef, _tagdef, id_decl) in take(&mut self.attaches) {
            // TODO: Handle `this`.
            let parse::Attachdef { ty: _, fndef } = self.tree.attachdefs[attachdef];
            let start = Body {
                x: self,
                locals: HashMap::new(),
                ctx,
            }
            .body(fndef, id_decl)?;
            let id_body = self.ir.bodies.push(start);
            assert_eq!(id_decl, id_body);
        }
        for (detachdef, id_decl) in take(&mut self.detaches) {
            // TODO: Handle `this`.
            let parse::Detachdef { fndef } = self.tree.detachdefs[detachdef];
            let start = Body {
                x: self,
                locals: HashMap::new(),
                ctx,
            }
            .body(fndef, id_decl)?;
            let id_body = self.ir.bodies.push(start);
            assert_eq!(id_decl, id_body);
        }
        Ok(())
    }

    fn program(&mut self) -> LowerResult<()> {
        self.imports()?;
        // TODO: Don't make declaration order significant.
        self.decls()?;
        self.bodies()?;
        Ok(())
    }
}

struct Body<'a, 'b> {
    x: &'b mut Lower<'a>,
    locals: HashMap<StrId, LocalId>,
    ctx: CtxId,
}

impl Body<'_, '_> {
    fn base(&self) -> Base {
        self.x.base.unwrap()
    }

    fn get(&mut self, token: TokenId) -> LowerResult<LocalId> {
        let name = self.x.name(token);
        match self.locals.get(&name) {
            Some(&local) => Ok(local),
            None => Err(LowerError::Undefined(token)),
        }
    }

    fn set(&mut self, token: TokenId, local: LocalId) {
        let name = self.x.name(token);
        self.locals.insert(name, local);
    }

    fn instr(&mut self, ty: TypeId, instr: Instr) -> LocalId {
        let id_local = self.x.ir.locals.push(ty);
        let id_instr = self.x.ir.instrs.push(instr);
        assert_eq!(id_local, id_instr);
        id_local
    }

    fn instr_tuple(&mut self, ty: TypeId, locals: &[LocalId]) -> LocalId {
        let start = self.x.ir.refs.len_idx();
        self.x.ir.refs.extend_from_slice(IndexSlice::new(locals));
        let end = self.x.ir.refs.len_idx();
        self.instr(ty, Instr::Tuple(IdRange { start, end }))
    }

    fn instr_record(&mut self, ty: TypeId, locals: &[LocalId]) -> LocalId {
        let start = self.x.ir.refs.len_idx();
        self.x.ir.refs.extend_from_slice(IndexSlice::new(locals));
        let end = self.x.ir.refs.len_idx();
        self.instr(ty, Instr::Record(ty, IdRange { start, end }))
    }

    fn expr(&mut self, expr: ExprId) -> LowerResult<LocalId> {
        match self.x.tree.exprs[expr] {
            Expr::Lit(token) => {
                let Base {
                    types,
                    lit_types,
                    lits,
                } = self.base();
                let (tydef_lit, tydef, fndef) = match self.x.lit(token)? {
                    // Unreachable cases.
                    (Val::Opaque(_, _), _) => unreachable!(),
                    (
                        Val::Uint31(_)
                        | Val::Uint32(_)
                        | Val::Int32(_)
                        | Val::Uint63(_)
                        | Val::Uint64(_)
                        | Val::Int64(_)
                        | Val::Uint(_)
                        | Val::Int(_),
                        None,
                    ) => unreachable!(),
                    (Val::Char(_) | Val::String(_), Some(_)) => unreachable!(),
                    (
                        Val::Int32(_) | Val::Int64(_) | Val::Int(_),
                        Some(IntType::Uint32 | IntType::Uint64 | IntType::Uint),
                    ) => unreachable!(),

                    // Integer literals ending in `u32`.
                    (Val::Uint31(_), Some(IntType::Uint32)) => {
                        (lit_types.uint31, types.uint32, lits.uint31_realize_uint32)
                    }
                    (Val::Uint32(_), Some(IntType::Uint32)) => {
                        (lit_types.uint32, types.uint32, lits.uint32_realize_uint32)
                    }
                    (Val::Uint63(_) | Val::Uint64(_) | Val::Uint(_), Some(IntType::Uint32)) => {
                        return Err(LowerError::Uint32High(token));
                    }

                    // Integer literals ending in `i32`.
                    (Val::Uint31(_), Some(IntType::Int32)) => {
                        (lit_types.uint31, types.int32, lits.uint31_realize_int32)
                    }
                    (Val::Int32(_), Some(IntType::Int32)) => {
                        (lit_types.int32, types.int32, lits.int32_realize_int32)
                    }
                    (Val::Int64(_) | Val::Int(_), Some(IntType::Int32)) => {
                        return Err(LowerError::Int32Low(token));
                    }
                    (
                        Val::Uint32(_) | Val::Uint63(_) | Val::Uint64(_) | Val::Uint(_),
                        Some(IntType::Int32),
                    ) => {
                        return Err(LowerError::Int32High(token));
                    }

                    // Integer literals ending in `u64`.
                    (Val::Uint31(_), Some(IntType::Uint64)) => {
                        (lit_types.uint31, types.uint64, lits.uint31_realize_uint64)
                    }
                    (Val::Uint32(_), Some(IntType::Uint64)) => {
                        (lit_types.uint32, types.uint64, lits.uint32_realize_uint64)
                    }
                    (Val::Uint63(_), Some(IntType::Uint64)) => {
                        (lit_types.uint63, types.uint64, lits.uint63_realize_uint64)
                    }
                    (Val::Uint64(_), Some(IntType::Uint64)) => {
                        (lit_types.uint64, types.uint64, lits.uint64_realize_uint64)
                    }
                    (Val::Uint(_), Some(IntType::Uint64)) => {
                        return Err(LowerError::Uint64High(token));
                    }

                    // Integer literals ending in `i64`.
                    (Val::Uint31(_), Some(IntType::Int64)) => {
                        (lit_types.uint31, types.int64, lits.uint31_realize_int64)
                    }
                    (Val::Uint32(_), Some(IntType::Int64)) => {
                        (lit_types.uint32, types.int64, lits.uint32_realize_int64)
                    }
                    (Val::Int32(_), Some(IntType::Int64)) => {
                        (lit_types.int32, types.int64, lits.int32_realize_int64)
                    }
                    (Val::Uint63(_), Some(IntType::Int64)) => {
                        (lit_types.uint63, types.int64, lits.uint63_realize_int64)
                    }
                    (Val::Int64(_), Some(IntType::Int64)) => {
                        (lit_types.int64, types.int64, lits.int64_realize_int64)
                    }
                    (Val::Int(_), Some(IntType::Int64)) => return Err(LowerError::Int64Low(token)),
                    (Val::Uint64(_) | Val::Uint(_), Some(IntType::Int64)) => {
                        return Err(LowerError::Int64High(token));
                    }

                    // Integer literals ending in `u`.
                    (Val::Uint31(_), Some(IntType::Uint)) => {
                        (lit_types.uint31, types.uint, lits.uint31_realize_uint)
                    }
                    (Val::Uint32(_), Some(IntType::Uint)) => {
                        (lit_types.uint32, types.uint, lits.uint32_realize_uint)
                    }
                    (Val::Uint63(_), Some(IntType::Uint)) => {
                        (lit_types.uint63, types.uint, lits.uint63_realize_uint)
                    }
                    (Val::Uint64(_), Some(IntType::Uint)) => {
                        (lit_types.uint64, types.uint, lits.uint64_realize_uint)
                    }
                    (Val::Uint(_), Some(IntType::Uint)) => {
                        (lit_types.uint, types.uint, lits.uint_realize_uint)
                    }

                    // Integer literals with no suffix.
                    (Val::Uint31(_), Some(IntType::Int)) => {
                        (lit_types.uint31, types.int, lits.uint31_realize_int)
                    }
                    (Val::Uint32(_), Some(IntType::Int)) => {
                        (lit_types.uint32, types.int, lits.uint32_realize_int)
                    }
                    (Val::Int32(_), Some(IntType::Int)) => {
                        (lit_types.int32, types.int, lits.int32_realize_int)
                    }
                    (Val::Uint63(_), Some(IntType::Int)) => {
                        (lit_types.uint63, types.int, lits.uint63_realize_int)
                    }
                    (Val::Uint64(_), Some(IntType::Int)) => {
                        (lit_types.uint64, types.int, lits.uint64_realize_int)
                    }
                    (Val::Int64(_), Some(IntType::Int)) => {
                        (lit_types.int64, types.int, lits.int64_realize_int)
                    }
                    (Val::Uint(_), Some(IntType::Int)) => {
                        (lit_types.uint, types.int, lits.uint_realize_int)
                    }
                    (Val::Int(_), Some(IntType::Int)) => {
                        (lit_types.int, types.int, lits.int_realize_int)
                    }

                    // Other literals.
                    (Val::Char(_), None) => (lit_types.char, types.char, lits.char_realize),
                    (Val::String(_), None) => (lit_types.string, types.string, lits.string_realize),
                };
                // TODO: Bind the actual literal itself.
                let ty_result = self.x.ty(Type::Opaque(tydef, self.x.ir.empty_ctx()));
                let ty_unit = self.x.ty_unit();
                let arg = self.instr_tuple(ty_unit, &[]);
                Ok(self.instr(ty_result, Instr::Call(fndef, arg)))
            }
            Expr::Path(path) => {
                let name = self.x.name(path.last);
                if path.prefix.is_empty()
                    && let Some(&local) = self.locals.get(&name)
                {
                    return Ok(local);
                }
                let Named::Valdef(valdef) = self.x.path(path)? else {
                    return Err(LowerError::NotVal(path.last));
                };
                let ty = self.x.ir.valdefs[valdef].ty;
                Ok(self.instr(ty, Instr::Val(valdef, self.ctx)))
            }
            Expr::Tag(path, inner) => {
                let Named::Tagdef(tagdef) = self.x.path(path)? else {
                    return Err(LowerError::NotNominal(path.last));
                };
                let local = self.expr(inner)?;
                let ty = self.x.ir.locals[local];
                Ok(self.instr(ty, Instr::Nominal(tagdef, local)))
            }
            Expr::Record(_lbrace, fields, _rbrace) => {
                let sorted = fields
                    .into_iter()
                    .map(|field| {
                        let Field { name, val } = self.x.tree.fields[field];
                        Ok((self.x.slice(name), self.expr(val)?))
                    })
                    .collect::<LowerResult<BTreeMap<&str, LocalId>>>()?;
                let fields = sorted
                    .iter()
                    .map(|(&string, &local)| {
                        (self.x.ir.strings.make_id(string), self.x.ir.locals[local])
                    })
                    .collect::<Vec<(StrId, TypeId)>>();
                let ty = self.x.ty_record(&fields);
                let locals = sorted.values().copied().collect::<Vec<LocalId>>();
                Ok(self.instr_record(ty, &locals))
            }
            Expr::Field(object, field) => {
                let obj = self.expr(object)?;
                let name = self.x.name(field);
                let fields =
                    &self.x.ir.records[self.x.ir.types[self.x.ir.locals[obj].index()].record()];
                // TODO: Make this not be linear time.
                let id = FieldId::from_usize(
                    fields.iter().position(|&(field, _)| field == name).unwrap(),
                );
                let (_, ty) = fields[id.index()];
                Ok(self.instr(ty, Instr::Field(obj, id)))
            }
            Expr::Method(object, method, args) => {
                let obj = self.expr(object)?;
                let ty = self.x.ir.locals[obj];
                let name = self.x.name(method);
                let Some(fndef) = self.x.method(ty, name) else {
                    return Err(LowerError::Undefined(method));
                };
                let Fndef {
                    ctx: _,
                    param,
                    result,
                } = self.x.ir.fndefs[fndef];
                let params = &self.x.ir.tuples[self.x.ir.types[param.index()].tuple()];
                if args.len() != params.len() {
                    return Err(LowerError::ArgCount(expr));
                }
                let locals = args
                    .into_iter()
                    .map(|arg| self.expr(arg))
                    .collect::<LowerResult<Vec<LocalId>>>()?;
                let arg = self.instr_tuple(param, &locals);
                // TODO: Contextually set `this` to `obj`.
                Ok(self.instr(result, Instr::Call(fndef, arg)))
            }
            Expr::Call(callee, binds, args) => {
                let Named::Fndef(fndef) = self.x.path(callee)? else {
                    return Err(LowerError::NotFn(callee.last));
                };
                let Fndef {
                    ctx: _,
                    param,
                    result,
                } = self.x.ir.fndefs[fndef];
                // TODO: Handle bindings attached to function calls.
                let params = &self.x.ir.tuples[self.x.ir.types[param.index()].tuple()];
                if args.len() != params.len() {
                    return Err(LowerError::ArgCount(expr));
                }
                let locals = args
                    .into_iter()
                    .map(|arg| self.expr(arg))
                    .collect::<LowerResult<Vec<LocalId>>>()?;
                let arg = self.instr_tuple(param, &locals);
                let call = self.instr(result, Instr::Call(fndef, arg));
                Ok(call)
            }
            Expr::Unary(op, inner) => {
                let v = self.expr(inner)?;
                match op {
                    Unop::Neg => todo!(),
                    Unop::Not => todo!(),
                }
            }
            Expr::Binary(left, op, right) => {
                let l = self.expr(left)?;
                let r = self.expr(right)?;
                match op {
                    Binop::Eq => todo!(),
                    Binop::Ne => todo!(),
                    Binop::Lt => todo!(),
                    Binop::Gt => todo!(),
                    Binop::Le => todo!(),
                    Binop::Ge => todo!(),
                    Binop::Add => todo!(),
                    Binop::Sub => todo!(),
                    Binop::Mul => todo!(),
                    Binop::Div => todo!(),
                    Binop::Rem => todo!(),
                    Binop::Shl => todo!(),
                    Binop::Shr => todo!(),
                    Binop::And => todo!(),
                    Binop::Or => todo!(),
                    Binop::Xor => todo!(),
                }
            }
            Expr::If(cond, yes, no) => {
                let unit = self.x.ty_unit();
                let cond = self.expr(cond)?;
                // We don't know the type yet.
                let start = self.instr(unit, Instr::If(cond, unit));
                let mut local = self.block(yes)?;
                // Rewrite the original instruction now that we know the type.
                let ty = self.x.ir.locals[local];
                self.x.ir.instrs[start] = Instr::If(cond, ty);
                if let Some(block) = no {
                    self.instr(ty, Instr::Else(local));
                    local = self.block(block)?;
                }
                Ok(self.instr(ty, Instr::EndIf(local)))
            }
            Expr::Bind(_, bindings) => {
                let mut context = Ctx::default();
                for binding in bindings {
                    match self.x.tree.bindings[binding] {
                        parse::Binding::Single(bind) => {
                            let parse::Bind { key, val } = self.x.tree.binds[bind];
                            if val.is_none() {
                                // Should we instead use this syntax to allow for binding a context
                                // which has previously been assigned to a local variable?
                                return Err(LowerError::BindMissing(bind));
                            }
                            self.x.bind(&mut context, bind)?;
                            let parse::Bind { key, val } = self.x.tree.binds[bind];
                            let (lhs, ctx1) = self.x.spec(key)?;
                            match val {
                                // Should we instead use this syntax to allow for binding a context
                                // which has previously been assigned to a local variable?
                                None => return Err(LowerError::BindMissing(bind)),
                                Some(parse::Entry::Lit(token)) => todo!(),
                                Some(parse::Entry::Ref(spec)) => {
                                    let (rhs, ctx2) = self.x.spec(spec)?;
                                    match (lhs, rhs) {
                                        (Named::Tydef(tydef), Named::Tydef(def)) => {
                                            let (local, slot) = self.extract_ty(def, ctx2)?;
                                            todo!();
                                        }
                                        (Named::Tydef(tydef), Named::Tagdef(def)) => {
                                            todo!();
                                        }
                                        (Named::Tydef(tydef), Named::Aliasdef(def)) => {
                                            todo!();
                                        }
                                        (Named::Fndef(fndef), Named::Fndef(def)) => {
                                            let (local, slot) = self.extract_fn(def, ctx2)?;
                                            todo!();
                                        }
                                        (Named::Valdef(valdef), Named::Valdef(def)) => {
                                            let (local, slot) = self.extract_val(def, ctx2)?;
                                            todo!();
                                        }
                                        (Named::Ctxdef(_), _) => {
                                            return Err(LowerError::BindContext(bind));
                                        }
                                        (Named::Module(_), _) => {
                                            return Err(LowerError::BindModule(bind));
                                        }
                                        (Named::Tagdef(_), _) => {
                                            return Err(LowerError::BindNominal(bind));
                                        }
                                        (Named::Aliasdef(_), _) => {
                                            return Err(LowerError::BindAlias(bind));
                                        }
                                        _ => return Err(LowerError::BindMismatch(bind)),
                                    }
                                }
                            }
                        }
                        parse::Binding::Composite(expr) => {
                            let local = self.expr(expr)?;
                            todo!();
                        }
                    }
                }
                todo!()
            }
        }
    }

    fn stmts(&mut self, stmts: IdRange<StmtId>) -> LowerResult<()> {
        for stmt in stmts {
            match self.x.tree.stmts[stmt] {
                Stmt::Let(name, rhs) => {
                    let local = self.expr(rhs)?;
                    let ty = self.x.ir.locals[local];
                    let copy = self.instr(ty, Instr::Copy(local));
                    self.set(name, copy);
                }
                Stmt::Var(name, rhs) => {
                    let local = self.expr(rhs)?;
                    let ty = self.x.ir.locals[local];
                    let copy = self.instr(ty, Instr::Copy(local));
                    self.set(name, copy);
                }
                Stmt::Assign(lhs, rhs) => match self.x.tree.exprs[lhs] {
                    Expr::Path(path) => {
                        assert!(path.prefix.is_empty());
                        let before = self.get(path.last)?;
                        let after = self.expr(rhs)?;
                        let unit = self.x.ty_unit();
                        self.instr(unit, Instr::Set(before, after));
                    }
                    _ => panic!(),
                },
                Stmt::While(cond, body) => {
                    assert!(body.expr.is_none());
                    let unit = self.x.ty_unit();
                    self.instr(unit, Instr::Loop);
                    let local = self.expr(cond)?;
                    self.instr(unit, Instr::If(local, unit));
                    self.stmts(body.stmts)?;
                    let fake = self.instr(unit, Instr::Br(Depth(1)));
                    self.instr(unit, Instr::EndIf(fake));
                    self.instr(unit, Instr::EndLoop);
                }
                Stmt::Expr(expr) => {
                    self.expr(expr)?;
                }
            }
        }
        Ok(())
    }

    fn block(&mut self, block: Block) -> LowerResult<LocalId> {
        self.stmts(block.stmts)?;
        match block.expr {
            Some(expr) => Ok(self.expr(expr)?),
            None => {
                let ty = self.x.ty_unit();
                Ok(self.instr_tuple(ty, &[]))
            }
        }
    }

    fn body(&mut self, fndef: parse::Fndef, id_decl: FndefId) -> LowerResult<Option<LocalId>> {
        let parse::Fndef {
            name: _,
            needs: _,
            params,
            result: _,
            def,
        } = fndef;
        let Some(body) = def else { return Ok(None) };
        let start = self.x.ir.instrs.len_idx();
        let Fndef {
            ctx,
            param: tuple_ty,
            result: ret_ty,
        } = self.x.ir.fndefs[id_decl];
        self.ctx = ctx;
        let tuple_local = self.instr(tuple_ty, Instr::Param);
        let types = self.x.ir.types[tuple_ty.index()].tuple();
        let mut index = 0;
        for (param, tuple_loc) in params.into_iter().zip(types) {
            let ty = self.x.ir.tuples[tuple_loc];
            let local = self.instr(ty, Instr::Elem(tuple_local, ElemId::new(index)));
            let name = self.x.tree.params[param].name;
            self.set(name, local);
            index += 1;
        }
        let ret = self.block(body)?;
        self.instr(ret_ty, Instr::Return(ret));
        Ok(Some(start))
    }
}

pub fn lower(
    source: &str,
    starts: &TokenStarts,
    tree: &Tree,
    ir: &mut IR,
    names: &mut Names,
    base: Option<Base>,
    prelude: ModuleId,
    imports: &[ModuleId],
) -> LowerResult<ModuleId> {
    assert_eq!(tree.imports.len(), imports.len());
    let module = ir.modules.push(());
    let mut drills = IndexSet::new();
    drills.insert(None);
    let mut lower = Lower {
        source,
        starts,
        tree,
        ir,
        names,
        base,
        prelude,
        module,
        imports,
        tydefs: IndexVec::new(),
        tagdefs: IndexVec::new(),
        aliasdefs: IndexVec::new(),
        funcdefs: IndexVec::new(),
        attachdefs: IndexVec::new(),
        detachdefs: IndexVec::new(),
        valdefs: IndexVec::new(),
        ctxdefs: IndexVec::new(),
        funcs: Vec::new(),
        attaches: Vec::new(),
        detaches: Vec::new(),
        drills,
    };
    lower.program()?;
    Ok(module)
}
