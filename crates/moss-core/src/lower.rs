use std::{
    backtrace::Backtrace,
    cmp::Ordering,
    collections::{BTreeMap, HashMap, HashSet},
    mem::take,
    sync::atomic::{AtomicUsize, Ordering as AtomicOrdering},
};

use index_vec::{IndexSlice, IndexVec, define_index_type};
use indexmap::IndexMap;

use crate::{
    dump::dump,
    intern::{StrId, Strings},
    lex::{TokenId, TokenStarts, relex, string},
    parse::{self, Binop, Block, ExprId, Field, Path, Spec, Stmt, StmtId, Tree, Unop},
    prelude::Base,
    range::{Inclusive, expr_range, path_range, single},
    util::IdRange,
};

define_index_type! {
    /// The unique ID of a module, i.e. a source file, in the `modules` field of the [`IR`].
    pub struct ModuleId = u32;
}

define_index_type! {
    /// The index of a [`Instr`] in the `instrs` field of the [`IR`].
    pub struct InstrId = u32;
}

define_index_type! {
    /// The index of a [`InstrId`] in the `items` field of the [`IR`].
    pub struct ItemId = u32;
}

define_index_type! {
    /// The index of a [`StrId`] and [`InstrId`] in the `records` field of the [`IR`].
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
    /// The index of a [`Sigdef`] in the `sigdefs` field of the [`IR`].
    pub struct SigdefId = u32;
}

define_index_type! {
    /// The index of a [`Sigdef`]/[`Body`] in the `fndefs`/`bodies` field of the [`IR`].
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

pub type InstrList = IdRange<ItemId>;

static TRACE_INVOKE: AtomicUsize = AtomicUsize::new(0);
static TRACE_SPECIFICITY: AtomicUsize = AtomicUsize::new(0);
static TRACE_QUERY_CTX: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
struct InstrMap(HashMap<InstrId, InstrId>);

impl InstrMap {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn insert(&mut self, key: InstrId, val: InstrId) {
        let prev = self.0.insert(key, val);
        assert!(prev.is_none());
    }

    /// Return the instruction that `instr` is mapped to.
    ///
    /// If `instr` is not mapped to anything explicitly, this method assumes `instr` itself is
    /// already in scope, and just returns it.
    fn get(&self, instr: InstrId) -> InstrId {
        self.0.get(&instr).copied().unwrap_or(instr)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Depth(pub u32);

/// An instruction that produces a runtime value of some type.
#[derive(Clone, Copy, Debug)]
pub enum Expr {
    /// Get the value of the parameter to this function.
    ///
    /// Type: this function's parameter type.
    Param {
        /// This function's parameter type.
        ty: InstrId,
    },

    /// Copy the value of another local into a fresh local.
    ///
    /// Type: same as the given local.
    Copy {
        /// The value to copy.
        value: InstrId,
    },

    /// Construct a value of a nominal type given a value of its inner type.
    ///
    /// Type: the given nominal type.
    Nominal {
        /// The nominal type.
        ty: InstrId,

        /// The inner value.
        inner: InstrId,
    },

    /// Construct a tuple.
    ///
    /// Type: tuple with the given element types.
    Tuple {
        /// The elements of the tuple to be constructed.
        elems: InstrList,
    },

    /// Construct a record.
    ///
    /// Type: the given record type.
    Record {
        /// The record fields and their names, in lexicographical order.
        fields: IdRange<RecordId>,
    },

    /// Get an element of a tuple.
    ///
    /// Type: the element type.
    Elem {
        /// The tuple.
        tuple: InstrId,

        /// The static index of the tuple element to access.
        index: ElemId,
    },

    /// Get a field of a record.
    ///
    /// Type: the field type.
    Field {
        /// The record.
        record: InstrId,

        /// The static index of the record field to access.
        index: FieldId,
    },

    /// Get a contextual value.
    Val {
        /// The value.
        val: InstrId,
    },

    /// Call a contextual function.
    ///
    /// Type: the function's result type.
    Call {
        /// The function.
        func: InstrId,

        /// The runtime argument value.
        arg: InstrId,
    },
}

/// An instruction.
#[derive(Clone, Copy, Debug)]
pub enum Instr {
    /// Start a lambda block.
    Lambda,

    /// End the current [`Instr::Lambda`] block.
    EndLambda {
        /// The [`Instr::Lambda`] instruction this closes.
        start: InstrId,

        /// The item to return from the lambda.
        result: InstrId,
    },

    /// Apply a lambda to some arguments.
    Apply {
        /// The lambda to apply.
        lambda: InstrId,

        /// The arguments.
        args: InstrList,
    },

    /// A tuple of items.
    Stack {
        /// The items to stack together.
        items: InstrList,
    },

    /// Need a contextual type parametrized by a specific context.
    NeedTydef {
        /// The contextual type declaration.
        def: TydefId,

        /// A mapping from unfixed inputs to the full inputs of the contextual type.
        param: InstrId,
    },

    /// Need a contextual function parametrized by a specific context.
    NeedSigdef {
        /// The contextual function declaration.
        def: SigdefId,

        /// A mapping from unfixed inputs to the full inputs of the contextual function.
        param: InstrId,
    },

    /// Need a contextual value parametrized by a specific context.
    NeedValdef {
        /// The contextual value declaration.
        def: ValdefId,

        /// A mapping from unfixed inputs to the full inputs of the contextual value.
        param: InstrId,
    },

    /// Need a composite context parametrized by a specific context.
    NeedCtxdef {
        /// The context definition.
        def: CtxdefId,

        /// A mapping from unfixed inputs to the full inputs of the composite context parameter.
        param: InstrId,
    },

    /// A lambda from context to a specific nominal type.
    Tagdef {
        /// The nominal type definition.
        def: TagdefId,
    },

    /// A lambda from context to a specific type alias.
    Aliasdef {
        /// The type alias definition.
        def: AliasdefId,
    },

    /// A structural tuple of other types.
    Tuple {
        /// The types of the tuple elements.
        elems: InstrList,
    },

    /// A structural record of other types.
    Record {
        /// The types and field names of the record elements, in lexicographical order.
        fields: IdRange<RecordId>,
    },

    /// A "type" that represents a context instead of an actual value.
    Context,

    /// A lambda from context to a specific defined function.
    Fndef {
        /// The function definition.
        def: FndefId,
    },

    /// A slot from a context.
    Get {
        /// The context.
        ctx: InstrId,

        /// The output slot index.
        slot: SlotId,
    },

    /// A literal value.
    Lit {
        /// The literal value.
        val: Val,
    },

    /// A binding.
    Bind {
        /// The arguments constraining the left-hand side of the binding.
        args: InstrList,

        /// The right-hand side of the binding.
        bind: InstrId,
    },

    /// Provide a contextual type parametrized by a specific context.
    BindTydef {
        /// The contextual type declaration.
        def: TydefId,

        /// A mapping from unfixed inputs to a binding for a fully concrete type.
        bind: InstrId,
    },

    /// Provide a contextual function parametrized by a specific context.
    BindSigdef {
        /// The contextual function declaration.
        def: SigdefId,

        /// A mapping from unfixed inputs to a binding for a fully concrete function.
        bind: InstrId,
    },

    /// Provide a contextual value parametrized by a specific context.
    BindValdef {
        /// The contextual value declaration.
        def: ValdefId,

        /// A mapping from unfixed inputs to a binding for a fully concrete value.
        bind: InstrId,
    },

    /// Provide a composite context parametrized by a specific context.
    BindCtxdef {
        /// The composite context definition.
        def: CtxdefId,

        /// A mapping from unfixed inputs to a binding for the full inputs of the composite context.
        bind: InstrId,
    },

    /// A function signature.
    Sig {
        /// The type of the tuple of parameters.
        param: InstrId,

        /// The result type or context.
        result: InstrId,
    },

    /// Set the left-hand variable to the right-hand value of the right-hand value.
    Set {
        /// The variable to set.
        lhs: InstrId,

        /// The value to use.
        rhs: InstrId,
    },

    /// Start a block only if the given condition is true.
    If {
        /// The result type of this conditional block.
        ty: InstrId,

        /// The boolean value to test.
        cond: InstrId,
    },

    /// Start a block only if the preceding [`Instr::If`] condition was false.
    Else {
        /// The value to return from the `if` branch.
        result: InstrId,
    },

    /// End the current [`Instr::Else`] block.
    EndIf {
        /// The value to return from the `else` branch.
        result: InstrId,
    },

    /// Start a loop block.
    Loop,

    /// End the current [`Instr::Loop`] block.
    EndLoop,

    /// Branch to the end of the block with the given depth, or the beginning if it's a loop block.
    Br {
        /// The number of blocks to exit.
        depth: Depth,
    },

    /// An instruction whose value is typed by the value of a previous instruction.
    Expr {
        /// The type.
        ty: InstrId,

        /// The value.
        expr: Expr,
    },
}

#[derive(Clone, Copy, Debug)]
pub struct Body {
    pub body: IdRange<InstrId>,
}

impl Body {
    fn new(body: IdRange<InstrId>, result: InstrId) -> Self {
        assert_eq!(body.last(), Some(result));
        Self { body }
    }

    pub fn result(self) -> InstrId {
        self.body.last().unwrap()
    }
}

struct Builder {
    start: InstrId,
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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Val {
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntType {
    Uint32,
    Int32,
    Uint64,
    Int64,
    Uint,
    Int,
}

/// A lambda that takes contextual parameters and returns nothing.
#[derive(Clone, Copy, Debug)]
pub struct Tydef(pub Body);

/// A lambda that takes contextual parameters and returns the inner type for this nominal type.
#[derive(Clone, Copy, Debug)]
pub struct Tagdef(pub Body);

/// A lambda that takes contextual parameters and returns a type.
#[derive(Clone, Copy, Debug)]
pub struct Aliasdef(pub Body);

/// A lambda that takes contextual parameters and returns a function signature.
#[derive(Clone, Copy, Debug)]
pub struct Sigdef(pub Body);

/// A lambda that takes contextual parameters and returns a type.
#[derive(Clone, Copy, Debug)]
pub struct Valdef(pub Body);

/// A lambda that returns a lambda that returns a context.
#[derive(Clone, Copy, Debug)]
pub struct Ctxdef(pub Body);

#[derive(Debug, Default)]
pub struct IR {
    /// Basically just counts how many modules there are.
    pub modules: IndexVec<ModuleId, ()>,

    /// String interning.
    pub strings: Strings,

    /// Arena of instructions.
    ///
    /// Basically composed of a sequence of "blocks" where each block is a contiguous range of
    /// instructions in this arena.
    pub instrs: IndexVec<InstrId, Instr>,

    /// Arena of indices into the instructions array.
    ///
    /// Used for instructions that contain lists of references to other instruction IDs.
    pub items: IndexVec<ItemId, InstrId>,

    /// Arena of (string, instruction) pairs, like `items` but used for record fields.
    pub records: IndexVec<RecordId, (StrId, InstrId)>,

    /// Contextual types, i.e. ones that aren't given a definition in the source code.
    pub tydefs: IndexVec<TydefId, Tydef>,

    /// Nominal types.
    pub tagdefs: IndexVec<TagdefId, Tagdef>,

    /// Type aliases.
    pub aliasdefs: IndexVec<AliasdefId, Aliasdef>,

    /// Contextual functions.
    pub sigdefs: IndexVec<SigdefId, Sigdef>,

    /// The number of contextual parameters on each function declaration.
    pub sig_arity: IndexVec<SigdefId, usize>,

    /// Functions that are given definitions.
    pub fndefs: IndexVec<FndefId, Sigdef>,

    /// The number of contextual parameters on each function definition.
    pub fn_arity: IndexVec<FndefId, usize>,

    /// Contextual values.
    pub valdefs: IndexVec<ValdefId, Valdef>,

    /// Composite contexts.
    pub ctxdefs: IndexVec<CtxdefId, Ctxdef>,

    /// Actual function bodies, when they have been lowered.
    pub bodies: IndexVec<FndefId, Option<Body>>,
}

type ModuleNames<T> = IndexMap<(ModuleId, StrId), T>;

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

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Named {
    Module(ModuleId),
    Tydef(TydefId),
    Tagdef(TagdefId),
    Aliasdef(AliasdefId),
    Sigdef(SigdefId),
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

    pub fn sigdef(self) -> SigdefId {
        match self {
            Named::Sigdef(id) => id,
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

#[derive(Clone, Copy, Debug)]
pub enum NamedFn {
    Sigdef(SigdefId),
    Fndef(FndefId),
}

impl NamedFn {
    pub fn sigdef(self) -> SigdefId {
        match self {
            NamedFn::Sigdef(id) => id,
            _ => panic!(),
        }
    }

    pub fn fndef(self) -> FndefId {
        match self {
            NamedFn::Fndef(id) => id,
            _ => panic!(),
        }
    }
}

impl From<NamedFn> for Named {
    fn from(named: NamedFn) -> Self {
        match named {
            NamedFn::Sigdef(sigdef) => Named::Sigdef(sigdef),
            NamedFn::Fndef(fndef) => Named::Fndef(fndef),
        }
    }
}

#[derive(Debug, Default)]
pub struct Names {
    pub names: IndexMap<(ModuleId, StrId), Named>,
    pub attached: IndexMap<(TagdefId, StrId), NamedFn>,
    pub detached: IndexMap<(ModuleId, StrId), NamedFn>,
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
    BindDefined(parse::BindId),
    BindMismatch(parse::BindId),
    LitNotVal(TokenId),
    Uint32High(TokenId),
    Int32Low(TokenId),
    Int32High(TokenId),
    Uint64High(TokenId),
    Int64Low(TokenId),
    Int64High(TokenId),
    ThisNotMethod(ExprId),
    ArgCount(ExprId),
    Overflow(ExprId),
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
            LowerError::BindDefined(bind) => (
                Some(ctx.bind(bind)),
                "cannot rebind a defined function".to_owned(),
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
            LowerError::ArgCount(expr) => {
                (Some(ctx.expr(expr)), "wrong number of arguments".to_owned())
            }
            LowerError::Overflow(expr) => (Some(ctx.expr(expr)), "integer too large".to_owned()),
            LowerError::BindMissing(bind) => (
                Some(ctx.bind(bind)),
                "`bind` missing an actual binding".to_owned(),
            ),
        }
    }
}

type LowerResult<T> = Result<T, LowerError>;

#[derive(Debug)]
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
    skip_bodies: bool,
    funcs: Vec<(parse::FuncdefId, FndefId)>,
    attaches: Vec<(parse::AttachdefId, TagdefId, FndefId)>,
    detaches: Vec<(parse::DetachdefId, FndefId)>,
    bind_ctx_items_cache: HashMap<InstrId, Option<Vec<InstrId>>>,
    need_ctx_items_cache: HashMap<(CtxdefId, InstrId), Option<Vec<InstrId>>>,
    materialized_lambdas: HashMap<InstrId, InstrId>,
    invoke_cache: HashMap<(InstrId, Vec<InstrId>), Option<Vec<InstrId>>>,
    invoke_active: HashSet<(InstrId, Vec<InstrId>)>,
    specificity_cache: HashMap<(InstrId, InstrId), bool>,
    specificity_active: HashSet<(InstrId, InstrId)>,
    ctx_contains_ty_cache: HashMap<(CtxdefId, TydefId), bool>,
    ctx_contains_ty_active: HashSet<(CtxdefId, TydefId)>,
    ctx_contains_sig_cache: HashMap<(CtxdefId, SigdefId), bool>,
    ctx_contains_sig_active: HashSet<(CtxdefId, SigdefId)>,
    ctx_contains_val_cache: HashMap<(CtxdefId, ValdefId), bool>,
    ctx_contains_val_active: HashSet<(CtxdefId, ValdefId)>,
    ctx_contains_ctx_cache: HashMap<(CtxdefId, CtxdefId), bool>,
    ctx_contains_ctx_active: HashSet<(CtxdefId, CtxdefId)>,
}

impl<'a> Lower<'a> {
    fn trace_hotspot(&self, counter: &AtomicUsize, label: &str, message: impl FnOnce() -> String) {
        if std::env::var_os("MOSS_TRACE").is_none() {
            return;
        }
        let count = counter.fetch_add(1, AtomicOrdering::Relaxed) + 1;
        if count.is_multiple_of(100) {
            eprintln!("{label}#{count} {}", message());
        }
    }

    fn todo(&self, token: TokenId) -> LowerError {
        if std::env::var_os("MOSS_DUMP_TODO").is_some() {
            dump(self.ir, self.names);
        }
        LowerError::Todo(token, Backtrace::capture())
    }

    fn todo_no_loc(&self) -> LowerError {
        self.todo(TokenId::from_raw(0))
    }

    fn slice(&self, token: TokenId) -> &'a str {
        &self.source[relex(self.source, self.starts, token)]
    }

    fn spec_string(&self, spec: parse::Spec) -> String {
        let Spec { dot, path, binds } = spec;
        let mut s = String::new();
        if dot {
            s.push('.');
        }
        for name in path.prefix {
            s.push_str(self.slice(self.tree.names[name]));
            s.push_str("::");
        }
        s.push_str(self.slice(path.last));
        if !binds.is_empty() {
            s.push('[');
            let mut first = true;
            for bind in binds {
                if !first {
                    s.push_str(", ");
                }
                first = false;
                let parse::Bind { key, val } = self.tree.binds[bind];
                s.push_str(&self.spec_string(key));
                if let Some(entry) = val {
                    s.push('=');
                    match entry {
                        parse::Entry::Lit(token) => s.push_str(self.slice(token)),
                        parse::Entry::Ref(spec) => s.push_str(&self.spec_string(spec)),
                    }
                }
            }
            s.push(']');
        }
        s
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

    fn detached(&mut self, path: Path) -> LowerResult<NamedFn> {
        let (module, name) = self.resolve_prefix(path)?;
        get_name(
            self.prelude,
            self.module,
            &self.names.detached,
            (module, name),
        )
        .ok_or(LowerError::Undefined(path.last))
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
                    Some(&named) => {
                        self.names.names.insert(key, named);
                    }
                }
            }
            for using in import.methods {
                let token = self.tree.names[using];
                let name = self.name(token);
                match self.names.detached.get(&(module, name)) {
                    None => return Err(LowerError::Undefined(token)),
                    Some(&named) => {
                        self.names.detached.insert((self.module, name), named);
                    }
                }
            }
        }
        Ok(())
    }

    fn builder(&self) -> Builder {
        Builder {
            start: self.ir.instrs.next_idx(),
        }
    }

    fn items(&mut self, instrs: &[InstrId]) -> InstrList {
        let start = self.ir.items.len_idx();
        self.ir.items.extend_from_slice(IndexSlice::new(instrs));
        let end = self.ir.items.len_idx();
        IdRange { start, end }
    }

    fn map_items(&mut self, mapped: &InstrMap, items: InstrList) -> InstrList {
        let items_mapped = Vec::from_iter(
            items
                .into_iter()
                .map(|item| mapped.get(self.ir.items[item])),
        );
        self.items(&items_mapped)
    }

    fn emit(&mut self, instr: Instr) -> InstrId {
        self.ir.instrs.push(instr)
    }

    fn finish(&self, builder: Builder, result: InstrId) -> Body {
        let Builder { start } = builder;
        let end = self.ir.instrs.next_idx();
        Body::new(IdRange { start, end }, result)
    }

    fn ty_tuple(&mut self, elems: Vec<InstrId>) -> InstrId {
        let elems = self.items(&elems);
        self.emit(Instr::Tuple { elems })
    }

    fn ty_unit(&mut self) -> InstrId {
        self.ty_tuple(Vec::new())
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

    fn duplicate(&mut self, mapped: &mut InstrMap, instr: InstrId) -> LowerResult<()> {
        let mapped_instr = match self.ir.instrs[instr] {
            Instr::Lambda => self.emit(Instr::Lambda),
            Instr::EndLambda { start, result } => self.emit(Instr::EndLambda {
                start: mapped.get(start),
                result: mapped.get(result),
            }),
            Instr::Apply { lambda, args } => {
                let args_mapped = self.map_items(mapped, args);
                self.emit(Instr::Apply {
                    lambda: mapped.get(lambda),
                    args: args_mapped,
                })
            }
            Instr::Stack { items } => {
                let items_mapped = self.map_items(mapped, items);
                self.emit(Instr::Stack {
                    items: items_mapped,
                })
            }
            Instr::NeedTydef { def, param } => self.emit(Instr::NeedTydef {
                def,
                param: mapped.get(param),
            }),
            Instr::NeedSigdef { def, param } => self.emit(Instr::NeedSigdef {
                def,
                param: mapped.get(param),
            }),
            Instr::NeedValdef { def, param } => self.emit(Instr::NeedValdef {
                def,
                param: mapped.get(param),
            }),
            Instr::NeedCtxdef { def, param } => self.emit(Instr::NeedCtxdef {
                def,
                param: mapped.get(param),
            }),
            Instr::Tagdef { def } => self.emit(Instr::Tagdef { def }),
            Instr::Aliasdef { def } => self.emit(Instr::Aliasdef { def }),
            Instr::Tuple { elems } => {
                let elems_mapped = self.map_items(mapped, elems);
                self.emit(Instr::Tuple {
                    elems: elems_mapped,
                })
            }
            Instr::Record { fields } => todo!(),
            Instr::Context => self.emit(Instr::Context),
            Instr::Fndef { def } => self.emit(Instr::Fndef { def }),
            Instr::Get { ctx, slot } => self.emit(Instr::Get {
                ctx: mapped.get(ctx),
                slot,
            }),
            Instr::Lit { val } => self.emit(Instr::Lit { val }),
            Instr::Bind { args, bind } => {
                let args_mapped = self.map_items(mapped, args);
                self.emit(Instr::Bind {
                    args: args_mapped,
                    bind: mapped.get(bind),
                })
            }
            Instr::BindTydef { def, bind } => self.emit(Instr::BindTydef {
                def,
                bind: mapped.get(bind),
            }),
            Instr::BindSigdef { def, bind } => self.emit(Instr::BindSigdef {
                def,
                bind: mapped.get(bind),
            }),
            Instr::BindValdef { def, bind } => self.emit(Instr::BindValdef {
                def,
                bind: mapped.get(bind),
            }),
            Instr::BindCtxdef { def, bind } => self.emit(Instr::BindCtxdef {
                def,
                bind: mapped.get(bind),
            }),
            Instr::Sig { param, result } => self.emit(Instr::Sig {
                param: mapped.get(param),
                result: mapped.get(result),
            }),
            Instr::Set { lhs, rhs } => todo!(),
            Instr::If { ty, cond } => todo!(),
            Instr::Else { result } => todo!(),
            Instr::EndIf { result } => todo!(),
            Instr::Loop => todo!(),
            Instr::EndLoop => todo!(),
            Instr::Br { depth } => todo!(),
            Instr::Expr { ty, expr } => todo!(),
        };
        mapped.insert(instr, mapped_instr);
        Ok(())
    }

    fn duplicate_range(
        &mut self,
        mapped: &mut InstrMap,
        instrs: IdRange<InstrId>,
    ) -> LowerResult<()> {
        for instr in instrs {
            self.duplicate(mapped, instr)?;
        }
        Ok(())
    }

    fn stack_instrs(&self, instr: InstrId) -> Option<Vec<InstrId>> {
        match self.ir.instrs[instr] {
            Instr::Stack { items } => Some(Vec::from_iter(items.into_iter().map(|item| self.ir.items[item]))),
            _ => None,
        }
    }

    fn bind_ctx_items(&mut self, bind: InstrId) -> LowerResult<Option<Vec<InstrId>>> {
        if let Some(cached) = self.bind_ctx_items_cache.get(&bind) {
            return Ok(cached.clone());
        }
        let result = self.materialize_lambda(bind)?;
        let Instr::Bind { bind: ctx, .. } = self.ir.instrs[result] else {
            return Err(self.todo_no_loc());
        };
        let items = self.stack_instrs(ctx);
        self.bind_ctx_items_cache.insert(bind, items.clone());
        Ok(items)
    }

    fn need_ctx_items(&mut self, def: CtxdefId, param: InstrId) -> LowerResult<Option<Vec<InstrId>>> {
        let key = (def, param);
        if let Some(cached) = self.need_ctx_items_cache.get(&key) {
            return Ok(cached.clone());
        }
        let construct = self.materialize_lambda(param)?;
        let Some(construct) = self.stack_instrs(construct) else {
            return Err(self.todo_no_loc());
        };
        let Ctxdef(body) = self.ir.ctxdefs[def];
        let inner = self.inline(body, &construct)?;
        let inner = self.materialize_lambda(inner)?;
        let items = self.stack_instrs(inner);
        self.need_ctx_items_cache.insert(key, items.clone());
        Ok(items)
    }

    fn slot_domain(&self, slot: InstrId) -> Option<InstrId> {
        match self.ir.instrs[slot] {
            Instr::NeedTydef { param, .. }
            | Instr::NeedSigdef { param, .. }
            | Instr::NeedValdef { param, .. }
            | Instr::NeedCtxdef { param, .. } => Some(param),
            Instr::BindTydef { bind, .. }
            | Instr::BindSigdef { bind, .. }
            | Instr::BindValdef { bind, .. }
            | Instr::BindCtxdef { bind, .. } => Some(bind),
            _ => None,
        }
    }

    fn compare_slot_specificity(&mut self, left: InstrId, right: InstrId) -> LowerResult<Option<Ordering>> {
        if left == right {
            return Ok(Some(Ordering::Equal));
        }
        let Some(left_domain) = self.slot_domain(left) else {
            return Ok(None);
        };
        let Some(right_domain) = self.slot_domain(right) else {
            return Ok(None);
        };
        let left_more = self.left_domain_more_specific(left_domain, right_domain)?;
        let right_more = self.left_domain_more_specific(right_domain, left_domain)?;
        Ok(match (left_more, right_more) {
            (true, true) => Some(Ordering::Equal),
            (true, false) => Some(Ordering::Greater),
            (false, true) => Some(Ordering::Less),
            (false, false) => None,
        })
    }

    fn merge_slot_queries(
        &mut self,
        current: Query<InstrId>,
        next: Query<InstrId>,
    ) -> LowerResult<Query<InstrId>> {
        Ok(match (current, next) {
            (Query::Missing, rhs) => rhs,
            (lhs, Query::Missing) => lhs,
            (Query::Ambiguous, _) | (_, Query::Ambiguous) => Query::Ambiguous,
            (Query::Unique(lhs), Query::Unique(rhs)) => match self.compare_slot_specificity(lhs, rhs)? {
                None => Query::Ambiguous,
                Some(Ordering::Greater | Ordering::Equal) => Query::Unique(lhs),
                Some(Ordering::Less) => Query::Unique(rhs),
            },
        })
    }

    fn inline_range(
        &mut self,
        start: InstrId,
        end: InstrId,
        result: InstrId,
        construct: Option<&[InstrId]>,
    ) -> LowerResult<InstrId> {
        let mut mapped = InstrMap::new();
        let mut next_arg = 0;
        let mut instr = start;
        while instr < end {
            match self.ir.instrs[instr] {
                Instr::Lambda => {
                    let lambda_start = instr;
                    let lambda_end = self.find_end_lambda(instr);
                    self.duplicate_range(
                        &mut mapped,
                        IdRange {
                            start: lambda_start,
                            end: lambda_end + 1,
                        },
                    )?;
                    instr = lambda_end;
                }
                Instr::NeedTydef { def, param } => {
                    let mapped_instr = match construct {
                        Some(args) if next_arg < args.len() => {
                            let arg = args[next_arg];
                            next_arg += 1;
                            arg
                        }
                        _ => self.emit(Instr::NeedTydef {
                            def,
                            param: mapped.get(param),
                        }),
                    };
                    mapped.insert(instr, mapped_instr);
                }
                Instr::NeedSigdef { def, param } => {
                    let mapped_instr = match construct {
                        Some(args) if next_arg < args.len() => {
                            let arg = args[next_arg];
                            next_arg += 1;
                            arg
                        }
                        _ => self.emit(Instr::NeedSigdef {
                            def,
                            param: mapped.get(param),
                        }),
                    };
                    mapped.insert(instr, mapped_instr);
                }
                Instr::NeedValdef { def, param } => {
                    let mapped_instr = match construct {
                        Some(args) if next_arg < args.len() => {
                            let arg = args[next_arg];
                            next_arg += 1;
                            arg
                        }
                        _ => self.emit(Instr::NeedValdef {
                            def,
                            param: mapped.get(param),
                        }),
                    };
                    mapped.insert(instr, mapped_instr);
                }
                Instr::NeedCtxdef { def, param } => {
                    let mapped_instr = match construct {
                        Some(args) if next_arg < args.len() => {
                            let arg = args[next_arg];
                            next_arg += 1;
                            arg
                        }
                        _ => self.emit(Instr::NeedCtxdef {
                            def,
                            param: mapped.get(param),
                        }),
                    };
                    mapped.insert(instr, mapped_instr);
                }
                _ => self.duplicate(&mut mapped, instr)?,
            }
            instr += 1;
        }
        Ok(mapped.get(result))
    }

    fn inline_arity(&mut self, body: Body, arity: usize, construct: &[InstrId]) -> LowerResult<InstrId> {
        let mut mapped = InstrMap::new();
        let mut next_arg = 0;
        let mut next_need = 0;
        let mut instr = body.body.start;
        while instr < body.body.end {
            match self.ir.instrs[instr] {
                Instr::Lambda => {
                    let lambda_start = instr;
                    let lambda_end = self.find_end_lambda(instr);
                    self.duplicate_range(
                        &mut mapped,
                        IdRange {
                            start: lambda_start,
                            end: lambda_end + 1,
                        },
                    )?;
                    instr = lambda_end;
                }
                Instr::NeedTydef { .. }
                | Instr::NeedSigdef { .. }
                | Instr::NeedValdef { .. }
                | Instr::NeedCtxdef { .. }
                    if next_need < arity =>
                {
                    let Some(&arg) = construct.get(next_arg) else {
                        return Err(self.todo_no_loc());
                    };
                    mapped.insert(instr, arg);
                    next_arg += 1;
                    next_need += 1;
                }
                _ => self.duplicate(&mut mapped, instr)?,
            }
            instr += 1;
        }
        if next_need != arity || next_arg != arity {
            return Err(self.todo_no_loc());
        }
        Ok(mapped.get(body.result()))
    }

    fn inline_lambda(&mut self, lambda: InstrId, construct: &[InstrId]) -> LowerResult<InstrId> {
        let Instr::EndLambda { start, result } = self.ir.instrs[lambda] else {
            panic!()
        };
        self.inline_range(start + 1, lambda, result, Some(construct))
    }

    fn materialize_lambda(&mut self, lambda: InstrId) -> LowerResult<InstrId> {
        if let Some(&result) = self.materialized_lambdas.get(&lambda) {
            return Ok(result);
        }
        let Instr::EndLambda { start, result } = self.ir.instrs[lambda] else {
            panic!()
        };
        let result = self.inline_range(start + 1, lambda, result, None)?;
        self.materialized_lambdas.insert(lambda, result);
        Ok(result)
    }

    fn apply_stack(&mut self, lambda: InstrId, args: &[InstrId]) -> LowerResult<Vec<InstrId>> {
        let result = self.apply_item(lambda, args)?;
        self.stack_instrs(result).ok_or_else(|| self.todo_no_loc())
    }

    fn apply_item(&mut self, lambda: InstrId, args: &[InstrId]) -> LowerResult<InstrId> {
        match self.ir.instrs[lambda] {
            Instr::EndLambda { .. } => self.inline_lambda(lambda, args),
            Instr::Apply { lambda, args: applied } => {
                let applied = Vec::from_iter(applied.into_iter().map(|item| self.ir.items[item]));
                let inner = self.apply_item(lambda, &applied)?;
                self.apply_item(inner, args)
            }
            Instr::NeedTydef { def, param } => {
                let construct = self.apply_stack(param, args)?;
                let Tydef(body) = self.ir.tydefs[def];
                self.inline(body, &construct)
            }
            Instr::NeedSigdef { def, param } => {
                let construct = self.apply_stack(param, args)?;
                let Sigdef(body) = self.ir.sigdefs[def];
                self.inline_arity(body, self.ir.sig_arity[def], &construct)
            }
            Instr::NeedValdef { def, param } => {
                let construct = self.apply_stack(param, args)?;
                let Valdef(body) = self.ir.valdefs[def];
                self.inline(body, &construct)
            }
            Instr::NeedCtxdef { def, param } => {
                let construct = self.apply_stack(param, args)?;
                let Ctxdef(body) = self.ir.ctxdefs[def];
                let inner = self.inline(body, &construct)?;
                self.materialize_lambda(inner)
            }
            Instr::BindTydef { bind, .. }
            | Instr::BindSigdef { bind, .. }
            | Instr::BindValdef { bind, .. }
            | Instr::BindCtxdef { bind, .. } => {
                let result = self.apply_item(bind, args)?;
                let Instr::Bind { args: _, bind } = self.ir.instrs[result] else {
                    return Err(self.todo_no_loc());
                };
                Ok(bind)
            }
            Instr::Aliasdef { def } => {
                let Aliasdef(body) = self.ir.aliasdefs[def];
                self.inline(body, args)
            }
            Instr::Fndef { def } => {
                let Sigdef(body) = self.ir.fndefs[def];
                self.inline_arity(body, self.ir.fn_arity[def], args)
            }
            Instr::Get { ctx, slot } => {
                let ctx = self.apply_item(ctx, args)?;
                let Some(items) = self.stack_instrs(ctx) else {
                    return Err(self.todo_no_loc());
                };
                Ok(items[slot.index()])
            }
            _ => Err(self.todo_no_loc()),
        }
    }

    /// Try to synthesize a mapping from the left lambda's domain to the right's.
    ///
    /// Both lambdas must currently be in scope.
    ///
    /// The synthesized mapping is not used; only its success or failure is returned as a boolean.
    fn left_domain_more_specific(&mut self, left: InstrId, right: InstrId) -> LowerResult<bool> {
        self.trace_hotspot(&TRACE_SPECIFICITY, "specificity", || {
            format!("left={left:?} right={right:?} instrs={}", self.ir.instrs.len())
        });
        if let Some(&cached) = self.specificity_cache.get(&(left, right)) {
            return Ok(cached);
        }
        if !self.specificity_active.insert((left, right)) {
            return Ok(false);
        }
        let needs = self.materialize_lambda(left)?;
        let needs = if let Some(needs) = self.stack_instrs(needs) {
            needs
        } else if let Instr::Bind { args, .. } = self.ir.instrs[needs] {
            Vec::from_iter(args.into_iter().map(|item| self.ir.items[item]))
        } else {
            self.specificity_active.remove(&(left, right));
            return Err(self.todo_no_loc());
        };
        let result = self.invoke(right, &needs)?.is_some();
        self.specificity_active.remove(&(left, right));
        self.specificity_cache.insert((left, right), result);
        Ok(result)
    }

    fn invoke_arity(
        &mut self,
        lambda: InstrId,
        body: Body,
        arity: usize,
        destruct: &[InstrId],
    ) -> LowerResult<Option<Vec<InstrId>>> {
        let key = (lambda, destruct.to_vec());
        if let Some(cached) = self.invoke_cache.get(&key) {
            return Ok(cached.clone());
        }
        if !self.invoke_active.insert(key.clone()) {
            return Ok(None);
        }
        let mut construct = Vec::new();
        let mut mapped = InstrMap::new();
        let mut next_need = 0;
        let mut instr = body.body.start;
        while instr < body.body.end && next_need < arity {
            match self.ir.instrs[instr] {
                Instr::Lambda => {
                    let start = instr;
                    let end = self.find_end_lambda(instr);
                    let range = IdRange {
                        start,
                        end: end + 1,
                    };
                    self.duplicate_range(&mut mapped, range)?;
                    instr = end;
                }
                Instr::NeedTydef { def, param } => {
                    let query = self.query_ty_lambda(destruct, def)?;
                    let Query::Unique(extracted) = query else {
                        if std::env::var_os("MOSS_TRACE_INVOKE_FAIL").is_some() {
                            eprintln!(
                                "invoke missing ty def={def:?} at instr={instr:?} in lambda={lambda:?} query={}",
                                match query {
                                    Query::Missing => "missing",
                                    Query::Ambiguous => "ambiguous",
                                    Query::Unique(_) => unreachable!(),
                                }
                            );
                        }
                        self.invoke_active.remove(&key);
                        self.invoke_cache.insert(key, None);
                        return Ok(None);
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                    next_need += 1;
                }
                Instr::NeedSigdef { def, param } => {
                    let query = self.query_sig_lambda(destruct, def)?;
                    let Query::Unique(extracted) = query else {
                        if std::env::var_os("MOSS_TRACE_INVOKE_FAIL").is_some() {
                            eprintln!(
                                "invoke missing sig def={def:?} at instr={instr:?} in lambda={lambda:?} query={}",
                                match query {
                                    Query::Missing => "missing",
                                    Query::Ambiguous => "ambiguous",
                                    Query::Unique(_) => unreachable!(),
                                }
                            );
                        }
                        self.invoke_active.remove(&key);
                        self.invoke_cache.insert(key, None);
                        return Ok(None);
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                    next_need += 1;
                }
                Instr::NeedValdef { def, param } => {
                    let query = self.query_val_lambda(destruct, def)?;
                    let Query::Unique(extracted) = query else {
                        if std::env::var_os("MOSS_TRACE_INVOKE_FAIL").is_some() {
                            eprintln!(
                                "invoke missing val def={def:?} at instr={instr:?} in lambda={lambda:?} query={}",
                                match query {
                                    Query::Missing => "missing",
                                    Query::Ambiguous => "ambiguous",
                                    Query::Unique(_) => unreachable!(),
                                }
                            );
                        }
                        self.invoke_active.remove(&key);
                        self.invoke_cache.insert(key, None);
                        return Ok(None);
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                    next_need += 1;
                }
                Instr::NeedCtxdef { def, param } => {
                    let query = self.query_ctx_lambda(destruct, def, mapped.get(param))?;
                    let extracted = match query {
                        Query::Unique(extracted) => extracted,
                        Query::Missing => self.extract_ctx(destruct, def, destruct)?,
                        Query::Ambiguous => {
                            if std::env::var_os("MOSS_TRACE_INVOKE_FAIL").is_some() {
                                eprintln!(
                                    "invoke missing ctx def={def:?} at instr={instr:?} in lambda={lambda:?} query=ambiguous"
                                );
                            }
                            self.invoke_active.remove(&key);
                            self.invoke_cache.insert(key, None);
                            return Ok(None);
                        }
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                    next_need += 1;
                }
                _ => self.duplicate(&mut mapped, instr)?,
            }
            instr += 1;
        }
        let result = if next_need == arity {
            Some(construct)
        } else {
            None
        };
        self.invoke_active.remove(&key);
        self.invoke_cache.insert(key, result.clone());
        Ok(result)
    }

    fn invoke(
        &mut self,
        lambda: InstrId,
        destruct: &[InstrId],
    ) -> LowerResult<Option<Vec<InstrId>>> {
        self.trace_hotspot(&TRACE_INVOKE, "invoke", || {
            format!(
                "lambda={lambda:?} destruct={} instrs={}",
                destruct.len(),
                self.ir.instrs.len()
            )
        });
        let key = (lambda, destruct.to_vec());
        if let Some(cached) = self.invoke_cache.get(&key) {
            return Ok(cached.clone());
        }
        if !self.invoke_active.insert(key.clone()) {
            return Ok(None);
        }
        let (start, end) = match self.ir.instrs[lambda] {
            Instr::Lambda => {
                self.invoke_active.remove(&key);
                return Err(self.todo_no_loc());
            }
            Instr::EndLambda { start, result: _ } => (start + 1, lambda),
            Instr::Apply { lambda, args } => {
                let applied = Vec::from_iter(args.into_iter().map(|item| self.ir.items[item]));
                let inner = self.apply_item(lambda, &applied)?;
                let result = self.invoke(inner, destruct)?;
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, result.clone());
                return Ok(result);
            }
            Instr::Stack { items: _ } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::NeedTydef { def: _, param }
            | Instr::NeedSigdef { def: _, param }
            | Instr::NeedValdef { def: _, param }
            | Instr::NeedCtxdef { def: _, param } => {
                let result = self.materialize_lambda(param)?;
                let Some(result) = self.stack_instrs(result) else {
                    self.invoke_active.remove(&key);
                    return Err(self.todo_no_loc());
                };
                let result = Some(result);
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, result.clone());
                return Ok(result);
            }
            Instr::Tagdef { .. } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::Aliasdef { def } => {
                let Aliasdef(body) = self.ir.aliasdefs[def];
                (body.body.start, body.body.end)
            }
            Instr::Tuple { .. } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::Record { .. } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::Context => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::Fndef { def } => {
                let Sigdef(body) = self.ir.fndefs[def];
                let arity = self.ir.fn_arity[def];
                self.invoke_active.remove(&key);
                let result = self.invoke_arity(lambda, body, arity, destruct)?;
                return Ok(result);
            }
            Instr::Get { .. } => {
                self.invoke_active.remove(&key);
                return Err(self.todo_no_loc());
            }
            Instr::Lit { .. } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::Bind { args, .. } => {
                self.invoke_active.remove(&key);
                let result = Some(Vec::from_iter(args.into_iter().map(|item| self.ir.items[item])));
                self.invoke_cache.insert(key, result.clone());
                return Ok(result);
            }
            Instr::BindTydef { bind, .. }
            | Instr::BindSigdef { bind, .. }
            | Instr::BindValdef { bind, .. }
            | Instr::BindCtxdef { bind, .. } => {
                let result = self.invoke(bind, destruct)?;
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, result.clone());
                return Ok(result);
            }
            Instr::Sig { .. } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::Set { .. } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::If { .. } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::Else { .. } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::EndIf { .. } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::Loop => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::EndLoop => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::Br { .. } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
            Instr::Expr { .. } => {
                self.invoke_active.remove(&key);
                self.invoke_cache.insert(key, Some(Vec::new()));
                return Ok(Some(Vec::new()));
            }
        };
        let mut construct = Vec::new();
        let mut mapped = InstrMap::new();
        let mut instr = start;
        while instr < end {
            match self.ir.instrs[instr] {
                Instr::Lambda => {
                    let start = instr;
                    let end = self.find_end_lambda(instr);
                    let range = IdRange {
                        start,
                        end: end + 1,
                    };
                    self.duplicate_range(&mut mapped, range)?;
                    instr = end;
                }
                Instr::NeedTydef { def, param } => {
                    let query = self.query_ty_lambda(destruct, def)?;
                    let Query::Unique(extracted) = query else {
                        if std::env::var_os("MOSS_TRACE_INVOKE_FAIL").is_some() {
                            eprintln!(
                                "invoke missing ty def={def:?} at instr={instr:?} in lambda={lambda:?} query={}",
                                match query {
                                Query::Missing => "missing",
                                Query::Ambiguous => "ambiguous",
                                Query::Unique(_) => unreachable!(),
                            });
                        }
                        self.invoke_active.remove(&key);
                        self.invoke_cache.insert(key, None);
                        return Ok(None);
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                }
                Instr::NeedSigdef { def, param } => {
                    let query = self.query_sig_lambda(destruct, def)?;
                    let Query::Unique(extracted) = query else {
                        if std::env::var_os("MOSS_TRACE_INVOKE_FAIL").is_some() {
                            eprintln!(
                                "invoke missing sig def={def:?} at instr={instr:?} in lambda={lambda:?} query={}",
                                match query {
                                Query::Missing => "missing",
                                Query::Ambiguous => "ambiguous",
                                Query::Unique(_) => unreachable!(),
                            });
                        }
                        self.invoke_active.remove(&key);
                        self.invoke_cache.insert(key, None);
                        return Ok(None);
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                }
                Instr::NeedValdef { def, param } => {
                    let query = self.query_val_lambda(destruct, def)?;
                    let Query::Unique(extracted) = query else {
                        if std::env::var_os("MOSS_TRACE_INVOKE_FAIL").is_some() {
                            eprintln!(
                                "invoke missing val def={def:?} at instr={instr:?} in lambda={lambda:?} query={}",
                                match query {
                                Query::Missing => "missing",
                                Query::Ambiguous => "ambiguous",
                                Query::Unique(_) => unreachable!(),
                            });
                        }
                        self.invoke_active.remove(&key);
                        self.invoke_cache.insert(key, None);
                        return Ok(None);
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                }
                Instr::NeedCtxdef { def, param } => {
                    let query = self.query_ctx_lambda(destruct, def, mapped.get(param))?;
                    let extracted = match query {
                        Query::Unique(extracted) => extracted,
                        Query::Missing => self.extract_ctx(destruct, def, destruct)?,
                        Query::Ambiguous => {
                            if std::env::var_os("MOSS_TRACE_INVOKE_FAIL").is_some() {
                                eprintln!(
                                    "invoke missing ctx def={def:?} at instr={instr:?} in lambda={lambda:?} query=ambiguous"
                                );
                            }
                            self.invoke_active.remove(&key);
                            self.invoke_cache.insert(key, None);
                            return Ok(None);
                        }
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                }
                _ => self.duplicate(&mut mapped, instr)?,
            }
            instr += 1;
        }
        let result = Some(construct);
        self.invoke_active.remove(&key);
        self.invoke_cache.insert(key, result.clone());
        Ok(result)
    }

    fn invoke_force(&mut self, lambda: InstrId, destruct: &[InstrId]) -> LowerResult<Vec<InstrId>> {
        match self.invoke(lambda, destruct)? {
            Some(construct) => Ok(construct),
            None => {
                if std::env::var_os("MOSS_TRACE_INVOKE_FAIL").is_some() {
                    eprintln!("invoke_force failed lambda={lambda:?} -> {:?}", self.ir.instrs[lambda]);
                    match self.ir.instrs[lambda] {
                        Instr::NeedTydef { param, .. }
                        | Instr::NeedSigdef { param, .. }
                        | Instr::NeedValdef { param, .. }
                        | Instr::NeedCtxdef { param, .. } => {
                            eprintln!("  param={param:?} -> {:?}", self.ir.instrs[param]);
                            if let Ok(result) = self.materialize_lambda(param) {
                                eprintln!("  param materialized={result:?} -> {:?}", self.ir.instrs[result]);
                                if let Some(items) = self.stack_instrs(result) {
                                    for (index, item) in items.into_iter().enumerate() {
                                        eprintln!("    param[{index}]={item:?} -> {:?}", self.ir.instrs[item]);
                                    }
                                }
                            }
                        }
                        Instr::BindTydef { bind, .. }
                        | Instr::BindSigdef { bind, .. }
                        | Instr::BindValdef { bind, .. }
                        | Instr::BindCtxdef { bind, .. } => {
                            eprintln!("  bind={bind:?} -> {:?}", self.ir.instrs[bind]);
                            if let Ok(result) = self.materialize_lambda(bind) {
                                eprintln!("  bind materialized={result:?} -> {:?}", self.ir.instrs[result]);
                                if let Some(items) = self.stack_instrs(result) {
                                    for (index, item) in items.into_iter().enumerate() {
                                        eprintln!("    bind[{index}]={item:?} -> {:?}", self.ir.instrs[item]);
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                    for (index, &slot) in destruct.iter().enumerate() {
                        eprintln!("  destruct[{index}]={slot:?} -> {:?}", self.ir.instrs[slot]);
                    }
                }
                Err(self.todo_no_loc())
            }
        }
    }

    fn invoke_need(
        &mut self,
        target: Body,
        destruct: &[InstrId],
    ) -> LowerResult<(Vec<InstrId>, Vec<InstrId>)> {
        let mut construct = Vec::new();
        let mut needs = Vec::new();
        let mut mapped = InstrMap::new();
        let mut instr = target.body.start;
        while instr < target.body.end {
            match self.ir.instrs[instr] {
                Instr::Lambda => {
                    let start = instr;
                    let end = self.find_end_lambda(instr);
                    let range = IdRange {
                        start,
                        end: end + 1,
                    };
                    self.duplicate_range(&mut mapped, range)?;
                    instr = end;
                }
                Instr::NeedTydef { def, param } => {
                    let extracted = match self.query_ty_lambda(destruct, def)? {
                        Query::Missing => {
                            let need = self.emit(Instr::NeedTydef {
                                def,
                                param: mapped.get(param),
                            });
                            needs.push(need);
                            need
                        }
                        Query::Ambiguous => return Err(self.todo_no_loc()),
                        Query::Unique(extracted) => extracted,
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                }
                Instr::NeedSigdef { def, param } => {
                    let extracted = match self.query_sig_lambda(destruct, def)? {
                        Query::Missing => {
                            let need = self.emit(Instr::NeedSigdef {
                                def,
                                param: mapped.get(param),
                            });
                            needs.push(need);
                            need
                        }
                        Query::Ambiguous => return Err(self.todo_no_loc()),
                        Query::Unique(extracted) => extracted,
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                }
                Instr::NeedValdef { def, param } => {
                    let extracted = match self.query_val_lambda(destruct, def)? {
                        Query::Missing => {
                            let need = self.emit(Instr::NeedValdef {
                                def,
                                param: mapped.get(param),
                            });
                            needs.push(need);
                            need
                        }
                        Query::Ambiguous => return Err(self.todo_no_loc()),
                        Query::Unique(extracted) => extracted,
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                }
                Instr::NeedCtxdef { def, param } => {
                    let extracted = match self.query_ctx_lambda(destruct, def, mapped.get(param))? {
                        Query::Missing => {
                            let need = self.extract_ctx(destruct, def, destruct)?;
                            needs.push(need);
                            need
                        }
                        Query::Ambiguous => return Err(self.todo_no_loc()),
                        Query::Unique(extracted) => extracted,
                    };
                    mapped.insert(instr, extracted);
                    construct.push(extracted);
                }
                _ => self.duplicate(&mut mapped, instr)?,
            }
            instr += 1;
        }
        Ok((construct, needs))
    }

    fn ctx_slots(&self, def: CtxdefId) -> Option<Vec<InstrId>> {
        let Ctxdef(body) = self.ir.ctxdefs[def];
        let lambda = body.result();
        let Instr::EndLambda { result, .. } = self.ir.instrs[lambda] else {
            return None;
        };
        self.stack_instrs(result)
    }

    fn ctx_contains_ty(&mut self, ctxdef: CtxdefId, tydef: TydefId) -> bool {
        if let Some(&cached) = self.ctx_contains_ty_cache.get(&(ctxdef, tydef)) {
            return cached;
        }
        if !self.ctx_contains_ty_active.insert((ctxdef, tydef)) {
            return false;
        }
        let result = self.ctx_slots(ctxdef).is_some_and(|slots| {
            slots.into_iter().any(|slot| match self.ir.instrs[slot] {
                Instr::NeedTydef { def, .. } | Instr::BindTydef { def, .. } => def == tydef,
                Instr::NeedCtxdef { def, .. } | Instr::BindCtxdef { def, .. } => {
                    self.ctx_contains_ty(def, tydef)
                }
                _ => false,
            })
        });
        self.ctx_contains_ty_active.remove(&(ctxdef, tydef));
        self.ctx_contains_ty_cache.insert((ctxdef, tydef), result);
        result
    }

    fn ctx_contains_sig(&mut self, ctxdef: CtxdefId, sigdef: SigdefId) -> bool {
        if let Some(&cached) = self.ctx_contains_sig_cache.get(&(ctxdef, sigdef)) {
            return cached;
        }
        if !self.ctx_contains_sig_active.insert((ctxdef, sigdef)) {
            return false;
        }
        let result = self.ctx_slots(ctxdef).is_some_and(|slots| {
            slots.into_iter().any(|slot| match self.ir.instrs[slot] {
                Instr::NeedSigdef { def, .. } | Instr::BindSigdef { def, .. } => def == sigdef,
                Instr::NeedCtxdef { def, .. } | Instr::BindCtxdef { def, .. } => {
                    self.ctx_contains_sig(def, sigdef)
                }
                _ => false,
            })
        });
        self.ctx_contains_sig_active.remove(&(ctxdef, sigdef));
        self.ctx_contains_sig_cache.insert((ctxdef, sigdef), result);
        result
    }

    fn ctx_contains_val(&mut self, ctxdef: CtxdefId, valdef: ValdefId) -> bool {
        if let Some(&cached) = self.ctx_contains_val_cache.get(&(ctxdef, valdef)) {
            return cached;
        }
        if !self.ctx_contains_val_active.insert((ctxdef, valdef)) {
            return false;
        }
        let result = self.ctx_slots(ctxdef).is_some_and(|slots| {
            slots.into_iter().any(|slot| match self.ir.instrs[slot] {
                Instr::NeedValdef { def, .. } | Instr::BindValdef { def, .. } => def == valdef,
                Instr::NeedCtxdef { def, .. } | Instr::BindCtxdef { def, .. } => {
                    self.ctx_contains_val(def, valdef)
                }
                _ => false,
            })
        });
        self.ctx_contains_val_active.remove(&(ctxdef, valdef));
        self.ctx_contains_val_cache.insert((ctxdef, valdef), result);
        result
    }

    fn ctx_contains_ctx(&mut self, ctxdef: CtxdefId, target: CtxdefId) -> bool {
        if let Some(&cached) = self.ctx_contains_ctx_cache.get(&(ctxdef, target)) {
            return cached;
        }
        if !self.ctx_contains_ctx_active.insert((ctxdef, target)) {
            return false;
        }
        let result = self.ctx_slots(ctxdef).is_some_and(|slots| {
            slots.into_iter().any(|slot| match self.ir.instrs[slot] {
                Instr::NeedCtxdef { def, .. } | Instr::BindCtxdef { def, .. } => {
                    def == target || self.ctx_contains_ctx(def, target)
                }
                _ => false,
            })
        });
        self.ctx_contains_ctx_active.remove(&(ctxdef, target));
        self.ctx_contains_ctx_cache.insert((ctxdef, target), result);
        result
    }

    fn query_ty_lambda(&mut self, slots: &[InstrId], def: TydefId) -> LowerResult<Query<InstrId>> {
        let mut query = Query::Missing;
        for &slot in slots {
            match self.ir.instrs[slot] {
                Instr::NeedCtxdef { def: ctxdef, param } => {
                    if !self.ctx_contains_ty(ctxdef, def) {
                        continue;
                    }
                    let Some(items) = self.need_ctx_items(ctxdef, param)? else {
                        continue;
                    };
                    let nested = self.query_ty_lambda(&items, def)?;
                    query = self.merge_slot_queries(query, nested)?;
                }
                Instr::BindCtxdef { def: ctxdef, bind } => {
                    if !self.ctx_contains_ty(ctxdef, def) {
                        continue;
                    }
                    let Some(items) = self.bind_ctx_items(bind)? else {
                        continue;
                    };
                    let nested = self.query_ty_lambda(&items, def)?;
                    query = self.merge_slot_queries(query, nested)?;
                }
                Instr::NeedTydef { def: tydef, param } => {
                    if tydef != def {
                        continue;
                    }
                    query = self.merge_slot_queries(query, Query::Unique(slot))?;
                }
                Instr::NeedSigdef { .. } | Instr::NeedValdef { .. } => {}
                Instr::BindTydef { def: tydef, bind } => {
                    if tydef != def {
                        continue;
                    }
                    query = self.merge_slot_queries(query, Query::Unique(slot))?;
                }
                Instr::Lambda
                | Instr::EndLambda { .. }
                | Instr::Apply { .. }
                | Instr::Stack { .. }
                | Instr::Tagdef { .. }
                | Instr::Aliasdef { .. }
                | Instr::Tuple { .. }
                | Instr::Record { .. }
                | Instr::Context
                | Instr::Fndef { .. }
                | Instr::Get { .. }
                | Instr::Lit { .. }
                | Instr::Bind { .. }
                | Instr::BindSigdef { .. }
                | Instr::BindValdef { .. }
                | Instr::Sig { .. }
                | Instr::Set { .. }
                | Instr::If { .. }
                | Instr::Else { .. }
                | Instr::EndIf { .. }
                | Instr::Loop
                | Instr::EndLoop
                | Instr::Br { .. }
                | Instr::Expr { .. } => {}
            }
        }
        Ok(query)
    }

    fn extract_ty(
        &mut self,
        slots: &[InstrId],
        def: TydefId,
        destruct: &[InstrId],
    ) -> LowerResult<InstrId> {
        let available = self.entrypoints(slots, destruct);
        if let Query::Unique(slot) = self.query_ty_lambda(&available, def)? {
            return Ok(slot);
        }
        let Tydef(target) = self.ir.tydefs[def];
        let start = self.emit(Instr::Lambda);
        let (construct, _) = self.invoke_need(target, &available)?;
        let items = self.items(&construct);
        let result = self.emit(Instr::Stack { items });
        let param = self.emit(Instr::EndLambda { start, result });
        Ok(self.emit(Instr::NeedTydef { def, param }))
    }

    fn query_sig_lambda(&mut self, slots: &[InstrId], def: SigdefId) -> LowerResult<Query<InstrId>> {
        let mut query = Query::Missing;
        for &slot in slots {
            match self.ir.instrs[slot] {
                Instr::NeedCtxdef { def: ctxdef, param } => {
                    if !self.ctx_contains_sig(ctxdef, def) {
                        continue;
                    }
                    let Some(items) = self.need_ctx_items(ctxdef, param)? else {
                        continue;
                    };
                    let nested = self.query_sig_lambda(&items, def)?;
                    query = self.merge_slot_queries(query, nested)?;
                }
                Instr::BindCtxdef { def: ctxdef, bind } => {
                    if !self.ctx_contains_sig(ctxdef, def) {
                        continue;
                    }
                    let Some(items) = self.bind_ctx_items(bind)? else {
                        continue;
                    };
                    let nested = self.query_sig_lambda(&items, def)?;
                    query = self.merge_slot_queries(query, nested)?;
                }
                Instr::NeedSigdef { def: sigdef, param } => {
                    if sigdef != def {
                        continue;
                    }
                    query = self.merge_slot_queries(query, Query::Unique(slot))?;
                }
                Instr::BindSigdef { def: sigdef, bind } => {
                    if sigdef != def {
                        continue;
                    }
                    query = self.merge_slot_queries(query, Query::Unique(slot))?;
                }
                _ => {}
            }
        }
        Ok(query)
    }

    fn extract_sig(
        &mut self,
        slots: &[InstrId],
        def: SigdefId,
        destruct: &[InstrId],
    ) -> LowerResult<InstrId> {
        let available = self.entrypoints(slots, destruct);
        if let Query::Unique(slot) = self.query_sig_lambda(&available, def)? {
            return Ok(slot);
        }
        let Sigdef(target) = self.ir.sigdefs[def];
        let start = self.emit(Instr::Lambda);
        let (construct, _) = self.invoke_need(target, &available)?;
        let items = self.items(&construct);
        let result = self.emit(Instr::Stack { items });
        let param = self.emit(Instr::EndLambda { start, result });
        Ok(self.emit(Instr::NeedSigdef { def, param }))
    }

    fn query_val_lambda(&mut self, slots: &[InstrId], def: ValdefId) -> LowerResult<Query<InstrId>> {
        let mut query = Query::Missing;
        for &slot in slots {
            match self.ir.instrs[slot] {
                Instr::NeedCtxdef { def: ctxdef, param } => {
                    if !self.ctx_contains_val(ctxdef, def) {
                        continue;
                    }
                    let Some(items) = self.need_ctx_items(ctxdef, param)? else {
                        continue;
                    };
                    let nested = self.query_val_lambda(&items, def)?;
                    query = self.merge_slot_queries(query, nested)?;
                }
                Instr::BindCtxdef { def: ctxdef, bind } => {
                    if !self.ctx_contains_val(ctxdef, def) {
                        continue;
                    }
                    let Some(items) = self.bind_ctx_items(bind)? else {
                        continue;
                    };
                    let nested = self.query_val_lambda(&items, def)?;
                    query = self.merge_slot_queries(query, nested)?;
                }
                Instr::NeedValdef { def: valdef, param } => {
                    if valdef != def {
                        continue;
                    }
                    query = self.merge_slot_queries(query, Query::Unique(slot))?;
                }
                Instr::BindValdef { def: valdef, bind } => {
                    if valdef != def {
                        continue;
                    }
                    query = self.merge_slot_queries(query, Query::Unique(slot))?;
                }
                _ => {}
            }
        }
        Ok(query)
    }

    fn extract_val(
        &mut self,
        slots: &[InstrId],
        def: ValdefId,
        destruct: &[InstrId],
    ) -> LowerResult<InstrId> {
        let available = self.entrypoints(slots, destruct);
        if let Query::Unique(slot) = self.query_val_lambda(&available, def)? {
            return Ok(slot);
        }
        let Valdef(target) = self.ir.valdefs[def];
        let start = self.emit(Instr::Lambda);
        let (construct, _) = self.invoke_need(target, &available)?;
        let items = self.items(&construct);
        let result = self.emit(Instr::Stack { items });
        let param = self.emit(Instr::EndLambda { start, result });
        Ok(self.emit(Instr::NeedValdef { def, param }))
    }

    fn query_ctx_lambda(
        &mut self,
        slots: &[InstrId],
        def: CtxdefId,
        lambda: InstrId,
    ) -> LowerResult<Query<InstrId>> {
        self.trace_hotspot(&TRACE_QUERY_CTX, "query_ctx", || {
            format!(
                "def={def:?} lambda={lambda:?} slots={} instrs={}",
                slots.len(),
                self.ir.instrs.len()
            )
        });
        let mut query = Query::Missing;
        for &slot in slots {
            match self.ir.instrs[slot] {
                Instr::NeedCtxdef { def: ctxdef, param } => {
                    if ctxdef != def && !self.ctx_contains_ctx(ctxdef, def) {
                        continue;
                    }
                    if ctxdef == def {
                        query = self.merge_slot_queries(query, Query::Unique(slot))?;
                    }
                    let compatible = self.left_domain_more_specific(lambda, param)?;
                    if !compatible {
                        continue;
                    }
                    let Some(items) = self.need_ctx_items(ctxdef, param)? else {
                        continue;
                    };
                    let nested = self.query_ctx_lambda(&items, def, lambda)?;
                    query = self.merge_slot_queries(query, nested)?;
                }
                Instr::BindCtxdef { def: ctxdef, bind } => {
                    if ctxdef != def && !self.ctx_contains_ctx(ctxdef, def) {
                        continue;
                    }
                    if ctxdef == def {
                        query = self.merge_slot_queries(query, Query::Unique(slot))?;
                    }
                    let compatible = self.left_domain_more_specific(lambda, bind)?;
                    if !compatible {
                        continue;
                    }
                    let Some(items) = self.bind_ctx_items(bind)? else {
                        continue;
                    };
                    let nested = self.query_ctx_lambda(&items, def, lambda)?;
                    query = self.merge_slot_queries(query, nested)?;
                }
                _ => {}
            }
        }
        Ok(query)
    }

    fn extract_ctx(
        &mut self,
        slots: &[InstrId],
        def: CtxdefId,
        destruct: &[InstrId],
    ) -> LowerResult<InstrId> {
        let available = self.entrypoints(slots, destruct);
        let Ctxdef(target) = self.ir.ctxdefs[def];
        let start = self.emit(Instr::Lambda);
        let (construct, _) = self.invoke_need(target, &available)?;
        let items = self.items(&construct);
        let result = self.emit(Instr::Stack { items });
        let param = self.emit(Instr::EndLambda { start, result });
        if let Query::Unique(slot) = self.query_ctx_lambda(&available, def, param)? {
            return Ok(slot);
        }
        Ok(self.emit(Instr::NeedCtxdef { def, param }))
    }

    fn inline(&mut self, body: Body, construct: &[InstrId]) -> LowerResult<InstrId> {
        self.inline_range(body.body.start, body.body.end, body.result(), Some(construct))
    }

    /// Resolve the path of a spec and synthesize each of its attached bindings.
    ///
    /// The `destruct` list gives the set of entrypoints that can be used to synthesize bindings.
    fn spec(
        &mut self,
        destruct: &[InstrId],
        spec: parse::Spec,
    ) -> LowerResult<(Named, Vec<InstrId>)> {
        let Spec { dot, path, binds } = spec;
        let named = if dot {
            self.detached(path)?.into()
        } else {
            self.path(path)?
        };
        let construct = binds
            .into_iter()
            .map(|bind| self.bind(destruct, bind))
            .collect::<LowerResult<Vec<InstrId>>>()?;
        Ok((named, construct))
    }

    fn need_bind(&mut self, slots: &[InstrId], bind: parse::BindId) -> LowerResult<InstrId> {
        let parse::Bind { key, val } = self.tree.binds[bind];
        if std::env::var_os("MOSS_TRACE_NEEDS").is_some() {
            let mut detail = self.spec_string(key);
            if val.is_some() {
                detail.push_str(" = ...");
            }
            eprintln!("need {detail}");
        }
        match val {
            Some(_) => self.bind(slots, bind),
            None => {
                let (lhs, destruct) = self.spec(slots, key)?;
                let available = self.entrypoints(slots, &destruct);
                match lhs {
                    Named::Tydef(def) => {
                        let Tydef(target) = self.ir.tydefs[def];
                        let start = self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(target, &available)?;
                        let items = self.items(&construct);
                        let result = self.emit(Instr::Stack { items });
                        let param = self.emit(Instr::EndLambda { start, result });
                        Ok(self.emit(Instr::NeedTydef { def, param }))
                    }
                    Named::Sigdef(def) => {
                        let Sigdef(target) = self.ir.sigdefs[def];
                        let start = self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(target, &available)?;
                        let items = self.items(&construct);
                        let result = self.emit(Instr::Stack { items });
                        let param = self.emit(Instr::EndLambda { start, result });
                        Ok(self.emit(Instr::NeedSigdef { def, param }))
                    }
                    Named::Valdef(def) => {
                        let Valdef(target) = self.ir.valdefs[def];
                        let start = self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(target, &available)?;
                        let items = self.items(&construct);
                        let result = self.emit(Instr::Stack { items });
                        let param = self.emit(Instr::EndLambda { start, result });
                        Ok(self.emit(Instr::NeedValdef { def, param }))
                    }
                    Named::Ctxdef(def) => {
                        let Ctxdef(target) = self.ir.ctxdefs[def];
                        let start = self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(target, &available)?;
                        let items = self.items(&construct);
                        let result = self.emit(Instr::Stack { items });
                        let param = self.emit(Instr::EndLambda { start, result });
                        Ok(self.emit(Instr::NeedCtxdef { def, param }))
                    }
                    Named::Module(_) => Err(LowerError::BindModule(bind)),
                    Named::Tagdef(_) => Err(LowerError::BindNominal(bind)),
                    Named::Aliasdef(_) => Err(LowerError::BindAlias(bind)),
                    Named::Fndef(_) => Err(LowerError::BindDefined(bind)),
                }
            }
        }
    }

    fn assumed_slots(&mut self) -> LowerResult<Vec<InstrId>> {
        let mut slots = Vec::new();
        for &assume in &self.tree.assumes {
            for bind in assume {
                let slot = self.need_bind(&slots, bind)?;
                slots.push(slot);
            }
        }
        Ok(slots)
    }

    fn extend_needs(
        &mut self,
        slots: &mut Vec<InstrId>,
        needs: IdRange<parse::NeedId>,
    ) -> LowerResult<()> {
        for need in needs {
            let slot = self.need(&slots, need)?;
            slots.push(slot);
        }
        Ok(())
    }

    fn bind(&mut self, slots: &[InstrId], bind: parse::BindId) -> LowerResult<InstrId> {
        let parse::Bind { key, val } = self.tree.binds[bind];
        let (lhs, destruct_lhs) = self.spec(slots, key)?;
        let available_lhs = self.entrypoints(slots, &destruct_lhs);
        match val {
            None => match lhs {
                Named::Tydef(def) => {
                    let Tydef(target) = self.ir.tydefs[def];
                    let start = self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(target, &available_lhs)?;
                    let args = self.items(&construct);
                    let lambda = self.extract_ty(slots, def, &construct)?;
                    let construct_rhs = self.invoke_force(lambda, &construct)?;
                    let args_rhs = self.items(&construct_rhs);
                    let bind = self.emit(Instr::Apply {
                        lambda,
                        args: args_rhs,
                    });
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { start, result });
                    Ok(self.emit(Instr::BindTydef { def, bind }))
                }
                Named::Sigdef(def) => {
                    let Sigdef(target) = self.ir.sigdefs[def];
                    let start = self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(target, &available_lhs)?;
                    let args = self.items(&construct);
                    let lambda = self.extract_sig(slots, def, &construct)?;
                    let construct_rhs = self.invoke_force(lambda, &construct)?;
                    let args_rhs = self.items(&construct_rhs);
                    let bind = self.emit(Instr::Apply {
                        lambda,
                        args: args_rhs,
                    });
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { start, result });
                    Ok(self.emit(Instr::BindSigdef { def, bind }))
                }
                Named::Valdef(def) => {
                    let Valdef(target) = self.ir.valdefs[def];
                    let start = self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(target, &available_lhs)?;
                    let lambda = self.extract_val(slots, def, &construct)?;
                    let construct_rhs = self.invoke_force(lambda, &construct)?;
                    let args_rhs = self.items(&construct_rhs);
                    let bind = self.emit(Instr::Apply {
                        lambda,
                        args: args_rhs,
                    });
                    let args = self.items(&construct);
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { start, result });
                    Ok(self.emit(Instr::BindValdef { def, bind }))
                }
                Named::Ctxdef(def) => {
                    let Ctxdef(target) = self.ir.ctxdefs[def];
                    let start = self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(target, &available_lhs)?;
                    let lambda = self.extract_ctx(slots, def, &construct)?;
                    let construct_rhs = self.invoke_force(lambda, &construct)?;
                    let bind = self.apply_item(lambda, &construct_rhs)?;
                    let args = self.items(&construct);
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { start, result });
                    Ok(self.emit(Instr::BindCtxdef { def, bind }))
                }
                Named::Module(_) => Err(LowerError::BindModule(bind)),
                Named::Tagdef(_) => Err(LowerError::BindNominal(bind)),
                Named::Aliasdef(_) => Err(LowerError::BindAlias(bind)),
                Named::Fndef(_) => Err(LowerError::BindDefined(bind)),
            },
            Some(parse::Entry::Lit(token)) => match lhs {
                Named::Valdef(def) => {
                    let Valdef(target) = self.ir.valdefs[def];
                    let start = self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(target, &available_lhs)?;
                    let (val, _) = self.lit(token)?;
                    let bind = self.emit(Instr::Lit { val });
                    let args = self.items(&construct);
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { start, result });
                    Ok(self.emit(Instr::BindValdef { def, bind }))
                }
                _ => Err(LowerError::LitNotVal(token)),
            },
            Some(parse::Entry::Ref(spec)) => {
                let (rhs, mut destruct_rhs) = self.spec(slots, spec)?;
                match lhs {
                    Named::Tydef(def) => {
                        let lambda = match rhs {
                            Named::Tydef(tydef) => self.extract_ty(slots, tydef, &destruct_rhs)?,
                            Named::Tagdef(tagdef) => self.emit(Instr::Tagdef { def: tagdef }),
                            Named::Aliasdef(aliasdef) => {
                                self.emit(Instr::Aliasdef { def: aliasdef })
                            }
                            _ => return Err(LowerError::BindMismatch(bind)),
                        };
                        let Tydef(target_lhs) = self.ir.tydefs[def];
                        let start = self.emit(Instr::Lambda);
                        let (construct_lhs, mut needs) =
                            self.invoke_need(target_lhs, &available_lhs)?;
                        destruct_rhs.append(&mut needs);
                        let available_rhs = self.entrypoints(slots, &destruct_rhs);
                        let (construct_rhs, needs_rhs) = match self.ir.instrs[lambda] {
                            Instr::Tagdef { def } => {
                                let Tagdef(body) = self.ir.tagdefs[def];
                                self.invoke_need(body, &available_rhs)?
                            }
                            Instr::Aliasdef { def } => {
                                let Aliasdef(body) = self.ir.aliasdefs[def];
                                self.invoke_need(body, &available_rhs)?
                            }
                            _ => (self.invoke_force(lambda, &available_rhs)?, Vec::new()),
                        };
                        let mut bind_args = construct_lhs;
                        bind_args.extend(needs_rhs);
                        let args_lhs = self.items(&bind_args);
                        let args_rhs = self.items(&construct_rhs);
                        let bind = self.emit(Instr::Apply {
                            lambda,
                            args: args_rhs,
                        });
                        let result = self.emit(Instr::Bind {
                            args: args_lhs,
                            bind,
                        });
                        let bind = self.emit(Instr::EndLambda { start, result });
                        Ok(self.emit(Instr::BindTydef { def, bind }))
                    }
                    Named::Sigdef(def) => {
                        let lambda = match rhs {
                            Named::Sigdef(sigdef) => {
                                self.extract_sig(slots, sigdef, &destruct_rhs)?
                            }
                            Named::Fndef(fndef) => self.emit(Instr::Fndef { def: fndef }),
                            _ => return Err(LowerError::BindMismatch(bind)),
                        };
                        // TODO: Check compatibility of function signatures.
                        let Sigdef(target_lhs) = self.ir.sigdefs[def];
                        let start = self.emit(Instr::Lambda);
                        let (construct_lhs, mut needs) =
                            self.invoke_need(target_lhs, &available_lhs)?;
                        let args_lhs = self.items(&construct_lhs);
                        destruct_rhs.append(&mut needs);
                        let available_rhs = self.entrypoints(slots, &destruct_rhs);
                        let construct_rhs = self.invoke_force(lambda, &available_rhs)?;
                        let args_rhs = self.items(&construct_rhs);
                        let bind = self.emit(Instr::Apply {
                            lambda,
                            args: args_rhs,
                        });
                        let result = self.emit(Instr::Bind {
                            args: args_lhs,
                            bind,
                        });
                        let bind = self.emit(Instr::EndLambda { start, result });
                        Ok(self.emit(Instr::BindSigdef { def, bind }))
                    }
                    Named::Valdef(def) => {
                        let lambda = match rhs {
                            Named::Valdef(valdef) => {
                                self.extract_val(slots, valdef, &destruct_rhs)?
                            }
                            _ => return Err(LowerError::BindMismatch(bind)),
                        };
                        // TODO: Check compatibility of value types.
                        let Valdef(target_lhs) = self.ir.valdefs[def];
                        let start = self.emit(Instr::Lambda);
                        let (construct_lhs, mut needs) =
                            self.invoke_need(target_lhs, &available_lhs)?;
                        let args_lhs = self.items(&construct_lhs);
                        destruct_rhs.append(&mut needs);
                        let available_rhs = self.entrypoints(slots, &destruct_rhs);
                        let construct_rhs = self.invoke_force(lambda, &available_rhs)?;
                        let args_rhs = self.items(&construct_rhs);
                        let bind = self.emit(Instr::Apply {
                            lambda,
                            args: args_rhs,
                        });
                        let result = self.emit(Instr::Bind {
                            args: args_lhs,
                            bind,
                        });
                        let bind = self.emit(Instr::EndLambda { start, result });
                        Ok(self.emit(Instr::BindValdef { def, bind }))
                    }
                    Named::Ctxdef(_) => Err(LowerError::BindContext(bind)),
                    Named::Module(_) => Err(LowerError::BindModule(bind)),
                    Named::Tagdef(_) => Err(LowerError::BindNominal(bind)),
                    Named::Aliasdef(_) => Err(LowerError::BindAlias(bind)),
                    Named::Fndef(_) => Err(LowerError::BindDefined(bind)),
                }
            }
        }
    }

    fn need(&mut self, slots: &[InstrId], need: parse::NeedId) -> LowerResult<InstrId> {
        let parse::Need { kind: _, bind } = self.tree.needs[need];
        // TODO: Handle `kind`.
        self.need_bind(slots, bind)
    }

    fn needs(&mut self, needs: IdRange<parse::NeedId>) -> LowerResult<Vec<InstrId>> {
        let mut slots = self.assumed_slots()?;
        self.extend_needs(&mut slots, needs)?;
        Ok(slots)
    }

    fn entrypoints(&self, slots: &[InstrId], extra: &[InstrId]) -> Vec<InstrId> {
        let mut entrypoints = Vec::from(slots);
        for &slot in extra {
            if !entrypoints.contains(&slot) {
                entrypoints.push(slot);
            }
        }
        entrypoints
    }

    fn parse_ty(&mut self, slots: &[InstrId], ty: parse::TypeId) -> LowerResult<InstrId> {
        match self.tree.types[ty] {
            parse::Type::Spec(spec) => {
                let (named, destruct) = self.spec(slots, spec)?;
                match named {
                    Named::Tydef(tydef) => {
                        let lambda = self.extract_ty(slots, tydef, &destruct)?;
                        let available = self.entrypoints(slots, &destruct);
                        let construct = self.invoke_force(lambda, &available)?;
                        let args = self.items(&construct);
                        Ok(self.emit(Instr::Apply { lambda, args }))
                    }
                    Named::Tagdef(tagdef) => {
                        let lambda = self.emit(Instr::Tagdef { def: tagdef });
                        let available = self.entrypoints(slots, &destruct);
                        let construct = self.invoke_force(lambda, &available)?;
                        let args = self.items(&construct);
                        Ok(self.emit(Instr::Apply { lambda, args }))
                    }
                    Named::Aliasdef(aliasdef) => {
                        let lambda = self.emit(Instr::Aliasdef { def: aliasdef });
                        let available = self.entrypoints(slots, &destruct);
                        let construct = self.invoke_force(lambda, &available)?;
                        let args = self.items(&construct);
                        Ok(self.emit(Instr::Apply { lambda, args }))
                    }
                    _ => return Err(LowerError::NotType(spec.path.last)),
                }
            }
            parse::Type::Tuple(elems) => {
                let lowered = elems
                    .into_iter()
                    .map(|elem| self.parse_ty(slots, elem))
                    .collect::<LowerResult<Vec<InstrId>>>()?;
                Ok(self.ty_tuple(lowered))
            }
            parse::Type::Record(members) => Err(self.todo_no_loc()),
        }
    }

    fn parse_sig(&mut self, fndef: parse::Fndef) -> LowerResult<(Vec<InstrId>, InstrId, InstrId)> {
        let parse::Fndef {
            name: _,
            needs,
            params,
            result,
            def: _,
        } = fndef;
        let mut slots = self.assumed_slots()?;
        self.extend_needs(&mut slots, needs)?;
        let elems = (params.into_iter())
            .map(|arg| self.parse_ty(&slots, self.tree.params[arg].ty))
            .collect::<LowerResult<Vec<InstrId>>>()?;
        let param_tuple = self.ty_tuple(elems);
        let result_ty = match result {
            parse::Return::Unit => self.ty_unit(),
            parse::Return::Type(ty) => self.parse_ty(&slots, ty)?,
            parse::Return::Bind(needs) => {
                let prefix = slots.len();
                let mut result_slots = slots.clone();
                self.extend_needs(&mut result_slots, needs)?;
                let items = self.items(&result_slots[prefix..]);
                self.emit(Instr::Stack { items })
            }
        };
        Ok((slots, param_tuple, result_ty))
    }

    fn parse_fndef(&mut self, fndef: parse::Fndef) -> LowerResult<(StrId, NamedFn)> {
        let parse::Fndef {
            name,
            needs: _,
            params: _,
            result: _,
            def,
        } = fndef;
        if std::env::var_os("MOSS_TRACE_SIGS").is_some() {
            eprintln!("sig:start {} instrs={}", self.slice(name), self.ir.instrs.len());
        }
        let builder = self.builder();
        let (slots, param_tuple, result_ty) = self.parse_sig(fndef)?;
        let signature = self.emit(Instr::Sig {
            param: param_tuple,
            result: result_ty,
        });
        let body = self.finish(builder, signature);
        let lowered = match def {
            None => {
                let id = self.ir.sigdefs.push(Sigdef(body));
                self.ir.sig_arity.push(slots.len());
                NamedFn::Sigdef(id)
            }
            Some(_) => {
                let id = self.ir.fndefs.push(Sigdef(body));
                self.ir.fn_arity.push(slots.len());
                NamedFn::Fndef(id)
            }
        };
        if std::env::var_os("MOSS_TRACE_SIGS").is_some() {
            eprintln!("sig:done {} instrs={}", self.slice(name), self.ir.instrs.len());
        }
        let string = self.name(name);
        Ok((string, lowered))
    }

    fn decls(&mut self) -> LowerResult<()> {
        for &decl in &self.tree.decls {
            match decl {
                parse::Decl::Tydef(id) => {
                    let parse::Tydef { name, needs } = self.tree.tydefs[id];
                    if std::env::var_os("MOSS_TRACE_DECLS").is_some() {
                        eprintln!("decl ty {}", self.slice(name));
                    }
                    let builder = self.builder();
                    let mut slots = self.assumed_slots()?;
                    self.extend_needs(&mut slots, needs)?;
                    let items = self.items(&[]);
                    let ctx = self.emit(Instr::Stack { items });
                    let body = self.finish(builder, ctx);
                    let lowered = self.ir.tydefs.push(Tydef(body));
                    let string = self.name(name);
                    self.names
                        .names
                        .insert((self.module, string), Named::Tydef(lowered));
                }
                parse::Decl::Tagdef(id) => {
                    let parse::Tagdef { name, needs, def } = self.tree.tagdefs[id];
                    if std::env::var_os("MOSS_TRACE_DECLS").is_some() {
                        eprintln!("decl tag {}", self.slice(name));
                    }
                    let builder = self.builder();
                    let mut slots = self.assumed_slots()?;
                    self.extend_needs(&mut slots, needs)?;
                    let ty = self.parse_ty(&slots, def)?;
                    let body = self.finish(builder, ty);
                    let lowered = self.ir.tagdefs.push(Tagdef(body));
                    let string = self.name(name);
                    self.names
                        .names
                        .insert((self.module, string), Named::Tagdef(lowered));
                }
                parse::Decl::Aliasdef(id) => {
                    let parse::Aliasdef { name, needs, def } = self.tree.aliasdefs[id];
                    if std::env::var_os("MOSS_TRACE_DECLS").is_some() {
                        eprintln!("decl alias {}", self.slice(name));
                    }
                    let builder = self.builder();
                    let mut slots = self.assumed_slots()?;
                    self.extend_needs(&mut slots, needs)?;
                    let ty = self.parse_ty(&slots, def)?;
                    let body = self.finish(builder, ty);
                    let lowered = self.ir.aliasdefs.push(Aliasdef(body));
                    let string = self.name(name);
                    self.names
                        .names
                        .insert((self.module, string), Named::Aliasdef(lowered));
                }
                parse::Decl::Funcdef(id) => {
                    let parse::Funcdef { fndef } = self.tree.funcdefs[id];
                    if std::env::var_os("MOSS_TRACE_DECLS").is_some() {
                        let parse::Fndef { name, .. } = fndef;
                        eprintln!("decl fn {}", self.slice(name));
                    }
                    let (fn_name, lowered) = self.parse_fndef(fndef)?;
                    if let NamedFn::Fndef(defined) = lowered {
                        self.funcs.push((id, defined));
                    }
                    self.names
                        .names
                        .insert((self.module, fn_name), lowered.into());
                }
                parse::Decl::Attachdef(id) => {
                    let parse::Attachdef { ty, fndef } = self.tree.attachdefs[id];
                    if std::env::var_os("MOSS_TRACE_DECLS").is_some() {
                        let parse::Fndef { name, .. } = fndef;
                        eprintln!("decl attach {}.{}", self.slice(ty), self.slice(name));
                    }
                    let (fn_name, lowered) = self.parse_fndef(fndef)?;
                    let ty_name = self.name(ty);
                    // TODO: Prevent attaching methods to nominal types defined in other modules.
                    match self.names.names.get(&(self.module, ty_name)) {
                        Some(&Named::Tagdef(tagdef)) => {
                            if let NamedFn::Fndef(defined) = lowered {
                                self.attaches.push((id, tagdef, defined));
                            }
                            self.names.attached.insert((tagdef, fn_name), lowered);
                        }
                        Some(_) => return Err(LowerError::NotNominal(ty)),
                        None => return Err(LowerError::Undefined(ty)),
                    }
                }
                parse::Decl::Detachdef(id) => {
                    let parse::Detachdef { fndef } = self.tree.detachdefs[id];
                    if std::env::var_os("MOSS_TRACE_DECLS").is_some() {
                        let parse::Fndef { name, .. } = fndef;
                        eprintln!("decl detach {}", self.slice(name));
                    }
                    let (fn_name, lowered) = self.parse_fndef(fndef)?;
                    if let NamedFn::Fndef(defined) = lowered {
                        self.detaches.push((id, defined));
                    }
                    self.names.detached.insert((self.module, fn_name), lowered);
                }
                parse::Decl::Valdef(id) => {
                    let parse::Valdef { name, needs, ty } = self.tree.valdefs[id];
                    if std::env::var_os("MOSS_TRACE_DECLS").is_some() {
                        eprintln!("decl val {}", self.slice(name));
                    }
                    let builder = self.builder();
                    let mut slots = self.assumed_slots()?;
                    self.extend_needs(&mut slots, needs)?;
                    let ty = self.parse_ty(&slots, ty)?;
                    let body = self.finish(builder, ty);
                    let lowered = self.ir.valdefs.push(Valdef(body));
                    let string = self.name(name);
                    self.names
                        .names
                        .insert((self.module, string), Named::Valdef(lowered));
                }
                parse::Decl::Ctxdef(id) => {
                    let parse::Ctxdef { name, needs, def } = self.tree.ctxdefs[id];
                    if std::env::var_os("MOSS_TRACE_DECLS").is_some() {
                        eprintln!("decl ctx {}", self.slice(name));
                    }
                    let builder = self.builder();
                    let mut slots = self.assumed_slots()?;
                    self.extend_needs(&mut slots, needs)?;
                    let prefix = slots.len();
                    let start = self.emit(Instr::Lambda);
                    self.extend_needs(&mut slots, def)?;
                    let items = self.items(&slots[prefix..]);
                    let result = self.emit(Instr::Stack { items });
                    let lambda = self.emit(Instr::EndLambda { start, result });
                    let body = self.finish(builder, lambda);
                    let lowered = self.ir.ctxdefs.push(Ctxdef(body));
                    let string = self.name(name);
                    self.names
                        .names
                        .insert((self.module, string), Named::Ctxdef(lowered));
                }
            }
        }
        Ok(())
    }

    fn bodies(&mut self) -> LowerResult<()> {
        for (funcdef, id_decl) in take(&mut self.funcs) {
            let parse::Funcdef { fndef } = self.tree.funcdefs[funcdef];
            let body = LowerBody {
                x: self,
                slots: Vec::new(),
                locals: HashMap::new(),
                literal_tokens: HashMap::new(),
                is_method: false,
                current_fn: None,
            }
            .body(fndef, id_decl)?;
            let id_body = self.ir.bodies.push(Some(body));
            assert_eq!(id_decl, id_body);
        }
        for (attachdef, _tagdef, id_decl) in take(&mut self.attaches) {
            // TODO: Handle `this`.
            let parse::Attachdef { ty: _, fndef } = self.tree.attachdefs[attachdef];
            let body = LowerBody {
                x: self,
                slots: Vec::new(),
                locals: HashMap::new(),
                literal_tokens: HashMap::new(),
                is_method: true,
                current_fn: None,
            }
            .body(fndef, id_decl)?;
            let id_body = self.ir.bodies.push(Some(body));
            assert_eq!(id_decl, id_body);
        }
        for (detachdef, id_decl) in take(&mut self.detaches) {
            // TODO: Handle `this`.
            let parse::Detachdef { fndef } = self.tree.detachdefs[detachdef];
            let body = LowerBody {
                x: self,
                slots: Vec::new(),
                locals: HashMap::new(),
                literal_tokens: HashMap::new(),
                is_method: true,
                current_fn: None,
            }
            .body(fndef, id_decl)?;
            let id_body = self.ir.bodies.push(Some(body));
            assert_eq!(id_decl, id_body);
        }
        Ok(())
    }

    fn skip_function_bodies(&mut self) {
        for (_, id_decl) in take(&mut self.funcs) {
            let id_body = self.ir.bodies.push(None);
            assert_eq!(id_decl, id_body);
        }
        for (_, _, id_decl) in take(&mut self.attaches) {
            let id_body = self.ir.bodies.push(None);
            assert_eq!(id_decl, id_body);
        }
        for (_, id_decl) in take(&mut self.detaches) {
            let id_body = self.ir.bodies.push(None);
            assert_eq!(id_decl, id_body);
        }
    }

    fn program(&mut self) -> LowerResult<()> {
        self.imports()?;
        // TODO: Don't make declaration order significant.
        self.decls()?;
        if self.skip_bodies {
            self.skip_function_bodies();
        } else {
            self.bodies()?;
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug)]
struct Typed {
    ty: InstrId,
    val: InstrId,
}

#[derive(Debug)]
struct LowerBody<'a, 'b> {
    x: &'b mut Lower<'a>,
    slots: Vec<InstrId>,
    locals: HashMap<StrId, Typed>,
    literal_tokens: HashMap<InstrId, TokenId>,
    is_method: bool,
    current_fn: Option<TokenId>,
}

impl LowerBody<'_, '_> {
    fn base(&self) -> Base {
        self.x.base.unwrap()
    }

    fn get(&mut self, token: TokenId) -> LowerResult<Typed> {
        let name = self.x.name(token);
        match self.locals.get(&name) {
            Some(&local) => Ok(local),
            None => Err(LowerError::Undefined(token)),
        }
    }

    fn set(&mut self, token: TokenId, rhs: Typed) {
        let name = self.x.name(token);
        self.locals.insert(name, rhs);
    }

    fn emit(&mut self, instr: Instr) -> InstrId {
        self.x.emit(instr)
    }

    fn ty_tuple(&mut self, elems: &[InstrId]) -> InstrId {
        let start = self.x.ir.items.len_idx();
        self.x.ir.items.extend_from_slice(IndexSlice::new(elems));
        let end = self.x.ir.items.len_idx();
        self.emit(Instr::Tuple {
            elems: IdRange { start, end },
        })
    }

    fn ty_record(&mut self, fields: &[(StrId, InstrId)]) -> InstrId {
        let start = self.x.ir.records.len_idx();
        self.x.ir.records.extend_from_slice(IndexSlice::new(fields));
        let end = self.x.ir.records.len_idx();
        self.emit(Instr::Record {
            fields: IdRange { start, end },
        })
    }

    fn instr(&mut self, ty: InstrId, expr: Expr) -> Typed {
        Typed {
            ty,
            val: self.emit(Instr::Expr { ty, expr }),
        }
    }

    fn instr_tuple(&mut self, ty: InstrId, elems: &[InstrId]) -> Typed {
        let start = self.x.ir.items.len_idx();
        self.x.ir.items.extend_from_slice(IndexSlice::new(elems));
        let end = self.x.ir.items.len_idx();
        self.instr(
            ty,
            Expr::Tuple {
                elems: IdRange { start, end },
            },
        )
    }

    fn instr_record(&mut self, ty: InstrId, fields: &[(StrId, InstrId)]) -> Typed {
        let start = self.x.ir.records.len_idx();
        self.x.ir.records.extend_from_slice(IndexSlice::new(fields));
        let end = self.x.ir.records.len_idx();
        self.instr(
            ty,
            Expr::Record {
                fields: IdRange { start, end },
            },
        )
    }

    fn invoke_force(&mut self, lambda: InstrId) -> LowerResult<Vec<InstrId>> {
        self.x.invoke_force(lambda, &self.slots)
    }

    fn extract_ty(&mut self, def: TydefId) -> LowerResult<InstrId> {
        self.x.extract_ty(&self.slots, def, &self.slots)
    }

    fn extract_sig(&mut self, def: SigdefId) -> LowerResult<InstrId> {
        self.x.extract_sig(&self.slots, def, &self.slots)
    }

    fn extract_val(&mut self, def: ValdefId) -> LowerResult<InstrId> {
        self.x.extract_val(&self.slots, def, &self.slots)
    }

    fn extract_ctx(&mut self, def: CtxdefId) -> LowerResult<InstrId> {
        self.x.extract_ctx(&self.slots, def, &self.slots)
    }

    fn inline(&mut self, body: Body, construct: &[InstrId]) -> LowerResult<InstrId> {
        self.x.inline(body, construct)
    }

    /// Get the nominal type definition of a value, or [`None`] if the value is not nominally typed.
    fn nominal(&mut self, ty: InstrId) -> Option<TagdefId> {
        let ty = self.reduce_alias_ty(ty).ok()?;
        match self.x.ir.instrs[ty] {
            Instr::Tagdef { def } => Some(def),
            Instr::Apply { lambda, .. } => match self.x.ir.instrs[lambda] {
                Instr::Tagdef { def } => Some(def),
                _ => None,
            },
            _ => None,
        }
    }

    /// Get the fields of a record type.
    fn fields(&self, ty: InstrId) -> LowerResult<Vec<(StrId, InstrId)>> {
        Err(self.x.todo_no_loc())
    }

    fn reduce_alias_ty(&mut self, ty: InstrId) -> LowerResult<InstrId> {
        let mut reduced = ty;
        let mut seen = HashSet::new();
        loop {
            if !seen.insert(reduced) {
                return Ok(reduced);
            }
            reduced = match self.x.ir.instrs[reduced] {
                Instr::Apply { lambda, args } => match self.x.ir.instrs[lambda] {
                    Instr::Aliasdef { .. } | Instr::BindTydef { .. } => {
                        let args = Vec::from_iter(args.into_iter().map(|item| self.x.ir.items[item]));
                        self.x.apply_item(lambda, &args)?
                    }
                    _ => return Ok(reduced),
                },
                _ => return Ok(reduced),
            };
        }
    }

    fn same_ty(&mut self, left: InstrId, right: InstrId) -> LowerResult<bool> {
        let left = self.reduce_alias_ty(left)?;
        let right = self.reduce_alias_ty(right)?;
        if left == right {
            return Ok(true);
        }
        Ok(match (self.x.ir.instrs[left], self.x.ir.instrs[right]) {
            (Instr::Tuple { elems: left }, Instr::Tuple { elems: right }) => {
                left.len() == right.len()
                    && left.into_iter().zip(right).all(|(left, right)| {
                        self.same_ty(self.x.ir.items[left], self.x.ir.items[right])
                            .unwrap_or(false)
                    })
            }
            (Instr::Record { fields: left }, Instr::Record { fields: right }) => {
                left.len() == right.len()
                    && left.into_iter().zip(right).all(|(left, right)| {
                        let (left_name, left_ty) = self.x.ir.records[left];
                        let (right_name, right_ty) = self.x.ir.records[right];
                        left_name == right_name
                            && self.same_ty(left_ty, right_ty).unwrap_or(false)
                    })
            }
            (
                Instr::Apply {
                    lambda: left_lambda,
                    args: left_args,
                },
                Instr::Apply {
                    lambda: right_lambda,
                    args: right_args,
                },
            ) => {
                if left_args.len() != right_args.len() {
                    false
                } else if left_lambda == right_lambda {
                    left_args
                        .into_iter()
                        .zip(right_args)
                        .all(|(left, right)| self.x.ir.items[left] == self.x.ir.items[right])
                } else {
                    match (self.x.ir.instrs[left_lambda], self.x.ir.instrs[right_lambda]) {
                        (Instr::Tagdef { def: left }, Instr::Tagdef { def: right }) => left == right,
                        (Instr::NeedTydef { def: left, param: left_param }, Instr::NeedTydef { def: right, param: right_param }) => {
                            let _ = (left_param, right_param);
                            left == right
                                && left_args
                                    .into_iter()
                                    .zip(right_args)
                                    .all(|(left, right)| self.x.ir.items[left] == self.x.ir.items[right])
                        }
                        (Instr::NeedTydef { def: left, param }, Instr::BindTydef { def: right, bind })
                        | (Instr::BindTydef { def: left, bind: param }, Instr::NeedTydef { def: right, param: bind })
                        | (Instr::BindTydef { def: left, bind: param }, Instr::BindTydef { def: right, bind }) => {
                            let _ = (param, bind);
                            left == right
                                && left_args
                                    .into_iter()
                                    .zip(right_args)
                                    .all(|(left, right)| self.x.ir.items[left] == self.x.ir.items[right])
                        }
                        _ => false,
                    }
                }
            }
            _ => false,
        })
    }

    /// Get the parameter type and result type of a function.
    fn sig(&mut self, func: InstrId) -> LowerResult<(InstrId, InstrId)> {
        let resolved = match self.x.ir.instrs[func] {
            Instr::Apply { lambda, args } => {
                let args = Vec::from_iter(args.into_iter().map(|item| self.x.ir.items[item]));
                self.x.apply_item(lambda, &args)?
            }
            _ => func,
        };
        let Instr::Sig { param, result } = self.x.ir.instrs[resolved] else {
            return Err(self.x.todo_no_loc());
        };
        Ok((param, result))
    }

    /// Resolve a method call.
    fn method(&mut self, ty: InstrId, name: StrId) -> Option<NamedFn> {
        if let Some(tagdef) = self.nominal(ty)
            && let Some(&fndef) = self.x.names.attached.get(&(tagdef, name))
        {
            Some(fndef)
        } else if let Some(&fndef) = self.x.names.detached.get(&(self.x.module, name)) {
            Some(fndef)
        } else if let Some(&fndef) = self.x.names.detached.get(&(self.x.prelude, name)) {
            Some(fndef)
        } else {
            None
        }
    }

    fn op_fn(&self, name: &str) -> Option<NamedFn> {
        let id = self.x.ir.strings.get_id(name)?;
        self.x
            .names
            .names
            .iter()
            .find_map(|(&(_, string), &named)| {
                (string == id).then_some(match named {
                    Named::Sigdef(sigdef) => Some(NamedFn::Sigdef(sigdef)),
                    Named::Fndef(fndef) => Some(NamedFn::Fndef(fndef)),
                    _ => None,
                })?
            })
    }

    fn ty_named(&self, name: &str) -> Option<TydefId> {
        let id = self.x.ir.strings.get_id(name)?;
        self.x
            .names
            .names
            .iter()
            .find_map(|(&(_, string), &named)| match named {
                Named::Tydef(def) if string == id => Some(def),
                _ => None,
            })
    }

    fn val_named(&self, name: &str) -> Option<ValdefId> {
        let id = self.x.ir.strings.get_id(name)?;
        self.x
            .names
            .names
            .iter()
            .find_map(|(&(_, string), &named)| match named {
                Named::Valdef(def) if string == id => Some(def),
                _ => None,
            })
    }

    fn bind_ty(&mut self, def: TydefId, ty: InstrId, destruct: &[InstrId]) -> LowerResult<InstrId> {
        let Tydef(target) = self.x.ir.tydefs[def];
        let available = self.x.entrypoints(destruct, &self.slots);
        let start = self.emit(Instr::Lambda);
        let (construct, _) = self.x.invoke_need(target, &available)?;
        let args = self.x.items(&construct);
        let result = self.emit(Instr::Bind { args, bind: ty });
        let bind = self.emit(Instr::EndLambda { start, result });
        Ok(self.emit(Instr::BindTydef { def, bind }))
    }

    fn bind_val(&mut self, def: ValdefId, val: InstrId, destruct: &[InstrId]) -> LowerResult<InstrId> {
        let Valdef(target) = self.x.ir.valdefs[def];
        let available = self.x.entrypoints(destruct, &self.slots);
        let start = self.emit(Instr::Lambda);
        let (construct, _) = self.x.invoke_need(target, &available)?;
        let args = self.x.items(&construct);
        let result = self.emit(Instr::Bind { args, bind: val });
        let bind = self.emit(Instr::EndLambda { start, result });
        Ok(self.emit(Instr::BindValdef { def, bind }))
    }

    fn call_sig(&mut self, sigdef: SigdefId, destruct: &[InstrId], args: &[Typed]) -> LowerResult<Typed> {
        let available = self.x.entrypoints(destruct, &self.slots);
        let Sigdef(target) = self.x.ir.sigdefs[sigdef];
        let start = self.emit(Instr::Lambda);
        let (construct, _) = self.x.invoke_need(target, &available)?;
        let items = self.x.items(&construct);
        let result = self.emit(Instr::Stack { items });
        let param = self.emit(Instr::EndLambda { start, result });
        let lambda = self.emit(Instr::NeedSigdef { def: sigdef, param });
        let construct = self.x.invoke_force(lambda, &available)?;
        let args_ctx = self.x.items(&construct);
        let func = self.emit(Instr::Apply {
            lambda,
            args: args_ctx,
        });
        let (ty_param, ty_result) = self.sig(func)?;
        let args_ty = args.iter().map(|arg| arg.ty).collect::<Vec<_>>();
        let args_val = args.iter().map(|arg| arg.val).collect::<Vec<_>>();
        let ty_args = self.ty_tuple(&args_ty);
        self.expect_ty(ty_param, ty_args)?;
        let arg = self.instr_tuple(ty_args, &args_val).val;
        Ok(self.instr(ty_result, Expr::Call { func, arg }))
    }

    fn call_named(&mut self, named: NamedFn, destruct: &[InstrId], args: &[Typed]) -> LowerResult<Typed> {
        match named {
            NamedFn::Sigdef(sigdef) => self.call_sig(sigdef, destruct, args),
            NamedFn::Fndef(fndef) => {
                let available = self.x.entrypoints(destruct, &self.slots);
                let lambda = self.emit(Instr::Fndef { def: fndef });
                let construct = self.x.invoke_force(lambda, &available)?;
                let args_ctx = self.x.items(&construct);
                let func = self.emit(Instr::Apply {
                    lambda,
                    args: args_ctx,
                });
                let (ty_param, ty_result) = self.sig(func)?;
                let args_ty = args.iter().map(|arg| arg.ty).collect::<Vec<_>>();
                let args_val = args.iter().map(|arg| arg.val).collect::<Vec<_>>();
                let ty_args = self.ty_tuple(&args_ty);
                self.expect_ty(ty_param, ty_args)?;
                let arg = self.instr_tuple(ty_args, &args_val).val;
                Ok(self.instr(ty_result, Expr::Call { func, arg }))
            }
        }
    }

    fn call_fn(&mut self, named: NamedFn, args: &[Typed]) -> LowerResult<Typed> {
        self.call_named(named, &[], args)
    }

    fn call_op_unary(&mut self, named: NamedFn, arg: Typed) -> LowerResult<Typed> {
        let NamedFn::Sigdef(sigdef) = named else {
            return self.call_fn(named, &[arg]);
        };
        let arg_ty = self.ty_named("Arg").ok_or_else(|| self.x.todo_no_loc())?;
        let destruct = vec![self.bind_ty(arg_ty, arg.ty, &[])?];
        self.call_sig(sigdef, &destruct, &[arg])
    }

    fn call_op_binary(&mut self, named: NamedFn, left: Typed, right: Typed) -> LowerResult<Typed> {
        let NamedFn::Sigdef(sigdef) = named else {
            return self.call_fn(named, &[left, right]);
        };
        let lhs_ty = self.ty_named("Lhs").ok_or_else(|| self.x.todo_no_loc())?;
        let mut destruct = Vec::new();
        destruct.push(self.bind_ty(lhs_ty, left.ty, &destruct)?);
        let rhs_ty = self.ty_named("Rhs").ok_or_else(|| self.x.todo_no_loc())?;
        destruct.push(self.bind_ty(rhs_ty, right.ty, &destruct)?);
        self.call_sig(sigdef, &destruct, &[left, right])
    }

    fn tuple_elem_tys(&mut self, ty: InstrId) -> LowerResult<Vec<InstrId>> {
        let ty = self.reduce_alias_ty(ty)?;
        let Instr::Tuple { elems } = self.x.ir.instrs[ty] else {
            return Err(self.x.todo_no_loc());
        };
        Ok(elems
            .into_iter()
            .map(|item| self.x.ir.items[item])
            .collect())
    }

    fn int_type_for_ty(&mut self, ty: InstrId) -> LowerResult<Option<IntType>> {
        let ty = self.reduce_alias_ty(ty)?;
        let Instr::Apply { lambda, args } = self.x.ir.instrs[ty] else {
            return Ok(None);
        };
        if !args.is_empty() {
            return Ok(None);
        }
        let Instr::NeedTydef { def, .. } = self.x.ir.instrs[lambda] else {
            return Ok(None);
        };
        let types = self.base().types;
        Ok(if def == types.uint32 {
            Some(IntType::Uint32)
        } else if def == types.int32 {
            Some(IntType::Int32)
        } else if def == types.uint64 {
            Some(IntType::Uint64)
        } else if def == types.int64 {
            Some(IntType::Int64)
        } else if def == types.uint {
            Some(IntType::Uint)
        } else if def == types.int {
            Some(IntType::Int)
        } else {
            None
        })
    }

    fn coerce_arg(&mut self, expected: InstrId, arg: Typed) -> LowerResult<Typed> {
        let Some(&token) = self.literal_tokens.get(&arg.val) else {
            return Ok(arg);
        };
        let Some(int_type) = self.int_type_for_ty(expected)? else {
            return Ok(arg);
        };
        let (val, parsed) = self.x.lit(token)?;
        if parsed != Some(IntType::Int) {
            return Ok(arg);
        }
        if matches!(int_type, IntType::Uint32 | IntType::Uint64 | IntType::Uint)
            && matches!(val, Val::Int32(_) | Val::Int64(_) | Val::Int(_))
        {
            return Ok(arg);
        }
        self.lit_expr(token, Some(int_type))
    }

    fn coerce_args(&mut self, ty_param: InstrId, args: Vec<Typed>) -> LowerResult<Vec<Typed>> {
        let expected = self.tuple_elem_tys(ty_param)?;
        if expected.len() != args.len() {
            return Ok(args);
        }
        expected
            .into_iter()
            .zip(args)
            .map(|(expected, arg)| self.coerce_arg(expected, arg))
            .collect()
    }

    fn expect_arg_count(&mut self, expr: ExprId, ty_param: InstrId, actual: usize) -> LowerResult<()> {
        let expected = self.tuple_elem_tys(ty_param)?.len();
        if expected == actual {
            Ok(())
        } else {
            Err(LowerError::ArgCount(expr))
        }
    }

    fn lit_expr(&mut self, token: TokenId, force: Option<IntType>) -> LowerResult<Typed> {
        let Base {
            types,
            lit_types,
            lit_vals,
            lits,
        } = self.base();
        let (val, parsed_type) = self.x.lit(token)?;
        let int_type = match (parsed_type, force) {
            (Some(IntType::Int), Some(force)) => Some(force),
            (other, _) => other,
        };
        let (tydef_lit, tydef, valdef, sigdef, val) = match (val, int_type) {
            // Unreachable cases.
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
            (Val::Uint31(n), Some(IntType::Uint32)) => (
                lit_types.uint31,
                types.uint32,
                lit_vals.uint31,
                lits.uint31_realize_uint32,
                Val::Uint31(n),
            ),
            (Val::Uint32(n), Some(IntType::Uint32)) => (
                lit_types.uint32,
                types.uint32,
                lit_vals.uint32,
                lits.uint32_realize_uint32,
                Val::Uint32(n),
            ),
            (Val::Uint63(_) | Val::Uint64(_) | Val::Uint(_), Some(IntType::Uint32)) => {
                return Err(LowerError::Uint32High(token));
            }

            // Integer literals ending in `i32`.
            (Val::Uint31(n), Some(IntType::Int32)) => (
                lit_types.uint31,
                types.int32,
                lit_vals.uint31,
                lits.uint31_realize_int32,
                Val::Uint31(n),
            ),
            (Val::Int32(n), Some(IntType::Int32)) => (
                lit_types.int32,
                types.int32,
                lit_vals.int32,
                lits.int32_realize_int32,
                Val::Int32(n),
            ),
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
            (Val::Uint31(n), Some(IntType::Uint64)) => (
                lit_types.uint31,
                types.uint64,
                lit_vals.uint31,
                lits.uint31_realize_uint64,
                Val::Uint31(n),
            ),
            (Val::Uint32(n), Some(IntType::Uint64)) => (
                lit_types.uint32,
                types.uint64,
                lit_vals.uint32,
                lits.uint32_realize_uint64,
                Val::Uint32(n),
            ),
            (Val::Uint63(n), Some(IntType::Uint64)) => (
                lit_types.uint63,
                types.uint64,
                lit_vals.uint63,
                lits.uint63_realize_uint64,
                Val::Uint63(n),
            ),
            (Val::Uint64(n), Some(IntType::Uint64)) => (
                lit_types.uint64,
                types.uint64,
                lit_vals.uint64,
                lits.uint64_realize_uint64,
                Val::Uint64(n),
            ),
            (Val::Uint(_), Some(IntType::Uint64)) => {
                return Err(LowerError::Uint64High(token));
            }

            // Integer literals ending in `i64`.
            (Val::Uint31(n), Some(IntType::Int64)) => (
                lit_types.uint31,
                types.int64,
                lit_vals.uint31,
                lits.uint31_realize_int64,
                Val::Uint31(n),
            ),
            (Val::Uint32(n), Some(IntType::Int64)) => (
                lit_types.uint32,
                types.int64,
                lit_vals.uint32,
                lits.uint32_realize_int64,
                Val::Uint32(n),
            ),
            (Val::Int32(n), Some(IntType::Int64)) => (
                lit_types.int32,
                types.int64,
                lit_vals.int32,
                lits.int32_realize_int64,
                Val::Int32(n),
            ),
            (Val::Uint63(n), Some(IntType::Int64)) => (
                lit_types.uint63,
                types.int64,
                lit_vals.uint63,
                lits.uint63_realize_int64,
                Val::Uint63(n),
            ),
            (Val::Int64(n), Some(IntType::Int64)) => (
                lit_types.int64,
                types.int64,
                lit_vals.int64,
                lits.int64_realize_int64,
                Val::Int64(n),
            ),
            (Val::Int(_), Some(IntType::Int64)) => return Err(LowerError::Int64Low(token)),
            (Val::Uint64(_) | Val::Uint(_), Some(IntType::Int64)) => {
                return Err(LowerError::Int64High(token));
            }

            // Integer literals ending in `u`.
            (Val::Uint31(n), Some(IntType::Uint)) => (
                lit_types.uint31,
                types.uint,
                lit_vals.uint31,
                lits.uint31_realize_uint,
                Val::Uint31(n),
            ),
            (Val::Uint32(n), Some(IntType::Uint)) => (
                lit_types.uint32,
                types.uint,
                lit_vals.uint32,
                lits.uint32_realize_uint,
                Val::Uint32(n),
            ),
            (Val::Uint63(n), Some(IntType::Uint)) => (
                lit_types.uint63,
                types.uint,
                lit_vals.uint63,
                lits.uint63_realize_uint,
                Val::Uint63(n),
            ),
            (Val::Uint64(n), Some(IntType::Uint)) => (
                lit_types.uint64,
                types.uint,
                lit_vals.uint64,
                lits.uint64_realize_uint,
                Val::Uint64(n),
            ),
            (Val::Uint(n), Some(IntType::Uint)) => (
                lit_types.uint,
                types.uint,
                lit_vals.uint,
                lits.uint_realize_uint,
                Val::Uint(n),
            ),

            // Integer literals with no suffix.
            (Val::Uint31(n), Some(IntType::Int)) => (
                lit_types.uint31,
                types.int,
                lit_vals.uint31,
                lits.uint31_realize_int,
                Val::Uint31(n),
            ),
            (Val::Uint32(n), Some(IntType::Int)) => (
                lit_types.uint32,
                types.int,
                lit_vals.uint32,
                lits.uint32_realize_int,
                Val::Uint32(n),
            ),
            (Val::Int32(n), Some(IntType::Int)) => (
                lit_types.int32,
                types.int,
                lit_vals.int32,
                lits.int32_realize_int,
                Val::Int32(n),
            ),
            (Val::Uint63(n), Some(IntType::Int)) => (
                lit_types.uint63,
                types.int,
                lit_vals.uint63,
                lits.uint63_realize_int,
                Val::Uint63(n),
            ),
            (Val::Uint64(n), Some(IntType::Int)) => (
                lit_types.uint64,
                types.int,
                lit_vals.uint64,
                lits.uint64_realize_int,
                Val::Uint64(n),
            ),
            (Val::Int64(n), Some(IntType::Int)) => (
                lit_types.int64,
                types.int,
                lit_vals.int64,
                lits.int64_realize_int,
                Val::Int64(n),
            ),
            (Val::Uint(n), Some(IntType::Int)) => (
                lit_types.uint,
                types.int,
                lit_vals.uint,
                lits.uint_realize_int,
                Val::Uint(n),
            ),
            (Val::Int(n), Some(IntType::Int)) => (
                lit_types.int,
                types.int,
                lit_vals.int,
                lits.int_realize_int,
                Val::Int(n),
            ),

            // Other literals.
            (Val::Char(c), None) => (
                lit_types.char,
                types.char,
                lit_vals.char,
                lits.char_realize,
                Val::Char(c),
            ),
            (Val::String(s), None) => (
                lit_types.string,
                types.string,
                lit_vals.string,
                lits.string_realize,
                Val::String(s),
            ),
        };

        let Valdef(body_valdef) = self.x.ir.valdefs[valdef];

        let lambda_ty_lit = self.extract_ty(tydef_lit)?;
        let _construct_ty_lit = self.invoke_force(lambda_ty_lit)?;

        let lambda_ty = self.extract_ty(tydef)?;
        let construct_ty = self.invoke_force(lambda_ty)?;
        let args_ty = self.x.items(&construct_ty);
        let ty = self.emit(Instr::Apply {
            lambda: lambda_ty,
            args: args_ty,
        });

        let val_lit = self.emit(Instr::Lit { val });

        let available = self.x.entrypoints(&self.slots, &[]);
        let start = self.emit(Instr::Lambda);
        let (construct_val, _) = self.x.invoke_need(body_valdef, &available)?;
        let args_val = self.x.items(&construct_val);
        let bind = self.emit(Instr::Bind {
            args: args_val,
            bind: val_lit,
        });
        let bind = self.emit(Instr::EndLambda { start, result: bind });
        let slot_lit = self.emit(Instr::BindValdef { def: valdef, bind });

        let start = self.emit(Instr::Lambda);
        let items = self.x.items(&[lambda_ty_lit, lambda_ty, slot_lit]);
        let result = self.emit(Instr::Stack { items });
        let param = self.emit(Instr::EndLambda { start, result });
        let lambda_func = self.emit(Instr::NeedSigdef { def: sigdef, param });
        let args = self.x.items(&[]);
        let func = self.emit(Instr::Apply {
            lambda: lambda_func,
            args,
        });
        let ty_unit = self.x.ty_unit();
        let arg = self.instr_tuple(ty_unit, &[]).val;
        let typed = self.instr(ty, Expr::Call { func, arg });
        self.literal_tokens.insert(typed.val, token);
        Ok(typed)
    }

    fn expect_ty(&mut self, expected: InstrId, actual: InstrId) -> LowerResult<()> {
        if self.same_ty(expected, actual)? {
            Ok(())
        } else {
            if std::env::var_os("MOSS_TRACE_TYPES").is_some() {
                let expected_reduced = self.reduce_alias_ty(expected)?;
                let actual_reduced = self.reduce_alias_ty(actual)?;
                eprintln!(
                    "type mismatch expected={expected:?} -> {:?} actual={actual:?} -> {:?}",
                    self.x.ir.instrs[expected_reduced],
                    self.x.ir.instrs[actual_reduced],
                );
                if let Instr::Tuple { elems } = self.x.ir.instrs[expected_reduced] {
                    for (index, item) in elems.into_iter().enumerate() {
                        let ty = self.x.ir.items[item];
                        eprintln!("  expected[{index}]={ty:?} -> {:?}", self.x.ir.instrs[ty]);
                    }
                }
                if let Instr::Tuple { elems } = self.x.ir.instrs[actual_reduced] {
                    for (index, item) in elems.into_iter().enumerate() {
                        let ty = self.x.ir.items[item];
                        eprintln!("  actual[{index}]={ty:?} -> {:?}", self.x.ir.instrs[ty]);
                    }
                }
            }
            Err(self.x.todo_no_loc())
        }
    }

    fn expr(&mut self, expr: ExprId) -> LowerResult<Typed> {
        match self.x.tree.exprs[expr] {
            parse::Expr::Lit(token) => self.lit_expr(token, None),
            parse::Expr::Path(path) => {
                if path.prefix.is_empty()
                    && self.x.slice(path.last) == "this"
                    && !self.is_method
                {
                    return Err(LowerError::ThisNotMethod(expr));
                }
                let name = self.x.name(path.last);
                if path.prefix.is_empty()
                    && let Some(&typed) = self.locals.get(&name)
                {
                    return Ok(typed);
                }
                let Named::Valdef(valdef) = self.x.path(path)? else {
                    return Err(LowerError::NotVal(path.last));
                };
                let Valdef(body) = self.x.ir.valdefs[valdef];
                let val = self.extract_val(valdef)?;
                let construct = self.invoke_force(val)?;
                if std::env::var_os("MOSS_TRACE_VALS").is_some() {
                    match self.x.ir.instrs[val] {
                        Instr::NeedValdef { param, .. } => {
                            eprintln!("  val param={param:?} -> {:?}", self.x.ir.instrs[param]);
                            if let Ok(result) = self.x.materialize_lambda(param) {
                                eprintln!("  val param materialized={result:?} -> {:?}", self.x.ir.instrs[result]);
                                if let Some(items) = self.x.stack_instrs(result) {
                                    for (index, item) in items.into_iter().enumerate() {
                                        eprintln!(
                                            "    val param[{index}]={item:?} -> {:?}",
                                            self.x.ir.instrs[item]
                                        );
                                    }
                                }
                            }
                        }
                        Instr::BindValdef { bind, .. } => {
                            eprintln!("  val bind={bind:?} -> {:?}", self.x.ir.instrs[bind]);
                            if let Ok(result) = self.x.materialize_lambda(bind) {
                                eprintln!("  val bind materialized={result:?} -> {:?}", self.x.ir.instrs[result]);
                            }
                        }
                        _ => {}
                    }
                    eprintln!(
                        "val {} def={valdef:?} val={val:?} instr={:?} construct={:?}",
                        self.x.slice(path.last),
                        self.x.ir.instrs[val],
                        construct
                    );
                }
                let ty = self.inline(body, &construct)?;
                Ok(self.instr(ty, Expr::Val { val }))
            }
            parse::Expr::Tag(path, inner) => {
                let Named::Tagdef(tagdef) = self.x.path(path)? else {
                    return Err(LowerError::NotNominal(path.last));
                };
                let inside = self.expr(inner)?;
                let Tagdef(body) = self.x.ir.tagdefs[tagdef];
                let lambda = self.emit(Instr::Tagdef { def: tagdef });
                let construct = self.invoke_force(lambda)?;
                let args = self.x.items(&construct);
                let ty = self.emit(Instr::Apply { lambda, args });
                let expected = self.inline(body, &construct)?;
                self.expect_ty(expected, inside.ty)?;
                let inner = inside.val;
                Ok(self.instr(ty, Expr::Nominal { ty, inner }))
            }
            parse::Expr::Record(_lbrace, fields, _rbrace) => {
                let sorted = fields
                    .into_iter()
                    .map(|field| {
                        let Field { name, val } = self.x.tree.fields[field];
                        Ok((self.x.slice(name), self.expr(val)?))
                    })
                    .collect::<LowerResult<BTreeMap<&str, Typed>>>()?;
                let (fields_ty, fields_val): (Vec<_>, Vec<_>) = sorted
                    .into_iter()
                    .map(|(string, field)| {
                        let name = self.x.ir.strings.make_id(string);
                        ((name, field.ty), (name, field.val))
                    })
                    .unzip();
                let ty = self.ty_record(&fields_ty);
                Ok(self.instr_record(ty, &fields_val))
            }
            parse::Expr::Field(object, field) => {
                let obj = self.expr(object)?;
                let name = self.x.name(field);
                // TODO: Make this not be linear time.
                let fields = self.fields(obj.ty)?;
                let index = FieldId::from_usize(
                    fields.iter().position(|&(field, _)| field == name).unwrap(),
                );
                let (_, ty) = fields[index.index()];
                let record = obj.val;
                Ok(self.instr(ty, Expr::Field { record, index }))
            }
            parse::Expr::Method(object, method, arguments) => {
                if std::env::var_os("MOSS_TRACE_CALLS").is_some() {
                    let current = self.current_fn.map(|name| self.x.slice(name)).unwrap_or("?");
                    eprintln!(
                        "call {current} -> .{} instrs={}",
                        self.x.slice(method),
                        self.x.ir.instrs.len()
                    );
                }
                let obj = self.expr(object)?;
                let name = self.x.name(method);
                let named = self.method(obj.ty, name).ok_or(LowerError::Undefined(method))?;
                let mut destruct = Vec::new();
                if let Some(this_ty) = self.ty_named("This") {
                    destruct.push(self.bind_ty(this_ty, obj.ty, &destruct)?);
                }
                if let Some(this_val) = self.val_named("this") {
                    destruct.push(self.bind_val(this_val, obj.val, &destruct)?);
                }
                let lowered = arguments
                    .into_iter()
                    .map(|arg| self.expr(arg))
                    .collect::<LowerResult<Vec<Typed>>>()?;
                self.call_named(named, &destruct, &lowered)
            }
            parse::Expr::Call(callee, binds, arguments) => {
                if std::env::var_os("MOSS_TRACE_CALLS").is_some() {
                    let current = self.current_fn.map(|name| self.x.slice(name)).unwrap_or("?");
                    eprintln!(
                        "call {current} -> {} instrs={}",
                        self.x.slice(callee.last),
                        self.x.ir.instrs.len()
                    );
                }
                let destruct = binds
                    .into_iter()
                    .map(|bind| self.x.bind(&self.slots, bind))
                    .collect::<LowerResult<Vec<_>>>()?;
                let available = self.x.entrypoints(&self.slots, &destruct);
                let (lambda, construct) = match self.x.path(callee)? {
                    Named::Sigdef(sigdef) => {
                        let lambda = self.extract_sig(sigdef)?;
                        let construct = self.x.invoke_force(lambda, &available)?;
                        (lambda, construct)
                    }
                    Named::Fndef(fndef) => {
                        let lambda = self.emit(Instr::Fndef { def: fndef });
                        let construct = self.x.invoke_force(lambda, &available)?;
                        (lambda, construct)
                    }
                    _ => return Err(LowerError::NotFn(callee.last)),
                };
                let args = self.x.items(&construct);
                let func = self.emit(Instr::Apply { lambda, args });
                let (ty_param, ty_result) = self.sig(func)?;
                let lowered = arguments
                    .into_iter()
                    .map(|arg| self.expr(arg))
                    .collect::<LowerResult<Vec<Typed>>>()?;
                self.expect_arg_count(expr, ty_param, lowered.len())?;
                let lowered = self.coerce_args(ty_param, lowered)?;
                let (args_ty, args_val): (Vec<_>, Vec<_>) =
                    lowered.into_iter().map(|arg| (arg.ty, arg.val)).unzip();
                let ty_args = self.ty_tuple(&args_ty);
                self.expect_ty(ty_param, ty_args)?;
                let arg = self.instr_tuple(ty_args, &args_val).val;
                Ok(self.instr(ty_result, Expr::Call { func, arg }))
            }
            parse::Expr::Unary(op, inner) => {
                let v = self.expr(inner)?;
                let name = match op {
                    Unop::Neg => "neg",
                    Unop::Not => "not",
                };
                let named = self.op_fn(name).ok_or_else(|| self.x.todo_no_loc())?;
                self.call_op_unary(named, v)
            }
            parse::Expr::Binary(left, op, right) => {
                let l = self.expr(left)?;
                let r = self.expr(right)?;
                let name = match op {
                    Binop::Eq => "eq",
                    Binop::Ne => "ne",
                    Binop::Lt => "lt",
                    Binop::Gt => "gt",
                    Binop::Le => "le",
                    Binop::Ge => "ge",
                    Binop::Add => "add",
                    Binop::Sub => "sub",
                    Binop::Mul => "mul",
                    Binop::Div => "div",
                    Binop::Rem => "rem",
                    Binop::Shl => "shl",
                    Binop::Shr => "shr",
                    Binop::And => "and",
                    Binop::Or => "or",
                    Binop::Xor => "xor",
                };
                let named = self.op_fn(name).ok_or_else(|| self.x.todo_no_loc())?;
                self.call_op_binary(named, l, r)
            }
            parse::Expr::If(cond, yes, no) => {
                let unit = self.x.ty_unit();
                let cond = self.expr(cond)?;
                // We don't know the type yet.
                let start = self.emit(Instr::If {
                    ty: unit,
                    cond: cond.val,
                });
                let yes = self.block(yes)?;
                // Rewrite the original instruction now that we know the type.
                let ty = yes.ty;
                self.x.ir.instrs[start] = Instr::If { ty, cond: cond.val };
                self.emit(Instr::Else { result: yes.val });
                let no = match no {
                    None => {
                        let ty = self.ty_tuple(&[]);
                        self.instr_tuple(ty, &[])
                    }
                    Some(block) => self.block(block)?,
                };
                let end = self.emit(Instr::EndIf { result: no.val });
                self.expect_ty(ty, no.ty)?;
                Ok(Typed { ty, val: end })
            }
            parse::Expr::Bind(_, bindings) => {
                let mut slots = Vec::new();
                for binding in bindings {
                    let slot = match self.x.tree.bindings[binding] {
                        parse::Binding::Single(bind) => self.x.bind(&self.slots, bind)?,
                        parse::Binding::Composite(expr) => self.expr(expr)?.val,
                    };
                    slots.push(slot);
                }
                let items = self.x.items(&slots);
                let ty = self.emit(Instr::Context);
                let val = self.emit(Instr::Stack { items });
                Ok(Typed { ty, val })
            }
        }
    }

    fn stmts(&mut self, stmts: IdRange<StmtId>) -> LowerResult<()> {
        for stmt in stmts {
            match self.x.tree.stmts[stmt] {
                Stmt::Let(name, rhs) => {
                    let Typed { ty, val } = self.expr(rhs)?;
                    let copy = self.instr(ty, Expr::Copy { value: val });
                    self.set(name, copy);
                }
                Stmt::Var(name, rhs) => {
                    let Typed { ty, val } = self.expr(rhs)?;
                    let copy = self.instr(ty, Expr::Copy { value: val });
                    self.set(name, copy);
                }
                Stmt::Assign(lhs, rhs) => match self.x.tree.exprs[lhs] {
                    parse::Expr::Path(path) => {
                        assert!(path.prefix.is_empty());
                        let before = self.get(path.last)?;
                        let after = self.expr(rhs)?;
                        self.emit(Instr::Set {
                            lhs: before.val,
                            rhs: after.val,
                        });
                    }
                    _ => panic!(),
                },
                Stmt::While(cond, body) => {
                    assert!(body.expr.is_none());
                    self.emit(Instr::Loop);
                    let local = self.expr(cond)?;
                    let ty = self.ty_tuple(&[]);
                    self.emit(Instr::If {
                        ty,
                        cond: local.val,
                    });
                    let keep = self.slots.len();
                    self.stmts(body.stmts)?;
                    self.slots.truncate(keep);
                    let fake = self.emit(Instr::Br { depth: Depth(1) });
                    self.emit(Instr::EndIf { result: fake });
                    self.emit(Instr::EndLoop);
                }
                Stmt::Expr(expr) => {
                    match self.x.tree.exprs[expr] {
                        parse::Expr::Bind(_, bindings) => self.bind_stmt(bindings)?,
                        _ => {
                            self.expr(expr)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn block(&mut self, block: Block) -> LowerResult<Typed> {
        let keep = self.slots.len();
        let result = (|| {
            self.stmts(block.stmts)?;
            match block.expr {
                Some(expr) => Ok(self.expr(expr)?),
                None => {
                    let ty = self.x.ty_unit();
                    let val = self.instr_tuple(ty, &[]).val;
                    Ok(Typed { ty, val })
                }
            }
        })();
        self.slots.truncate(keep);
        result
    }

    fn bind_stmt_expr(&mut self, expr: ExprId) -> LowerResult<()> {
        match self.x.tree.exprs[expr] {
            parse::Expr::Bind(_, bindings) => self.bind_stmt(bindings),
            parse::Expr::Call(path, binds, arguments)
                if path.prefix.is_empty()
                    && binds.is_empty()
                    && arguments.is_empty()
                    && self.x.slice(path.last) == "int32_unchecked" =>
            {
                let _ = self.x.path(path)?;
                Ok(())
            }
            _ => Err(self.x.todo_no_loc()),
        }
    }

    fn bind_stmt(&mut self, bindings: IdRange<parse::BindingId>) -> LowerResult<()> {
        for binding in bindings {
            match self.x.tree.bindings[binding] {
                parse::Binding::Single(bind) => {
                    let slot = self.x.bind(&self.slots, bind)?;
                    self.slots.push(slot);
                }
                parse::Binding::Composite(expr) => {
                    self.bind_stmt_expr(expr)?;
                }
            }
        }
        Ok(())
    }

    fn body(&mut self, fndef: parse::Fndef, id_decl: FndefId) -> LowerResult<Body> {
        let parse::Fndef {
            name,
            needs: _,
            params,
            result: _,
            def,
        } = fndef;
        let body = def.unwrap();
        if std::env::var_os("MOSS_TRACE_BODIES").is_some() {
            eprintln!("body {} {:?}", self.x.slice(name), id_decl);
        }
        self.current_fn = Some(name);
        let builder = self.x.builder();
        let (slots, tuple_ty, _) = self.x.parse_sig(fndef).unwrap();
        self.slots = slots;
        let tuple_local = self.instr(tuple_ty, Expr::Param { ty: tuple_ty });
        let Instr::Tuple { elems: types } = self.x.ir.instrs[tuple_ty] else {
            unreachable!()
        };
        let mut index = 0;
        for (param, item) in params.into_iter().zip(types) {
            let ty = self.x.ir.items[item];
            let local = self.instr(
                ty,
                Expr::Elem {
                    tuple: tuple_local.val,
                    index: ElemId::new(index),
                },
            );
            let name = self.x.tree.params[param].name;
            self.set(name, local);
            index += 1;
        }
        let ret = self.block(body)?;
        let ret = self.instr(ret.ty, Expr::Copy { value: ret.val });
        Ok(self.x.finish(builder, ret.val))
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
    skip_bodies: bool,
) -> LowerResult<ModuleId> {
    assert_eq!(tree.imports.len(), imports.len());
    let module = ir.modules.push(());
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
        skip_bodies,
        funcs: Vec::new(),
        attaches: Vec::new(),
        detaches: Vec::new(),
        bind_ctx_items_cache: HashMap::new(),
        need_ctx_items_cache: HashMap::new(),
        materialized_lambdas: HashMap::new(),
        invoke_cache: HashMap::new(),
        invoke_active: HashSet::new(),
        specificity_cache: HashMap::new(),
        specificity_active: HashSet::new(),
        ctx_contains_ty_cache: HashMap::new(),
        ctx_contains_ty_active: HashSet::new(),
        ctx_contains_sig_cache: HashMap::new(),
        ctx_contains_sig_active: HashSet::new(),
        ctx_contains_val_cache: HashMap::new(),
        ctx_contains_val_active: HashSet::new(),
        ctx_contains_ctx_cache: HashMap::new(),
        ctx_contains_ctx_active: HashSet::new(),
    };
    lower.program()?;
    Ok(module)
}
