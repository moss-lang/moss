use std::{
    array,
    backtrace::Backtrace,
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    fmt,
    mem::take,
    ops::{Add, Index, IndexMut, Sub},
};

use index_vec::{IndexSlice, IndexVec, define_index_type};
use indexmap::{IndexMap, IndexSet};

use crate::{
    dump::dump,
    intern::{StrId, Strings},
    lex::{TokenId, TokenStarts, relex, string},
    parse::{self, Binop, Block, ExprId, Field, Path, Spec, Stmt, StmtId, Tree, Unop},
    prelude::{Arith, Base, Builders, Numerals},
    range::{Inclusive, expr_range, path_range, single},
    tuples::{TupleRange, Tuples},
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

define_index_type! {
    pub struct NodeId = u32;
}

pub type NodeList = TupleRange;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Level(u8);

impl Level {
    const ZERO: Self = Self(0);

    const ONE: Self = Self(1);

    fn succ(self) -> Self {
        self + Self::ONE
    }
}

impl Add for Level {
    type Output = Level;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0.strict_add(rhs.0))
    }
}

impl Sub for Level {
    type Output = Level;

    fn sub(self, rhs: Self) -> Self::Output {
        Self(self.0.strict_sub(rhs.0))
    }
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// A correspondence between `a`-side and `b`-side [`Level`]s, threaded through
/// [`Lower::unify`] as it descends under matching binders. Equivalence of two
/// terms is decided against this renaming rather than by canonicalizing levels;
/// the caller seeds it with the correspondence of the enclosing context.
#[derive(Clone, Copy)]
struct Renaming {
    /// Maps each `b`-side level to the `a`-side level it corresponds to.
    a_of_b: LevelMap,
}

impl Renaming {
    /// The identity correspondence: `a` and `b` share their enclosing context.
    fn identity() -> Self {
        Self {
            a_of_b: LevelMap::default(),
        }
    }

    /// Record that we have descended under binder `a` on the left and `b` on the right.
    fn bind(&mut self, a: Level, b: Level) {
        self.a_of_b[b] = a;
    }
}

/// A mapping from [`Level`] to [`Level`].
#[derive(Clone, Copy)]
struct LevelMap([Level; 256]);

impl Default for LevelMap {
    fn default() -> Self {
        Self(array::from_fn(|level| Level(level.try_into().unwrap())))
    }
}

impl Index<Level> for LevelMap {
    type Output = Level;

    fn index(&self, level: Level) -> &Level {
        &self.0[usize::from(level.0)]
    }
}

impl IndexMut<Level> for LevelMap {
    fn index_mut(&mut self, level: Level) -> &mut Level {
        &mut self.0[usize::from(level.0)]
    }
}

/// Which kind of contextual definition an `extract` is resolving, paired with its id.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum DefKind {
    Ty(TydefId),
    Sig(SigdefId),
    Val(ValdefId),
    Ctx(CtxdefId),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Node {
    Nothing,
    Lambda {
        level: Level,
        needs: NodeList,
        result: NodeId,
    },
    Apply {
        lambda: NodeId,
        args: NodeList,
    },
    List {
        items: NodeList,
    },
    NeedTydef {
        level: Level,
        def: TydefId,
        param: NodeId,
    },
    NeedSigdef {
        level: Level,
        def: SigdefId,
        param: NodeId,
    },
    NeedValdef {
        level: Level,
        def: ValdefId,
        param: NodeId,
    },
    NeedCtxdef {
        level: Level,
        def: CtxdefId,
        param: NodeId,
    },
    Tagdef {
        def: TagdefId,
    },
    Aliasdef {
        def: AliasdefId,
    },
    Tuple {
        elems: NodeList,
    },
    Context,
    Fndef {
        def: FndefId,
    },
    Get {
        ctx: NodeId,
        slot: SlotId,
    },
    Lit {
        val: Val,
    },
    Bind {
        args: NodeList,
        bind: NodeId,
    },
    BindTydef {
        def: TydefId,
        bind: NodeId,
    },
    BindSigdef {
        def: SigdefId,
        bind: NodeId,
    },
    BindValdef {
        def: ValdefId,
        bind: NodeId,
    },
    BindCtxdef {
        def: CtxdefId,
        bind: NodeId,
    },
    Sig {
        param: NodeId,
        result: NodeId,
    },
}

pub type InstrList = IdRange<ItemId>;

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
        ty: NodeId,
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
        ty: NodeId,

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
        val: NodeId,
    },

    /// Call a contextual function.
    ///
    /// Type: the function's result type.
    Call {
        /// The function.
        func: NodeId,

        /// The runtime argument value.
        arg: InstrId,
    },

    /// Yield a context instead of a value.
    ///
    /// Type: [`Node::Context`].
    Bind { ctx: NodeId },
}

/// An instruction.
#[derive(Clone, Copy, Debug)]
pub enum Instr {
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
        ty: NodeId,

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
        ty: NodeId,

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

#[derive(Clone, Copy, Debug)]
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
pub struct Tydef(pub NodeId);

/// A lambda that takes contextual parameters and returns the inner type for this nominal type.
#[derive(Clone, Copy, Debug)]
pub struct Tagdef(pub NodeId);

/// A lambda that takes contextual parameters and returns a type.
#[derive(Clone, Copy, Debug)]
pub struct Aliasdef(pub NodeId);

/// A lambda that takes contextual parameters and returns a function signature.
#[derive(Clone, Copy, Debug)]
pub struct Sigdef(pub NodeId);

/// A lambda that takes contextual parameters and returns a type.
#[derive(Clone, Copy, Debug)]
pub struct Valdef(pub NodeId);

/// A lambda that returns a lambda that returns a context.
#[derive(Clone, Copy, Debug)]
pub struct Ctxdef(pub NodeId);

#[derive(Debug, Default)]
pub struct IR {
    /// Basically just counts how many modules there are.
    pub modules: IndexVec<ModuleId, ()>,

    /// String interning.
    pub strings: Strings,

    /// Hash-consed arena of static nodes, like types and contexts.
    pub nodes: IndexSet<Node>,

    /// Hash-consed arena of lists of indices into the node arena.
    pub lists: Tuples<NodeId>,

    /// Arena of instructions.
    ///
    /// Basically composed of a sequence of "blocks" where each block is a contiguous range of
    /// instructions in this arena, corresponding to a single function body.
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

    /// Functions that are given definitions.
    pub fndefs: IndexVec<FndefId, Sigdef>,

    /// Contextual values.
    pub valdefs: IndexVec<ValdefId, Valdef>,

    /// Composite contexts.
    pub ctxdefs: IndexVec<CtxdefId, Ctxdef>,

    /// Actual function bodies.
    pub bodies: IndexVec<FndefId, Body>,
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

    /// Absolute ordering of global bindings.
    ///
    /// Not semantically important, but used for human-readable IR dumps.
    pub order: Vec<Named>,
}

impl Names {
    fn create_named(&mut self, module: ModuleId, name: StrId, named: Named) {
        let prev = self.names.insert((module, name), named);
        assert!(prev.is_none());
        self.order.push(named);
    }

    fn create_attached(&mut self, tagdef: TagdefId, name: StrId, named: NamedFn) {
        let prev = self.attached.insert((tagdef, name), named);
        assert!(prev.is_none());
        self.order.push(named.into());
    }

    fn create_detached(&mut self, module: ModuleId, name: StrId, named: NamedFn) {
        let prev = self.detached.insert((module, name), named);
        assert!(prev.is_none());
        self.order.push(named.into());
    }
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
            LowerError::Overflow(expr) => (Some(ctx.expr(expr)), "integer too large".to_owned()),
            LowerError::BindMissing(bind) => (
                Some(ctx.bind(bind)),
                "`bind` missing an actual binding".to_owned(),
            ),
        }
    }
}

type LowerResult<T> = Result<T, LowerError>;

trait Transform {
    fn check(&self, id: NodeId) -> Option<NodeId>;
    fn floor(&self) -> Level;
    fn map(&self, node: Node) -> Node;
    fn put(&mut self, key: NodeId, val: NodeId);
}

struct Raise {
    floor: Level,
    add: Level,
    cache: HashMap<NodeId, NodeId>, // TODO: Use a smarter and less temporary hashing strategy.
}

impl Transform for Raise {
    fn check(&self, id: NodeId) -> Option<NodeId> {
        self.cache.get(&id).copied()
    }

    fn floor(&self) -> Level {
        self.floor
    }

    fn map(&self, node: Node) -> Node {
        match node {
            Node::Lambda {
                level,
                needs,
                result,
            } => Node::Lambda {
                level: level + self.add,
                needs,
                result,
            },
            Node::NeedTydef { level, def, param } => Node::NeedTydef {
                level: level + self.add,
                def,
                param,
            },
            Node::NeedSigdef { level, def, param } => Node::NeedSigdef {
                level: level + self.add,
                def,
                param,
            },
            Node::NeedValdef { level, def, param } => Node::NeedValdef {
                level: level + self.add,
                def,
                param,
            },
            Node::NeedCtxdef { level, def, param } => Node::NeedCtxdef {
                level: level + self.add,
                def,
                param,
            },
            _ => node,
        }
    }

    fn put(&mut self, key: NodeId, val: NodeId) {
        self.cache.insert(key, val);
    }
}

struct Substitute {
    floor: Level,
    mapping: HashMap<NodeId, NodeId>,
}

impl Transform for Substitute {
    fn check(&self, id: NodeId) -> Option<NodeId> {
        self.mapping.get(&id).copied()
    }

    fn floor(&self) -> Level {
        self.floor
    }

    fn map(&self, node: Node) -> Node {
        node
    }

    fn put(&mut self, key: NodeId, val: NodeId) {
        self.mapping.insert(key, val);
    }
}

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
    funcs: Vec<(parse::FuncdefId, FndefId)>,
    attaches: Vec<(parse::AttachdefId, TagdefId, FndefId)>,
    detaches: Vec<(parse::DetachdefId, FndefId)>,
}

impl<'a> Lower<'a> {
    fn todo(&self, token: TokenId) -> LowerError {
        dump(self.ir, self.names);
        LowerError::Todo(token, Backtrace::capture())
    }

    fn todo_no_loc(&self) -> LowerError {
        self.todo(TokenId::from_raw(0))
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

    fn node(&self, id: NodeId) -> Node {
        self.ir.nodes[id.index()]
    }

    fn mk_node(&mut self, node: Node) -> NodeId {
        let (i, _) = self.ir.nodes.insert_full(node);
        NodeId::from_usize(i)
    }

    fn mk_node_list(&mut self, nodes: &[NodeId]) -> NodeList {
        self.ir.lists.make(nodes)
    }

    fn ty_tuple(&mut self, elems: &[NodeId]) -> NodeId {
        let elems = self.mk_node_list(elems);
        self.mk_node(Node::Tuple { elems })
    }

    fn ty_record(&mut self, fields: &[(StrId, NodeId)]) -> NodeId {
        todo!()
    }

    fn ty_unit(&mut self) -> NodeId {
        self.ty_tuple(&[])
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

    fn map_nodes(
        &mut self,
        nodes: NodeList,
        mut f: impl FnMut(&mut Self, NodeId) -> NodeId,
    ) -> NodeList {
        let mapped = Vec::from_iter(nodes.into_iter().map(|loc| f(self, self.ir.lists[loc])));
        self.mk_node_list(&mapped)
    }

    fn transforms(&mut self, map: &mut impl Transform, nodes: NodeList) -> NodeList {
        self.map_nodes(nodes, |this, node| this.transform(map, node))
    }

    fn transform(&mut self, map: &mut impl Transform, node: NodeId) -> NodeId {
        if let Some(mapped) = map.check(node) {
            return mapped;
        }
        let mapped = match self.node(node) {
            Node::Nothing => self.mk_node(map.map(Node::Nothing)),
            Node::Lambda {
                level,
                needs,
                result,
            } => {
                if level < map.floor() {
                    node
                } else {
                    let needs = self.transforms(map, needs);
                    let result = self.transform(map, result);
                    self.mk_node(map.map(Node::Lambda {
                        level,
                        needs,
                        result,
                    }))
                }
            }
            Node::Apply { lambda, args } => {
                let lambda = self.transform(map, lambda);
                let args = self.transforms(map, args);
                self.mk_node(map.map(Node::Apply { lambda, args }))
            }
            Node::List { items } => {
                let items = self.transforms(map, items);
                self.mk_node(map.map(Node::List { items }))
            }
            Node::NeedTydef { level, def, param } => {
                if level < map.floor() {
                    node
                } else {
                    let param = self.transform(map, param);
                    self.mk_node(map.map(Node::NeedTydef { level, def, param }))
                }
            }
            Node::NeedSigdef { level, def, param } => {
                if level < map.floor() {
                    node
                } else {
                    let param = self.transform(map, param);
                    self.mk_node(map.map(Node::NeedSigdef { level, def, param }))
                }
            }
            Node::NeedValdef { level, def, param } => {
                if level < map.floor() {
                    node
                } else {
                    let param = self.transform(map, param);
                    self.mk_node(map.map(Node::NeedValdef { level, def, param }))
                }
            }
            Node::NeedCtxdef { level, def, param } => {
                if level < map.floor() {
                    node
                } else {
                    let param = self.transform(map, param);
                    self.mk_node(map.map(Node::NeedCtxdef { level, def, param }))
                }
            }
            Node::Tagdef { def } => self.mk_node(map.map(Node::Tagdef { def })),
            Node::Aliasdef { def } => self.mk_node(map.map(Node::Aliasdef { def })),
            Node::Tuple { elems } => {
                let elems = self.transforms(map, elems);
                self.mk_node(map.map(Node::Tuple { elems }))
            }
            Node::Context => self.mk_node(map.map(Node::Context)),
            Node::Fndef { def } => self.mk_node(map.map(Node::Fndef { def })),
            Node::Get { ctx, slot } => {
                let ctx = self.transform(map, ctx);
                self.mk_node(map.map(Node::Get { ctx, slot }))
            }
            Node::Lit { val } => self.mk_node(map.map(Node::Lit { val })),
            Node::Bind { args, bind } => {
                let args = self.transforms(map, args);
                let bind = self.transform(map, bind);
                self.mk_node(map.map(Node::Bind { args, bind }))
            }
            Node::BindTydef { def, bind } => {
                let bind = self.transform(map, bind);
                self.mk_node(map.map(Node::BindTydef { def, bind }))
            }
            Node::BindSigdef { def, bind } => {
                let bind = self.transform(map, bind);
                self.mk_node(map.map(Node::BindSigdef { def, bind }))
            }
            Node::BindValdef { def, bind } => {
                let bind = self.transform(map, bind);
                self.mk_node(map.map(Node::BindValdef { def, bind }))
            }
            Node::BindCtxdef { def, bind } => {
                let bind = self.transform(map, bind);
                self.mk_node(map.map(Node::BindCtxdef { def, bind }))
            }
            Node::Sig { param, result } => {
                let param = self.transform(map, param);
                let result = self.transform(map, result);
                self.mk_node(map.map(Node::Sig { param, result }))
            }
        };
        map.put(node, mapped);
        mapped
    }

    /// Return a new node where all levels at least `floor` are incremented by `add`.
    fn raise(&mut self, node: NodeId, floor: Level, add: Level) -> NodeId {
        let mut map = Raise {
            floor,
            add,
            cache: HashMap::new(),
        };
        self.transform(&mut map, node)
    }

    fn substitute(
        &mut self,
        floor: Level,
        before: NodeList,
        after: NodeList,
        node: NodeId,
    ) -> NodeId {
        assert_eq!(before.len(), after.len());
        let mut mapping = HashMap::new();
        for (&x, &y) in (self.ir.lists[before].iter()).zip(self.ir.lists[after].iter()) {
            mapping.insert(x, y);
        }
        let mut map = Substitute { floor, mapping };
        self.transform(&mut map, node)
    }

    /// Explode a [`Node::NeedCtxdef`] into a list of individual pieces of context.
    fn explode(&mut self, level: Level, def: CtxdefId, param: NodeId) -> LowerResult<NodeList> {
        let Node::Lambda {
            level: _,
            needs,
            result,
        } = self.node(param)
        else {
            panic!()
        };
        assert!(needs.is_empty()); // TODO: Handle partially-applied parametric composite contexts.
        let Node::List { items } = self.node(result) else {
            panic!()
        };
        let Ctxdef(target) = self.ir.ctxdefs[def];
        let raised = self.raise(target, Level::ZERO, level);
        let Node::Lambda {
            level: level_target,
            needs: needs_target,
            result: result_target,
        } = self.node(raised)
        else {
            panic!()
        };
        assert_eq!(level_target, level); // The original level should have been zero.
        let substituted = self.substitute(level, needs_target, items, result_target);
        let Node::Lambda {
            level: level_substituted,
            needs: _,
            result: result_substituted,
        } = self.node(substituted)
        else {
            panic!()
        };
        assert_eq!(level_substituted, level.succ());
        let Node::List {
            items: items_result,
        } = self.node(result_substituted)
        else {
            panic!()
        };
        Ok(items_result)
    }

    /// If one of `binds` is a call that *forwards* the whole composite context `target` -- i.e. a
    /// context-returning function call whose return type is `bind <target>` (`std`'s
    /// `bind bootstrap()`, where `bootstrap: bind Std`) -- return that call node together with the
    /// number of members `target` has. The call's runtime value is those members flattened in
    /// ctxdef order, so the caller can forward member `k` as `Get { ctx: call, slot: k }`.
    fn forwarding_bind(
        &mut self,
        binds: &[NodeId],
        target: CtxdefId,
    ) -> LowerResult<Option<(NodeId, usize)>> {
        for &bind in binds {
            let reduced = self.reduce(bind)?;
            let Node::Sig { result: ret, .. } = self.node(reduced) else {
                continue;
            };
            let ret = self.reduce(ret)?;
            let Node::Lambda { result: ret_list, .. } = self.node(ret) else {
                continue;
            };
            let Node::List { items } = self.node(ret_list) else {
                continue;
            };
            // Only a single-context return (`bind Std`) is a pure forward.
            let [only] = self.ir.lists[items] else {
                continue;
            };
            let Node::NeedCtxdef { level, def, param } = self.node(only) else {
                continue;
            };
            if def != target {
                continue;
            }
            let members = self.explode(level, def, param)?;
            return Ok(Some((bind, self.ir.lists[members].len())));
        }
        Ok(None)
    }

    /// Satisfy a single `need` from the available context `destruct`, building the composite
    /// lambda that provides it, or `None` if nothing in `destruct` does.
    fn resolve_need(&mut self, need: NodeId, destruct: &[NodeId]) -> LowerResult<Option<NodeId>> {
        let (kind, param, level) = match self.node(need) {
            Node::NeedTydef { def, param, level } => (DefKind::Ty(def), param, level),
            Node::NeedSigdef { def, param, level } => (DefKind::Sig(def), param, level),
            Node::NeedValdef { def, param, level } => (DefKind::Val(def), param, level),
            Node::NeedCtxdef { def, param, level } => (DefKind::Ctx(def), param, level),
            _ => panic!(), // a needs list contains only `Need*` nodes
        };
        let Some((callee, synth)) = self.extract_lambda(destruct, kind, param)? else {
            // No single slot provides this need. A composite context can instead be *synthesized*
            // from its members scattered across `destruct` (e.g. `Bootstrap` assembled from `Wasi`
            // plus the type bindings supplied at a call site).
            if let DefKind::Ctx(def) = kind {
                let Node::Lambda { result, .. } = self.node(param) else {
                    panic!()
                };
                let Node::List { items } = self.node(result) else {
                    panic!()
                };
                let outer = self.ir.lists[items].to_vec();
                return Ok(Some(self.extract_ctx(level, destruct, def, &outer, true)?));
            }
            return Ok(None);
        };
        let Node::Lambda {
            level: level_synth,
            needs: needs_synth,
            result: result_synth,
        } = self.node(synth)
        else {
            panic!()
        };
        let Node::List { items } = self.node(result_synth) else {
            panic!()
        };
        let result_composite = self.mk_node(Node::Apply {
            lambda: callee,
            args: items,
        });
        Ok(Some(self.mk_node(Node::Lambda {
            level: level_synth,
            needs: needs_synth,
            result: result_composite,
        })))
    }

    fn invoke(&mut self, lambda: NodeId, destruct: &[NodeId]) -> LowerResult<Option<Vec<NodeId>>> {
        match self.node(lambda) {
            Node::Nothing => todo!(),
            Node::Lambda {
                level: _,
                needs,
                result: _,
            } => {
                let mut construct = Vec::new();
                for loc in needs {
                    let need = self.ir.lists[loc];
                    match self.resolve_need(need, destruct)? {
                        Some(composite) => construct.push(composite),
                        None => return Ok(None),
                    }
                }
                Ok(Some(construct))
            }
            Node::Apply { lambda, args } => todo!(),
            Node::List { items } => todo!(),
            Node::NeedTydef {
                level: _,
                def: _,
                param,
            }
            | Node::NeedSigdef {
                level: _,
                def: _,
                param,
            }
            | Node::NeedValdef {
                level: _,
                def: _,
                param,
            }
            | Node::NeedCtxdef {
                level: _,
                def: _,
                param,
            } => self.invoke(param, destruct),
            Node::Tagdef { def } => todo!(),
            // Transparent definitions unfold to their bodies, mirroring `reduce`.
            Node::Aliasdef { def } => {
                let body = self.ir.aliasdefs[def].0;
                self.invoke(body, destruct)
            }
            Node::Tuple { elems } => todo!(),
            Node::Context => todo!(),
            Node::Fndef { def } => {
                let body = self.ir.fndefs[def].0;
                self.invoke(body, destruct)
            }
            Node::Get { ctx, slot } => match {
                // Reduce the context first so a nested-context accessor (a `Get` chain) collapses to
                // the stuck `Apply { NeedCtxdef }` (or `List`) form the cases below expect.
                let ctx = self.reduce(ctx)?;
                self.node(ctx)
            } {
                Node::Nothing => todo!(),
                Node::Lambda {
                    level,
                    needs,
                    result,
                } => todo!(),
                Node::Apply {
                    lambda: curried,
                    args,
                } => match self.node(curried) {
                    Node::Nothing => todo!(),
                    Node::Lambda {
                        level,
                        needs,
                        result,
                    } => todo!(),
                    Node::Apply { lambda, args } => todo!(),
                    Node::List { items } => todo!(),
                    Node::NeedTydef { level, def, param } => todo!(),
                    Node::NeedSigdef { level, def, param } => todo!(),
                    Node::NeedValdef { level, def, param } => todo!(),
                    Node::NeedCtxdef { level, def, param } => {
                        assert!(args.is_empty()); // TODO: Handle parametrized composite contexts.
                        let exploded = self.explode(level, def, param)?;
                        let single = self.ir.lists[exploded][slot.index()];
                        self.invoke(single, destruct)
                    }
                    Node::Tagdef { def } => todo!(),
                    Node::Aliasdef { def } => todo!(),
                    Node::Tuple { elems } => todo!(),
                    Node::Context => todo!(),
                    Node::Fndef { def } => todo!(),
                    Node::Get { ctx, slot } => todo!(),
                    Node::Lit { val } => todo!(),
                    Node::Bind { args, bind } => todo!(),
                    Node::BindTydef { def, bind } => todo!(),
                    Node::BindSigdef { def, bind } => todo!(),
                    Node::BindValdef { def, bind } => todo!(),
                    Node::BindCtxdef { def, bind } => todo!(),
                    Node::Sig { param, result } => todo!(),
                },
                Node::List { items } => todo!(),
                Node::NeedTydef { level, def, param } => todo!(),
                Node::NeedSigdef { level, def, param } => todo!(),
                Node::NeedValdef { level, def, param } => todo!(),
                Node::NeedCtxdef { level, def, param } => todo!(),
                Node::Tagdef { def } => todo!(),
                Node::Aliasdef { def } => todo!(),
                Node::Tuple { elems } => todo!(),
                Node::Context => todo!(),
                Node::Fndef { def } => todo!(),
                Node::Get { ctx, slot } => todo!(),
                Node::Lit { val } => todo!(),
                Node::Bind { args, bind } => todo!(),
                Node::BindTydef { def, bind } => todo!(),
                Node::BindSigdef { def, bind } => todo!(),
                Node::BindValdef { def, bind } => todo!(),
                Node::BindCtxdef { def, bind } => todo!(),
                Node::Sig { param, result } => todo!(),
            },
            // A ground literal has no needs, so it takes no constructed arguments.
            Node::Lit { val: _ } => Ok(Some(Vec::new())),
            // A binding's needs live in its inner `bind` lambda; invoking the binding resolves
            // those, just as applying the binding (in `reduce`) feeds them to that same lambda.
            Node::Bind { args: _, bind }
            | Node::BindTydef { def: _, bind }
            | Node::BindSigdef { def: _, bind }
            | Node::BindValdef { def: _, bind }
            | Node::BindCtxdef { def: _, bind } => self.invoke(bind, destruct),
            Node::Sig { param, result } => todo!(),
        }
    }

    fn invoke_force(&mut self, lambda: NodeId, destruct: &[NodeId]) -> LowerResult<Vec<NodeId>> {
        Ok(self.invoke(lambda, destruct)?.unwrap()) // TODO: Return an actual error here.
    }

    fn invoke_need(
        &mut self,
        level: Level,
        target: NodeId,
        destruct: &[NodeId],
    ) -> LowerResult<(Vec<NodeId>, Vec<NodeId>)> {
        let Node::Lambda {
            level: floor,
            needs: _,
            result: _,
        } = self.node(target)
        else {
            panic!()
        };
        let raised = self.raise(target, floor, level - floor);
        let Node::Lambda {
            level: _,
            needs,
            result: _,
        } = self.node(raised)
        else {
            unreachable!()
        };
        let mut construct = Vec::new();
        let mut propagate = Vec::new();
        for loc in needs {
            let need = self.ir.lists[loc];
            match self.resolve_need(need, destruct)? {
                Some(composite) => construct.push(composite),
                None => {
                    // TODO: Instead of clumsily constructing `before` and `after` on the fly here,
                    // get better asymptotic complexity by maintaining a running mapping.
                    let prefix = self.ir.lists[needs][..construct.len()].to_vec();
                    let before = self.mk_node_list(&prefix);
                    let after = self.mk_node_list(&construct);
                    let substituted = self.substitute(level, before, after, need);
                    construct.push(substituted);
                    propagate.push(substituted);
                }
            }
        }
        Ok((construct, propagate))
    }

    /// Strip redundant nullary application wrappers: `Apply { X, [] }` (applying `X` to zero
    /// arguments) denotes the same value as `X`. Used only for equivalence comparison in [`Lower::unify`]
    /// and [`Lower::spec_unify`]; the canonical reduced form keeps the wrapper because the `Get`
    /// projection in [`Lower::reduce`]/[`Lower::invoke`] pattern-matches on the stuck
    /// `Apply { NeedCtxdef, [] }` shape.
    fn peel_nullary_apply(&self, mut node: NodeId) -> NodeId {
        while let Node::Apply { lambda, args } = self.node(node) {
            if !self.ir.lists[args].is_empty() {
                break;
            }
            node = lambda;
        }
        node
    }

    /// Match `a` against `b`, solving for the metavariables in `constraints` (the needs of
    /// the `b` side) and deciding level equivalence against `env`.
    ///
    /// Returns `Ok(true)` if the terms match (with `constraints` possibly refined), `Ok(false)`
    /// if they are structurally inequivalent, and `Err` only for genuine compiler errors. The
    /// relation is asymmetric: the only metavariables are on the `b` side, recorded in
    /// `constraints`; a metavariable is solved by binding it to the corresponding `a` node.
    fn unify(
        &mut self,
        env: &Renaming,
        constraints: &mut HashMap<NodeId, Option<NodeId>>,
        a: NodeId,
        b: NodeId,
    ) -> LowerResult<bool> {
        debug_assert!(!constraints.contains_key(&a)); // asymmetry: metavariables are on `b`.
        if constraints.contains_key(&b) {
            match constraints.get_mut(&b).unwrap() {
                slot @ None => *slot = Some(a),
                Some(prev) if *prev == a => {}
                other => *other = None, // conflicting solutions: leave unsolved
            }
            return Ok(true);
        }
        // Decide equivalence up to reduction; needs stay opaque, so this leaves them comparable.
        // Peel any redundant nullary application (`Apply { X, [] }` denotes the same value as `X`):
        // `reduce` leaves a stuck head wrapped in its nullary `Apply` (the pervasive
        // `Apply { member, [] }` projection shape), so the very same context member can arrive here
        // as `Apply { NeedTydef, [] }` on one side and bare `NeedTydef` on the other. Without peeling,
        // `unify` would compare those two references to the *same* type and wrongly reject -- the
        // mismatch that stalled the composite digit-context bridge (`Numerals[Uint]=Numerals[I32]`)
        // at the `bind Std` assembly. `reduce` itself must keep the wrapper (`invoke`/`reduce`'s
        // `Get` arms expect the stuck `Apply { NeedCtxdef, [] }` form), so peel only here.
        let a = self.reduce(a)?;
        let a = self.peel_nullary_apply(a);
        let b = self.reduce(b)?;
        let b = self.peel_nullary_apply(b);
        match (self.node(a), self.node(b)) {
            (
                Node::Lambda {
                    level: la,
                    needs: na,
                    result: ra,
                },
                Node::Lambda {
                    level: lb,
                    needs: nb,
                    result: rb,
                },
            ) => {
                let mut env = *env;
                env.bind(la, lb);
                Ok(self.unify_list(&env, constraints, na, nb)?
                    && self.unify(&env, constraints, ra, rb)?)
            }
            (
                Node::Apply {
                    lambda: fa,
                    args: aa,
                },
                Node::Apply {
                    lambda: fb,
                    args: ab,
                },
            ) => Ok(self.unify(env, constraints, fa, fb)?
                && self.unify_list(env, constraints, aa, ab)?),
            (Node::List { items: ia }, Node::List { items: ib }) => {
                self.unify_list(env, constraints, ia, ib)
            }
            (Node::Tuple { elems: ea }, Node::Tuple { elems: eb }) => {
                self.unify_list(env, constraints, ea, eb)
            }
            (
                Node::Sig {
                    param: pa,
                    result: ra,
                },
                Node::Sig {
                    param: pb,
                    result: rb,
                },
            ) => Ok(self.unify(env, constraints, pa, pb)? && self.unify(env, constraints, ra, rb)?),
            (
                Node::NeedTydef {
                    level: la,
                    def: da,
                    param: pa,
                },
                Node::NeedTydef {
                    level: lb,
                    def: db,
                    param: pb,
                },
            ) => Ok(da == db && env.a_of_b[lb] == la && self.unify(env, constraints, pa, pb)?),
            (
                Node::NeedSigdef {
                    level: la,
                    def: da,
                    param: pa,
                },
                Node::NeedSigdef {
                    level: lb,
                    def: db,
                    param: pb,
                },
            ) => Ok(da == db && env.a_of_b[lb] == la && self.unify(env, constraints, pa, pb)?),
            (
                Node::NeedValdef {
                    level: la,
                    def: da,
                    param: pa,
                },
                Node::NeedValdef {
                    level: lb,
                    def: db,
                    param: pb,
                },
            ) => Ok(da == db && env.a_of_b[lb] == la && self.unify(env, constraints, pa, pb)?),
            (
                Node::NeedCtxdef {
                    level: la,
                    def: da,
                    param: pa,
                },
                Node::NeedCtxdef {
                    level: lb,
                    def: db,
                    param: pb,
                },
            ) => Ok(da == db && env.a_of_b[lb] == la && self.unify(env, constraints, pa, pb)?),
            (
                Node::Get {
                    ctx: ca,
                    slot: sa,
                },
                Node::Get {
                    ctx: cb,
                    slot: sb,
                },
            ) => Ok(sa == sb && self.unify(env, constraints, ca, cb)?),
            (
                Node::Bind {
                    args: aa,
                    bind: ba,
                },
                Node::Bind {
                    args: ab,
                    bind: bb,
                },
            ) => Ok(self.unify_list(env, constraints, aa, ab)?
                && self.unify(env, constraints, ba, bb)?),
            (Node::BindTydef { def: da, bind: ba }, Node::BindTydef { def: db, bind: bb }) => {
                Ok(da == db && self.unify(env, constraints, ba, bb)?)
            }
            (Node::BindSigdef { def: da, bind: ba }, Node::BindSigdef { def: db, bind: bb }) => {
                Ok(da == db && self.unify(env, constraints, ba, bb)?)
            }
            (Node::BindValdef { def: da, bind: ba }, Node::BindValdef { def: db, bind: bb }) => {
                Ok(da == db && self.unify(env, constraints, ba, bb)?)
            }
            (Node::BindCtxdef { def: da, bind: ba }, Node::BindCtxdef { def: db, bind: bb }) => {
                Ok(da == db && self.unify(env, constraints, ba, bb)?)
            }
            (Node::Nothing, Node::Nothing) => Ok(true),
            (Node::Context, Node::Context) => Ok(true),
            (Node::Tagdef { def: da }, Node::Tagdef { def: db }) => Ok(da == db),
            (Node::Fndef { def: da }, Node::Fndef { def: db }) => Ok(da == db),
            (Node::Lit { val: va }, Node::Lit { val: vb }) => Ok(va == vb),
            // Aliases are transparent and must be unfolded before comparison; not yet handled.
            (Node::Aliasdef { .. }, _) | (_, Node::Aliasdef { .. }) => Err(self.todo_no_loc()),
            (x, y) => {
                if std::mem::discriminant(&x) == std::mem::discriminant(&y) {
                    Err(self.todo_no_loc()) // same shape, this arm isn't implemented yet
                } else {
                    Ok(false) // different shapes: not unifiable
                }
            }
        }
    }

    /// [`Lower::unify`] applied position by position to two lists of equal length.
    fn unify_list(
        &mut self,
        env: &Renaming,
        constraints: &mut HashMap<NodeId, Option<NodeId>>,
        a: NodeList,
        b: NodeList,
    ) -> LowerResult<bool> {
        if a.len() != b.len() {
            return Ok(false);
        }
        let pairs: Vec<(NodeId, NodeId)> = self.ir.lists[a]
            .iter()
            .copied()
            .zip(self.ir.lists[b].iter().copied())
            .collect();
        for (x, y) in pairs {
            if !self.unify(env, constraints, x, y)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Given two lambdas, return a third lambda that composes with the latter to match the former.
    ///
    /// The given lambdas `alpha` and `beta` must have the same result shape, but may have different
    /// input shapes. This method constructs a third lambda, `gamma`, such that feeding the result
    /// of `gamma` into `beta` yields the same result as just using `alpha` by itself.
    fn synthesize(&mut self, alpha: NodeId, beta: NodeId) -> LowerResult<Option<NodeId>> {
        let (
            Node::Lambda {
                level: level_alpha,
                needs: needs_alpha,
                result: result_alpha,
            },
            Node::Lambda {
                level: level_beta,
                needs: needs_beta,
                result: result_beta,
            },
        ) = (self.node(alpha), self.node(beta))
        else {
            panic!()
        };
        // The needs of `beta` are the metavariables to solve for.
        let mut constraints =
            HashMap::from_iter(self.ir.lists[needs_beta].iter().map(|&need| (need, None)));
        // Relate the two stripped binders; the rest of the correspondence is the identity,
        // i.e. `alpha` and `beta` are compared under a shared enclosing context.
        let mut env = Renaming::identity();
        env.bind(level_alpha, level_beta);
        if !self.unify(&env, &mut constraints, result_alpha, result_beta)? {
            return Ok(None);
        }
        let Some(results) = self.ir.lists[needs_beta]
            .iter()
            .map(|&need| constraints[&need])
            .collect::<Option<Vec<_>>>()
        else {
            return Ok(None);
        };
        let items = self.mk_node_list(&results);
        let result = self.mk_node(Node::List { items });
        Ok(Some(self.mk_node(Node::Lambda {
            level: level_alpha,
            needs: needs_alpha,
            result,
        })))
    }

    /// Like `synthesize`, but where `beta` returns a [`Node::Bind`] that must be unwrapped first.
    fn synthesize_bind(&mut self, alpha: NodeId, beta: NodeId) -> LowerResult<Option<NodeId>> {
        let Node::Lambda {
            level,
            needs,
            result,
        } = self.node(beta)
        else {
            panic!()
        };
        let Node::Bind { args, bind: _ } = self.node(result) else {
            panic!()
        };
        let result = self.mk_node(Node::List { items: args });
        let beta = self.mk_node(Node::Lambda {
            level,
            needs,
            result,
        });
        self.synthesize(alpha, beta)
    }

    /// Try to satisfy a need of definition `kind` from a single context `slot`, given the
    /// required `lambda`. Returns the entrypoint into the slot together with the synthesized
    /// adapter, or `None` if this slot can't satisfy the need.
    fn try_extract_lambda(
        &mut self,
        slot: NodeId,
        kind: DefKind,
        lambda: NodeId,
    ) -> LowerResult<Option<(NodeId, NodeId)>> {
        match self.node(slot) {
            // A slot whose shape is not one of the matchable forms below simply does not provide
            // this need. (Reached when assembling a `bind <Ctx>` return member-wise against
            // providers that include desugared value bindings such as `false = 0`, whose value
            // reduces to a non-context.)
            Node::Nothing | Node::Lambda { .. } | Node::Apply { .. } | Node::List { .. } => Ok(None),
            Node::NeedTydef { def, param, .. } => {
                self.match_need(kind, DefKind::Ty(def), slot, lambda, param)
            }
            Node::NeedSigdef { def, param, .. } => {
                self.match_need(kind, DefKind::Sig(def), slot, lambda, param)
            }
            Node::NeedValdef { def, param, .. } => {
                self.match_need(kind, DefKind::Val(def), slot, lambda, param)
            }
            Node::NeedCtxdef { level, def, param } => {
                let mut options = Vec::new();
                // A composite-context need is satisfied directly by a slot for the same context,
                // exactly as the other `Need*` arms match their own kind via `match_need`. (The
                // explode-and-search-inside below additionally lets a *member* need be satisfied
                // from within this context.)
                if let Some(option) =
                    self.match_need(kind, DefKind::Ctx(def), slot, lambda, param)?
                {
                    options.push(option);
                }
                let exploded = self.explode(level, def, param)?;
                let args = self.mk_node_list(&[]); // TODO: Handle partially-applied contexts.
                let ctx = self.mk_node(Node::Apply { lambda: slot, args });
                let owned = self.ir.lists[exploded].to_vec(); // TODO: Don't make a `Vec` here.
                for (i, node) in owned.into_iter().enumerate() {
                    if let Some((extracted, synth)) = self.try_extract_lambda(node, kind, lambda)? {
                        let id = SlotId::from_usize(i);
                        let got = self.mk_node(Node::Get { ctx, slot: id });
                        // `extracted` is the entrypoint into `node`. If `node` itself was a nested
                        // context, `extracted` is a `Get` chain rooted at `node`; rebase it onto the
                        // accessor `got` that reaches `node` from this (outer) context. Otherwise
                        // `node` was directly satisfying and `extracted == node`, so use `got`.
                        let rebased = if extracted == node {
                            got
                        } else {
                            self.rebase_get(extracted, node, got)
                        };
                        options.push((rebased, synth));
                    }
                }
                self.unique_option(&options)
            }
            Node::Tagdef { def } => todo!(),
            Node::Aliasdef { def } => todo!(),
            Node::Tuple { elems } => todo!(),
            Node::Context => todo!(),
            Node::Fndef { def } => todo!(),
            Node::Get { ctx, slot } => todo!(),
            Node::Lit { val } => todo!(),
            Node::Bind { args, bind } => todo!(),
            Node::BindTydef { def, bind } => {
                self.match_bind(kind, DefKind::Ty(def), slot, lambda, bind)
            }
            Node::BindSigdef { def, bind } => {
                self.match_bind(kind, DefKind::Sig(def), slot, lambda, bind)
            }
            Node::BindValdef { def, bind } => {
                self.match_bind(kind, DefKind::Val(def), slot, lambda, bind)
            }
            Node::BindCtxdef { def, bind } => {
                self.match_bind(kind, DefKind::Ctx(def), slot, lambda, bind)
            }
            Node::Sig { param, result } => todo!(),
        }
    }

    /// Rewrite the `Get`/`Apply` accessor chain `node`, replacing every reference to `from` (the
    /// inner context member that the chain was originally rooted at) with `to` (the accessor that
    /// reaches that member from an enclosing context).
    fn rebase_get(&mut self, node: NodeId, from: NodeId, to: NodeId) -> NodeId {
        if node == from {
            return to;
        }
        match self.node(node) {
            Node::Get { ctx, slot } => {
                let ctx = self.rebase_get(ctx, from, to);
                self.mk_node(Node::Get { ctx, slot })
            }
            Node::Apply { lambda, args } => {
                let lambda = self.rebase_get(lambda, from, to);
                self.mk_node(Node::Apply { lambda, args })
            }
            _ => node,
        }
    }

    /// If `want` is the kind being resolved, try to satisfy it from a `Need*` slot whose
    /// parameter is `param`; otherwise this slot doesn't apply.
    fn match_need(
        &mut self,
        kind: DefKind,
        want: DefKind,
        slot: NodeId,
        lambda: NodeId,
        param: NodeId,
    ) -> LowerResult<Option<(NodeId, NodeId)>> {
        if kind == want
            && let Some(synth) = self.synthesize(lambda, param)?
        {
            Ok(Some((slot, synth)))
        } else {
            Ok(None)
        }
    }

    /// Like [`Lower::match_need`], but for a `Bind*` slot, whose bound value is unwrapped first.
    fn match_bind(
        &mut self,
        kind: DefKind,
        want: DefKind,
        slot: NodeId,
        lambda: NodeId,
        bind: NodeId,
    ) -> LowerResult<Option<(NodeId, NodeId)>> {
        if kind == want {
            if let Some(synth) = self.synthesize_bind(lambda, bind)? {
                return Ok(Some((slot, synth)));
            }
        }
        Ok(None)
    }

    /// The composite a candidate `(callee, synth)` resolves to: the synthesized adapter's binder
    /// wrapped around an application of `callee` to the adapter's result.
    fn composite(&mut self, callee: NodeId, synth: NodeId) -> NodeId {
        let Node::Lambda {
            level,
            needs,
            result,
        } = self.node(synth)
        else {
            panic!()
        };
        let Node::List { items } = self.node(result) else {
            panic!()
        };
        let result = self.mk_node(Node::Apply {
            lambda: callee,
            args: items,
        });
        self.mk_node(Node::Lambda {
            level,
            needs,
            result,
        })
    }

    /// Reduce several candidate resolutions for the same need to a single one, following the
    /// context model's "most specific wins" rule. Candidate `A` is at least as specific as `B` when
    /// [`Lower::at_least_as_specific`] holds (`A` is an instance of `B`, and a bound need dominates
    /// the abstract version of the same definition); the winner is the unique candidate that is at
    /// least as specific as every other. If there is no such greatest element, resolution is
    /// genuinely ambiguous and yields `None`.
    fn unique_option(
        &mut self,
        options: &[(NodeId, NodeId)],
    ) -> LowerResult<Option<(NodeId, NodeId)>> {
        if options.is_empty() {
            return Ok(None);
        }
        let composites = options
            .iter()
            .map(|&(callee, synth)| self.composite(callee, synth))
            .collect::<Vec<_>>();
        // The winner must be at least as specific as every other candidate. Any candidate that
        // dominates all others is mutually equivalent to every other dominator, so the first such
        // candidate is a valid choice; if none dominates all others, resolution is ambiguous.
        'candidate: for i in 0..options.len() {
            for j in 0..options.len() {
                if i != j && !self.at_least_as_specific(composites[i], composites[j])? {
                    continue 'candidate;
                }
            }
            return Ok(Some(options[i]));
        }
        Ok(None)
    }

    /// Reduce `node` toward weak-head normal form like [`Lower::reduce`], but **stop at a binding
    /// head** instead of projecting it to its bound value. Where `reduce` turns
    /// `Apply { BindTydef { bind, .. }, args }` into the value `bind` denotes (erasing the binding),
    /// this leaves the application stuck on the `Bind*` head so the binding is still observable.
    ///
    /// This is scoped to specificity ranking ([`Lower::at_least_as_specific`]): a bound need must
    /// be distinguishable from the abstract need it provides, and that distinction lives precisely
    /// in the binding that the normal `reduce` projects away. Everything else — `Apply`-β,
    /// `Get`-projection out of composite contexts, and `Fndef`/`Aliasdef` unfolding — matches
    /// `reduce`. Leave `reduce` itself untouched: the digit desugar relies on its projection.
    fn head_reduce(&mut self, node: NodeId) -> LowerResult<NodeId> {
        match self.node(node) {
            Node::Apply { lambda, args } => {
                let head = self.head_reduce(lambda)?;
                match self.node(head) {
                    Node::Lambda { .. } => {
                        let args = self.ir.lists[args].to_vec();
                        let inlined = self.inline(head, &args)?;
                        self.head_reduce(inlined)
                    }
                    // Unlike `reduce`, a binding head does NOT project: leave the application stuck
                    // on the `Bind*` so the binding stays observable for specificity ranking.
                    _ => Ok(self.mk_node(Node::Apply { lambda: head, args })),
                }
            }
            Node::Get { ctx, slot } => {
                let ctx = self.head_reduce(ctx)?;
                match self.node(ctx) {
                    Node::Apply {
                        lambda: curried,
                        args,
                    } => match self.node(curried) {
                        Node::NeedCtxdef { level, def, param } => {
                            assert!(args.is_empty()); // TODO: Handle parametrized composite contexts.
                            let exploded = self.explode(level, def, param)?;
                            let single = self.ir.lists[exploded][slot.index()];
                            self.head_reduce(single)
                        }
                        _ => Ok(self.mk_node(Node::Get { ctx, slot })),
                    },
                    Node::List { items } => {
                        let elem = self.ir.lists[items][slot.index()];
                        self.head_reduce(elem)
                    }
                    _ => Ok(self.mk_node(Node::Get { ctx, slot })),
                }
            }
            Node::Fndef { def } => {
                let body = self.ir.fndefs[def].0;
                self.head_reduce(body)
            }
            Node::Aliasdef { def } => {
                let body = self.ir.aliasdefs[def].0;
                self.head_reduce(body)
            }
            // Already in weak-head normal form (including `Bind*`, which we deliberately keep stuck).
            _ => Ok(node),
        }
    }

    /// Decide whether candidate `a` is **at least as specific** as candidate `b`, directionally.
    ///
    /// This is the specificity comparison used by [`Lower::unique_option`]. Like
    /// [`Lower::synthesize`]'s match it strips the two candidate lambdas and treats `b`'s input
    /// needs as holes to be filled from `a`, but it reports only whether the shapes match (no
    /// adapter is synthesized, and holes need not be uniquely solved). Crucially, it compares heads
    /// with [`Lower::head_reduce`] rather than the projecting [`Lower::reduce`], so a **bound** need
    /// (`Bind { def }`) can be told apart from the **abstract** version (`Need { def }`) of the same
    /// definition: the former is strictly more specific than the latter.
    fn at_least_as_specific(&mut self, a: NodeId, b: NodeId) -> LowerResult<bool> {
        let (
            Node::Lambda {
                level: la,
                needs: _,
                result: ra,
            },
            Node::Lambda {
                level: lb,
                needs: nb,
                result: rb,
            },
        ) = (self.node(a), self.node(b))
        else {
            panic!()
        };
        let mut constraints =
            HashMap::from_iter(self.ir.lists[nb].iter().map(|&need| (need, None)));
        let mut env = Renaming::identity();
        env.bind(la, lb);
        self.spec_unify(&env, &mut constraints, ra, rb)
    }

    /// Directional structural match for [`Lower::at_least_as_specific`]: like [`Lower::unify`] but
    /// (1) reduces heads with the non-projecting [`Lower::head_reduce`], and (2) applies the
    /// directional specificity rule when both sides are bindings/needs of the same definition:
    /// `Bind { def }` is ≥-specific than `Need { def }` (true), but `Need { def }` is not
    /// ≥-specific than `Bind { def }` (false). `Bind`/`Bind` of the same def recurses into the
    /// bound values; `Need`/`Need` compares structurally (def + level via the renaming). Holes
    /// (`b`'s input needs) are filled from `a` but are never required to be uniquely solved.
    fn spec_unify(
        &mut self,
        env: &Renaming,
        constraints: &mut HashMap<NodeId, Option<NodeId>>,
        a: NodeId,
        b: NodeId,
    ) -> LowerResult<bool> {
        if constraints.contains_key(&b) {
            // `b` is a hole; record a witness but never fail on conflicts (we don't require a
            // unique solution, only that the shapes line up).
            match constraints.get_mut(&b).unwrap() {
                slot @ None => *slot = Some(a),
                Some(prev) if *prev == a => {}
                other => *other = None,
            }
            return Ok(true);
        }
        // See `unify`: a stuck nullary application `Apply { X, [] }` denotes the same value as `X`,
        // so peel it for comparison (specificity must agree on the two equivalent reference shapes).
        let a = self.head_reduce(a)?;
        let a = self.peel_nullary_apply(a);
        let b = self.head_reduce(b)?;
        let b = self.peel_nullary_apply(b);
        match (self.node(a), self.node(b)) {
            (
                Node::Lambda {
                    level: la,
                    needs: na,
                    result: ra,
                },
                Node::Lambda {
                    level: lb,
                    needs: nb,
                    result: rb,
                },
            ) => {
                let mut env = *env;
                env.bind(la, lb);
                Ok(self.spec_unify_list(&env, constraints, na, nb)?
                    && self.spec_unify(&env, constraints, ra, rb)?)
            }
            (
                Node::Apply {
                    lambda: fa,
                    args: aa,
                },
                Node::Apply {
                    lambda: fb,
                    args: ab,
                },
            ) => Ok(self.spec_unify(env, constraints, fa, fb)?
                && self.spec_unify_list(env, constraints, aa, ab)?),
            (Node::List { items: ia }, Node::List { items: ib }) => {
                self.spec_unify_list(env, constraints, ia, ib)
            }
            (Node::Tuple { elems: ea }, Node::Tuple { elems: eb }) => {
                self.spec_unify_list(env, constraints, ea, eb)
            }
            (
                Node::Sig {
                    param: pa,
                    result: ra,
                },
                Node::Sig {
                    param: pb,
                    result: rb,
                },
            ) => Ok(self.spec_unify(env, constraints, pa, pb)?
                && self.spec_unify(env, constraints, ra, rb)?),
            (
                Node::Get {
                    ctx: ca,
                    slot: sa,
                },
                Node::Get {
                    ctx: cb,
                    slot: sb,
                },
            ) => Ok(sa == sb && self.spec_unify(env, constraints, ca, cb)?),
            // Same-definition specificity: a binding dominates the abstract need it provides.
            (Node::BindTydef { def: da, bind: ba }, Node::BindTydef { def: db, bind: bb }) => {
                Ok(da == db && self.spec_unify(env, constraints, ba, bb)?)
            }
            (Node::BindSigdef { def: da, bind: ba }, Node::BindSigdef { def: db, bind: bb }) => {
                Ok(da == db && self.spec_unify(env, constraints, ba, bb)?)
            }
            (Node::BindValdef { def: da, bind: ba }, Node::BindValdef { def: db, bind: bb }) => {
                Ok(da == db && self.spec_unify(env, constraints, ba, bb)?)
            }
            (Node::BindCtxdef { def: da, bind: ba }, Node::BindCtxdef { def: db, bind: bb }) => {
                Ok(da == db && self.spec_unify(env, constraints, ba, bb)?)
            }
            // `a` is bound where `b` is the abstract need of the same def: `a` is strictly MORE
            // specific, so it is ≥-specific than `b` — true.
            (Node::BindTydef { def: da, .. }, Node::NeedTydef { def: db, .. }) => Ok(da == db),
            (Node::BindSigdef { def: da, .. }, Node::NeedSigdef { def: db, .. }) => Ok(da == db),
            (Node::BindValdef { def: da, .. }, Node::NeedValdef { def: db, .. }) => Ok(da == db),
            (Node::BindCtxdef { def: da, .. }, Node::NeedCtxdef { def: db, .. }) => Ok(da == db),
            // `a` is the abstract need where `b` is bound: `a` is strictly LESS specific, so it is
            // NOT ≥-specific than `b` — false.
            (Node::NeedTydef { .. }, Node::BindTydef { .. })
            | (Node::NeedSigdef { .. }, Node::BindSigdef { .. })
            | (Node::NeedValdef { .. }, Node::BindValdef { .. })
            | (Node::NeedCtxdef { .. }, Node::BindCtxdef { .. }) => Ok(false),
            (
                Node::NeedTydef {
                    level: la,
                    def: da,
                    param: pa,
                },
                Node::NeedTydef {
                    level: lb,
                    def: db,
                    param: pb,
                },
            ) => Ok(da == db
                && env.a_of_b[lb] == la
                && self.spec_unify(env, constraints, pa, pb)?),
            (
                Node::NeedSigdef {
                    level: la,
                    def: da,
                    param: pa,
                },
                Node::NeedSigdef {
                    level: lb,
                    def: db,
                    param: pb,
                },
            ) => Ok(da == db
                && env.a_of_b[lb] == la
                && self.spec_unify(env, constraints, pa, pb)?),
            (
                Node::NeedValdef {
                    level: la,
                    def: da,
                    param: pa,
                },
                Node::NeedValdef {
                    level: lb,
                    def: db,
                    param: pb,
                },
            ) => Ok(da == db
                && env.a_of_b[lb] == la
                && self.spec_unify(env, constraints, pa, pb)?),
            (
                Node::NeedCtxdef {
                    level: la,
                    def: da,
                    param: pa,
                },
                Node::NeedCtxdef {
                    level: lb,
                    def: db,
                    param: pb,
                },
            ) => Ok(da == db
                && env.a_of_b[lb] == la
                && self.spec_unify(env, constraints, pa, pb)?),
            (
                Node::Bind {
                    args: aa,
                    bind: ba,
                },
                Node::Bind {
                    args: ab,
                    bind: bb,
                },
            ) => Ok(self.spec_unify_list(env, constraints, aa, ab)?
                && self.spec_unify(env, constraints, ba, bb)?),
            (Node::Nothing, Node::Nothing) => Ok(true),
            (Node::Context, Node::Context) => Ok(true),
            (Node::Tagdef { def: da }, Node::Tagdef { def: db }) => Ok(da == db),
            (Node::Fndef { def: da }, Node::Fndef { def: db }) => Ok(da == db),
            (Node::Lit { val: va }, Node::Lit { val: vb }) => Ok(va == vb),
            (Node::Aliasdef { .. }, _) | (_, Node::Aliasdef { .. }) => Err(self.todo_no_loc()),
            (x, y) => {
                if std::mem::discriminant(&x) == std::mem::discriminant(&y) {
                    Err(self.todo_no_loc())
                } else {
                    Ok(false)
                }
            }
        }
    }

    /// [`Lower::spec_unify`] applied position by position to two lists of equal length.
    fn spec_unify_list(
        &mut self,
        env: &Renaming,
        constraints: &mut HashMap<NodeId, Option<NodeId>>,
        a: NodeList,
        b: NodeList,
    ) -> LowerResult<bool> {
        if a.len() != b.len() {
            return Ok(false);
        }
        let pairs: Vec<(NodeId, NodeId)> = self.ir.lists[a]
            .iter()
            .copied()
            .zip(self.ir.lists[b].iter().copied())
            .collect();
        for (x, y) in pairs {
            if !self.spec_unify(env, constraints, x, y)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// Collect the slots that satisfy a need of definition `kind`. Succeeds when, up to the
    /// equivalence of mutually-adaptable candidates, exactly one does. (Cross-frame search and the
    /// general "most specific" partial order are not yet modeled.)
    fn extract_lambda(
        &mut self,
        slots: &[NodeId],
        kind: DefKind,
        lambda: NodeId,
    ) -> LowerResult<Option<(NodeId, NodeId)>> {
        let mut options = Vec::new();
        for &slot in slots {
            if let Some(option) = self.try_extract_lambda(slot, kind, lambda)? {
                options.push(option);
            }
        }
        self.unique_option(&options)
    }

    /// The body node of the target definition named by `kind`.
    fn target(&self, kind: DefKind) -> NodeId {
        match kind {
            DefKind::Ty(def) => self.ir.tydefs[def].0,
            DefKind::Sig(def) => self.ir.sigdefs[def].0,
            DefKind::Val(def) => self.ir.valdefs[def].0,
            DefKind::Ctx(def) => self.ir.ctxdefs[def].0,
        }
    }

    fn extract(
        &mut self,
        level: Level,
        slots: &[NodeId],
        kind: DefKind,
        destruct: &[NodeId],
    ) -> LowerResult<NodeId> {
        let target = self.target(kind);
        let raised = self.raise(target, Level::ZERO, level);
        let (construct, needs) = self.invoke_need(level, raised, destruct)?;
        let needs = self.mk_node_list(&needs);
        let items = self.mk_node_list(&construct);
        let result = self.mk_node(Node::List { items });
        let lambda = self.mk_node(Node::Lambda {
            level,
            needs,
            result,
        });
        match self.extract_lambda(slots, kind, lambda)? {
            Some((node, _)) => Ok(node),
            None => Err(todo!()),
        }
    }

    /// Rewrite `node`, replacing every abstract type reference `NeedTydef { def, .. }` whose `def`
    /// is bound in `tybinds` with the type it is bound to. Used to resolve a composite member's
    /// abstract type references (e.g. `Uint32`) through the in-scope type-binds (e.g. `Uint32=I32`)
    /// before matching, so the two sides of a context-to-context bridge agree on the concrete type.
    fn resolve_tyrefs(&mut self, node: NodeId, tybinds: &HashMap<TydefId, NodeId>) -> NodeId {
        if tybinds.is_empty() {
            return node;
        }
        let mut seen = std::collections::BTreeSet::new();
        let mut before = Vec::new();
        let mut after = Vec::new();
        let mut stack = vec![node];
        while let Some(id) = stack.pop() {
            if !seen.insert(id) {
                continue;
            }
            let n = self.node(id);
            // Skip the member root itself (`id != node`): rewriting a direct nullary type member
            // (e.g. `Bool`/`Int`) into its bound value would strip its `Need*` head, so it would
            // bypass `resolve_need`'s `Get`-projection and reach eval unbound. Only strictly-nested
            // abstract refs are rewritten; the root stays a `Need*` and is projected explicitly.
            if id != node
                && let Node::NeedTydef { def, .. } = n
                && let Some(&value) = tybinds.get(&def)
            {
                before.push(id);
                after.push(value);
                continue; // replace the whole reference; don't descend into it
            }
            match n {
                Node::Lambda { needs, result, .. } => {
                    stack.extend(self.ir.lists[needs].iter().copied());
                    stack.push(result);
                }
                Node::Apply { lambda, args } => {
                    stack.push(lambda);
                    stack.extend(self.ir.lists[args].iter().copied());
                }
                Node::List { items } | Node::Tuple { elems: items } => {
                    stack.extend(self.ir.lists[items].iter().copied())
                }
                Node::NeedTydef { param, .. }
                | Node::NeedSigdef { param, .. }
                | Node::NeedValdef { param, .. }
                | Node::NeedCtxdef { param, .. } => stack.push(param),
                Node::Get { ctx, .. } => stack.push(ctx),
                Node::Bind { args, bind } => {
                    stack.extend(self.ir.lists[args].iter().copied());
                    stack.push(bind);
                }
                Node::BindTydef { bind, .. }
                | Node::BindSigdef { bind, .. }
                | Node::BindValdef { bind, .. }
                | Node::BindCtxdef { bind, .. } => stack.push(bind),
                Node::Sig { param, result } => {
                    stack.push(param);
                    stack.push(result);
                }
                _ => {}
            }
        }
        if before.is_empty() {
            return node;
        }
        let before = self.mk_node_list(&before);
        let after = self.mk_node_list(&after);
        self.substitute(Level::ZERO, before, after, node)
    }

    /// Resolve a composite-context definition from the available context `slots`.
    ///
    /// Unlike [`Lower::extract`], which finds a single slot that satisfies a `Ty`/`Sig`/`Val` need,
    /// a composite context is *synthesized* from its members: the context is exploded into its
    /// individual member needs, each is resolved against `slots` (or propagated as a fresh need if
    /// no slot satisfies it), and the per-member providers are bundled into one context-valued
    /// lambda whose `result` is the `List` of those providers. This mirrors the shape that the
    /// `need` arm builds for a `NeedCtxdef` and that [`Lower::explode`]/[`Lower::invoke`] consume.
    fn extract_ctx(
        &mut self,
        level: Level,
        slots: &[NodeId],
        def: CtxdefId,
        destruct: &[NodeId],
        // When `false`, an unsatisfied member is kept inline (as its own `Need*`) and NOT exposed as
        // a fresh need of the assembled context. Used by `bind <Ctx>` returns, where the assembled
        // value must be a complete, need-free, ctxdef-ordered context even if some members (e.g.
        // `print`/`to_string` in the prototype) have no provider yet.
        propagate: bool,
    ) -> LowerResult<NodeId> {
        // Build the outer-parameter construct (e.g. `[Base]`) into the `param` lambda that
        // `explode` expects, then explode `def` into its individual member needs at this level.
        let outer = self.mk_node_list(destruct);
        let result = self.mk_node(Node::List { items: outer });
        let needs = self.mk_node_list(&[]);
        let param = self.mk_node(Node::Lambda {
            level,
            needs,
            result,
        });
        let members = self.explode(level, def, param)?;
        let members = self.ir.lists[members].to_vec();
        // Raise the ambient `slots` into the same frame as the exploded members before matching;
        // otherwise `synthesize` compares a member and a slot whose free references to the (shared)
        // enclosing context sit at different levels.
        //
        // The amount is `level.succ()`, not `level`, because a composite context is a *two-level*
        // lambda -- `Lambda(level)[parameters] -> Lambda(level+1)[needs] -> members`. `explode`
        // raises the ctxdef by `level` and substitutes the parameters away, landing the members
        // under the inner *needs* binder at `level.succ()` (it asserts `level_substituted ==
        // level.succ()`). So an exploded member's free references live in a `level.succ()`-deep
        // frame, while a `slot` is a plain entrypoint at the bare frame. Raising by `level` alone
        // would match only the parameter binder and leave the slots one level shallow -- under-
        // reaching the members by exactly the inner needs binder.
        let slots: Vec<NodeId> = slots
            .iter()
            .map(|&slot| self.raise(slot, Level::ZERO, level.succ()))
            .collect();
        // Build a map from each nullary type-def bound in the (raised) `slots` to the type it is
        // bound to. A member's abstract reference to such a type (e.g. `Uint32`) is then resolved
        // through the bind (e.g. `Uint32=I32`) before matching, so a context-to-context bridge
        // (`Numerals[Uint32]=Numerals[I32]`) and the need (`Numerals[Uint32]`) agree on the concrete
        // type. The identity is sound -- it is a *declared* bind in scope (see the `Bootstrap`
        // context), not a blanket equation.
        let mut tybinds: HashMap<TydefId, NodeId> = HashMap::new();
        for &slot in &slots {
            if let Node::BindTydef { def, bind } = self.node(slot) {
                // Project the bind's bound value while *preserving its context-projection form*.
                //
                // The bound value of an in-scope `Uint=I32` slot is a `Get`-projection out of the
                // abstract enclosing context (`Get{..}(Apply(NeedCtxdef(Bootstrap)))`). A full
                // `reduce` of the nullary application would explode that abstract context and
                // collapse the projection all the way to a bare `NeedTydef(I32)`. But the bridge
                // bind we must match against (`Numerals[Uint]=Numerals[I32]`) refers to that same
                // `I32` through the *same* `Get{Bootstrap}` projection, since its RHS was lowered
                // against the same abstract context. Collapsing to a bare need on the need side
                // produces two irreconcilable forms of the same free `I32` -- a bare need vs a stuck
                // `Get` -- which `unify` cannot bridge (the abstract `Bootstrap` `Get` never reduces
                // to the bare need). So β-reduce only as far as the `Bind`, then take its bound value
                // *unreduced*, leaving the projection intact so both sides coincide under `unify`.
                let inlined = self.inline(bind, &[])?;
                let reduced = self.reduce(inlined)?;
                let value = match self.node(reduced) {
                    Node::Bind { bind, .. } => bind,
                    _ => reduced,
                };
                tybinds.entry(def).or_insert(value);
            }
        }
        // Resolve each member against `slots`, propagating any unsatisfied member as a fresh need.
        let mut providers = Vec::new();
        let mut propagate_needs = Vec::new();
        for member in members {
            let member = self.resolve_tyrefs(member, &tybinds);
            // A member that is already a binding (e.g. `Bool=I32` in the context's definition) is
            // fixed by the definition and provides itself; only an open `Need*` member is resolved
            // against `slots`, propagating as a fresh need if nothing satisfies it.
            let is_need = matches!(
                self.node(member),
                Node::NeedTydef { .. }
                    | Node::NeedSigdef { .. }
                    | Node::NeedValdef { .. }
                    | Node::NeedCtxdef { .. }
            );
            if !is_need {
                providers.push(member);
                continue;
            }
            match self.resolve_need(member, &slots)? {
                Some(provider) => providers.push(provider),
                None => {
                    providers.push(member);
                    if propagate {
                        propagate_needs.push(member);
                    }
                }
            }
        }
        let needs = self.mk_node_list(&propagate_needs);
        let items = self.mk_node_list(&providers);
        let result = self.mk_node(Node::List { items });
        Ok(self.mk_node(Node::Lambda {
            level: level.succ(),
            needs,
            result,
        }))
    }

    /// Build a `BindTydef` slot binding the (nullary) type `def` to the (nullary) type `rhs`,
    /// resolved against `slots`. This mirrors the `bind` method's `Entry::Ref`/`Named::Tydef`
    /// branch but for two type definitions known directly rather than via the parse tree.
    fn bind_tydef(
        &mut self,
        level: Level,
        slots: &[NodeId],
        def: TydefId,
        rhs: TydefId,
    ) -> LowerResult<NodeId> {
        let destruct_rhs: Vec<NodeId> = Vec::new();
        let lambda = self.extract(level.succ(), slots, DefKind::Ty(rhs), &destruct_rhs)?;
        let Tydef(target_lhs) = self.ir.tydefs[def];
        let destruct_lhs: Vec<NodeId> = Vec::new();
        let (construct_lhs, mut needs_vec) = self.invoke_need(level.succ(), target_lhs, &destruct_lhs)?;
        let needs = self.mk_node_list(&needs_vec);
        let args_lhs = self.mk_node_list(&construct_lhs);
        let mut destruct_rhs = destruct_rhs;
        destruct_rhs.append(&mut needs_vec);
        let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
        let args_rhs = self.mk_node_list(&construct_rhs);
        let bind = self.mk_node(Node::Apply {
            lambda,
            args: args_rhs,
        });
        let result = self.mk_node(Node::Bind {
            args: args_lhs,
            bind,
        });
        let bind = self.mk_node(Node::Lambda {
            level: level.succ(),
            needs,
            result,
        });
        Ok(self.mk_node(Node::BindTydef { def, bind }))
    }

    /// Apply a `Lambda` to a `construct` (one resolved value per need) by substituting the needs,
    /// yielding the lambda's result with those needs filled in.
    fn inline(&mut self, lambda: NodeId, construct: &[NodeId]) -> LowerResult<NodeId> {
        let Node::Lambda {
            level,
            needs,
            result,
        } = self.node(lambda)
        else {
            panic!()
        };
        let after = self.mk_node_list(construct);
        Ok(self.substitute(level, needs, after, result))
    }

    /// Reduce `node` toward weak-head normal form: β-reduce applications, project out of contexts,
    /// and unfold transparent definitions, until the head is irreducible. The opaque heads — the
    /// nominal `Tagdef` and the contextual `Need*` — are left in place: their declarations' bodies
    /// are metadata (a type's body is "nothing", a sig's is a signature), not substitutable values.
    fn reduce(&mut self, node: NodeId) -> LowerResult<NodeId> {
        match self.node(node) {
            Node::Apply { lambda, args } => {
                let head = self.reduce(lambda)?;
                match self.node(head) {
                    Node::Lambda { .. } => {
                        let args = self.ir.lists[args].to_vec();
                        let inlined = self.inline(head, &args)?;
                        self.reduce(inlined)
                    }
                    // Applying a binding projects out the value it binds: the binding's lambda maps
                    // the leftover needs to a `Bind { args, bind }`, whose `bind` is the bound value.
                    // This makes a bound type compare equal to the type it is bound to, which the
                    // literal desugar relies on when disambiguating a digit by an explicit binding.
                    Node::BindTydef { bind, .. }
                    | Node::BindSigdef { bind, .. }
                    | Node::BindValdef { bind, .. }
                    | Node::BindCtxdef { bind, .. } => {
                        let args = self.ir.lists[args].to_vec();
                        let inlined = self.inline(bind, &args)?;
                        let reduced = self.reduce(inlined)?;
                        match self.node(reduced) {
                            Node::Bind { bind, .. } => self.reduce(bind),
                            _ => Ok(reduced),
                        }
                    }
                    // Applying an opaque head (a need, a nominal type): a stuck application, in WHNF.
                    _ => Ok(self.mk_node(Node::Apply { lambda: head, args })),
                }
            }
            Node::Get { ctx, slot } => {
                // Reduce the context first so a nested-context accessor (a `Get` chain) collapses to
                // the stuck `Apply { NeedCtxdef }` (or `List`) form the cases below expect.
                let ctx = self.reduce(ctx)?;
                match self.node(ctx) {
                    Node::Apply {
                        lambda: curried,
                        args,
                    } => match self.node(curried) {
                        Node::NeedCtxdef { level, def, param } => {
                            assert!(args.is_empty()); // TODO: Handle parametrized composite contexts.
                            let exploded = self.explode(level, def, param)?;
                            let single = self.ir.lists[exploded][slot.index()];
                            self.reduce(single)
                        }
                        _ => todo!(),
                    },
                    Node::List { items } => {
                        let elem = self.ir.lists[items][slot.index()];
                        self.reduce(elem)
                    }
                    _ => todo!(),
                }
            }
            // Opaque heads, already in weak-head normal form.
            Node::Lambda { .. }
            | Node::Sig { .. }
            | Node::Tuple { .. }
            | Node::Tagdef { .. }
            | Node::Context
            | Node::Nothing
            | Node::Lit { .. }
            | Node::List { .. }
            | Node::NeedTydef { .. }
            | Node::NeedSigdef { .. }
            | Node::NeedValdef { .. }
            | Node::NeedCtxdef { .. }
            | Node::Bind { .. }
            | Node::BindTydef { .. }
            | Node::BindSigdef { .. }
            | Node::BindValdef { .. }
            | Node::BindCtxdef { .. } => Ok(node),
            // Transparent definitions unfold to their bodies.
            Node::Fndef { def } => {
                let body = self.ir.fndefs[def].0;
                self.reduce(body)
            }
            Node::Aliasdef { def } => {
                let body = self.ir.aliasdefs[def].0;
                self.reduce(body)
            }
            _ => todo!(),
        }
    }

    /// The signature (a `Sig`) of a contextual function `def`, given the `param` captured by its
    /// need and the `args` resolving `param`'s leftover needs. Scoped to signature queries: a `sig`
    /// declaration's body returns a signature, which is metadata, not a substitutable value, so this
    /// must not be folded into the general `reduce`.
    fn signature(&mut self, def: SigdefId, param: NodeId, args: NodeList) -> LowerResult<NodeId> {
        let Node::Lambda {
            level,
            needs,
            result,
        } = self.node(param)
        else {
            panic!()
        };
        let Node::List { items } = self.node(result) else {
            panic!()
        };
        let body = self.raise(self.ir.sigdefs[def].0, Level::ZERO, level);
        let construct = self.ir.lists[items].to_vec();
        let inlined = self.inline(body, &construct)?;
        let sig_lambda = self.mk_node(Node::Lambda {
            level,
            needs,
            result: inlined,
        });
        let args = self.ir.lists[args].to_vec();
        let applied = self.inline(sig_lambda, &args)?;
        self.reduce(applied)
    }

    /// Resolve the path of a spec and synthesize each of its attached bindings.
    ///
    /// The `destruct` list gives the set of entrypoints that can be used to synthesize bindings.
    fn spec(
        &mut self,
        level: Level,
        destruct: &[NodeId],
        spec: parse::Spec,
    ) -> LowerResult<(Named, Vec<NodeId>)> {
        let Spec { dot, path, binds } = spec;
        let named = if dot {
            self.detached(path)?.into()
        } else {
            self.path(path)?
        };
        let construct = binds
            .into_iter()
            .map(|bind| self.bind(level, destruct, bind))
            .collect::<LowerResult<Vec<_>>>()?;
        Ok((named, construct))
    }

    fn bind(&mut self, level: Level, slots: &[NodeId], bind: parse::BindId) -> LowerResult<NodeId> {
        let parse::Bind { key, val } = self.tree.binds[bind];
        let (lhs, destruct_lhs) = self.spec(level, slots, key)?;
        match val {
            None => match lhs {
                Named::Tydef(def) => {
                    let lambda = self.extract(level.succ(), slots, DefKind::Ty(def), &destruct_lhs)?;
                    let Tydef(target) = self.ir.tydefs[def];
                    let (construct_lhs, mut needs_vec) =
                        self.invoke_need(level.succ(), target, &destruct_lhs)?;
                    let needs = self.mk_node_list(&needs_vec);
                    let args_lhs = self.mk_node_list(&construct_lhs);
                    let mut destruct_rhs = destruct_lhs;
                    destruct_rhs.append(&mut needs_vec);
                    let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
                    let args_rhs = self.mk_node_list(&construct_rhs);
                    let bind = self.mk_node(Node::Apply {
                        lambda,
                        args: args_rhs,
                    });
                    let result = self.mk_node(Node::Bind {
                        args: args_lhs,
                        bind,
                    });
                    let bind = self.mk_node(Node::Lambda {
                        level: level.succ(),
                        needs,
                        result,
                    });
                    Ok(self.mk_node(Node::BindTydef { def, bind }))
                }
                Named::Sigdef(def) => {
                    let lambda = self.extract(level.succ(), slots, DefKind::Sig(def), &destruct_lhs)?;
                    let Sigdef(target) = self.ir.sigdefs[def];
                    let (construct_lhs, mut needs_vec) =
                        self.invoke_need(level.succ(), target, &destruct_lhs)?;
                    let needs = self.mk_node_list(&needs_vec);
                    let args_lhs = self.mk_node_list(&construct_lhs);
                    let mut destruct_rhs = destruct_lhs;
                    destruct_rhs.append(&mut needs_vec);
                    let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
                    let args_rhs = self.mk_node_list(&construct_rhs);
                    let bind = self.mk_node(Node::Apply {
                        lambda,
                        args: args_rhs,
                    });
                    let result = self.mk_node(Node::Bind {
                        args: args_lhs,
                        bind,
                    });
                    let bind = self.mk_node(Node::Lambda {
                        level: level.succ(),
                        needs,
                        result,
                    });
                    Ok(self.mk_node(Node::BindSigdef { def, bind }))
                }
                Named::Valdef(def) => {
                    let lambda = self.extract(level.succ(), slots, DefKind::Val(def), &destruct_lhs)?;
                    let Valdef(target) = self.ir.valdefs[def];
                    let (construct_lhs, mut needs_vec) =
                        self.invoke_need(level.succ(), target, &destruct_lhs)?;
                    let needs = self.mk_node_list(&needs_vec);
                    let args_lhs = self.mk_node_list(&construct_lhs);
                    let mut destruct_rhs = destruct_lhs;
                    destruct_rhs.append(&mut needs_vec);
                    let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
                    let args_rhs = self.mk_node_list(&construct_rhs);
                    let bind = self.mk_node(Node::Apply {
                        lambda,
                        args: args_rhs,
                    });
                    let result = self.mk_node(Node::Bind {
                        args: args_lhs,
                        bind,
                    });
                    let bind = self.mk_node(Node::Lambda {
                        level: level.succ(),
                        needs,
                        result,
                    });
                    Ok(self.mk_node(Node::BindValdef { def, bind }))
                }
                Named::Ctxdef(def) => {
                    let lambda = self.extract_ctx(level.succ(), slots, def, &destruct_lhs, true)?;
                    let Ctxdef(target) = self.ir.ctxdefs[def];
                    let (construct_lhs, mut needs_vec) =
                        self.invoke_need(level.succ(), target, &destruct_lhs)?;
                    let needs = self.mk_node_list(&needs_vec);
                    let args_lhs = self.mk_node_list(&construct_lhs);
                    let mut destruct_rhs = destruct_lhs;
                    destruct_rhs.append(&mut needs_vec);
                    let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
                    let args_rhs = self.mk_node_list(&construct_rhs);
                    let bind = self.mk_node(Node::Apply {
                        lambda,
                        args: args_rhs,
                    });
                    let result = self.mk_node(Node::Bind {
                        args: args_lhs,
                        bind,
                    });
                    let bind = self.mk_node(Node::Lambda {
                        level: level.succ(),
                        needs,
                        result,
                    });
                    Ok(self.mk_node(Node::BindCtxdef { def, bind }))
                }
                Named::Module(_) => Err(LowerError::BindModule(bind)),
                Named::Tagdef(_) => Err(LowerError::BindNominal(bind)),
                Named::Aliasdef(_) => Err(LowerError::BindAlias(bind)),
                Named::Fndef(_) => Err(LowerError::BindDefined(bind)),
            },
            Some(parse::Entry::Lit(token)) => match lhs {
                Named::Valdef(def) => {
                    let Valdef(target) = self.ir.valdefs[def];
                    let (construct, needs) =
                        self.invoke_need(level.succ(), target, &destruct_lhs)?;
                    let needs = self.mk_node_list(&needs);
                    let (val, _) = self.lit(token)?;
                    let bind = self.mk_node(Node::Lit { val });
                    let args = self.mk_node_list(&construct);
                    let result = self.mk_node(Node::Bind { args, bind });
                    let bind = self.mk_node(Node::Lambda {
                        level: level.succ(),
                        needs,
                        result,
                    });
                    Ok(self.mk_node(Node::BindValdef { def, bind }))
                }
                _ => Err(LowerError::LitNotVal(token)),
            },
            Some(parse::Entry::Ref(spec)) => {
                let (rhs, rhs_binds) = self.spec(level, slots, spec)?;
                // The right-hand side is invoked against the ambient context (`slots`) as well as
                // its own bindings, so a referenced implementation can resolve the context it needs
                // (e.g. `ops::add[...]=uint32_add`, where `uint32_add` itself needs `Wasi`).
                let rhs_binds_saved = rhs_binds.clone();
                let mut destruct_rhs = slots.to_vec();
                destruct_rhs.extend(rhs_binds);
                match lhs {
                    Named::Tydef(def) => {
                        let lambda = match rhs {
                            Named::Tydef(tydef) => {
                                self.extract(level.succ(), slots, DefKind::Ty(tydef), &destruct_rhs)?
                            }
                            Named::Tagdef(tagdef) => self.mk_node(Node::Tagdef { def: tagdef }),
                            Named::Aliasdef(aliasdef) => {
                                self.mk_node(Node::Aliasdef { def: aliasdef })
                            }
                            _ => return Err(LowerError::BindMismatch(bind)),
                        };
                        let Tydef(target_lhs) = self.ir.tydefs[def];
                        let (construct_lhs, mut needs_vec) =
                            self.invoke_need(level.succ(), target_lhs, &destruct_lhs)?;
                        let needs = self.mk_node_list(&needs_vec);
                        let args_lhs = self.mk_node_list(&construct_lhs);
                        destruct_rhs.append(&mut needs_vec);
                        let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
                        let args_rhs = self.mk_node_list(&construct_rhs);
                        let bind = self.mk_node(Node::Apply {
                            lambda,
                            args: args_rhs,
                        });
                        let result = self.mk_node(Node::Bind {
                            args: args_lhs,
                            bind,
                        });
                        let bind = self.mk_node(Node::Lambda {
                            level: level.succ(),
                            needs,
                            result,
                        });
                        Ok(self.mk_node(Node::BindTydef { def, bind }))
                    }
                    Named::Sigdef(def) => {
                        let lambda = match rhs {
                            Named::Sigdef(sigdef) => {
                                self.extract(level.succ(), slots, DefKind::Sig(sigdef), &destruct_rhs)?
                            }
                            Named::Fndef(fndef) => self.mk_node(Node::Fndef { def: fndef }),
                            _ => return Err(LowerError::BindMismatch(bind)),
                        };
                        // TODO: Check compatibility of function signatures.
                        let Sigdef(target_lhs) = self.ir.sigdefs[def];
                        let (construct_lhs, mut needs_vec) =
                            self.invoke_need(level.succ(), target_lhs, &destruct_lhs)?;
                        let needs = self.mk_node_list(&needs_vec);
                        let args_lhs = self.mk_node_list(&construct_lhs);
                        destruct_rhs.append(&mut needs_vec);
                        let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
                        let args_rhs = self.mk_node_list(&construct_rhs);
                        let bind = self.mk_node(Node::Apply {
                            lambda,
                            args: args_rhs,
                        });
                        let result = self.mk_node(Node::Bind {
                            args: args_lhs,
                            bind,
                        });
                        let bind = self.mk_node(Node::Lambda {
                            level: level.succ(),
                            needs,
                            result,
                        });
                        Ok(self.mk_node(Node::BindSigdef { def, bind }))
                    }
                    Named::Valdef(def) => {
                        let lambda = match rhs {
                            Named::Valdef(valdef) => {
                                self.extract(level.succ(), slots, DefKind::Val(valdef), &destruct_rhs)?
                            }
                            _ => return Err(LowerError::BindMismatch(bind)),
                        };
                        // TODO: Check compatibility of value types.
                        let Valdef(target_lhs) = self.ir.valdefs[def];
                        let (construct_lhs, mut needs_vec) =
                            self.invoke_need(level.succ(), target_lhs, &destruct_lhs)?;
                        let needs = self.mk_node_list(&needs_vec);
                        let args_lhs = self.mk_node_list(&construct_lhs);
                        destruct_rhs.append(&mut needs_vec);
                        let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
                        let args_rhs = self.mk_node_list(&construct_rhs);
                        let bind = self.mk_node(Node::Apply {
                            lambda,
                            args: args_rhs,
                        });
                        let result = self.mk_node(Node::Bind {
                            args: args_lhs,
                            bind,
                        });
                        let bind = self.mk_node(Node::Lambda {
                            level: level.succ(),
                            needs,
                            result,
                        });
                        Ok(self.mk_node(Node::BindValdef { def, bind }))
                    }
                    Named::Ctxdef(def) => {
                        let lambda = match rhs {
                            Named::Ctxdef(rhs_def) => {
                                // Forward an existing concrete provider of the RHS context (e.g.
                                // Wasi's `Numerals[Number=I32]`) rather than re-deriving the ctxdef
                                // with `extract`, which only applies the outer param and leaves the
                                // inner member-needs open. Build the RHS context need (mirroring
                                // `need`) and resolve it, which tries `extract_lambda` first.
                                let Ctxdef(rhs_target) = self.ir.ctxdefs[rhs_def];
                                let (rhs_construct, rhs_needs) =
                                    self.invoke_need(level.succ(), rhs_target, &rhs_binds_saved)?;
                                let rhs_needs = self.mk_node_list(&rhs_needs);
                                let rhs_items = self.mk_node_list(&rhs_construct);
                                let rhs_result = self.mk_node(Node::List { items: rhs_items });
                                let rhs_param = self.mk_node(Node::Lambda {
                                    level: level.succ(),
                                    needs: rhs_needs,
                                    result: rhs_result,
                                });
                                let rhs_need = self.mk_node(Node::NeedCtxdef {
                                    level,
                                    def: rhs_def,
                                    param: rhs_param,
                                });
                                match self.resolve_need(rhs_need, &destruct_rhs)? {
                                    Some(provider) => provider,
                                    None => return Err(LowerError::BindMismatch(bind)),
                                }
                            }
                            _ => return Err(LowerError::BindMismatch(bind)),
                        };
                        let Ctxdef(target_lhs) = self.ir.ctxdefs[def];
                        let (construct_lhs, mut needs_vec) =
                            self.invoke_need(level.succ(), target_lhs, &destruct_lhs)?;
                        let needs = self.mk_node_list(&needs_vec);
                        let args_lhs = self.mk_node_list(&construct_lhs);
                        destruct_rhs.append(&mut needs_vec);
                        let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
                        let args_rhs = self.mk_node_list(&construct_rhs);
                        let bind = self.mk_node(Node::Apply {
                            lambda,
                            args: args_rhs,
                        });
                        let result = self.mk_node(Node::Bind {
                            args: args_lhs,
                            bind,
                        });
                        let bind = self.mk_node(Node::Lambda {
                            level: level.succ(),
                            needs,
                            result,
                        });
                        Ok(self.mk_node(Node::BindCtxdef { def, bind }))
                    }
                    Named::Module(_) => Err(LowerError::BindModule(bind)),
                    Named::Tagdef(_) => Err(LowerError::BindNominal(bind)),
                    Named::Aliasdef(_) => Err(LowerError::BindAlias(bind)),
                    Named::Fndef(_) => Err(LowerError::BindDefined(bind)),
                }
            }
        }
    }

    fn need(
        &mut self,
        level: Level,
        params: &mut Vec<NodeId>,
        slots: &[NodeId],
        need: parse::NeedId,
    ) -> LowerResult<NodeId> {
        let parse::Need { kind: _, bind } = self.tree.needs[need];
        // TODO: Handle `kind`.
        self.need_bind(level, params, slots, bind)
    }

    fn need_bind(
        &mut self,
        level: Level,
        params: &mut Vec<NodeId>,
        slots: &[NodeId],
        bind: parse::BindId,
    ) -> LowerResult<NodeId> {
        let parse::Bind { key, val } = self.tree.binds[bind];
        match val {
            Some(_) => self.bind(level, slots, bind),
            None => {
                let (lhs, destruct) = self.spec(level, slots, key)?;
                match lhs {
                    Named::Tydef(def) => {
                        let Tydef(target) = self.ir.tydefs[def];
                        let (construct, needs) =
                            self.invoke_need(level.succ(), target, &destruct)?;
                        let needs = self.mk_node_list(&needs);
                        let items = self.mk_node_list(&construct);
                        let result = self.mk_node(Node::List { items });
                        let param = self.mk_node(Node::Lambda {
                            level: level.succ(),
                            needs,
                            result,
                        });
                        let node = self.mk_node(Node::NeedTydef { level, def, param });
                        params.push(node);
                        Ok(node)
                    }
                    Named::Sigdef(def) => {
                        let Sigdef(target) = self.ir.sigdefs[def];
                        let (construct, needs) =
                            self.invoke_need(level.succ(), target, &destruct)?;
                        let needs = self.mk_node_list(&needs);
                        let items = self.mk_node_list(&construct);
                        let result = self.mk_node(Node::List { items });
                        let param = self.mk_node(Node::Lambda {
                            level: level.succ(),
                            needs,
                            result,
                        });
                        let node = self.mk_node(Node::NeedSigdef { level, def, param });
                        params.push(node);
                        Ok(node)
                    }
                    Named::Valdef(def) => {
                        let Valdef(target) = self.ir.valdefs[def];
                        let (construct, needs) =
                            self.invoke_need(level.succ(), target, &destruct)?;
                        let needs = self.mk_node_list(&needs);
                        let items = self.mk_node_list(&construct);
                        let result = self.mk_node(Node::List { items });
                        let param = self.mk_node(Node::Lambda {
                            level: level.succ(),
                            needs,
                            result,
                        });
                        let node = self.mk_node(Node::NeedValdef { level, def, param });
                        params.push(node);
                        Ok(node)
                    }
                    Named::Ctxdef(def) => {
                        let Ctxdef(target) = self.ir.ctxdefs[def];
                        let (construct, needs) =
                            self.invoke_need(level.succ(), target, &destruct)?;
                        let needs = self.mk_node_list(&needs);
                        let items = self.mk_node_list(&construct);
                        let result = self.mk_node(Node::List { items });
                        let param = self.mk_node(Node::Lambda {
                            level: level.succ(),
                            needs,
                            result,
                        });
                        let node = self.mk_node(Node::NeedCtxdef { level, def, param });
                        params.push(node);
                        Ok(node)
                    }
                    Named::Module(_) => Err(LowerError::BindModule(bind)),
                    Named::Tagdef(_) => Err(LowerError::BindNominal(bind)),
                    Named::Aliasdef(_) => Err(LowerError::BindAlias(bind)),
                    Named::Fndef(_) => Err(LowerError::BindDefined(bind)),
                }
            }
        }
    }

    fn needs(
        &mut self,
        level: Level,
        params: &mut Vec<NodeId>,
        needs: IdRange<parse::NeedId>,
    ) -> LowerResult<Vec<NodeId>> {
        let mut slots = Vec::new();
        // A file-level `assume` seeds the base of every declaration's resolution
        // context, so its bindings act like additional `[needs]` prepended to
        // each declaration. For now we fold them flatly into the front of the
        // slots (the bottom frame), with the declaration's own needs above them.
        if level == Level::ZERO {
            for assume in self.tree.assumes.iter().copied() {
                for bind in assume {
                    slots.push(self.need_bind(level, params, &slots, bind)?);
                }
            }
        }
        for need in needs {
            slots.push(self.need(level, params, &slots, need)?);
        }
        Ok(slots)
    }

    fn parse_ty(&mut self, slots: &[NodeId], ty: parse::TypeId) -> LowerResult<NodeId> {
        match self.tree.types[ty] {
            parse::Type::Spec(spec) => {
                let (named, destruct) = self.spec(Level::ZERO, slots, spec)?;
                match named {
                    Named::Tydef(tydef) => {
                        let lambda = self.extract(Level::ONE, slots, DefKind::Ty(tydef), &destruct)?;
                        let construct = self.invoke_force(lambda, &destruct)?;
                        let args = self.mk_node_list(&construct);
                        Ok(self.mk_node(Node::Apply { lambda, args }))
                    }
                    Named::Tagdef(tagdef) => Err(self.todo_no_loc()),
                    Named::Aliasdef(aliasdef) => Err(self.todo_no_loc()),
                    _ => return Err(LowerError::NotType(spec.path.last)),
                }
            }
            parse::Type::Tuple(elems) => {
                let lowered = elems
                    .into_iter()
                    .map(|elem| self.parse_ty(slots, elem))
                    .collect::<LowerResult<Vec<_>>>()?;
                Ok(self.ty_tuple(&lowered))
            }
            parse::Type::Record(members) => Err(self.todo_no_loc()),
        }
    }

    fn parse_sig(&mut self, fndef: parse::Fndef) -> LowerResult<(Vec<NodeId>, NodeId, NodeId)> {
        let parse::Fndef {
            name: _,
            needs,
            params,
            result,
            def: _,
        } = fndef;
        let mut param_nodes = Vec::new();
        let slots = self.needs(Level::ZERO, &mut param_nodes, needs)?;
        let elems = (params.into_iter())
            .map(|arg| self.parse_ty(&slots, self.tree.params[arg].ty))
            .collect::<LowerResult<Vec<_>>>()?;
        let param_tuple = self.ty_tuple(&elems);
        let result_ty = match result {
            parse::Return::Unit => self.ty_unit(),
            parse::Return::Type(ty) => self.parse_ty(&slots, ty)?,
            parse::Return::Bind(needs) => {
                let mut param_nodes = Vec::new();
                let slots = self.needs(Level::ONE, &mut param_nodes, needs)?;
                let need_nodes = self.mk_node_list(&param_nodes);
                let items = self.mk_node_list(&slots);
                let result = self.mk_node(Node::List { items });
                self.mk_node(Node::Lambda {
                    level: Level::ONE,
                    needs: need_nodes,
                    result,
                })
            }
        };
        Ok((param_nodes, param_tuple, result_ty))
    }

    fn parse_fndef(&mut self, fndef: parse::Fndef) -> LowerResult<(StrId, NamedFn)> {
        let parse::Fndef {
            name,
            needs: _,
            params: _,
            result: _,
            def,
        } = fndef;
        let (params, param_tuple, result_ty) = self.parse_sig(fndef)?;
        let need_nodes = self.mk_node_list(&params);
        let signature = self.mk_node(Node::Sig {
            param: param_tuple,
            result: result_ty,
        });
        let body = self.mk_node(Node::Lambda {
            level: Level::ZERO,
            needs: need_nodes,
            result: signature,
        });
        let lowered = match def {
            None => NamedFn::Sigdef(self.ir.sigdefs.push(Sigdef(body))),
            Some(_) => NamedFn::Fndef(self.ir.fndefs.push(Sigdef(body))),
        };
        let string = self.name(name);
        Ok((string, lowered))
    }

    fn decls(&mut self) -> LowerResult<()> {
        for &decl in &self.tree.decls {
            match decl {
                parse::Decl::Tydef(id) => {
                    let parse::Tydef { name, needs } = self.tree.tydefs[id];
                    let mut params = Vec::new();
                    self.needs(Level::ZERO, &mut params, needs)?;
                    let need_nodes = self.mk_node_list(&params);
                    let result = self.mk_node(Node::Nothing);
                    let body = self.mk_node(Node::Lambda {
                        level: Level::ZERO,
                        needs: need_nodes,
                        result,
                    });
                    let lowered = self.ir.tydefs.push(Tydef(body));
                    let string = self.name(name);
                    self.names
                        .create_named(self.module, string, Named::Tydef(lowered));
                }
                parse::Decl::Tagdef(id) => {
                    let parse::Tagdef { name, needs, def } = self.tree.tagdefs[id];
                    let mut params = Vec::new();
                    let slots = self.needs(Level::ZERO, &mut params, needs)?;
                    let need_nodes = self.mk_node_list(&params);
                    let ty = self.parse_ty(&slots, def)?;
                    let body = self.mk_node(Node::Lambda {
                        level: Level::ZERO,
                        needs: need_nodes,
                        result: ty,
                    });
                    let lowered = self.ir.tagdefs.push(Tagdef(body));
                    let string = self.name(name);
                    self.names
                        .create_named(self.module, string, Named::Tagdef(lowered));
                }
                parse::Decl::Aliasdef(id) => {
                    let parse::Aliasdef { name, needs, def } = self.tree.aliasdefs[id];
                    let mut params = Vec::new();
                    let slots = self.needs(Level::ZERO, &mut params, needs)?;
                    let need_nodes = self.mk_node_list(&params);
                    let ty = self.parse_ty(&slots, def)?;
                    let body = self.mk_node(Node::Lambda {
                        level: Level::ZERO,
                        needs: need_nodes,
                        result: ty,
                    });
                    let lowered = self.ir.aliasdefs.push(Aliasdef(body));
                    let string = self.name(name);
                    self.names
                        .create_named(self.module, string, Named::Aliasdef(lowered));
                }
                parse::Decl::Funcdef(id) => {
                    let parse::Funcdef { fndef } = self.tree.funcdefs[id];
                    let (fn_name, lowered) = self.parse_fndef(fndef)?;
                    if let NamedFn::Fndef(defined) = lowered {
                        self.funcs.push((id, defined));
                    }
                    self.names
                        .create_named(self.module, fn_name, lowered.into());
                }
                parse::Decl::Attachdef(id) => {
                    let parse::Attachdef { ty, fndef } = self.tree.attachdefs[id];
                    let (fn_name, lowered) = self.parse_fndef(fndef)?;
                    let ty_name = self.name(ty);
                    // TODO: Prevent attaching methods to nominal types defined in other modules.
                    match self.names.names.get(&(self.module, ty_name)) {
                        Some(&Named::Tagdef(tagdef)) => {
                            if let NamedFn::Fndef(defined) = lowered {
                                self.attaches.push((id, tagdef, defined));
                            }
                            self.names.create_attached(tagdef, fn_name, lowered);
                        }
                        Some(_) => return Err(LowerError::NotNominal(ty)),
                        None => return Err(LowerError::Undefined(ty)),
                    }
                }
                parse::Decl::Detachdef(id) => {
                    let parse::Detachdef { fndef } = self.tree.detachdefs[id];
                    let (fn_name, lowered) = self.parse_fndef(fndef)?;
                    if let NamedFn::Fndef(defined) = lowered {
                        self.detaches.push((id, defined));
                    }
                    self.names.create_detached(self.module, fn_name, lowered);
                }
                parse::Decl::Valdef(id) => {
                    let parse::Valdef { name, needs, ty } = self.tree.valdefs[id];
                    let mut params = Vec::new();
                    let slots = self.needs(Level::ZERO, &mut params, needs)?;
                    let need_nodes = self.mk_node_list(&params);
                    let ty = self.parse_ty(&slots, ty)?;
                    let body = self.mk_node(Node::Lambda {
                        level: Level::ZERO,
                        needs: need_nodes,
                        result: ty,
                    });
                    let lowered = self.ir.valdefs.push(Valdef(body));
                    let string = self.name(name);
                    self.names
                        .create_named(self.module, string, Named::Valdef(lowered));
                }
                parse::Decl::Ctxdef(id) => {
                    let parse::Ctxdef { name, needs, def } = self.tree.ctxdefs[id];
                    let mut slots = Vec::new();
                    let mut params_outer = Vec::new();
                    for need in needs {
                        slots.push(self.need(Level::ZERO, &mut params_outer, &slots, need)?);
                    }
                    let mut params_inner = Vec::new();
                    // A file-level `assume` seeds the base of the context available to the
                    // members of this composite context, not its outer parameter list: members
                    // need the assumed context, but a use site like `[Mem]` should not have to
                    // supply it again, so a non-parametric context stays nullary at its uses.
                    // (For the prelude this coincides with the members' own folded needs, which
                    // already bubble the assumed context into `params_inner`.)
                    for assume in self.tree.assumes.iter().copied() {
                        for bind in assume {
                            slots.push(self.need_bind(Level::ONE, &mut params_inner, &slots, bind)?);
                        }
                    }
                    let member_base = slots.len();
                    for need in def {
                        slots.push(self.need(Level::ONE, &mut params_inner, &slots, need)?);
                    }
                    let needs_inner = self.mk_node_list(&params_inner);
                    let needs_outer = self.mk_node_list(&params_outer);
                    let items = self.mk_node_list(&slots[member_base..]);
                    let result = self.mk_node(Node::List { items });
                    let lambda = self.mk_node(Node::Lambda {
                        level: Level::ONE,
                        needs: needs_inner,
                        result,
                    });
                    let body = self.mk_node(Node::Lambda {
                        level: Level::ZERO,
                        needs: needs_outer,
                        result: lambda,
                    });
                    let lowered = self.ir.ctxdefs.push(Ctxdef(body));
                    let string = self.name(name);
                    self.names
                        .create_named(self.module, string, Named::Ctxdef(lowered));
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
            }
            .body(fndef, id_decl)?;
            let id_body = self.ir.bodies.push(body);
            assert_eq!(id_decl, id_body);
        }
        for (attachdef, _tagdef, id_decl) in take(&mut self.attaches) {
            // TODO: Handle `this`.
            let parse::Attachdef { ty: _, fndef } = self.tree.attachdefs[attachdef];
            let body = LowerBody {
                x: self,
                slots: Vec::new(),
                locals: HashMap::new(),
            }
            .body(fndef, id_decl)?;
            let id_body = self.ir.bodies.push(body);
            assert_eq!(id_decl, id_body);
        }
        for (detachdef, id_decl) in take(&mut self.detaches) {
            // TODO: Handle `this`.
            let parse::Detachdef { fndef } = self.tree.detachdefs[detachdef];
            let body = LowerBody {
                x: self,
                slots: Vec::new(),
                locals: HashMap::new(),
            }
            .body(fndef, id_decl)?;
            let id_body = self.ir.bodies.push(body);
            assert_eq!(id_decl, id_body);
        }
        Ok(())
    }

    fn program(&mut self) -> LowerResult<()> {
        self.imports()?;
        // TODO: Don't make declaration order significant.
        self.decls()?;
        // The module that declares the digit/radix values (`std.moss`) may itself contain numeric
        // literals (e.g. a string literal's sizes and code points). Its own declarations are now
        // registered, so seed `base.numerals` from them before lowering bodies, giving its literals
        // the same contextual digit resolution that every downstream module gets.
        self.seed_numerals();
        self.bodies()?;
        Ok(())
    }

    /// If `base.numerals` is not yet populated and the current module declares the digit/radix
    /// values and the `Number` type (i.e. this is `std.moss`), fill in [`Base::numerals`] from
    /// those just-registered declarations.
    fn seed_numerals(&mut self) {
        match self.base {
            Some(Base { numerals: Some(_), .. }) | None => return,
            Some(_) => {}
        }
        let module = self.module;
        let names = &self.names.names;
        let strings = &self.ir.strings;
        let valdef = |name: &str| match names.get(&(module, strings.get_id(name)?)) {
            Some(&Named::Valdef(def)) => Some(def),
            _ => None,
        };
        let tydef = |name: &str| match names.get(&(module, strings.get_id(name)?)) {
            Some(&Named::Tydef(def)) => Some(def),
            _ => None,
        };
        let numerals = (|| {
            Some(Numerals {
                number: tydef("Number")?,
                digit0: valdef("digit0")?,
                digit1: valdef("digit1")?,
                digit2: valdef("digit2")?,
                digit3: valdef("digit3")?,
                digit4: valdef("digit4")?,
                digit5: valdef("digit5")?,
                digit6: valdef("digit6")?,
                digit7: valdef("digit7")?,
                digit8: valdef("digit8")?,
                digit9: valdef("digit9")?,
                radix: valdef("radix")?,
            })
        })();
        if let Some(numerals) = numerals {
            self.base.as_mut().unwrap().numerals = Some(numerals);
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Typed {
    ty: NodeId,
    val: InstrId,
}

#[derive(Debug)]
struct LowerBody<'a, 'b> {
    x: &'b mut Lower<'a>,
    slots: Vec<NodeId>,
    locals: HashMap<StrId, Typed>,
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

    fn ty_tuple(&mut self, elems: &[NodeId]) -> NodeId {
        self.x.ty_tuple(elems)
    }

    fn ty_record(&mut self, fields: &[(StrId, NodeId)]) -> NodeId {
        self.x.ty_record(fields)
    }

    fn instr(&mut self, ty: NodeId, expr: Expr) -> Typed {
        Typed {
            ty,
            val: self.emit(Instr::Expr { ty, expr }),
        }
    }

    fn instr_tuple(&mut self, ty: NodeId, elems: &[InstrId]) -> Typed {
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

    fn instr_record(&mut self, ty: NodeId, fields: &[(StrId, InstrId)]) -> Typed {
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

    fn invoke_force(&mut self, lambda: NodeId) -> LowerResult<Vec<NodeId>> {
        self.x.invoke_force(lambda, &self.slots)
    }

    fn extract_ty(&mut self, def: TydefId) -> LowerResult<NodeId> {
        self.x
            .extract(Level::ONE, &self.slots, DefKind::Ty(def), &self.slots)
    }

    fn extract_sig(&mut self, def: SigdefId) -> LowerResult<NodeId> {
        self.x
            .extract(Level::ONE, &self.slots, DefKind::Sig(def), &self.slots)
    }

    fn extract_val(&mut self, def: ValdefId) -> LowerResult<NodeId> {
        self.x
            .extract(Level::ONE, &self.slots, DefKind::Val(def), &self.slots)
    }

    /// Like [`LowerBody::extract_val`], but resolve against `slots` (used for the disambiguating
    /// type bindings the literal desugar pushes on top of the body's own context).
    fn extract_val_in(&mut self, slots: &[NodeId], def: ValdefId) -> LowerResult<NodeId> {
        self.x.extract(Level::ONE, slots, DefKind::Val(def), slots)
    }

    /// Like [`LowerBody::extract_sig`], but resolve against `slots`.
    fn extract_sig_in(&mut self, slots: &[NodeId], def: SigdefId) -> LowerResult<NodeId> {
        self.x.extract(Level::ONE, slots, DefKind::Sig(def), slots)
    }

    /// The [`Numerals`] (digit/radix valdefs and the `Number` tydef) used to desugar numeric
    /// literals. Always populated by the time any literal is lowered: the prelude precompile fills
    /// [`Base::numerals`] as soon as the module that declares them (`std.moss`) has been lowered,
    /// before any later prelude module (or downstream code) whose body contains a literal.
    fn numerals(&self) -> Numerals {
        self.base()
            .numerals
            .expect("digit/radix valdefs must be available when lowering a numeric literal")
    }

    /// The valdef for the digit `n` (`0..=9`), resolved contextually at the literal's type by
    /// [`LowerBody::val_at`].
    fn digit_valdef(&self, n: u8) -> ValdefId {
        let numerals = self.numerals();
        match n {
            0 => numerals.digit0,
            1 => numerals.digit1,
            2 => numerals.digit2,
            3 => numerals.digit3,
            4 => numerals.digit4,
            5 => numerals.digit5,
            6 => numerals.digit6,
            7 => numerals.digit7,
            8 => numerals.digit8,
            9 => numerals.digit9,
            _ => unreachable!(),
        }
    }

    /// The `radix` valdef, resolved contextually like [`LowerBody::digit_valdef`].
    fn radix_valdef(&self) -> ValdefId {
        self.numerals().radix
    }

    /// Resolve a digit value `n` at concrete type `ty` (a [`TydefId`]), emitting an `Expr::Val`.
    fn digit(&mut self, ty: TydefId, n: u8) -> LowerResult<Typed> {
        let def = self.digit_valdef(n);
        self.val_at(ty, def)
    }

    /// Resolve the `radix` value at concrete type `ty`, emitting an `Expr::Val`.
    fn radix(&mut self, ty: TydefId) -> LowerResult<Typed> {
        let def = self.radix_valdef();
        self.val_at(ty, def)
    }

    /// Resolve a numeric value `def` (a digit or `radix`) under the binding `Number=ty`, emitting
    /// an `Expr::Val`. Without the binding the value would be ambiguous, since `Std` provides
    /// `Numerals[Number=...]` for every numeric type at once.
    fn val_at(&mut self, ty: TydefId, def: ValdefId) -> LowerResult<Typed> {
        let number = self.numerals().number;
        let bind = self.x.bind_tydef(Level::ZERO, &self.slots, number, ty)?;
        let mut slots = self.slots.clone();
        slots.push(bind);
        let val = self.extract_val_in(&slots, def)?;
        // The value's type is `Number`, which the binding pins to `ty`; build that concrete type
        // directly rather than inlining the (generic) valdef body, whose `Number` need is already
        // satisfied by the binding.
        let result = self.ty_of(ty)?;
        Ok(self.instr(result, Expr::Val { val }))
    }

    /// Construct the type node for a nullary concrete type `ty`.
    fn ty_of(&mut self, ty: TydefId) -> LowerResult<NodeId> {
        let lambda = self.extract_ty(ty)?;
        let construct = self.invoke_force(lambda)?;
        let args = self.x.mk_node_list(&construct);
        Ok(self.x.mk_node(Node::Apply { lambda, args }))
    }

    /// Apply a binary arithmetic operation `sigdef` (`add` or `mul`) at type `ty` to `lhs`/`rhs`,
    /// resolving the operation under the bindings `Lhs=ty, Rhs=ty` so that `Std`'s per-type
    /// `Arithmetic[Number=...]` entries do not make it ambiguous.
    fn op2(&mut self, ty: TydefId, sigdef: SigdefId, lhs: Typed, rhs: Typed) -> LowerResult<Typed> {
        let Arith { lhs: lhs_ty, rhs: rhs_ty, .. } = self.base().arith;
        let bind_lhs = self.x.bind_tydef(Level::ZERO, &self.slots, lhs_ty, ty)?;
        let bind_rhs = self.x.bind_tydef(Level::ZERO, &self.slots, rhs_ty, ty)?;
        let mut slots = self.slots.clone();
        slots.push(bind_lhs);
        slots.push(bind_rhs);
        let lambda = self.extract_sig_in(&slots, sigdef)?;
        let construct = self.x.invoke_force(lambda, &slots)?;
        let args = self.x.mk_node_list(&construct);
        let func = self.x.mk_node(Node::Apply { lambda, args });
        let (ty_param, ty_result) = self.sig(func)?;
        let args_ty = [lhs.ty, rhs.ty];
        let ty_args = self.ty_tuple(&args_ty);
        self.expect_ty(ty_param, ty_args)?;
        let arg = self.instr_tuple(ty_args, &[lhs.val, rhs.val]).val;
        Ok(self.instr(ty_result, Expr::Call { func, arg }))
    }

    /// The decimal digits (each `0..=9`) of a numeric literal token, ignoring its type suffix.
    fn decimal_digits(&self, token: TokenId) -> Vec<u8> {
        let slice = self.x.slice(token);
        let digits = slice
            .strip_suffix("u32")
            .or_else(|| slice.strip_suffix("i32"))
            .or_else(|| slice.strip_suffix("u64"))
            .or_else(|| slice.strip_suffix("i64"))
            .or_else(|| slice.strip_suffix("u"))
            .unwrap_or(slice);
        digits.bytes().map(|b| b - b'0').collect()
    }

    /// Build a numeric literal of decimal `digits` at concrete type `ty` by Horner's method over
    /// the contextual digit values and `add`/`mul`.
    fn numeric(&mut self, ty: TydefId, digits: &[u8]) -> LowerResult<Typed> {
        let Arith { add, mul, .. } = self.base().arith;
        let mut iter = digits.iter().copied();
        let first = iter.next().expect("a numeric literal has at least one digit");
        let mut acc = self.digit(ty, first)?;
        for d in iter {
            let radix = self.radix(ty)?;
            let scaled = self.op2(ty, mul, acc, radix)?;
            let digit = self.digit(ty, d)?;
            acc = self.op2(ty, add, scaled, digit)?;
        }
        Ok(acc)
    }

    /// Build a `Uint` numeric literal of value `n` (used for string lengths, indices, and code
    /// points), reusing the numeric desugar.
    fn uint(&mut self, n: u32) -> LowerResult<Typed> {
        let ty = self.base().types.uint;
        let digits: Vec<u8> = n
            .to_string()
            .bytes()
            .map(|b| b - b'0')
            .collect();
        self.numeric(ty, &digits)
    }

    /// Call a contextual function `sigdef` (taking no disambiguating bindings) on `args`.
    fn call_sig(&mut self, sigdef: SigdefId, args: &[Typed]) -> LowerResult<Typed> {
        let lambda = self.extract_sig(sigdef)?;
        let construct = self.invoke_force(lambda)?;
        let items = self.x.mk_node_list(&construct);
        let func = self.x.mk_node(Node::Apply { lambda, args: items });
        let (ty_param, ty_result) = self.sig(func)?;
        let args_ty: Vec<NodeId> = args.iter().map(|a| a.ty).collect();
        let args_val: Vec<InstrId> = args.iter().map(|a| a.val).collect();
        let ty_args = self.ty_tuple(&args_ty);
        self.expect_ty(ty_param, ty_args)?;
        let arg = self.instr_tuple(ty_args, &args_val).val;
        Ok(self.instr(ty_result, Expr::Call { func, arg }))
    }

    /// Build a character literal from its Unicode code point `c`.
    fn char_lit(&mut self, c: char) -> LowerResult<Typed> {
        let Builders { char_from_codepoint, .. } = self.base().builders;
        let codepoint = self.uint(c as u32)?;
        self.call_sig(char_from_codepoint, &[codepoint])
    }

    /// Build a string literal from its characters by filling a string builder.
    fn string_lit(&mut self, chars: &[char]) -> LowerResult<Typed> {
        let Builders {
            string_builder,
            set_char,
            build,
            ..
        } = self.base().builders;
        let len = self.uint(chars.len() as u32)?;
        let mut builder = self.call_sig(string_builder, &[len])?;
        for (i, &c) in chars.iter().enumerate() {
            let index = self.uint(i as u32)?;
            let character = self.char_lit(c)?;
            builder = self.call_sig(set_char, &[builder, index, character])?;
        }
        self.call_sig(build, &[builder])
    }

    fn extract_ctx(&mut self, def: CtxdefId) -> LowerResult<NodeId> {
        self.x
            .extract_ctx(Level::ONE, &self.slots, def, &self.slots, true)
    }

    fn inline(&mut self, lambda: NodeId, construct: &[NodeId]) -> LowerResult<NodeId> {
        self.x.inline(lambda, construct)
    }

    /// Get the nominal type definition of a value, or [`None`] if the value is not nominally typed.
    fn nominal(&self, ty: NodeId) -> Option<TagdefId> {
        todo!()
    }

    /// Get the fields of a record type.
    fn fields(&self, ty: NodeId) -> LowerResult<Vec<(StrId, NodeId)>> {
        Err(self.x.todo_no_loc())
    }

    /// Get the parameter type and result type of a function.
    fn sig(&mut self, func: NodeId) -> LowerResult<(NodeId, NodeId)> {
        let reduced = self.x.reduce(func)?;
        let sig = match self.x.node(reduced) {
            // A defined function reduces all the way to its signature.
            Node::Sig { .. } => reduced,
            // A contextual function is a stuck application; read its signature from the `sig` decl.
            Node::Apply { lambda, args } => {
                let Node::NeedSigdef { def, param, .. } = self.x.node(lambda) else {
                    panic!()
                };
                self.x.signature(def, param, args)?
            }
            _ => panic!(),
        };
        let Node::Sig { param, result } = self.x.node(sig) else {
            panic!()
        };
        Ok((param, result))
    }

    /// Resolve a method call.
    fn method(&self, ty: NodeId, name: StrId) -> Option<NamedFn> {
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

    /// Check that an `actual` type matches an `expected` type, up to reduction.
    fn expect_ty(&mut self, expected: NodeId, actual: NodeId) -> LowerResult<()> {
        let mut constraints = HashMap::new();
        if self
            .x
            .unify(&Renaming::identity(), &mut constraints, expected, actual)?
        {
            Ok(())
        } else {
            Err(self.x.todo_no_loc()) // TODO: a real type-mismatch error
        }
    }

    fn expr(&mut self, expr: ExprId) -> LowerResult<Typed> {
        match self.x.tree.exprs[expr] {
            parse::Expr::Lit(token) => {
                let types = self.base().types;
                // The concrete type is fixed by the suffix. The range-check error cases ensure the
                // literal fits in that type; the desugar then builds the value contextually.
                let tydef = match self.x.lit(token)? {
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
                    (Val::Uint31(_) | Val::Uint32(_), Some(IntType::Uint32)) => types.uint32,
                    (Val::Uint63(_) | Val::Uint64(_) | Val::Uint(_), Some(IntType::Uint32)) => {
                        return Err(LowerError::Uint32High(token));
                    }

                    // Integer literals ending in `i32`.
                    (Val::Uint31(_) | Val::Int32(_), Some(IntType::Int32)) => types.int32,
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
                    (
                        Val::Uint31(_) | Val::Uint32(_) | Val::Uint63(_) | Val::Uint64(_),
                        Some(IntType::Uint64),
                    ) => types.uint64,
                    (Val::Uint(_), Some(IntType::Uint64)) => {
                        return Err(LowerError::Uint64High(token));
                    }

                    // Integer literals ending in `i64`.
                    (
                        Val::Uint31(_)
                        | Val::Uint32(_)
                        | Val::Int32(_)
                        | Val::Uint63(_)
                        | Val::Int64(_),
                        Some(IntType::Int64),
                    ) => types.int64,
                    (Val::Int(_), Some(IntType::Int64)) => return Err(LowerError::Int64Low(token)),
                    (Val::Uint64(_) | Val::Uint(_), Some(IntType::Int64)) => {
                        return Err(LowerError::Int64High(token));
                    }

                    // Integer literals ending in `u`.
                    (
                        Val::Uint31(_)
                        | Val::Uint32(_)
                        | Val::Uint63(_)
                        | Val::Uint64(_)
                        | Val::Uint(_),
                        Some(IntType::Uint),
                    ) => types.uint,

                    // Integer literals with no suffix.
                    (
                        Val::Uint31(_)
                        | Val::Uint32(_)
                        | Val::Int32(_)
                        | Val::Uint63(_)
                        | Val::Uint64(_)
                        | Val::Int64(_)
                        | Val::Uint(_)
                        | Val::Int(_),
                        Some(IntType::Int),
                    ) => types.int,

                    // Character and string literals.
                    (Val::Char(c), None) => return self.char_lit(c),
                    (Val::String(s), None) => {
                        let chars: Vec<char> = self.x.ir.strings[s].chars().collect();
                        return self.string_lit(&chars);
                    }
                };
                let digits = self.decimal_digits(token);
                self.numeric(tydef, &digits)
            }
            parse::Expr::Path(path) => {
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
                let ty = self.inline(body, &construct)?;
                Ok(self.instr(ty, Expr::Val { val }))
            }
            parse::Expr::Tag(path, inner) => {
                let Named::Tagdef(tagdef) = self.x.path(path)? else {
                    return Err(LowerError::NotNominal(path.last));
                };
                let inside = self.expr(inner)?;
                let Tagdef(body) = self.x.ir.tagdefs[tagdef];
                let lambda = self.x.mk_node(Node::Tagdef { def: tagdef });
                let construct = self.invoke_force(lambda)?;
                let args = self.x.mk_node_list(&construct);
                let ty = self.x.mk_node(Node::Apply { lambda, args });
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
                let obj = self.expr(object)?;
                let name = self.x.name(method);
                // TODO: Contextually set `this` to `obj`.
                let lambda = match self.method(obj.ty, name) {
                    Some(NamedFn::Sigdef(sigdef)) => self.extract_sig(sigdef)?,
                    Some(NamedFn::Fndef(fndef)) => self.x.mk_node(Node::Fndef { def: fndef }),
                    None => return Err(LowerError::Undefined(method)),
                };
                let construct = self.invoke_force(lambda)?;
                let args = self.x.mk_node_list(&construct);
                let func = self.x.mk_node(Node::Apply { lambda, args });
                let (ty_param, ty_result) = self.sig(func)?;
                let lowered = arguments
                    .into_iter()
                    .map(|arg| self.expr(arg))
                    .collect::<LowerResult<Vec<Typed>>>()?;
                let (args_ty, args_val): (Vec<_>, Vec<_>) =
                    lowered.into_iter().map(|arg| (arg.ty, arg.val)).unzip();
                let ty_args = self.ty_tuple(&args_ty);
                self.expect_ty(ty_param, ty_args)?;
                let arg = self.instr_tuple(ty_args, &args_val).val;
                Ok(self.instr(ty_result, Expr::Call { func, arg }))
            }
            parse::Expr::Call(callee, binds, arguments) => {
                // TODO: Handle bindings attached to function calls.
                let lambda = match self.x.path(callee)? {
                    Named::Sigdef(sigdef) => self.extract_sig(sigdef)?,
                    Named::Fndef(fndef) => self.x.mk_node(Node::Fndef { def: fndef }),
                    _ => return Err(LowerError::NotFn(callee.last)),
                };
                let construct = self.invoke_force(lambda)?;
                let args = self.x.mk_node_list(&construct);
                let func = self.x.mk_node(Node::Apply { lambda, args });
                let (ty_param, ty_result) = self.sig(func)?;
                let lowered = arguments
                    .into_iter()
                    .map(|arg| self.expr(arg))
                    .collect::<LowerResult<Vec<Typed>>>()?;
                let (args_ty, args_val): (Vec<_>, Vec<_>) =
                    lowered.into_iter().map(|arg| (arg.ty, arg.val)).unzip();
                let ty_args = self.ty_tuple(&args_ty);
                self.expect_ty(ty_param, ty_args)?;
                let arg = self.instr_tuple(ty_args, &args_val).val;
                Ok(self.instr(ty_result, Expr::Call { func, arg }))
            }
            parse::Expr::Unary(op, inner) => {
                let v = self.expr(inner)?;
                match op {
                    Unop::Neg => Err(self.x.todo_no_loc()),
                    Unop::Not => Err(self.x.todo_no_loc()),
                }
            }
            parse::Expr::Binary(left, op, right) => {
                let l = self.expr(left)?;
                let r = self.expr(right)?;
                match op {
                    Binop::Eq => Err(self.x.todo_no_loc()),
                    Binop::Ne => Err(self.x.todo_no_loc()),
                    Binop::Lt => Err(self.x.todo_no_loc()),
                    Binop::Gt => Err(self.x.todo_no_loc()),
                    Binop::Le => Err(self.x.todo_no_loc()),
                    Binop::Ge => Err(self.x.todo_no_loc()),
                    Binop::Add => Err(self.x.todo_no_loc()),
                    Binop::Sub => Err(self.x.todo_no_loc()),
                    Binop::Mul => Err(self.x.todo_no_loc()),
                    Binop::Div => Err(self.x.todo_no_loc()),
                    Binop::Rem => Err(self.x.todo_no_loc()),
                    Binop::Shl => Err(self.x.todo_no_loc()),
                    Binop::Shr => Err(self.x.todo_no_loc()),
                    Binop::And => Err(self.x.todo_no_loc()),
                    Binop::Or => Err(self.x.todo_no_loc()),
                    Binop::Xor => Err(self.x.todo_no_loc()),
                }
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
                        parse::Binding::Single(bind) => {
                            self.x.bind(Level::ZERO, &self.slots, bind)?
                        }
                        parse::Binding::Composite(expr) => self.composite_bind(expr)?,
                    };
                    slots.push(slot);
                }
                let items = self.x.mk_node_list(&slots);
                let ty = self.x.mk_node(Node::Context);
                let ctx = self.x.mk_node(Node::List { items });
                Ok(self.instr(ty, Expr::Bind { ctx }))
            }
        }
    }

    /// Lower a composite binding `bind f[..]()` -- a binding whose value is the context produced by
    /// calling a context-returning function. The call's own `[..]` bindings, together with the
    /// ambient context, satisfy the callee's needs; the result is the callee applied to that
    /// construct, which reduces to the context the call yields and serves as one frame slot.
    fn composite_bind(&mut self, expr: ExprId) -> LowerResult<NodeId> {
        let parse::Expr::Call(callee, binds, args) = self.x.tree.exprs[expr] else {
            // Only call-shaped composite bindings are supported for now.
            return Err(self.x.todo_no_loc());
        };
        // A context-producing call takes no value arguments.
        assert!(args.into_iter().next().is_none());
        let lambda = match self.x.path(callee)? {
            Named::Fndef(fndef) => self.x.mk_node(Node::Fndef { def: fndef }),
            Named::Sigdef(sigdef) => self.extract_sig(sigdef)?,
            _ => return Err(LowerError::NotFn(callee.last)),
        };
        // Resolve the callee's needs against the ambient context plus the call's bindings.
        let mut destruct = self.slots.clone();
        for bind in binds {
            destruct.push(self.x.bind(Level::ZERO, &self.slots, bind)?);
        }
        let construct = self.x.invoke_force(lambda, &destruct)?;
        let args = self.x.mk_node_list(&construct);
        Ok(self.x.mk_node(Node::Apply { lambda, args }))
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
                    self.stmts(body.stmts)?;
                    let fake = self.emit(Instr::Br { depth: Depth(1) });
                    self.emit(Instr::EndIf { result: fake });
                    self.emit(Instr::EndLoop);
                }
                Stmt::Expr(expr) => {
                    self.expr(expr)?;
                }
            }
        }
        Ok(())
    }

    fn block(&mut self, block: Block) -> LowerResult<Typed> {
        self.stmts(block.stmts)?;
        match block.expr {
            // A trailing expression that is a bare local (or any already-emitted instruction)
            // yields an `InstrId` that is not the last one emitted, which would violate the body
            // invariant that the result is the final instruction. Materialize it with a `Copy`,
            // exactly as `let`/`var` do, so the body always ends with its result.
            Some(expr) => {
                let Typed { ty, val } = self.expr(expr)?;
                Ok(self.instr(ty, Expr::Copy { value: val }))
            }
            None => {
                let ty = self.x.ty_unit();
                let val = self.instr_tuple(ty, &[]).val;
                Ok(Typed { ty, val })
            }
        }
    }

    fn body(&mut self, fndef: parse::Fndef, id_decl: FndefId) -> LowerResult<Body> {
        let parse::Fndef {
            name: _,
            needs: _,
            params,
            result: _,
            def,
        } = fndef;
        let body = def.unwrap();
        let builder = self.x.builder();
        let (slots, tuple_ty, result_ty) = self.x.parse_sig(fndef).unwrap();
        self.slots = slots;
        let tuple_local = self.instr(tuple_ty, Expr::Param { ty: tuple_ty });
        let Node::Tuple { elems: types } = self.x.node(tuple_ty) else {
            unreachable!()
        };
        let mut index = 0;
        for (param, item) in params.into_iter().zip(types) {
            let ty = self.x.ir.lists[item];
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
        // A `bind <Ctx>` return must yield a context whose slots are in `<Ctx>`'s *ctxdef* order,
        // because every consumer (e.g. `main`'s `Get{Std, slot}`) picks `slot` by `explode`-ing the
        // ctxdef. The body's `bind` lists members in source/bind order, so re-assemble it in ctxdef
        // order via `extract_ctx` (which `explode`s the def and resolves each member against the
        // body's bindings plus the ambient context).
        let result_val = if let parse::Return::Bind(_) = fndef.result {
            self.reassemble_bind_return(result_ty, ret.val)?
        } else {
            ret.val
        };
        Ok(self.x.finish(builder, result_val))
    }

    /// Re-assemble a `bind <Ctx>` return value so its context is ctxdef-ordered. `result_ty` is the
    /// return-type lambda `parse_sig` produced (its result is a list of the annotation's needs);
    /// `body_result` is the body's `Expr::Bind { ctx }` instruction (bind order). Returns a fresh
    /// `Expr::Bind` whose context lists each annotated context's members in ctxdef order.
    fn reassemble_bind_return(
        &mut self,
        result_ty: NodeId,
        body_result: InstrId,
    ) -> LowerResult<InstrId> {
        let Node::Lambda {
            result: ret_list, ..
        } = self.x.node(result_ty)
        else {
            panic!("bind-return type is not a lambda")
        };
        let Node::List { items: ret_needs } = self.x.node(ret_list) else {
            panic!("bind-return type result is not a list")
        };
        // The body's bindings are the providers we resolve members against, alongside the ambient
        // context (`self.slots`); follow a `Copy` to reach the underlying `Bind`, as codegen does.
        let mut bind_instr = body_result;
        let ctx = loop {
            match self.x.ir.instrs[bind_instr] {
                Instr::Expr {
                    expr: Expr::Bind { ctx },
                    ..
                } => break ctx,
                Instr::Expr {
                    expr: Expr::Copy { value },
                    ..
                } => bind_instr = value,
                other => panic!("bind-return body result is not a `bind`: {other:?}"),
            }
        };
        let Node::List { items: binds } = self.x.node(ctx) else {
            panic!("bind ctx is not a list")
        };
        let bind_nodes: Vec<NodeId> = self.x.ir.lists[binds].to_vec();
        let mut providers = self.slots.clone();
        providers.extend(bind_nodes.iter().copied());
        // Assemble each annotated context in ctxdef order. (In practice a `bind <Ctx>` return names
        // exactly one composite context, e.g. `Std`.)
        let mut assembled = Vec::new();
        for need in self.x.ir.lists[ret_needs].to_vec() {
            match self.x.node(need) {
                Node::NeedCtxdef { def, .. } => {
                    // If a single body binding *forwards* this exact context -- e.g. `std`'s
                    // `bind bootstrap()`, where `bootstrap` returns `bind Std` -- its members can't
                    // be re-resolved member-by-member (they're sealed inside that call's returned
                    // context). Instead forward them positionally: the call's runtime value (via the
                    // backend's `eval_ctx_body`) is the members flattened in ctxdef order, so member
                    // `k` is `Get { ctx: <call>, slot: k }`.
                    let forwarded = self.x.forwarding_bind(&bind_nodes, def)?;
                    if let Some((call, count)) = forwarded {
                        for k in 0..count {
                            let id = SlotId::from_usize(k);
                            assembled.push(self.x.mk_node(Node::Get { ctx: call, slot: id }));
                        }
                        continue;
                    }
                    let lambda = self.x.extract_ctx(Level::ONE, &providers, def, &[], false)?;
                    // `extract_ctx` returns `Lambda { needs: [], result: List[members] }`; the
                    // returned context *is* those members, so splice them in directly.
                    let Node::Lambda { result, .. } = self.x.node(lambda) else {
                        unreachable!()
                    };
                    let Node::List { items } = self.x.node(result) else {
                        unreachable!()
                    };
                    assembled.extend(self.x.ir.lists[items].iter().copied());
                }
                // A non-context need in the return annotation provides itself (resolved against the
                // body's bindings if possible).
                _ => match self.x.resolve_need(need, &providers)? {
                    Some(provider) => assembled.push(provider),
                    None => assembled.push(need),
                },
            }
        }
        let items = self.x.mk_node_list(&assembled);
        let new_ctx = self.x.mk_node(Node::List { items });
        let ty = self.x.mk_node(Node::Context);
        Ok(self.instr(ty, Expr::Bind { ctx: new_ctx }).val)
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
        funcs: Vec::new(),
        attaches: Vec::new(),
        detaches: Vec::new(),
    };
    lower.program()?;
    Ok(module)
}

#[cfg(test)]
mod ctxbind_repro {
    use super::*;
    use crate::lex::lex;
    use crate::parse::parse;

    /// Lower a self-contained source with no prelude/base (`base: None`, no imports).
    fn lower_str(src: &str) -> LowerResult<(IR, Names, ModuleId)> {
        let (tokens, starts) = lex(src).unwrap();
        let tree = parse(&tokens).unwrap();
        let mut ir = IR::default();
        let mut names = Names::default();
        let preprelude = ir.modules.push(());
        let module = lower(src, &starts, &tree, &mut ir, &mut names, None, preprelude, &[])?;
        Ok((ir, names, module))
    }

    /// Isolated reproduction of the composite-context bridging bug that blocks the
    /// backend (`hello` panics at `wasm.rs:366`). A `bind <Ctx>` return that bridges
    /// one instance of a parametric composite context to another instance exercises
    /// `synthesize_bind` matching a `BindCtxdef` against a `NeedCtxdef`.
    ///
    /// IGNORED: currently fails. NOTE on fidelity: standalone, the RHS provider
    /// (`Inner[P=B]`) is an *abstract need*, so this fails during bind *construction*
    /// (`extract` -> `todo!`), whereas the prelude's `Numerals[Uint]=Numerals[I32]`
    /// has a *concrete* RHS (Wasi's `Numerals[I32]`) and instead fails later in
    /// `synthesize_bind` at `unify`. Both are the same root cause: a need and a bind
    /// at different nesting depths with free references to different enclosing
    /// contexts that `synthesize`'s identity `Renaming` seed does not relate. See
    /// `scratch/backend-synthesize-ctxbind-handoff3.md`.
    #[test]
    #[ignore = "composite-context bind/need synthesis (de Bruijn outer-correspondence) unimplemented"]
    fn composite_context_bind() {
        let src = "type A; type B; type P;\n\
val item[P]: P;\n\
context Inner[P] = item[P];\n\
context Outer = Inner[P=A];\n\
fn provide[Inner[P=B]](): bind Outer { bind Inner[P=A]=Inner[P=B] }\n";
        lower_str(src).unwrap();
    }
}
