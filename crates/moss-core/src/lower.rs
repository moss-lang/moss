use std::{
    backtrace::Backtrace,
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    mem::take,
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

    fn duplicate(&mut self, mapped: &mut InstrMap, instr: InstrId) {
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
            Instr::NeedTydef { def, param } => todo!(),
            Instr::NeedSigdef { def, param } => todo!(),
            Instr::NeedValdef { def, param } => todo!(),
            Instr::NeedCtxdef { def, param } => todo!(),
            Instr::Tagdef { def } => todo!(),
            Instr::Aliasdef { def } => todo!(),
            Instr::Tuple { elems } => todo!(),
            Instr::Record { fields } => todo!(),
            Instr::Context => todo!(),
            Instr::Fndef { def } => todo!(),
            Instr::Get { ctx, slot } => todo!(),
            Instr::Lit { val } => todo!(),
            Instr::Bind { args, bind } => {
                let args_mapped = self.map_items(mapped, args);
                self.emit(Instr::Bind {
                    args: args_mapped,
                    bind: mapped.get(bind),
                })
            }
            Instr::BindTydef { def, bind } => todo!(),
            Instr::BindSigdef { def, bind } => todo!(),
            Instr::BindValdef { def, bind } => todo!(),
            Instr::BindCtxdef { def, bind } => todo!(),
            Instr::Sig { param, result } => todo!(),
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
    }

    fn duplicate_range(&mut self, mapped: &mut InstrMap, instrs: IdRange<InstrId>) {
        for instr in instrs {
            self.duplicate(mapped, instr);
        }
    }

    /// Try to synthesize a mapping from the left lambda's domain to the right's.
    ///
    /// Both lambdas must currently be in scope.
    ///
    /// The synthesized mapping is not used; only its success or failure is returned as a boolean.
    fn left_domain_more_specific(&mut self, left: InstrId, right: InstrId) -> LowerResult<bool> {
        let start_dummy = self.emit(Instr::Lambda);
        let mut needs = Vec::new();
        let Instr::EndLambda { start, result: _ } = self.ir.instrs[right] else {
            panic!()
        };
        let mut mapped = InstrMap::new();
        let mut instr = start + 1;
        while instr < right {
            match self.ir.instrs[instr] {
                Instr::NeedTydef { def, param } => todo!(),
                Instr::NeedSigdef { def, param } => todo!(),
                Instr::NeedValdef { def, param } => todo!(),
                Instr::NeedCtxdef { def, param } => todo!(),
                _ => self.duplicate(&mut mapped, instr),
            }
            instr += 1;
        }
        let success = self.invoke(right, &needs)?.is_some();
        let items = self.items(&[]);
        let dummy = self.emit(Instr::Stack { items });
        self.emit(Instr::EndLambda {
            start: start_dummy,
            result: dummy,
        });
        Ok(success)
    }

    fn invoke(
        &mut self,
        lambda: InstrId,
        destruct: &[InstrId],
    ) -> LowerResult<Option<Vec<InstrId>>> {
        let start = match self.ir.instrs[lambda] {
            Instr::Lambda => todo!(),
            Instr::EndLambda { start, result: _ } => start,
            Instr::Apply { lambda, args } => todo!(),
            Instr::Stack { items } => todo!(),
            Instr::NeedTydef { def: _, param }
            | Instr::NeedSigdef { def: _, param }
            | Instr::NeedValdef { def: _, param }
            | Instr::NeedCtxdef { def: _, param } => return self.invoke(param, destruct),
            Instr::Tagdef { def } => todo!(),
            Instr::Aliasdef { def } => todo!(),
            Instr::Tuple { elems } => todo!(),
            Instr::Record { fields } => todo!(),
            Instr::Context => todo!(),
            Instr::Fndef { def } => todo!(),
            Instr::Get { ctx, slot } => todo!(),
            Instr::Lit { val } => todo!(),
            Instr::Bind { args, bind } => todo!(),
            Instr::BindTydef { def, bind } => todo!(),
            Instr::BindSigdef { def, bind } => todo!(),
            Instr::BindValdef { def, bind } => todo!(),
            Instr::BindCtxdef { def, bind } => todo!(),
            Instr::Sig { param, result } => todo!(),
            Instr::Set { lhs, rhs } => todo!(),
            Instr::If { ty, cond } => todo!(),
            Instr::Else { result } => todo!(),
            Instr::EndIf { result } => todo!(),
            Instr::Loop => todo!(),
            Instr::EndLoop => todo!(),
            Instr::Br { depth } => todo!(),
            Instr::Expr { ty, expr } => todo!(),
        };
        let mut construct = Vec::new();
        let mut mapped = InstrMap::new();
        let mut instr = start + 1;
        while instr < lambda {
            match self.ir.instrs[instr] {
                Instr::Lambda => {
                    let start = instr;
                    let end = self.find_end_lambda(instr);
                    let range = IdRange {
                        start,
                        end: end + 1,
                    };
                    self.duplicate_range(&mut mapped, range);
                    instr = end;
                }
                Instr::NeedTydef { def, param } => todo!(),
                Instr::NeedSigdef { def, param } => todo!(),
                Instr::NeedValdef { def, param } => todo!(),
                Instr::NeedCtxdef { def, param } => todo!(),
                _ => self.duplicate(&mut mapped, instr),
            }
            instr += 1;
        }
        Ok(Some(construct))
    }

    fn invoke_force(&mut self, lambda: InstrId, destruct: &[InstrId]) -> LowerResult<Vec<InstrId>> {
        Ok(self.invoke(lambda, destruct)?.unwrap()) // TODO: Return an actual error here.
    }

    fn invoke_need(
        &mut self,
        target: Body,
        destruct: &[InstrId],
    ) -> LowerResult<(Vec<InstrId>, Vec<InstrId>)> {
        let mut construct = Vec::new();
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
                    self.duplicate_range(&mut mapped, range);
                    instr = end;
                }
                Instr::NeedTydef { def, param } => {
                    let extracted = self.extract_ty_lambda(destruct, def, param)?;
                    construct.push(extracted); // TODO: I don't think this is quite right.
                }
                Instr::NeedSigdef { def, param } => todo!(),
                Instr::NeedValdef { def, param } => todo!(),
                Instr::NeedCtxdef { def, param } => todo!(),
                _ => self.duplicate(&mut mapped, instr),
            }
            instr += 1;
        }
        Ok((construct, Vec::new()))
    }

    fn extract_ty_lambda(
        &mut self,
        slots: &[InstrId],
        def: TydefId,
        lambda: InstrId,
    ) -> LowerResult<InstrId> {
        let mut options = Vec::new();
        for &slot in slots {
            match self.ir.instrs[slot] {
                Instr::Lambda => todo!(),
                Instr::EndLambda { start, result } => todo!(),
                Instr::Apply { lambda, args } => todo!(),
                Instr::Stack { items } => todo!(),
                Instr::NeedTydef { def: tydef, param } => {
                    if tydef != def {
                        continue;
                    }
                    if self.left_domain_more_specific(lambda, param)? {
                        options.push(slot);
                    }
                }
                Instr::NeedSigdef { def: _, param: _ } | Instr::NeedValdef { def: _, param: _ } => {
                }
                Instr::NeedCtxdef { def, param } => todo!(),
                Instr::Tagdef { def } => todo!(),
                Instr::Aliasdef { def } => todo!(),
                Instr::Tuple { elems } => todo!(),
                Instr::Record { fields } => todo!(),
                Instr::Context => todo!(),
                Instr::Fndef { def } => todo!(),
                Instr::Get { ctx, slot } => todo!(),
                Instr::Lit { val } => todo!(),
                Instr::Bind { args, bind } => todo!(),
                Instr::BindTydef { def: tydef, bind } => {
                    if tydef != def {
                        continue;
                    }
                    if self.left_domain_more_specific(lambda, bind)? {
                        options.push(slot);
                    }
                }
                Instr::BindSigdef { def, bind } => todo!(),
                Instr::BindValdef { def, bind } => todo!(),
                Instr::BindCtxdef { def, bind } => todo!(),
                Instr::Sig { param, result } => todo!(),
                Instr::Set { lhs, rhs } => todo!(),
                Instr::If { ty, cond } => todo!(),
                Instr::Else { result } => todo!(),
                Instr::EndIf { result } => todo!(),
                Instr::Loop => todo!(),
                Instr::EndLoop => todo!(),
                Instr::Br { depth } => todo!(),
                Instr::Expr { ty, expr } => todo!(),
            }
        }
        if options.len() != 1 {
            panic!()
        }
        Ok(options[0])
    }

    fn extract_ty(
        &mut self,
        slots: &[InstrId],
        def: TydefId,
        destruct: &[InstrId],
    ) -> LowerResult<InstrId> {
        let Tydef(target) = self.ir.tydefs[def];
        let start = self.emit(Instr::Lambda);
        let (construct, _) = self.invoke_need(target, destruct)?;
        let items = self.items(&construct);
        let result = self.emit(Instr::Stack { items });
        let lambda = self.emit(Instr::EndLambda { start, result });
        self.extract_ty_lambda(slots, def, lambda)
    }

    fn extract_sig(
        &mut self,
        slots: &[InstrId],
        def: SigdefId,
        destruct: &[InstrId],
    ) -> LowerResult<InstrId> {
        Err(self.todo_no_loc())
    }

    fn extract_val(
        &mut self,
        slots: &[InstrId],
        def: ValdefId,
        destruct: &[InstrId],
    ) -> LowerResult<InstrId> {
        Err(self.todo_no_loc())
    }

    fn extract_ctx(
        &mut self,
        slots: &[InstrId],
        def: CtxdefId,
        destruct: &[InstrId],
    ) -> LowerResult<InstrId> {
        Err(self.todo_no_loc())
    }

    fn inline(&mut self, body: Body, construct: &[InstrId]) -> LowerResult<InstrId> {
        Err(self.todo_no_loc())
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

    fn bind(&mut self, slots: &[InstrId], bind: parse::BindId) -> LowerResult<InstrId> {
        let parse::Bind { key, val } = self.tree.binds[bind];
        let (lhs, destruct_lhs) = self.spec(slots, key)?;
        match val {
            // TODO: Use the "extract" functions correctly in this branch.
            None => match lhs {
                Named::Tydef(def) => {
                    let Tydef(target) = self.ir.tydefs[def];
                    let start = self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(target, &destruct_lhs)?;
                    let args = self.items(&construct);
                    let bind = self.extract_ty(slots, def, &construct)?;
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { start, result });
                    Ok(self.emit(Instr::BindTydef { def, bind }))
                }
                Named::Sigdef(def) => {
                    let Sigdef(target) = self.ir.sigdefs[def];
                    let start = self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(target, &destruct_lhs)?;
                    let args = self.items(&construct);
                    let bind = self.extract_sig(slots, def, &construct)?;
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { start, result });
                    Ok(self.emit(Instr::BindSigdef { def, bind }))
                }
                Named::Valdef(def) => {
                    let Valdef(target) = self.ir.valdefs[def];
                    let start = self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(target, &destruct_lhs)?;
                    let bind = self.extract_val(slots, def, &construct)?;
                    let args = self.items(&construct);
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { start, result });
                    Ok(self.emit(Instr::BindValdef { def, bind }))
                }
                Named::Ctxdef(def) => {
                    let Ctxdef(target) = self.ir.ctxdefs[def];
                    let start = self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(target, &destruct_lhs)?;
                    let bind = self.extract_ctx(slots, def, &construct)?;
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
                    let (construct, _) = self.invoke_need(target, &destruct_lhs)?;
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
                            self.invoke_need(target_lhs, &destruct_lhs)?;
                        let args_lhs = self.items(&construct_lhs);
                        destruct_rhs.append(&mut needs);
                        let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
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
                            self.invoke_need(target_lhs, &destruct_lhs)?;
                        let args_lhs = self.items(&construct_lhs);
                        destruct_rhs.append(&mut needs);
                        let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
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
                            self.invoke_need(target_lhs, &destruct_lhs)?;
                        let args_lhs = self.items(&construct_lhs);
                        destruct_rhs.append(&mut needs);
                        let construct_rhs = self.invoke_force(lambda, &destruct_rhs)?;
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
        let parse::Bind { key, val } = self.tree.binds[bind];
        match val {
            Some(_) => self.bind(slots, bind),
            None => {
                let (lhs, destruct) = self.spec(slots, key)?;
                match lhs {
                    Named::Tydef(def) => {
                        let Tydef(target) = self.ir.tydefs[def];
                        let start = self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(target, &destruct)?;
                        let items = self.items(&construct);
                        let result = self.emit(Instr::Stack { items });
                        let param = self.emit(Instr::EndLambda { start, result });
                        Ok(self.emit(Instr::NeedTydef { def, param }))
                    }
                    Named::Sigdef(def) => {
                        let Sigdef(target) = self.ir.sigdefs[def];
                        let start = self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(target, &destruct)?;
                        let items = self.items(&construct);
                        let result = self.emit(Instr::Stack { items });
                        let param = self.emit(Instr::EndLambda { start, result });
                        Ok(self.emit(Instr::NeedSigdef { def, param }))
                    }
                    Named::Valdef(def) => {
                        let Valdef(target) = self.ir.valdefs[def];
                        let start = self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(target, &destruct)?;
                        let items = self.items(&construct);
                        let result = self.emit(Instr::Stack { items });
                        let param = self.emit(Instr::EndLambda { start, result });
                        Ok(self.emit(Instr::NeedValdef { def, param }))
                    }
                    Named::Ctxdef(def) => {
                        let Ctxdef(target) = self.ir.ctxdefs[def];
                        let start = self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(target, &destruct)?;
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

    fn needs(&mut self, needs: IdRange<parse::NeedId>) -> LowerResult<Vec<InstrId>> {
        let mut slots = Vec::new();
        for need in needs {
            slots.push(self.need(&slots, need)?);
        }
        Ok(slots)
    }

    fn parse_ty(&mut self, slots: &[InstrId], ty: parse::TypeId) -> LowerResult<InstrId> {
        match self.tree.types[ty] {
            parse::Type::Spec(spec) => {
                let (named, destruct) = self.spec(slots, spec)?;
                match named {
                    Named::Tydef(tydef) => {
                        let lambda = self.extract_ty(slots, tydef, &destruct)?;
                        let construct = self.invoke_force(lambda, &destruct)?;
                        let args = self.items(&construct);
                        Ok(self.emit(Instr::Apply { lambda, args }))
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
        let slots = self.needs(needs)?;
        let elems = (params.into_iter())
            .map(|arg| self.parse_ty(&slots, self.tree.params[arg].ty))
            .collect::<LowerResult<Vec<InstrId>>>()?;
        let param_tuple = self.ty_tuple(elems);
        let result_ty = match result {
            parse::Return::Unit => self.ty_unit(),
            parse::Return::Type(ty) => self.parse_ty(&slots, ty)?,
            parse::Return::Bind(needs) => {
                let slots = self.needs(needs)?;
                let items = self.items(&slots);
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
        let builder = self.builder();
        let (_, param_tuple, result_ty) = self.parse_sig(fndef)?;
        let signature = self.emit(Instr::Sig {
            param: param_tuple,
            result: result_ty,
        });
        let body = self.finish(builder, signature);
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
                    let builder = self.builder();
                    self.needs(needs)?;
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
                    let builder = self.builder();
                    let slots = self.needs(needs)?;
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
                    let builder = self.builder();
                    let slots = self.needs(needs)?;
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
                    let (fn_name, lowered) = self.parse_fndef(fndef)?;
                    if let NamedFn::Fndef(defined) = lowered {
                        self.detaches.push((id, defined));
                    }
                    self.names.detached.insert((self.module, fn_name), lowered);
                }
                parse::Decl::Valdef(id) => {
                    let parse::Valdef { name, needs, ty } = self.tree.valdefs[id];
                    let builder = self.builder();
                    let slots = self.needs(needs)?;
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
                    let builder = self.builder();
                    let mut slots = Vec::new();
                    for need in needs {
                        slots.push(self.need(&slots, need)?);
                    }
                    let start = self.emit(Instr::Lambda);
                    for need in def {
                        slots.push(self.need(&slots, need)?);
                    }
                    let items = self.items(&slots[needs.len()..]);
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
        self.bodies()?;
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
    fn nominal(&self, ty: InstrId) -> Option<TagdefId> {
        todo!()
    }

    /// Get the fields of a record type.
    fn fields(&self, ty: InstrId) -> LowerResult<Vec<(StrId, InstrId)>> {
        Err(self.x.todo_no_loc())
    }

    /// Get the parameter type and result type of a function.
    fn sig(&self, func: InstrId) -> (InstrId, InstrId) {
        todo!()
    }

    /// Resolve a method call.
    fn method(&self, ty: InstrId, name: StrId) -> Option<NamedFn> {
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

    fn expect_ty(&self, expected: InstrId, actual: InstrId) -> LowerResult<()> {
        Err(self.x.todo_no_loc())
    }

    fn expr(&mut self, expr: ExprId) -> LowerResult<Typed> {
        match self.x.tree.exprs[expr] {
            parse::Expr::Lit(token) => {
                let Base {
                    types,
                    lit_types,
                    lit_vals,
                    lits,
                } = self.base();
                let (tydef_lit, tydef, valdef, sigdef, val) = match self.x.lit(token)? {
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

                let Tydef(body_tydef_lit) = self.x.ir.tydefs[tydef_lit];
                let Tydef(body_tydef) = self.x.ir.tydefs[tydef];
                let Valdef(body_valdef) = self.x.ir.valdefs[valdef];
                let Sigdef(body_sigdef) = self.x.ir.sigdefs[sigdef];

                let lambda_ty_lit = self.extract_ty(tydef_lit)?;
                let construct_ty_lit = self.invoke_force(lambda_ty_lit)?;

                let lambda_ty = self.extract_ty(tydef)?;
                let construct_ty = self.invoke_force(lambda_ty)?;

                let val_lit = self.emit(Instr::Lit { val });

                // TODO: Use "invoke" correctly here.
                let lambda_func = self.extract_sig(sigdef)?;
                let construct_func = self.invoke_force(lambda_func)?;

                let items = self.x.items(&construct_func);
                let func = self.emit(Instr::Apply {
                    lambda: lambda_func,
                    args: items,
                });
                let ty_unit = self.x.ty_unit();
                let arg = self.instr_tuple(ty_unit, &[]).val;
                Ok(self.instr(lambda_ty, Expr::Call { func, arg }))
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
                let obj = self.expr(object)?;
                let name = self.x.name(method);
                // TODO: Contextually set `this` to `obj`.
                let lambda = match self.method(obj.ty, name) {
                    Some(NamedFn::Sigdef(sigdef)) => self.extract_sig(sigdef)?,
                    Some(NamedFn::Fndef(fndef)) => self.emit(Instr::Fndef { def: fndef }),
                    None => return Err(LowerError::Undefined(method)),
                };
                let construct = self.invoke_force(lambda)?;
                let args = self.x.items(&construct);
                let func = self.emit(Instr::Apply { lambda, args });
                let (ty_param, ty_result) = self.sig(func);
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
                    Named::Fndef(fndef) => self.emit(Instr::Fndef { def: fndef }),
                    _ => return Err(LowerError::NotFn(callee.last)),
                };
                let construct = self.invoke_force(lambda)?;
                let args = self.x.items(&construct);
                let func = self.emit(Instr::Apply { lambda, args });
                let (ty_param, ty_result) = self.sig(func);
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
            Some(expr) => Ok(self.expr(expr)?),
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
