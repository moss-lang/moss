use std::{
    backtrace::Backtrace,
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    mem::take,
};

use index_vec::{IndexSlice, IndexVec, define_index_type};

use crate::{
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
        /// The nominal type definition.
        def: TagdefId,

        /// Statics for the input slots of the parameter context for the nominal type.
        params: InstrList,

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

        /// Statics for the input slots of the parameter context for the function.
        params: InstrList,

        /// The runtime argument value.
        arg: InstrId,
    },

    /// Call a non-contextual function.
    ///
    /// Type: the function's result type.
    CallDirect {
        /// The function.
        func: FndefId,

        /// Statics for the input slots of the parameter context for the function.
        params: InstrList,

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

    fn result(self) -> InstrId {
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
pub struct Tydef(Body);

/// A lambda that takes contextual parameters and returns the inner type for this nominal type.
#[derive(Clone, Copy, Debug)]
pub struct Tagdef(Body);

/// A lambda that takes contextual parameters and returns a type.
#[derive(Clone, Copy, Debug)]
pub struct Aliasdef(Body);

/// A lambda that takes contextual parameters and returns a function signature.
#[derive(Clone, Copy, Debug)]
pub struct Sigdef(Body);

/// A lambda that takes contextual parameters and returns a type.
#[derive(Clone, Copy, Debug)]
pub struct Valdef(Body);

/// A lambda that returns a lambda that returns a context.
#[derive(Clone, Copy, Debug)]
pub struct Ctxdef(Body);

#[derive(Debug, Default)]
pub struct IR {
    pub modules: IndexVec<ModuleId, ()>,
    pub strings: Strings,
    pub instrs: IndexVec<InstrId, Instr>,
    pub items: IndexVec<ItemId, InstrId>,
    pub records: IndexVec<RecordId, (StrId, InstrId)>,
    pub tydefs: IndexVec<TydefId, Tydef>,
    pub tagdefs: IndexVec<TagdefId, Tagdef>,
    pub aliasdefs: IndexVec<AliasdefId, Aliasdef>,
    pub sigdefs: IndexVec<SigdefId, Sigdef>,
    pub fndefs: IndexVec<FndefId, Sigdef>,
    pub valdefs: IndexVec<ValdefId, Valdef>,
    pub ctxdefs: IndexVec<CtxdefId, Ctxdef>,
    pub bodies: IndexVec<FndefId, Body>,
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
    pub names: HashMap<(ModuleId, StrId), Named>,
    pub attached: HashMap<(TagdefId, StrId), NamedFn>,
    pub detached: HashMap<(ModuleId, StrId), NamedFn>,
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

    fn invoke(&mut self, destruct: &[InstrId], target: InstrId) -> LowerResult<Vec<InstrId>> {
        todo!()
    }

    fn invoke_need(
        &mut self,
        destruct: &[InstrId],
        target: Body,
    ) -> LowerResult<(Vec<InstrId>, Vec<InstrId>)> {
        todo!()
    }

    fn extract_ty(
        &mut self,
        destruct: &[InstrId],
        def: TydefId,
        construct: &[InstrId],
    ) -> LowerResult<InstrId> {
        todo!()
    }

    fn extract_sig(
        &mut self,
        destruct: &[InstrId],
        def: SigdefId,
        construct: &[InstrId],
    ) -> LowerResult<InstrId> {
        todo!()
    }

    fn extract_val(
        &mut self,
        destruct: &[InstrId],
        def: ValdefId,
        construct: &[InstrId],
    ) -> LowerResult<InstrId> {
        todo!()
    }

    fn extract_ctx(
        &mut self,
        destruct: &[InstrId],
        def: CtxdefId,
        construct: &[InstrId],
    ) -> LowerResult<InstrId> {
        todo!()
    }

    fn inline(
        &mut self,
        param: Body,
        ctx: Body,
        construct: &[InstrId],
        body: Body,
    ) -> LowerResult<InstrId> {
        todo!()
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
            .map(|bind| self.bind(&destruct, bind))
            .collect::<LowerResult<Vec<InstrId>>>()?;
        Ok((named, construct))
    }

    fn bind(&mut self, slots: &[InstrId], bind: parse::BindId) -> LowerResult<InstrId> {
        let parse::Bind { key, val } = self.tree.binds[bind];
        let (lhs, destruct_lhs) = self.spec(slots, key)?;
        match val {
            None => match lhs {
                Named::Tydef(def) => {
                    let Tydef(target) = self.ir.tydefs[def];
                    self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(&destruct_lhs, target)?;
                    let args = self.items(&construct);
                    let bind = self.extract_ty(&slots, def, &construct)?;
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { result });
                    Ok(self.emit(Instr::BindTydef { def, bind }))
                }
                Named::Sigdef(def) => {
                    let Sigdef(target) = self.ir.sigdefs[def];
                    self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(&destruct_lhs, target)?;
                    let args = self.items(&construct);
                    let bind = self.extract_sig(&slots, def, &construct)?;
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { result });
                    Ok(self.emit(Instr::BindSigdef { def, bind }))
                }
                Named::Valdef(def) => {
                    let Valdef(target) = self.ir.valdefs[def];
                    self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(&destruct_lhs, target)?;
                    let bind = self.extract_val(&slots, def, &construct)?;
                    let args = self.items(&construct);
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { result });
                    Ok(self.emit(Instr::BindValdef { def, bind }))
                }
                Named::Ctxdef(def) => {
                    let Ctxdef(target) = self.ir.ctxdefs[def];
                    self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(&destruct_lhs, target)?;
                    let bind = self.extract_ctx(&slots, def, &construct)?;
                    let args = self.items(&construct);
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { result });
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
                    self.emit(Instr::Lambda);
                    let (construct, _) = self.invoke_need(&destruct_lhs, target)?;
                    let (val, _) = self.lit(token)?;
                    let bind = self.emit(Instr::Lit { val });
                    let args = self.items(&construct);
                    let result = self.emit(Instr::Bind { args, bind });
                    let bind = self.emit(Instr::EndLambda { result });
                    Ok(self.emit(Instr::BindValdef { def, bind }))
                }
                _ => Err(LowerError::LitNotVal(token)),
            },
            Some(parse::Entry::Ref(spec)) => {
                let (rhs, destruct_rhs) = self.spec(slots, spec)?;
                match (lhs, rhs) {
                    (Named::Tydef(def), Named::Tydef(tydef)) => {
                        let Tydef { ctx: target_lhs } = self.ir.tydefs[def];
                        let Tydef { ctx: target_rhs } = self.ir.tydefs[tydef];
                        let construct_lhs = self.invoke_open(param, &destruct_lhs, target_lhs)?;
                        let construct_rhs = self.invoke_bind(
                            param,
                            &construct_lhs,
                            target_lhs,
                            &destruct_rhs,
                            target_rhs,
                        )?;
                        let bind = self.extract_ty(param, &slots, tydef, &construct_rhs)?;
                        let params = self.items(&construct_lhs);
                        let args = self.items(&construct_rhs);
                        Ok(self.emit(Instr::BindTydef {
                            def,
                            params,
                            bind,
                            args,
                        }))
                    }
                    (Named::Tydef(def), Named::Tagdef(tagdef)) => {
                        let Tydef { ctx: target_lhs } = self.ir.tydefs[def];
                        let Tagdef {
                            ctx: target_rhs,
                            inner: _,
                        } = self.ir.tagdefs[tagdef];
                        let construct_lhs = self.invoke_open(param, &destruct_lhs, target_lhs)?;
                        let construct_rhs = self.invoke_bind(
                            &construct_lhs,
                            target_lhs,
                            &destruct_rhs,
                            target_rhs,
                        )?;
                        let bind = tagdef;
                        let params = self.items(&construct_lhs);
                        let args = self.items(&construct_rhs);
                        Ok(self.emit(Instr::BindTagdef {
                            def,
                            params,
                            bind,
                            args,
                        }))
                    }
                    (Named::Tydef(def), Named::Aliasdef(aliasdef)) => {
                        let Tydef { ctx: target_lhs } = self.ir.tydefs[def];
                        let Aliasdef {
                            ctx: target_rhs,
                            def: _,
                        } = self.ir.aliasdefs[aliasdef];
                        let construct_lhs = self.invoke_open(param, &destruct_lhs, target_lhs)?;
                        let construct_rhs = self.invoke_bind(
                            param,
                            &construct_lhs,
                            target_lhs,
                            &destruct_rhs,
                            target_rhs,
                        )?;
                        let bind = aliasdef;
                        let params = self.items(&construct_lhs);
                        let args = self.items(&construct_rhs);
                        Ok(self.emit(Instr::BindAliasdef {
                            def,
                            params,
                            bind,
                            args,
                        }))
                    }
                    (Named::Sigdef(def), Named::Sigdef(sigdef)) => {
                        let Sigdef {
                            ctx: target_lhs,
                            sig: _,
                        } = self.ir.sigdefs[def];
                        let Sigdef {
                            ctx: target_rhs,
                            sig: _,
                        } = self.ir.sigdefs[sigdef];
                        // TODO: Check compatibility of function signatures.
                        let construct_lhs = self.invoke_open(param, &destruct_lhs, target_lhs)?;
                        let construct_rhs = self.invoke_bind(
                            param,
                            &construct_lhs,
                            target_lhs,
                            &destruct_rhs,
                            target_rhs,
                        )?;
                        let bind = self.extract_sig(param, &slots, sigdef, &construct_rhs)?;
                        let params = self.items(&construct_lhs);
                        let args = self.items(&construct_rhs);
                        Ok(self.emit(Instr::BindSigdef {
                            def,
                            params,
                            bind,
                            args,
                        }))
                    }
                    (Named::Sigdef(def), Named::Fndef(fndef)) => {
                        let Sigdef {
                            ctx: target_lhs,
                            sig: _,
                        } = self.ir.sigdefs[def];
                        let Sigdef {
                            ctx: target_rhs,
                            sig: _,
                        } = self.ir.fndefs[fndef];
                        // TODO: Check compatibility of function signatures.
                        let construct_lhs = self.invoke_open(param, &destruct_lhs, target_lhs)?;
                        let construct_rhs = self.invoke_bind(
                            param,
                            &construct_lhs,
                            target_lhs,
                            &destruct_rhs,
                            target_rhs,
                        )?;
                        let bind = fndef;
                        let params = self.items(&construct_lhs);
                        let args = self.items(&construct_rhs);
                        Ok(self.emit(Instr::BindFndef {
                            def,
                            params,
                            bind,
                            args,
                        }))
                    }
                    (Named::Valdef(def), Named::Valdef(valdef)) => {
                        let Valdef {
                            ctx: target_lhs,
                            ty: _,
                        } = self.ir.valdefs[def];
                        let Valdef {
                            ctx: target_rhs,
                            ty: _,
                        } = self.ir.valdefs[valdef];
                        // TODO: Check compatibility of value types.
                        let construct_lhs = self.invoke_open(param, &destruct_lhs, target_lhs)?;
                        let construct_rhs = self.invoke_bind(
                            param,
                            &construct_lhs,
                            target_lhs,
                            &destruct_rhs,
                            target_rhs,
                        )?;
                        let bind = self.extract_val(param, &slots, valdef, &construct_rhs)?;
                        let params = self.items(&construct_lhs);
                        let args = self.items(&construct_rhs);
                        Ok(self.emit(Instr::BindValdef {
                            def,
                            params,
                            bind,
                            args,
                        }))
                    }
                    (Named::Ctxdef(_), _) => Err(LowerError::BindContext(bind)),
                    (Named::Module(_), _) => Err(LowerError::BindModule(bind)),
                    (Named::Tagdef(_), _) => Err(LowerError::BindNominal(bind)),
                    (Named::Aliasdef(_), _) => Err(LowerError::BindAlias(bind)),
                    (Named::Fndef(_), _) => Err(LowerError::BindDefined(bind)),
                    _ => Err(LowerError::BindMismatch(bind)),
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
                        self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(&destruct, target)?;
                        let items = self.items(&construct);
                        let result = self.emit(Instr::Stack { items });
                        let param = self.emit(Instr::EndLambda { result });
                        Ok(self.emit(Instr::NeedTydef { def, param }))
                    }
                    Named::Sigdef(def) => {
                        let Sigdef(target) = self.ir.sigdefs[def];
                        self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(&destruct, target)?;
                        let items = self.items(&construct);
                        let result = self.emit(Instr::Stack { items });
                        let param = self.emit(Instr::EndLambda { result });
                        Ok(self.emit(Instr::NeedSigdef { def, param }))
                    }
                    Named::Valdef(def) => {
                        let Valdef(target) = self.ir.valdefs[def];
                        self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(&destruct, target)?;
                        let items = self.items(&construct);
                        let result = self.emit(Instr::Stack { items });
                        let param = self.emit(Instr::EndLambda { result });
                        Ok(self.emit(Instr::NeedValdef { def, param }))
                    }
                    Named::Ctxdef(def) => {
                        let Ctxdef(target) = self.ir.ctxdefs[def];
                        self.emit(Instr::Lambda);
                        let (construct, _) = self.invoke_need(&destruct, target)?;
                        let items = self.items(&construct);
                        let result = self.emit(Instr::Stack { items });
                        let param = self.emit(Instr::EndLambda { result });
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

    fn needs_raw(&mut self, needs: IdRange<parse::NeedId>) -> LowerResult<Vec<InstrId>> {
        let mut slots = Vec::new();
        for need in needs {
            slots.push(self.need(&slots, need)?);
        }
        Ok(slots)
    }

    fn needs(&mut self, needs: IdRange<parse::NeedId>) -> LowerResult<Body> {
        let builder = self.builder();
        let slots = self.needs_raw(needs)?;
        let items = self.items(&slots);
        let ctx = self.emit(Instr::Stack { items });
        Ok(self.finish(builder, ctx))
    }

    fn parse_ty(&mut self, slots: &[InstrId], ty: parse::TypeId) -> LowerResult<InstrId> {
        match self.tree.types[ty] {
            parse::Type::Spec(spec) => {
                let (named, destruct) = self.spec(slots, spec)?;
                match named {
                    Named::Tydef(tydef) => {
                        let Tydef(target) = self.ir.tydefs[tydef];
                        let construct = self.invoke(&destruct, target)?;
                        self.extract_ty(slots, tydef, &construct)
                    }
                    Named::Tagdef(tagdef) => todo!(),
                    Named::Aliasdef(aliasdef) => todo!(),
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
            parse::Type::Record(members) => todo!(),
        }
    }

    fn parse_fndef(&mut self, fndef: parse::Fndef) -> LowerResult<(StrId, NamedFn)> {
        let parse::Fndef {
            name,
            needs,
            params,
            result,
            def,
        } = fndef;
        let builder = self.builder();
        let slots = self.needs_raw(needs)?;
        let elems = (params.into_iter())
            .map(|arg| self.parse_ty(&slots, self.tree.params[arg].ty))
            .collect::<LowerResult<Vec<InstrId>>>()?;
        let param_tuple = self.ty_tuple(elems);
        let result_ty = match result {
            parse::Return::Unit => self.ty_unit(),
            parse::Return::Type(ty) => self.parse_ty(&slots, ty)?,
            parse::Return::Bind(needs) => {
                let slots = self.needs_raw(needs)?;
                let items = self.items(&slots);
                self.emit(Instr::Stack { items })
            }
        };
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
                    let body = self.needs(needs)?;
                    let lowered = self.ir.tydefs.push(Tydef(body));
                    let string = self.name(name);
                    self.names
                        .names
                        .insert((self.module, string), Named::Tydef(lowered));
                }
                parse::Decl::Tagdef(id) => {
                    let parse::Tagdef { name, needs, def } = self.tree.tagdefs[id];
                    let builder = self.builder();
                    let slots = self.needs_raw(needs)?;
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
                    let slots = self.needs_raw(needs)?;
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
                    let slots = self.needs_raw(needs)?;
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
                    let ctx = self.needs(empty, needs)?;
                    let def = self.needs(ctx, def)?;
                    let lowered = self.ir.ctxdefs.push(Ctxdef { ctx, def });
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
            let ctx = self.ir.fndefs[id_decl].ctx;
            let parse::Funcdef { fndef } = self.tree.funcdefs[funcdef];
            let body = LowerBody {
                x: self,
                ctx,
                slots: Vec::new(),
                locals: HashMap::new(),
            }
            .body(fndef, id_decl)?;
            let id_body = self.ir.bodies.push(body);
            assert_eq!(id_decl, id_body);
        }
        for (attachdef, _tagdef, id_decl) in take(&mut self.attaches) {
            let ctx = self.ir.fndefs[id_decl].ctx;
            // TODO: Handle `this`.
            let parse::Attachdef { ty: _, fndef } = self.tree.attachdefs[attachdef];
            let body = LowerBody {
                x: self,
                ctx,
                slots: Vec::new(),
                locals: HashMap::new(),
            }
            .body(fndef, id_decl)?;
            let id_body = self.ir.bodies.push(body);
            assert_eq!(id_decl, id_body);
        }
        for (detachdef, id_decl) in take(&mut self.detaches) {
            let ctx = self.ir.fndefs[id_decl].ctx;
            // TODO: Handle `this`.
            let parse::Detachdef { fndef } = self.tree.detachdefs[detachdef];
            let body = LowerBody {
                x: self,
                ctx,
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
    ctx: Body,
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

    fn invoke(&mut self, destruct: &[InstrId], target: Body) -> LowerResult<Vec<InstrId>> {
        self.x.invoke(self.ctx, destruct, target)
    }

    fn invoke_self_slots(&mut self, target: Body) -> LowerResult<Vec<InstrId>> {
        self.x.invoke(self.ctx, &self.slots, target)
    }

    fn extract_ty(&mut self, def: TydefId, construct: &[InstrId]) -> LowerResult<InstrId> {
        self.x.extract_ty(self.ctx, &self.slots, def, construct)
    }

    fn extract_sig(&mut self, def: SigdefId, construct: &[InstrId]) -> LowerResult<InstrId> {
        self.x.extract_sig(self.ctx, &self.slots, def, construct)
    }

    fn extract_val(&mut self, def: ValdefId, construct: &[InstrId]) -> LowerResult<InstrId> {
        self.x.extract_val(self.ctx, &self.slots, def, construct)
    }

    fn extract_ctx(&mut self, def: CtxdefId, construct: &[InstrId]) -> LowerResult<InstrId> {
        self.x.extract_ctx(self.ctx, &self.slots, def, construct)
    }

    fn inline(&mut self, ctx: Body, construct: &[InstrId], body: Body) -> LowerResult<InstrId> {
        self.x.inline(self.ctx, ctx, construct, body)
    }

    /// Get the nominal type definition of a value, or [`None`] if the value is not nominally typed.
    fn nominal(&self, ty: InstrId) -> Option<TagdefId> {
        todo!()
    }

    /// Get the fields of a record type.
    fn fields(&self, ty: InstrId) -> LowerResult<Vec<(StrId, InstrId)>> {
        todo!()
    }

    /// Get the parameter type and result type of a function.
    fn sig(&self, def: SigdefId, construct: &[InstrId]) -> (InstrId, InstrId) {
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
        todo!()
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

                let Tydef { ctx: ctx_tydef_lit } = self.x.ir.tydefs[tydef_lit];
                let Tydef { ctx: ctx_tydef } = self.x.ir.tydefs[tydef];
                let Valdef {
                    ctx: ctx_valdef,
                    ty: _,
                } = self.x.ir.valdefs[valdef];
                let Sigdef {
                    ctx: ctx_sigdef,
                    sig: _,
                } = self.x.ir.sigdefs[sigdef];

                let construct_ty_lit = self.invoke(&[], ctx_tydef_lit)?;
                let ty_lit = self.extract_ty(tydef_lit, &construct_ty_lit)?;

                let construct_ty = self.invoke(&[], ctx_tydef)?;
                let ty = self.extract_ty(tydef, &construct_ty)?;

                let construct_val = self.invoke(&[ty_lit], ctx_valdef)?;
                let params_val = self.x.items(&construct_val);
                let val_lit = self.emit(Instr::BindLit {
                    def: valdef,
                    params: params_val,
                    bind: val,
                });

                let construct_func = self.invoke(&[ty_lit, ty, val_lit], ctx_sigdef)?;
                let func = self.extract_sig(sigdef, &construct_func)?;

                let params = self.x.items(&construct_func);
                let ty_unit = self.x.ty_unit();
                let arg = self.instr_tuple(ty_unit, &[]).val;
                Ok(self.instr(ty, Expr::Call { func, params, arg }))
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
                let Valdef { ctx, ty } = self.x.ir.valdefs[valdef];
                let construct = self.invoke_self_slots(ctx)?;
                let val = self.extract_val(valdef, &construct)?;
                let ty_inlined = self.inline(ctx, &construct, ty)?;
                Ok(self.instr(ty_inlined, Expr::Val { val }))
            }
            parse::Expr::Tag(path, inner) => {
                let Named::Tagdef(tagdef) = self.x.path(path)? else {
                    return Err(LowerError::NotNominal(path.last));
                };
                let inside = self.expr(inner)?;
                let Tagdef { ctx, inner } = self.x.ir.tagdefs[tagdef];
                let construct = self.invoke_self_slots(ctx)?;
                let params = self.x.items(&construct);
                let ty = self.emit(Instr::Tagdef {
                    def: tagdef,
                    params,
                });
                self.expect_ty(ty, inside.ty)?; // TODO: Check the inner type, not the outer type.
                Ok(self.instr(
                    ty,
                    Expr::Nominal {
                        def: tagdef,
                        params,
                        inner: inside.val,
                    },
                ))
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
            parse::Expr::Method(object, method, args) => {
                let obj = self.expr(object)?;
                let name = self.x.name(method);
                let Some(NamedFn::Sigdef(sigdef)) = self.method(obj.ty, name) else {
                    // TODO: Process direct calls too.
                    return Err(LowerError::Undefined(method));
                };
                let Sigdef { ctx, sig } = self.x.ir.sigdefs[sigdef];
                let construct = self.invoke_self_slots(ctx)?;
                let (ty_param, ty_result) = self.sig(sigdef, &construct);
                let lowered = args
                    .into_iter()
                    .map(|arg| self.expr(arg))
                    .collect::<LowerResult<Vec<Typed>>>()?;
                let (args_ty, args_val): (Vec<_>, Vec<_>) =
                    lowered.into_iter().map(|arg| (arg.ty, arg.val)).unzip();
                let ty_args = self.ty_tuple(&args_ty);
                self.expect_ty(ty_param, ty_args)?;
                let arg = self.instr_tuple(ty_args, &args_val).val;
                // TODO: Contextually set `this` to `obj`.
                let func = self.extract_sig(sigdef, &construct)?;
                let params = self.x.items(&construct);
                Ok(self.instr(ty_result, Expr::Call { func, params, arg }))
            }
            parse::Expr::Call(callee, binds, args) => {
                let Named::Sigdef(sigdef) = self.x.path(callee)? else {
                    // TODO: Process direct calls too.
                    return Err(LowerError::NotFn(callee.last));
                };
                let Sigdef { ctx, sig } = self.x.ir.sigdefs[sigdef];
                // TODO: Handle bindings attached to function calls.
                let construct = self.invoke_self_slots(ctx)?;
                let (ty_param, ty_result) = self.sig(sigdef, &construct);
                let lowered = args
                    .into_iter()
                    .map(|arg| self.expr(arg))
                    .collect::<LowerResult<Vec<Typed>>>()?;
                let (args_ty, args_val): (Vec<_>, Vec<_>) =
                    lowered.into_iter().map(|arg| (arg.ty, arg.val)).unzip();
                let ty_args = self.ty_tuple(&args_ty);
                self.expect_ty(ty_param, ty_args)?;
                let arg = self.instr_tuple(ty_args, &args_val).val;
                let func = self.extract_sig(sigdef, &construct)?;
                let params = self.x.items(&construct);
                Ok(self.instr(ty_result, Expr::Call { func, params, arg }))
            }
            parse::Expr::Unary(op, inner) => {
                let v = self.expr(inner)?;
                match op {
                    Unop::Neg => todo!(),
                    Unop::Not => todo!(),
                }
            }
            parse::Expr::Binary(left, op, right) => {
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
                        parse::Binding::Single(bind) => self.x.bind(self.ctx, &self.slots, bind)?,
                        parse::Binding::Composite(expr) => self.expr(expr)?.val,
                    };
                    slots.push(slot);
                }
                let params = self.x.items(&slots);
                let ty = self.emit(Instr::Context);
                let val = self.emit(Instr::Ctx { slots });
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
        let Some(body) = def else { unreachable!() };
        let builder = self.x.builder();
        let Sigdef { ctx: _, sig } = self.x.ir.fndefs[id_decl];
        let Instr::Sig {
            param: tuple_ty,
            result: _,
        } = self.x.ir.instrs[sig.result()]
        else {
            unreachable!()
        };
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
