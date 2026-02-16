use std::{
    backtrace::Backtrace,
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    mem::take,
    ops::Index,
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

pub type InstrList = IdRange<ItemId>;

#[derive(Clone, Copy, Debug)]
pub struct Depth(pub u32);

/// An instruction that produces a runtime value of some type.
#[derive(Clone, Copy, Debug)]
pub enum Expr {
    /// Get the value of the parameter to this function.
    ///
    /// Type: this function's parameter type.
    Param,

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
    /// The parameter context.
    Param,

    /// An open slot.
    Open,

    /// A new context providing some number of slots.
    Ctx {
        /// Statics used to construct the output slots of the new context.
        slots: InstrList,
    },

    /// Need a contextual type parametrized by a specific context.
    NeedTydef {
        /// The contextual type declaration.
        def: TydefId,

        /// Statics destructured to satisfy the input slots of the parameter context.
        params: InstrList,
    },

    /// Need a contextual function parametrized by a specific context.
    NeedFndef {
        /// The contextual function declaration.
        def: FndefId,

        /// Statics destructured to satisfy the input slots of the parameter context.
        params: InstrList,
    },

    /// Need a contextual value parametrized by a specific context.
    NeedValdef {
        /// The contextual value declaration.
        def: ValdefId,

        /// Statics destructured to satisfy the input slots of the parameter context.
        params: InstrList,
    },

    /// Need a composite context parametrized by a specific context.
    NeedCtxdef {
        /// The context definition.
        def: CtxdefId,

        /// Statics destructured to satisfy the input slots of the parameter context.
        params: InstrList,
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

    /// A slot from a context.
    Get {
        /// The context.
        ctx: InstrId,

        /// The output slot index.
        slot: SlotId,
    },

    /// Provide a contextual type parametrized by a specific context.
    BindTydef {
        /// The contextual type declaration.
        def: TydefId,

        /// Statics for the input slots of the left-hand parameter context.
        params: InstrList,

        /// The type being provided.
        bind: InstrId,

        /// Statics for the input slots of the right-hand parameter context.
        args: InstrList,
    },

    /// Provide a nominal type for a contextual type parametrized by a specific context.
    BindTagdef {
        /// The contextual type declaration.
        def: TydefId,

        /// Statics for the input slots of the left-hand parameter context.
        params: InstrList,

        /// The nominal type being provided.
        bind: TagdefId,

        /// Statics for the input slots of the right-hand parameter context.
        args: InstrList,
    },

    /// Provide a type alias for a contextual type parametrized by a specific context.
    BindAliasdef {
        /// The contextual type declaration.
        def: TydefId,

        /// Statics for the input slots of the left-hand parameter context.
        params: InstrList,

        /// The type alias being provided.
        bind: AliasdefId,

        /// Statics for the input slots of the right-hand parameter context.
        args: InstrList,
    },

    /// Provide a contextual function parametrized by a specific context.
    BindFndef {
        /// The contextual function declaration.
        def: FndefId,

        /// Statics for the input slots of the left-hand parameter context.
        params: InstrList,

        /// The function being provided.
        bind: InstrId,

        /// Statics for the input slots of the right-hand parameter context.
        args: InstrList,
    },

    /// Provide a contextual value parametrized by a specific context.
    BindValdef {
        /// The contextual value declaration.
        def: ValdefId,

        /// Statics for the input slots of the left-hand parameter context.
        params: InstrList,

        /// The value being provided.
        bind: InstrId,

        /// Statics for the input slots of the right-hand parameter context.
        args: InstrList,
    },

    /// Provide a literal value for a contextual value parametrized by a specific context.
    BindLit {
        /// The contextual value declaration.
        def: ValdefId,

        /// Statics for the input slots of the left-hand parameter context.
        params: InstrList,

        /// The literal value being provided.
        bind: Val,
    },

    /// Provide a composite context parametrized by a specific context.
    BindCtxdef {
        /// The composite context definition.
        def: CtxdefId,

        /// Statics for the input slots of the left-hand parameter context.
        params: InstrList,

        /// The context being provided.
        bind: InstrId,

        /// Statics for the input slots of the right-hand parameter context.
        args: InstrList,
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

#[derive(Clone, Copy, Debug)]
pub struct Tydef {
    pub ctx: Body,
}

#[derive(Clone, Copy, Debug)]
pub struct Tagdef {
    pub ctx: Body,
    pub inner: Body,
}

#[derive(Clone, Copy, Debug)]
pub struct Aliasdef {
    pub ctx: Body,
    pub def: Body,
}

#[derive(Clone, Copy, Debug)]
pub struct Fndef {
    pub ctx: Body,
    pub sig: Body,
}

#[derive(Clone, Copy, Debug)]
pub struct Valdef {
    pub ctx: Body,
    pub ty: Body,
}

#[derive(Clone, Copy, Debug)]
pub struct Ctxdef {
    pub ctx: Body,
    pub def: Body,
}

#[derive(Debug)]
pub struct IR {
    pub modules: IndexVec<ModuleId, ()>,
    pub strings: Strings,
    pub instrs: IndexVec<InstrId, Instr>,
    pub items: IndexVec<ItemId, InstrId>,
    pub records: IndexVec<RecordId, (StrId, InstrId)>,
    pub empty: Body,
    pub tydefs: IndexVec<TydefId, Tydef>,
    pub tagdefs: IndexVec<TagdefId, Tagdef>,
    pub aliasdefs: IndexVec<AliasdefId, Aliasdef>,
    pub fndefs: IndexVec<FndefId, Fndef>,
    pub valdefs: IndexVec<ValdefId, Valdef>,
    pub ctxdefs: IndexVec<CtxdefId, Ctxdef>,
    pub bodies: IndexVec<FndefId, Option<Body>>,
}

impl Default for IR {
    fn default() -> Self {
        let mut instrs = IndexVec::new();
        let mut items = IndexVec::new();
        let empty = instrs.push(Instr::Ctx {
            slots: IdRange::new(&mut items, Vec::new()),
        });
        let body = IdRange {
            start: empty,
            end: instrs.len_idx(),
        };
        Self {
            modules: Default::default(),
            strings: Default::default(),
            instrs,
            items,
            records: Default::default(),
            empty: Body::new(body, empty),
            tydefs: Default::default(),
            tagdefs: Default::default(),
            aliasdefs: Default::default(),
            fndefs: Default::default(),
            valdefs: Default::default(),
            ctxdefs: Default::default(),
            bodies: Default::default(),
        }
    }
}

impl IR {
    pub fn empty(&self) -> Body {
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

    fn method(&mut self, ty: InstrId, name: StrId) -> Option<FndefId> {
        if let Type::Nominal(tagdef, _) = self.ir.instrs[ty]
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

    fn builder(&self) -> Builder {
        Builder {
            start: self.ir.instrs.next_idx(),
        }
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
        let elems = IdRange::new(&mut self.ir.items, elems);
        self.emit(Instr::Tuple { elems })
    }

    fn ty_unit(&mut self) -> InstrId {
        self.ty_tuple(Vec::new())
    }

    fn invoke(
        &mut self,
        param: Body,
        destruct: &[InstrId],
        target: Body,
    ) -> LowerResult<Vec<InstrId>> {
        todo!()
    }

    fn invoke_open(
        &mut self,
        param: Body,
        destruct: &[InstrId],
        target: Body,
    ) -> LowerResult<Vec<InstrId>> {
        todo!()
    }

    fn invoke_bind(
        &mut self,
        param: Body,
        construct: &[InstrId],
        source: Body,
        destruct: &[InstrId],
        target: Body,
    ) -> LowerResult<Vec<InstrId>> {
        todo!()
    }

    fn extract_ty(
        &mut self,
        param: Body,
        destruct: &[InstrId],
        def: TydefId,
        construct: &[InstrId],
    ) -> LowerResult<InstrId> {
        todo!()
    }

    fn extract_fn(
        &mut self,
        param: Body,
        destruct: &[InstrId],
        def: FndefId,
        construct: &[InstrId],
    ) -> LowerResult<InstrId> {
        todo!()
    }

    fn extract_val(
        &mut self,
        param: Body,
        destruct: &[InstrId],
        def: ValdefId,
        construct: &[InstrId],
    ) -> LowerResult<InstrId> {
        todo!()
    }

    fn extract_ctx(
        &mut self,
        param: Body,
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
    /// The `param` gives the meaning of [`Instr::Param`] in this context.
    ///
    /// The `destruct` list gives the set of entrypoints that can be used to synthesize bindings.
    fn spec(
        &mut self,
        param: Body,
        destruct: &[InstrId],
        spec: parse::Spec,
    ) -> LowerResult<(Named, Vec<InstrId>)> {
        let Spec { dot, path, binds } = spec;
        let named = if dot {
            Named::Fndef(self.detached(path)?)
        } else {
            self.path(path)?
        };
        let construct = binds
            .into_iter()
            .map(|bind| self.bind(param, &destruct, bind))
            .collect::<LowerResult<Vec<InstrId>>>()?;
        Ok((named, construct))
    }

    fn bind(
        &mut self,
        param: Body,
        slots: &[InstrId],
        bind: parse::BindId,
    ) -> LowerResult<InstrId> {
        let parse::Bind { key, val } = self.tree.binds[bind];
        let (lhs, destruct_lhs) = self.spec(param, slots, key)?;
        match val {
            None => match lhs {
                Named::Tydef(def) => {
                    let Tydef { ctx: target } = self.ir.tydefs[def];
                    let construct = self.invoke_open(param, &destruct_lhs, target)?;
                    let bind = self.extract_ty(param, &slots, def, &construct)?;
                    let params = IdRange::new(&mut self.ir.items, construct);
                    let args = params;
                    Ok(self.emit(Instr::BindTydef {
                        def,
                        params,
                        bind,
                        args,
                    }))
                }
                Named::Fndef(def) => {
                    let Fndef {
                        ctx: target,
                        sig: _,
                    } = self.ir.fndefs[def];
                    let construct = self.invoke_open(param, &destruct_lhs, target)?;
                    let bind = self.extract_fn(param, &slots, def, &construct)?;
                    let params = IdRange::new(&mut self.ir.items, construct);
                    let args = params;
                    Ok(self.emit(Instr::BindFndef {
                        def,
                        params,
                        bind,
                        args,
                    }))
                }
                Named::Valdef(def) => {
                    let Valdef { ctx: target, ty: _ } = self.ir.valdefs[def];
                    let construct = self.invoke_open(param, &destruct_lhs, target)?;
                    let bind = self.extract_val(param, &slots, def, &construct)?;
                    let params = IdRange::new(&mut self.ir.items, construct);
                    let args = params;
                    Ok(self.emit(Instr::BindValdef {
                        def,
                        params,
                        bind,
                        args,
                    }))
                }
                Named::Ctxdef(def) => {
                    let Ctxdef {
                        ctx: target,
                        def: _,
                    } = self.ir.ctxdefs[def];
                    let construct = self.invoke_open(param, &destruct_lhs, target)?;
                    let bind = self.extract_ctx(param, &slots, def, &construct)?;
                    let params = IdRange::new(&mut self.ir.items, construct);
                    let args = params;
                    Ok(self.emit(Instr::BindCtxdef {
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
                Named::Valdef(def) => {
                    let Valdef { ctx: target, ty: _ } = self.ir.valdefs[def];
                    let construct = self.invoke_open(param, &destruct_lhs, target)?;
                    let (bind, _) = self.lit(token)?;
                    let params = IdRange::new(&mut self.ir.items, construct);
                    Ok(self.emit(Instr::BindLit { def, params, bind }))
                }
                _ => Err(LowerError::LitNotVal(token)),
            },
            Some(parse::Entry::Ref(spec)) => {
                let (rhs, destruct_rhs) = self.spec(param, slots, spec)?;
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
                        let params = IdRange::new(&mut self.ir.items, construct_lhs);
                        let args = IdRange::new(&mut self.ir.items, construct_rhs);
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
                            param,
                            &construct_lhs,
                            target_lhs,
                            &destruct_rhs,
                            target_rhs,
                        )?;
                        let bind = tagdef;
                        let params = IdRange::new(&mut self.ir.items, construct_lhs);
                        let args = IdRange::new(&mut self.ir.items, construct_rhs);
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
                        let params = IdRange::new(&mut self.ir.items, construct_lhs);
                        let args = IdRange::new(&mut self.ir.items, construct_rhs);
                        Ok(self.emit(Instr::BindAliasdef {
                            def,
                            params,
                            bind,
                            args,
                        }))
                    }
                    (Named::Fndef(def), Named::Fndef(fndef)) => {
                        let Fndef {
                            ctx: target_lhs,
                            sig: _,
                        } = self.ir.fndefs[def];
                        let Fndef {
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
                        let bind = self.extract_fn(param, &slots, fndef, &construct_rhs)?;
                        let params = IdRange::new(&mut self.ir.items, construct_lhs);
                        let args = IdRange::new(&mut self.ir.items, construct_rhs);
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
                        let params = IdRange::new(&mut self.ir.items, construct_lhs);
                        let args = IdRange::new(&mut self.ir.items, construct_rhs);
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
                    _ => Err(LowerError::BindMismatch(bind)),
                }
            }
        }
    }

    fn need(
        &mut self,
        param: Body,
        slots: &[InstrId],
        need: parse::NeedId,
    ) -> LowerResult<InstrId> {
        let parse::Need { kind: _, bind } = self.tree.needs[need];
        // TODO: Handle `kind`.
        let parse::Bind { key, val } = self.tree.binds[bind];
        match val {
            Some(_) => self.bind(param, &slots, bind),
            None => {
                let (lhs, destruct) = self.spec(param, &slots, key)?;
                match lhs {
                    Named::Tydef(def) => {
                        let Tydef { ctx: target } = self.ir.tydefs[def];
                        let construct = self.invoke_open(param, &destruct, target)?;
                        let params = IdRange::new(&mut self.ir.items, construct);
                        Ok(self.emit(Instr::NeedTydef { def, params }))
                    }
                    Named::Fndef(def) => {
                        let Fndef {
                            ctx: target,
                            sig: _,
                        } = self.ir.fndefs[def];
                        let construct = self.invoke_open(param, &destruct, target)?;
                        let params = IdRange::new(&mut self.ir.items, construct);
                        Ok(self.emit(Instr::NeedFndef { def, params }))
                    }
                    Named::Valdef(def) => {
                        let Valdef { ctx: target, ty: _ } = self.ir.valdefs[def];
                        let construct = self.invoke_open(param, &destruct, target)?;
                        let params = IdRange::new(&mut self.ir.items, construct);
                        Ok(self.emit(Instr::NeedValdef { def, params }))
                    }
                    Named::Ctxdef(def) => {
                        let Ctxdef {
                            ctx: target,
                            def: _,
                        } = self.ir.ctxdefs[def];
                        let construct = self.invoke_open(param, &destruct, target)?;
                        let params = IdRange::new(&mut self.ir.items, construct);
                        Ok(self.emit(Instr::NeedCtxdef { def, params }))
                    }
                    Named::Module(_) => Err(LowerError::BindModule(bind)),
                    Named::Tagdef(_) => Err(LowerError::BindNominal(bind)),
                    Named::Aliasdef(_) => Err(LowerError::BindAlias(bind)),
                }
            }
        }
    }

    fn needs_raw(
        &mut self,
        param: Body,
        needs: IdRange<parse::NeedId>,
    ) -> LowerResult<Vec<InstrId>> {
        let mut slots = Vec::new();
        slots.push(self.emit(Instr::Param));
        for need in needs {
            slots.push(self.need(param, &slots, need)?);
        }
        Ok(slots)
    }

    fn needs(&mut self, param: Body, needs: IdRange<parse::NeedId>) -> LowerResult<Body> {
        let builder = self.builder();
        let slots = self.needs_raw(param, needs)?;
        let slots = IdRange::new(&mut self.ir.items, slots);
        let ctx = self.emit(Instr::Ctx { slots });
        Ok(self.finish(builder, ctx))
    }

    fn parse_ty(
        &mut self,
        param: Body,
        slots: &[InstrId],
        ty: parse::TypeId,
    ) -> LowerResult<InstrId> {
        match self.tree.types[ty] {
            parse::Type::Spec(spec) => {
                let (named, destruct) = self.spec(param, slots, spec)?;
                match named {
                    Named::Tydef(tydef) => {
                        let Tydef { ctx: target } = self.ir.tydefs[tydef];
                        let construct = self.invoke(param, &destruct, target)?;
                        self.extract_ty(param, slots, tydef, &construct)
                    }
                    Named::Tagdef(tagdef) => todo!(),
                    Named::Aliasdef(aliasdef) => todo!(),
                    _ => return Err(LowerError::NotType(spec.path.last)),
                }
            }
            parse::Type::Tuple(elems) => {
                let lowered = elems
                    .into_iter()
                    .map(|elem| self.parse_ty(param, slots, elem))
                    .collect::<LowerResult<Vec<InstrId>>>()?;
                Ok(self.ty_tuple(lowered))
            }
            parse::Type::Record(members) => todo!(),
        }
    }

    fn parse_fndef(&mut self, fndef: parse::Fndef) -> LowerResult<(StrId, FndefId)> {
        let empty = self.ir.empty();
        let parse::Fndef {
            name,
            needs,
            params,
            result,
            def: _,
        } = fndef;
        let ctx = self.needs(empty, needs)?;
        let builder = self.builder();
        let param = self.emit(Instr::Param);
        let slots = &[param];
        let elems = (params.into_iter())
            .map(|arg| self.parse_ty(ctx, slots, self.tree.params[arg].ty))
            .collect::<LowerResult<Vec<InstrId>>>()?;
        let param_tuple = self.ty_tuple(elems);
        let result_ty = match result {
            parse::Return::Unit => self.ty_unit(),
            parse::Return::Type(ty) => self.parse_ty(ctx, slots, ty)?,
            parse::Return::Bind(needs) => {
                let slots = self.needs_raw(ctx, needs)?;
                let slots = IdRange::new(&mut self.ir.items, slots);
                self.emit(Instr::Ctx { slots })
            }
        };
        let signature = self.emit(Instr::Sig {
            param: param_tuple,
            result: result_ty,
        });
        let sig = self.finish(builder, signature);
        let lowered = self.ir.fndefs.push(Fndef { ctx, sig });
        let string = self.name(name);
        Ok((string, lowered))
    }

    fn decls(&mut self) -> LowerResult<()> {
        let empty = self.ir.empty();
        for &decl in &self.tree.decls {
            match decl {
                parse::Decl::Tydef(id) => {
                    let parse::Tydef { name, needs } = self.tree.tydefs[id];
                    let ctx = self.needs(empty, needs)?;
                    let lowered = self.ir.tydefs.push(Tydef { ctx });
                    let string = self.name(name);
                    self.names
                        .names
                        .insert((self.module, string), Named::Tydef(lowered));
                }
                parse::Decl::Tagdef(id) => {
                    let parse::Tagdef { name, needs, def } = self.tree.tagdefs[id];
                    let ctx = self.needs(empty, needs)?;
                    let builder = self.builder();
                    let param = self.emit(Instr::Param);
                    let slots = &[param];
                    let ty = self.parse_ty(ctx, slots, def)?;
                    let inner = self.finish(builder, ty);
                    let lowered = self.ir.tagdefs.push(Tagdef { ctx, inner });
                    let string = self.name(name);
                    self.names
                        .names
                        .insert((self.module, string), Named::Tagdef(lowered));
                }
                parse::Decl::Aliasdef(id) => {
                    let parse::Aliasdef { name, needs, def } = self.tree.aliasdefs[id];
                    let ctx = self.needs(empty, needs)?;
                    let builder = self.builder();
                    let param = self.emit(Instr::Param);
                    let slots = &[param];
                    let ty = self.parse_ty(ctx, slots, def)?;
                    let def = self.finish(builder, ty);
                    let lowered = self.ir.aliasdefs.push(Aliasdef { ctx, def });
                    let string = self.name(name);
                    self.names
                        .names
                        .insert((self.module, string), Named::Aliasdef(lowered));
                }
                parse::Decl::Funcdef(id) => {
                    let parse::Funcdef { fndef } = self.tree.funcdefs[id];
                    let (fn_name, lowered) = self.parse_fndef(fndef)?;
                    self.funcs.push((id, lowered));
                    self.names
                        .names
                        .insert((self.module, fn_name), Named::Fndef(lowered));
                }
                parse::Decl::Attachdef(id) => {
                    let parse::Attachdef { ty, fndef } = self.tree.attachdefs[id];
                    let (fn_name, lowered) = self.parse_fndef(fndef)?;
                    let ty_name = self.name(ty);
                    // TODO: Prevent attaching methods to nominal types defined in other modules.
                    match self.names.names.get(&(self.module, ty_name)) {
                        Some(&Named::Tagdef(tagdef)) => {
                            self.attaches.push((id, tagdef, lowered));
                            self.names.attached.insert((tagdef, fn_name), lowered);
                        }
                        Some(_) => return Err(LowerError::NotNominal(ty)),
                        None => return Err(LowerError::Undefined(ty)),
                    }
                }
                parse::Decl::Detachdef(id) => {
                    let parse::Detachdef { fndef } = self.tree.detachdefs[id];
                    let (fn_name, lowered) = self.parse_fndef(fndef)?;
                    self.detaches.push((id, lowered));
                    self.names.detached.insert((self.module, fn_name), lowered);
                }
                parse::Decl::Valdef(id) => {
                    let parse::Valdef { name, needs, ty } = self.tree.valdefs[id];
                    let ctx = self.needs(empty, needs)?;
                    let builder = self.builder();
                    let param = self.emit(Instr::Param);
                    let slots = &[param];
                    let ty = self.parse_ty(ctx, slots, ty)?;
                    let ty = self.finish(builder, ty);
                    let lowered = self.ir.valdefs.push(Valdef { ctx, ty });
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

    fn extract_ty(&mut self, def: TydefId, construct: &[InstrId]) -> LowerResult<InstrId> {
        self.x.extract_ty(self.ctx, &self.slots, def, construct)
    }

    fn extract_fn(&mut self, def: FndefId, construct: &[InstrId]) -> LowerResult<InstrId> {
        self.x.extract_fn(self.ctx, &self.slots, def, construct)
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

    fn expr(&mut self, expr: ExprId) -> LowerResult<Typed> {
        match self.x.tree.exprs[expr] {
            parse::Expr::Lit(token) => {
                let Base {
                    types,
                    lit_types,
                    lit_vals,
                    lits,
                } = self.base();
                let (tydef_lit, tydef, valdef, fndef, val) = match self.x.lit(token)? {
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
                let Fndef {
                    ctx: ctx_fndef,
                    sig: _,
                } = self.x.ir.fndefs[fndef];

                let construct_ty_lit = self.invoke(&[], ctx_tydef_lit)?;
                let ty_lit = self.extract_ty(tydef_lit, &construct_ty_lit)?;

                let construct_ty = self.invoke(&[], ctx_tydef)?;
                let ty = self.extract_ty(tydef, &construct_ty)?;

                let construct_val = self.invoke(&[ty_lit], ctx_valdef)?;
                let val_lit = self.emit(Instr::BindLit {
                    def: valdef,
                    params: IdRange::new(&mut self.x.ir.items, construct_val),
                    bind: val,
                });

                let construct_func = self.invoke(&[ty_lit, ty, val_lit], ctx_fndef)?;
                let func = self.extract_fn(fndef, &construct_func)?;

                let params = IdRange::new(&mut self.x.ir.items, construct_func);
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
                let construct = self.invoke(&self.slots, ctx)?;
                let val = self.extract_val(valdef, &construct)?;
                let ty_inlined = self.inline(ctx, &construct, ty)?;
                Ok(self.instr(ty_inlined, Expr::Val { val }))
            }
            parse::Expr::Tag(path, inner) => {
                let Named::Tagdef(tagdef) = self.x.path(path)? else {
                    return Err(LowerError::NotNominal(path.last));
                };
                let local = self.expr(inner)?;
                let ty = self.x.ir.locals[local];
                Ok(self.instr(ty, Instr::Nominal(tagdef, local)))
            }
            parse::Expr::Record(_lbrace, fields, _rbrace) => {
                let sorted = fields
                    .into_iter()
                    .map(|field| {
                        let Field { name, val } = self.x.tree.fields[field];
                        Ok((self.x.slice(name), self.expr(val)?))
                    })
                    .collect::<LowerResult<BTreeMap<&str, InstrId>>>()?;
                let fields = sorted
                    .iter()
                    .map(|(&string, &local)| {
                        (self.x.ir.strings.make_id(string), self.x.ir.locals[local])
                    })
                    .collect::<Vec<(StrId, TypeId)>>();
                let ty = self.x.ty_record(&fields);
                let locals = sorted.values().copied().collect::<Vec<InstrId>>();
                Ok(self.instr_record(ty, &locals))
            }
            parse::Expr::Field(object, field) => {
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
            parse::Expr::Method(object, method, args) => {
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
                    .collect::<LowerResult<Vec<InstrId>>>()?;
                let arg = self.instr_tuple(param, &locals);
                // TODO: Contextually set `this` to `obj`.
                Ok(self.instr(result, Instr::Call(fndef, arg)))
            }
            parse::Expr::Call(callee, binds, args) => {
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
                    .collect::<LowerResult<Vec<InstrId>>>()?;
                let arg = self.instr_tuple(param, &locals);
                let call = self.instr(result, Instr::Call(fndef, arg));
                Ok(call)
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
            parse::Expr::Bind(_, bindings) => {
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
                    self.emit(Instr::If { cond: local.val });
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

    fn body(&mut self, fndef: parse::Fndef, id_decl: FndefId) -> LowerResult<Option<Body>> {
        let parse::Fndef {
            name: _,
            needs: _,
            params,
            result: _,
            def,
        } = fndef;
        let Some(body) = def else { return Ok(None) };
        let builder = self.x.builder();
        let Fndef { ctx: _, sig } = self.x.ir.fndefs[id_decl];
        let Instr::Sig {
            param: tuple_ty,
            result: _,
        } = self.x.ir.instrs[sig.result()]
        else {
            unreachable!()
        };
        let tuple_local = self.instr(tuple_ty, Expr::Param);
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
        Ok(Some(self.x.finish(builder, ret.val)))
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
