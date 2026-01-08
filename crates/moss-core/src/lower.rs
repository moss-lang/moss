use std::{
    collections::{BTreeMap, HashMap},
    fmt,
    mem::take,
};

use index_vec::{IndexSlice, IndexVec, define_index_type};
use indexmap::IndexSet;

use crate::{
    intern::{StrId, Strings},
    lex::{TokenId, TokenStarts, relex, string},
    parse::{self, Binop, Block, Expr, ExprId, Field, Path, Stmt, StmtId, Tree, Unop},
    range::{Inclusive, expr_range, path_range},
    tuples::{TupleRange, Tuples},
    util::IdRange,
};

define_index_type! {
    pub struct ModuleId = u32;
}

define_index_type! {
    pub struct CtxId = u32;
}

define_index_type! {
    pub struct TypeId = u32;
}

define_index_type! {
    pub struct FnId = u32;
}

define_index_type! {
    pub struct ValId = u32;
}

define_index_type! {
    pub struct ElemId = u32;
}

define_index_type! {
    pub struct TydefId = u32;
}

define_index_type! {
    pub struct TagdefId = u32;
}

define_index_type! {
    pub struct AliasdefId = u32;
}

define_index_type! {
    pub struct FndefId = u32;
}

define_index_type! {
    pub struct ValdefId = u32;
}

define_index_type! {
    pub struct CtxdefId = u32;
}

define_index_type! {
    pub struct NeedTyId = u32;
}

define_index_type! {
    pub struct NeedFnId = u32;
}

define_index_type! {
    pub struct NeedValId = u32;
}

define_index_type! {
    pub struct NeedCtxId = u32;
}

define_index_type! {
    pub struct FieldId = u32;
}

define_index_type! {
    pub struct LocalId = u32;
}

define_index_type! {
    pub struct RefId = u32;
}

#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct Ctx {
    tys: im_rc::HashMap<TydefId, im_rc::HashMap<CtxId, Option<TypeId>>>,
    fns: im_rc::HashMap<FndefId, im_rc::HashMap<CtxId, Option<Fn>>>,
    vals: im_rc::HashMap<ValdefId, im_rc::HashMap<CtxId, Option<Val>>>,
    ctxs: im_rc::HashMap<CtxdefId, im_rc::HashSet<CtxId>>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    /// An instance of an opaque type symbol in a specific context.
    Opaque(TydefId, CtxId),

    /// An inner type made nominal via an outer tag symbol.
    Nominal(TagdefId, CtxId),

    /// A structural type alias.
    Alias(AliasdefId, CtxId),

    /// A structural tuple of other types.
    ///
    /// All empty tuples are identically the unit type.
    Tuple(TupleRange),

    /// A structural record of other types.
    ///
    /// The fields are sorted lexicographically by name.
    Record(TupleRange),
}

impl Type {
    pub fn opaque(self) -> (TydefId, CtxId) {
        match self {
            Type::Opaque(tydef, ctx) => (tydef, ctx),
            _ => panic!(),
        }
    }

    pub fn nominal(self) -> (TagdefId, CtxId) {
        match self {
            Type::Nominal(tagdef, ctx) => (tagdef, ctx),
            _ => panic!(),
        }
    }

    pub fn alias(self) -> (AliasdefId, CtxId) {
        match self {
            Type::Alias(aliasdef, ctx) => (aliasdef, ctx),
            _ => panic!(),
        }
    }

    pub fn tuple(self) -> TupleRange {
        match self {
            Type::Tuple(range) => range,
            _ => panic!(),
        }
    }

    pub fn record(self) -> TupleRange {
        match self {
            Type::Record(range) => range,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Fn {
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
pub struct Tydef {
    pub ctx: CtxId,
}

#[derive(Clone, Copy, Debug)]
pub struct Tagdef {
    pub ctx: CtxId,
    pub inner: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Aliasdef {
    pub ctx: CtxId,
    pub def: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Fndef {
    pub ctx: CtxId,
    pub param: TypeId,
    pub result: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Valdef {
    pub ctx: CtxId,
    pub ty: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Ctxdef {
    pub ctx: CtxId,
    pub def: CtxId,
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

#[derive(Debug, Default)]
pub struct IR {
    pub modules: IndexVec<ModuleId, ()>,
    pub strings: Strings,
    pub ctxs: IndexSet<Ctx>,
    pub types: IndexSet<Type>,
    pub tuples: Tuples<TypeId>,
    pub records: Tuples<(StrId, TypeId)>,
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

impl IR {
    pub fn ctx(&mut self, ctx: Ctx) -> CtxId {
        let (i, _) = self.ctxs.insert_full(ctx);
        CtxId::from_usize(i)
    }

    pub fn ty(&mut self, ty: Type) -> TypeId {
        let (i, _) = self.types.insert_full(ty);
        TypeId::from_usize(i)
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
enum Named {
    Module(ModuleId),
    Tydef(TydefId),
    Tagdef(TagdefId),
    Aliasdef(AliasdefId),
    Fndef(FndefId),
    Valdef(ValdefId),
    Ctxdef(CtxdefId),
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

    fn ty(&self, id: TypeId) -> FatType<'_, '_> {
        FatType { ctx: self, id }
    }
}

struct FatType<'a, 'b> {
    ctx: &'b ErrorCtx<'a>,
    id: TypeId,
}

impl fmt::Display for FatType<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ctx.ir.types[self.id.index()] {
            Type::Opaque(_, _) => write!(f, "an opaque type"),
            Type::Nominal(_, _) => write!(f, "a nominal type"),
            Type::Alias(_, _) => write!(f, "a type alias"),
            Type::Tuple(_) => write!(f, "a tuple"),
            Type::Record(_) => write!(f, "a record"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum LowerError {
    Undefined(TokenId),
    NotModule(TokenId),
    NotType(TokenId),
    NotNominal(TokenId),
    NotVal(TokenId),
    NotContext(TokenId),
    BindContext(parse::BindId),
    BindModule(parse::BindId),
    BindNominal(parse::BindId),
    BindAlias(parse::BindId),
    BindMismatch(parse::BindId),
    LitNotVal(TokenId),
    ExpectedType(TypeId, ExprId, TypeId),
    ThisNotMethod(ExprId),
    Overflow(ExprId),
    ArgCount(ExprId),
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
            LowerError::Undefined(token) => (
                Some(Inclusive {
                    first: token,
                    last: token,
                }),
                "undefined".to_owned(),
            ),
            LowerError::NotModule(token) => (
                Some(Inclusive {
                    first: token,
                    last: token,
                }),
                "not a module".to_owned(),
            ),
            LowerError::NotType(token) => (
                Some(Inclusive {
                    first: token,
                    last: token,
                }),
                "not a type".to_owned(),
            ),
            LowerError::NotNominal(token) => (
                Some(Inclusive {
                    first: token,
                    last: token,
                }),
                "not a nominal type".to_owned(),
            ),
            LowerError::NotVal(token) => (
                Some(Inclusive {
                    first: token,
                    last: token,
                }),
                "not a value".to_owned(),
            ),
            LowerError::NotContext(token) => (
                Some(Inclusive {
                    first: token,
                    last: token,
                }),
                "not a piece of context".to_owned(),
            ),
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
                Some(Inclusive {
                    first: token,
                    last: token,
                }),
                "cannot bind a literal for something other than a `val`".to_owned(),
            ),
            LowerError::ExpectedType(expected, expr, actual) => (
                Some(ctx.expr(expr)),
                format!("expected {}, got {}", ctx.ty(expected), ctx.ty(actual)),
            ),
            LowerError::ThisNotMethod(expr) => (
                Some(ctx.expr(expr)),
                "cannot use `this` in a function that is not a method".to_owned(),
            ),
            LowerError::Overflow(expr) => (Some(ctx.expr(expr)), "integer too large".to_owned()),
            LowerError::ArgCount(expr) => {
                (Some(ctx.expr(expr)), "wrong number of arguments".to_owned())
            }
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
    module: ModuleId,
    prelude: ModuleId,
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
}

impl<'a> Lower<'a> {
    fn slice(&self, token: TokenId) -> &'a str {
        &self.source[relex(self.source, self.starts, token)]
    }

    fn name(&mut self, token: TokenId) -> StrId {
        self.ir.strings.make_id(self.slice(token))
    }

    fn lit(&mut self, token: TokenId) -> LowerResult<Val> {
        match self.slice(token).chars().next().unwrap() {
            '0'..='9' => todo!(),
            '"' => {
                let escaped = string(self.source, self.starts, token);
                let string = self.ir.strings.make_id(&escaped);
                Ok(Val::String(string))
            }
            _ => unreachable!(),
        }
    }

    fn ctx(&mut self, ctx: Ctx) -> CtxId {
        self.ir.ctx(ctx)
    }

    fn ty(&mut self, ty: Type) -> TypeId {
        self.ir.ty(ty)
    }

    fn ty_tuple(&mut self, elems: &[TypeId]) -> TypeId {
        let tuple = self.ir.tuples.make(elems);
        self.ty(Type::Tuple(tuple))
    }

    fn ty_unit(&mut self) -> TypeId {
        self.ty_tuple(&[])
    }

    fn ty_record(&mut self, fields: &[(StrId, TypeId)]) -> TypeId {
        let record = self.ir.records.make(fields);
        self.ty(Type::Record(record))
    }

    fn path(&mut self, path: Path) -> LowerResult<Named> {
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
        get_name(self.prelude, self.module, &self.names.names, (module, name))
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
                self.names
                    .names
                    .insert((self.module, name), Named::Fndef(id));
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

    fn spec(&mut self, spec: parse::Spec) -> LowerResult<(Named, CtxId)> {
        let named = self.path(spec.path)?;
        let ctx = self.binds(spec.binds)?;
        Ok((named, ctx))
    }

    fn bind(&mut self, ctx: &mut Ctx, bind: parse::BindId) -> LowerResult<()> {
        let parse::Bind { key, val } = self.tree.binds[bind];
        let (lhs, ctx1) = self.spec(key)?;
        match val {
            None => match lhs {
                Named::Tydef(tydef) => {
                    ctx.tys.entry(tydef).or_default().insert(ctx1, None);
                }
                Named::Fndef(fndef) => {
                    ctx.fns.entry(fndef).or_default().insert(ctx1, None);
                }
                Named::Valdef(valdef) => {
                    ctx.vals.entry(valdef).or_default().insert(ctx1, None);
                }
                Named::Ctxdef(ctxdef) => {
                    ctx.ctxs.entry(ctxdef).or_default().insert(ctx1);
                }
                Named::Module(_) => return Err(LowerError::BindModule(bind)),
                Named::Tagdef(_) => return Err(LowerError::BindNominal(bind)),
                Named::Aliasdef(_) => return Err(LowerError::BindAlias(bind)),
            },
            Some(parse::Entry::Lit(token)) => match lhs {
                Named::Valdef(valdef) => {
                    let val = self.lit(token)?;
                    ctx.vals.entry(valdef).or_default().insert(ctx1, Some(val));
                }
                _ => return Err(LowerError::LitNotVal(token)),
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
                        let f = Fn { ctx: ctx2, def };
                        ctx.fns.entry(fndef).or_default().insert(ctx1, Some(f));
                    }
                    (Named::Valdef(valdef), Named::Valdef(def)) => {
                        let val = Val::Opaque(ctx2, def);
                        ctx.vals.entry(valdef).or_default().insert(ctx1, Some(val));
                    }
                    (Named::Ctxdef(_), _) => return Err(LowerError::BindContext(bind)),
                    (Named::Module(_), _) => return Err(LowerError::BindModule(bind)),
                    (Named::Tagdef(_), _) => return Err(LowerError::BindNominal(bind)),
                    (Named::Aliasdef(_), _) => return Err(LowerError::BindAlias(bind)),
                    _ => return Err(LowerError::BindMismatch(bind)),
                }
            }
        }
        Ok(())
    }

    fn binds(&mut self, entries: IdRange<parse::BindId>) -> LowerResult<CtxId> {
        let mut ctx = Ctx::default();
        for bind in entries {
            self.bind(&mut ctx, bind)?;
        }
        Ok(self.ctx(ctx))
    }

    fn needs(&mut self, needs: IdRange<parse::NeedId>) -> LowerResult<CtxId> {
        let mut ctx = Ctx::default();
        for need in needs {
            let parse::Need { kind: _, bind } = self.tree.needs[need];
            // TODO: Handle `kind`.
            self.bind(&mut ctx, bind)?;
        }
        Ok(self.ctx(ctx))
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
                Some(ty) => self.parse_ty(ty)?,
                None => self.ty_unit(),
            },
        })
    }

    fn decls(&mut self) -> LowerResult<()> {
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
        self.names()?;
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
            Expr::Lit(token) => todo!(),
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
                    return Err(LowerError::Undefined(callee.last));
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
        module,
        prelude,
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
    };
    lower.program()?;
    Ok(module)
}
