use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    ops::{AddAssign, BitXorAssign, Index, SubAssign},
};

use index_vec::{Idx, define_index_type, index_vec};
use indexmap::IndexSet;

use crate::{
    intern::StrId,
    lower::{CtxdefId, FndefId, IR, Need, Needs, StructdefId, TydefId, Type, TypeId, ValdefId},
    subsets::Subsets,
    tuples::{TupleLoc, TupleRange, Tuples},
    util::default_hash,
};

define_index_type! {
    pub struct ContextId = u32;
}

define_index_type! {
    pub struct TyId = u32;
}

define_index_type! {
    pub struct BuiltinId = u32;
}

define_index_type! {
    pub struct FnId = u32;
}

define_index_type! {
    pub struct ValId = u32;
}

#[derive(Debug)]
struct NeedSets<I> {
    tydefs: Subsets<TydefId, I>,
    fndefs: Subsets<FndefId, I>,
    valdefs: Subsets<ValdefId, I>,
}

impl<I: Idx> NeedSets<I> {
    fn new(ir: &IR, len: I) -> Self {
        let mut tydefs = Subsets::new(ir.tydefs.len_idx());
        let mut fndefs = Subsets::new(ir.fndefs.len_idx());
        let mut valdefs = Subsets::new(ir.valdefs.len_idx());
        for _ in 0..len.index() {
            tydefs.push();
            fndefs.push();
            valdefs.push();
        }
        Self {
            tydefs,
            fndefs,
            valdefs,
        }
    }
}

impl NeedSets<CtxdefId> {
    fn fill(&mut self, ir: &IR) {
        for ctxdef in topsort(CtxdefGraph(ir)) {
            let Needs {
                tys,
                fns,
                vals,
                ctxs,
            } = ir.ctxdefs[ctxdef].def;
            let (view_tydefs, mut set_tydefs) = self.tydefs.get_mut(ctxdef);
            let (view_fndefs, mut set_fndefs) = self.fndefs.get_mut(ctxdef);
            let (view_valdefs, mut set_valdefs) = self.valdefs.get_mut(ctxdef);
            for need_ty in tys.get(&ir.need_tys) {
                set_tydefs.include(need_ty.id);
            }
            for need_fn in fns.get(&ir.need_fns) {
                set_fndefs.include(need_fn.id);
            }
            for need_val in vals.get(&ir.need_vals) {
                set_valdefs.include(need_val.id);
            }
            for need_ctx in ctxs.get(&ir.need_ctxs) {
                set_tydefs |= view_tydefs.get(need_ctx.id);
                set_fndefs |= view_fndefs.get(need_ctx.id);
                set_valdefs |= view_valdefs.get(need_ctx.id);
            }
        }
    }
}

impl<I> NeedSets<I> {
    fn full(
        ir: &IR,
        ctxdefs: &NeedSets<CtxdefId>,
        items: impl IntoIterator<Item = (I, Needs)>,
    ) -> Self {
        let mut tydefs = Subsets::new(ir.tydefs.len_idx());
        let mut fndefs = Subsets::new(ir.fndefs.len_idx());
        let mut valdefs = Subsets::new(ir.valdefs.len_idx());
        for (
            _,
            Needs {
                tys,
                fns,
                vals,
                ctxs,
            },
        ) in items
        {
            let (_, mut set_tydefs) = tydefs.push();
            let (_, mut set_fndefs) = fndefs.push();
            let (_, mut set_valdefs) = valdefs.push();
            for need_ty in tys.get(&ir.need_tys) {
                set_tydefs.include(need_ty.id);
            }
            for need_fn in fns.get(&ir.need_fns) {
                set_fndefs.include(need_fn.id);
            }
            for need_val in vals.get(&ir.need_vals) {
                set_valdefs.include(need_val.id);
            }
            for need_ctx in ctxs.get(&ir.need_ctxs) {
                set_tydefs |= ctxdefs.tydefs.get(need_ctx.id);
                set_fndefs |= ctxdefs.fndefs.get(need_ctx.id);
                set_valdefs |= ctxdefs.valdefs.get(need_ctx.id);
            }
        }
        Self {
            tydefs,
            fndefs,
            valdefs,
        }
    }
}

trait Graph {
    type Id: Idx;

    fn nodes(&self) -> impl ExactSizeIterator<Item = Self::Id>;

    fn arcs(&self, i: Self::Id, f: impl FnMut(Self::Id));
}

fn topsort<G: Graph>(graph: G) -> Vec<G::Id> {
    // Implementation adapted from here:
    // https://github.com/penrose/penrose/blob/v3.3.0/packages/core/src/utils/Graph.ts#L142-L168
    let mut sorted = Vec::new();
    let mut dependents = index_vec![0usize; graph.nodes().len()];
    for i in graph.nodes() {
        graph.arcs(i, |j| dependents[j] += 1);
    }
    let mut stack = Vec::new();
    for (i, &n) in dependents.iter_enumerated() {
        if n == 0 {
            stack.push(i);
        }
    }
    while let Some(i) = stack.pop() {
        sorted.push(i);
        graph.arcs(i, |j| {
            let n = &mut dependents[j];
            *n -= 1;
            if *n == 0 {
                stack.push(j);
            }
        });
    }
    let m = sorted.len();
    let n = graph.nodes().len();
    if m != n {
        panic!("could only sort {m} out of {n} total");
    }
    sorted.reverse();
    sorted
}

struct TypeGraph<'a>(&'a IR);

impl Graph for TypeGraph<'_> {
    type Id = TypeId;

    fn nodes(&self) -> impl ExactSizeIterator<Item = Self::Id> {
        (0..self.0.types.len()).map(TypeId::from_usize)
    }

    fn arcs(&self, i: Self::Id, mut f: impl FnMut(Self::Id)) {
        match self.0.types[i.index()] {
            Type::String | Type::Bool | Type::Int32 | Type::Int64 => {}
            Type::Tuple(elems) => {
                for &elem in &self.0.tuples[elems] {
                    f(elem);
                }
            }
            Type::Tydef(tydef) => match self.0.tydefs[tydef].def {
                None => {}
                Some(def) => f(def),
            },
            Type::Structdef(structdef) => {
                for &(_, field) in &self.0.fields[self.0.structdefs[structdef].fields] {
                    f(field);
                }
            }
        }
    }
}

struct CtxdefGraph<'a>(&'a IR);

impl Graph for CtxdefGraph<'_> {
    type Id = CtxdefId;

    fn nodes(&self) -> impl ExactSizeIterator<Item = Self::Id> {
        self.0.ctxdefs.iter_enumerated().map(|(i, _)| i)
    }

    fn arcs(&self, i: Self::Id, mut f: impl FnMut(Self::Id)) {
        for &Need { id, .. } in self.0.ctxdefs[i].def.ctxs.get(&self.0.need_ctxs) {
            f(id);
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
struct UnorderedHash(u64);

impl AddAssign<u64> for UnorderedHash {
    fn add_assign(&mut self, rhs: u64) {
        self.0.bitxor_assign(rhs);
    }
}

impl SubAssign<u64> for UnorderedHash {
    fn sub_assign(&mut self, rhs: u64) {
        self.0.bitxor_assign(rhs);
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Context {
    tydefs: im_rc::HashMap<TydefId, TyId>,
    fndefs: im_rc::HashMap<FndefId, FnId>,
    valdefs: im_rc::HashMap<ValdefId, ValId>,
    hash_tydefs: UnorderedHash,
    hash_fndefs: UnorderedHash,
    hash_valdefs: UnorderedHash,
}

impl Hash for Context {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash_tydefs.hash(state);
        self.hash_fndefs.hash(state);
        self.hash_valdefs.hash(state);
    }
}

impl Context {
    pub fn set_ty(&mut self, tydef: TydefId, t: TyId) {
        if let Some(prev) = self.tydefs.insert(tydef, t) {
            self.hash_tydefs -= default_hash(&(tydef, prev));
        }
        self.hash_tydefs += default_hash(&(tydef, t));
    }

    pub fn set_fn(&mut self, fndef: FndefId, f: FnId) {
        if let Some(prev) = self.fndefs.insert(fndef, f) {
            self.hash_fndefs -= default_hash(&(fndef, prev));
        }
        self.hash_fndefs += default_hash(&(fndef, f));
    }

    pub fn set_val(&mut self, valdef: ValdefId, v: ValId) {
        if let Some(prev) = self.valdefs.insert(valdef, v) {
            self.hash_valdefs -= default_hash(&(valdef, prev));
        }
        self.hash_valdefs += default_hash(&(valdef, v));
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Ty {
    String,
    Bool,
    Int32,
    Int64,
    Tuple(TupleRange),
    Structdef(StructdefId, TupleRange),
}

impl Ty {
    pub fn tuple(self) -> TupleRange {
        match self {
            Ty::Tuple(elems) => elems,
            _ => panic!(),
        }
    }

    pub fn structdef(self) -> TupleRange {
        match self {
            Ty::Structdef(_, fields) => fields,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Fn {
    Builtin(BuiltinId),
    Fndef(ContextId, FndefId),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Val {
    Bool(bool),
    Int32(u32),
    String(StrId),
    Dynamic(ValdefId, TyId),
}

#[derive(Debug)]
pub struct Cache<'a> {
    ir: &'a IR,
    contexts: IndexSet<Context>,
    tuples: Tuples<TyId>,
    tys: IndexSet<Ty>,
    fns: IndexSet<Fn>,
    vals: IndexSet<Val>,
    types: HashMap<(ContextId, TypeId), TyId>,
    fndefs: HashMap<(ContextId, FndefId), FnId>,

    /// Bindable tydefs needed by each abstract type.
    type_tydefs: Subsets<TydefId, TypeId>,

    /// Bindable defs needed by each fndef.
    fndef_needs: NeedSets<FndefId>,
}

impl<'a> Cache<'a> {
    pub fn new(ir: &'a IR) -> Self {
        let mut contexts = IndexSet::new();
        contexts.insert(Context::default());
        // TODO: Represent all these subsets and sparse graphs in a non-quadratic way.
        let mut type_tydefs = Subsets::new(ir.tydefs.len_idx());
        for _ in 0..ir.types.len() {
            type_tydefs.push();
        }
        // Even though types in `ir.types` can't directly refer to types that come after them in the
        // collection, they can do so indirectly via struct fields, since lowering first makes a
        // placeholder for each struct to allow name resolution to work correctly, then fills in the
        // actual fields in a second pass.
        for ty in topsort(TypeGraph(ir)) {
            let (prev, mut set) = type_tydefs.get_mut(ty);
            match ir.types[ty.index()] {
                Type::String | Type::Bool | Type::Int32 | Type::Int64 => {}
                Type::Tuple(elems) => {
                    for &elem in &ir.tuples[elems] {
                        set |= prev.get(elem);
                    }
                }
                Type::Tydef(tydef) => {
                    if ir.tydefs[tydef].def.is_none() {
                        set.include(tydef);
                    }
                }
                Type::Structdef(structdef) => {
                    for &(_, field) in &ir.fields[ir.structdefs[structdef].fields] {
                        set |= prev.get(field);
                    }
                }
            }
        }
        let mut ctxdefs = NeedSets::new(ir, ir.ctxdefs.len_idx());
        ctxdefs.fill(ir);
        let fndef_needs = NeedSets::full(
            ir,
            &ctxdefs,
            ir.fndefs.iter_enumerated().map(|(i, def)| (i, def.needs)),
        );
        Self {
            ir,
            contexts,
            tuples: Tuples::new(),
            tys: IndexSet::new(),
            fns: IndexSet::new(),
            vals: IndexSet::new(),
            types: HashMap::new(),
            fndefs: HashMap::new(),
            type_tydefs,
            fndef_needs,
        }
    }

    pub fn empty(&self) -> ContextId {
        ContextId::new(0)
    }

    pub fn make_ctx(&mut self, context: Context) -> ContextId {
        let (i, _) = self.contexts.insert_full(context);
        ContextId::from_usize(i)
    }

    pub fn next_fn(&self) -> FnId {
        FnId::from_usize(self.fns.len())
    }

    fn make_ty(&mut self, t: Ty) -> TyId {
        let (i, _) = self.tys.insert_full(t);
        TyId::from_usize(i)
    }

    fn make_fn(&mut self, f: Fn) -> FnId {
        let (i, _) = self.fns.insert_full(f);
        FnId::from_usize(i)
    }

    fn make_val(&mut self, v: Val) -> ValId {
        let (i, _) = self.vals.insert_full(v);
        ValId::from_usize(i)
    }

    pub fn ty_string(&mut self) -> TyId {
        self.make_ty(Ty::String)
    }

    pub fn ty_bool(&mut self) -> TyId {
        self.make_ty(Ty::Bool)
    }

    pub fn ty_int32(&mut self) -> TyId {
        self.make_ty(Ty::Int32)
    }

    pub fn ty_int64(&mut self) -> TyId {
        self.make_ty(Ty::Int64)
    }

    pub fn ty_tuple(&mut self, elems: &[TyId]) -> TyId {
        let tuple = self.tuples.make(elems);
        self.make_ty(Ty::Tuple(tuple))
    }

    pub fn ty_structdef(&mut self, structdef: StructdefId, elems: &[TyId]) -> TyId {
        let tuple = self.tuples.make(elems);
        self.make_ty(Ty::Structdef(structdef, tuple))
    }

    pub fn ty_unit(&mut self) -> TyId {
        self.ty_tuple(&[])
    }

    pub fn fn_builtin(&mut self, builtin: BuiltinId) -> FnId {
        self.make_fn(Fn::Builtin(builtin))
    }

    pub fn val_bool(&mut self, b: bool) -> ValId {
        self.make_val(Val::Bool(b))
    }

    pub fn val_int32(&mut self, n: u32) -> ValId {
        self.make_val(Val::Int32(n))
    }

    pub fn val_string(&mut self, s: StrId) -> ValId {
        self.make_val(Val::String(s))
    }

    pub fn val_dynamic(&mut self, valdef: ValdefId, ty: TyId) -> ValId {
        self.make_val(Val::Dynamic(valdef, ty))
    }

    fn ty_prune(&mut self, ctx: ContextId, ty: TypeId) -> ContextId {
        let mut subcontext = Context::default();
        let context = &self[ctx];
        for def in self.type_tydefs.get(ty) {
            subcontext.set_ty(def, context.tydefs[&def]);
        }
        self.make_ctx(subcontext)
    }

    fn ty_pruned(&mut self, ctx: ContextId, ty: TypeId) -> TyId {
        if let Some(&t) = self.types.get(&(ctx, ty)) {
            return t;
        }
        // TODO: Make this not susceptible to stack overflows.
        let t = match self.ir.types[ty.index()] {
            Type::String => self.ty_string(),
            Type::Bool => self.ty_bool(),
            Type::Int32 => self.ty_int32(),
            Type::Int64 => self.ty_int64(),
            Type::Tuple(elems) => {
                let types = self.ir.tuples[elems]
                    .iter()
                    .map(|&elem| self.ty_pruned(ctx, elem))
                    .collect::<Vec<TyId>>();
                self.ty_tuple(&types)
            }
            Type::Tydef(tydef) => match self.ir.tydefs[tydef].def {
                Some(def) => self.ty_pruned(ctx, def),
                None => self[ctx].tydefs[&tydef],
            },
            Type::Structdef(structdef) => {
                let types = self.ir.fields[self.ir.structdefs[structdef].fields]
                    .iter()
                    .map(|&(_, field)| self.ty_pruned(ctx, field))
                    .collect::<Vec<TyId>>();
                self.ty_structdef(structdef, &types)
            }
        };
        self.types.insert((ctx, ty), t);
        t
    }

    pub fn ty(&mut self, ctx: ContextId, ty: TypeId) -> TyId {
        if let Some(&t) = self.types.get(&(ctx, ty)) {
            return t;
        }
        let ctx2 = self.ty_prune(ctx, ty);
        let t = self.ty_pruned(ctx2, ty);
        self.types.insert((ctx, ty), t);
        t
    }

    fn fndef_prune(&mut self, ctx: ContextId, fndef: FndefId) -> ContextId {
        let mut subcontext = Context::default();
        let context = &self[ctx];
        for def in self.fndef_needs.tydefs.get(fndef) {
            subcontext.set_ty(def, context.tydefs[&def]);
        }
        for def in self.fndef_needs.fndefs.get(fndef) {
            subcontext.set_fn(def, context.fndefs[&def]);
        }
        for def in self.fndef_needs.valdefs.get(fndef) {
            subcontext.set_val(def, context.valdefs[&def]);
        }
        self.make_ctx(subcontext)
    }

    fn fndef_pruned(&mut self, ctx: ContextId, fndef: FndefId) -> FnId {
        self.make_fn(Fn::Fndef(ctx, fndef))
    }

    pub fn fndef(&mut self, ctx: ContextId, fndef: FndefId) -> FnId {
        if let Some(&f) = self[ctx].fndefs.get(&fndef) {
            return f;
        }
        if let Some(&f) = self.fndefs.get(&(ctx, fndef)) {
            return f;
        }
        let ctx2 = self.fndef_prune(ctx, fndef);
        let f = self.fndef_pruned(ctx2, fndef);
        self.fndefs.insert((ctx, fndef), f);
        f
    }

    pub fn valdef(&mut self, ctx: ContextId, valdef: ValdefId) -> ValId {
        self[ctx].valdefs[&valdef]
    }

    pub fn fndef_valdefs(&self, fndef: FndefId) -> impl Iterator<Item = ValdefId> {
        self.fndef_needs.valdefs.get(fndef).into_iter()
    }
}

impl<'a> Index<ContextId> for Cache<'a> {
    type Output = Context;

    fn index(&self, id: ContextId) -> &Context {
        &self.contexts[id.index()]
    }
}

impl<'a> Index<TupleLoc> for Cache<'a> {
    type Output = TyId;

    fn index(&self, loc: TupleLoc) -> &TyId {
        &self.tuples[loc]
    }
}

impl<'a> Index<TupleRange> for Cache<'a> {
    type Output = [TyId];

    fn index(&self, range: TupleRange) -> &[TyId] {
        &self.tuples[range]
    }
}

impl<'a> Index<TyId> for Cache<'a> {
    type Output = Ty;

    fn index(&self, t: TyId) -> &Ty {
        &self.tys[t.index()]
    }
}

impl<'a> Index<FnId> for Cache<'a> {
    type Output = Fn;

    fn index(&self, f: FnId) -> &Fn {
        &self.fns[f.index()]
    }
}

impl<'a> Index<ValId> for Cache<'a> {
    type Output = Val;

    fn index(&self, v: ValId) -> &Val {
        &self.vals[v.index()]
    }
}
