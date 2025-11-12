use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    ops::{AddAssign, BitXorAssign, Index, SubAssign},
};

use index_vec::{Idx, IndexVec, define_index_type, index_vec};
use indexmap::IndexSet;

use crate::{
    lower::{CtxdefId, FndefId, IR, Needs, StructdefId, TydefId, Type, TypeId, ValdefId},
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

struct NeedSets<I> {
    tydefs: Subsets<TydefId, I>,
    fndefs: Subsets<FndefId, I>,
    valdefs: Subsets<ValdefId, I>,
    ctxdefs: Subsets<CtxdefId, I>,
}

impl<I: Idx> NeedSets<I> {
    fn new(ir: &IR, len: I) -> Self {
        let mut tydefs = Subsets::new(ir.tydefs.len_idx());
        let mut fndefs = Subsets::new(ir.fndefs.len_idx());
        let mut valdefs = Subsets::new(ir.valdefs.len_idx());
        let mut ctxdefs = Subsets::new(ir.ctxdefs.len_idx());
        for _ in 0..len.index() {
            tydefs.push();
            fndefs.push();
            valdefs.push();
            ctxdefs.push();
        }
        Self {
            tydefs,
            fndefs,
            valdefs,
            ctxdefs,
        }
    }
}

impl<I> NeedSets<I> {
    fn direct(
        ir: &IR,
        type_tydefs: &Subsets<TydefId, TypeId>,
        items: impl IntoIterator<Item = (I, Needs)> + ExactSizeIterator,
    ) -> Self {
        let mut tydefs = Subsets::new(ir.tydefs.len_idx());
        let mut fndefs = Subsets::new(ir.fndefs.len_idx());
        let mut valdefs = Subsets::new(ir.valdefs.len_idx());
        let mut ctxdefs = Subsets::new(ir.ctxdefs.len_idx());
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
            for need_ty in tys.get(&ir.need_tys) {
                set_tydefs |= type_tydefs.get(need_ty.id);
            }
            let (_, mut set_fndefs) = fndefs.push();
            for need_fn in fns.get(&ir.need_fns) {
                set_fndefs.include(need_fn.id);
            }
            let (_, mut set_valdefs) = valdefs.push();
            for need_val in vals.get(&ir.need_vals) {
                set_valdefs.include(need_val.id);
            }
            let (_, mut set_ctxdefs) = ctxdefs.push();
            for need_ctx in ctxs.get(&ir.need_ctxs) {
                set_ctxdefs.include(need_ctx.id);
            }
        }
        Self {
            tydefs,
            fndefs,
            valdefs,
            ctxdefs,
        }
    }
}

#[derive(Clone, Copy)]
enum DefId {
    Fn(FndefId),
    Val(ValdefId),
    Ctx(CtxdefId),
}

struct Downstream {
    fndefs: IndexVec<FndefId, u32>,
    valdefs: IndexVec<ValdefId, u32>,
    ctxdefs: IndexVec<CtxdefId, u32>,
}

impl Downstream {
    fn new(ir: &IR) -> Self {
        Self {
            fndefs: index_vec![0; ir.fndefs.len()],
            valdefs: index_vec![0; ir.valdefs.len()],
            ctxdefs: index_vec![0; ir.ctxdefs.len()],
        }
    }

    fn accumulate<I>(&mut self, needs: &NeedSets<I>) {
        for defs in needs.fndefs.iter() {
            for def in defs {
                self.fndefs[def] += 1;
            }
        }
        for defs in needs.valdefs.iter() {
            for def in defs {
                self.valdefs[def] += 1;
            }
        }
        for defs in needs.ctxdefs.iter() {
            for def in defs {
                self.ctxdefs[def] += 1;
            }
        }
    }

    fn decrement<I: Idx>(&mut self, needs: &NeedSets<I>, i: I, mut f: impl FnMut(DefId)) {
        for def in needs.fndefs.get(i) {
            let pred = &mut self.fndefs[def];
            *pred -= 1;
            if *pred == 0 {
                f(DefId::Fn(def))
            }
        }
        for def in needs.valdefs.get(i) {
            let pred = &mut self.valdefs[def];
            *pred -= 1;
            if *pred == 0 {
                f(DefId::Val(def))
            }
        }
        for def in needs.ctxdefs.get(i) {
            let pred = &mut self.ctxdefs[def];
            *pred -= 1;
            if *pred == 0 {
                f(DefId::Ctx(def))
            }
        }
    }
}

fn topological_sort(
    ir: &IR,
    fndefs: &NeedSets<FndefId>,
    valdefs: &NeedSets<ValdefId>,
    ctxdefs: &NeedSets<CtxdefId>,
) -> Vec<DefId> {
    // Implementation adapted from here:
    // https://github.com/penrose/penrose/blob/v3.3.0/packages/core/src/utils/Graph.ts#L142-L168
    let mut sorted = Vec::new();
    let mut downstream = Downstream::new(ir);
    downstream.accumulate(fndefs);
    downstream.accumulate(valdefs);
    downstream.accumulate(ctxdefs);
    let mut stack = Vec::new();
    for (def, &succ) in downstream.fndefs.iter_enumerated() {
        if succ == 0 {
            stack.push(DefId::Fn(def));
        }
    }
    for (def, &succ) in downstream.valdefs.iter_enumerated() {
        if succ == 0 {
            stack.push(DefId::Val(def));
        }
    }
    for (def, &succ) in downstream.ctxdefs.iter_enumerated() {
        if succ == 0 {
            stack.push(DefId::Ctx(def));
        }
    }
    while let Some(def) = stack.pop() {
        sorted.push(def);
        match def {
            DefId::Fn(fndef) => downstream.decrement(fndefs, fndef, |id| stack.push(id)),
            DefId::Val(valdef) => downstream.decrement(valdefs, valdef, |id| stack.push(id)),
            DefId::Ctx(ctxdef) => downstream.decrement(ctxdefs, ctxdef, |id| stack.push(id)),
        }
    }
    let m = sorted.len();
    let n = ir.fndefs.len() + ir.valdefs.len() + ir.ctxdefs.len();
    if m != n {
        panic!("could only sort {m} nodes out of {n} total");
    }
    sorted.reverse();
    sorted
}

fn transitive_closure(
    ir: &IR,
    fndef_direct: &NeedSets<FndefId>,
    valdef_direct: &NeedSets<ValdefId>,
    ctxdef_direct: &NeedSets<CtxdefId>,
) -> (NeedSets<FndefId>, NeedSets<ValdefId>, NeedSets<CtxdefId>) {
    let mut fndefs = NeedSets::new(ir, ir.fndefs.len_idx());
    let mut valdefs = NeedSets::new(ir, ir.valdefs.len_idx());
    let mut ctxdefs = NeedSets::new(ir, ir.ctxdefs.len_idx());
    for def in topological_sort(ir, fndef_direct, valdef_direct, ctxdef_direct) {
        match def {
            DefId::Fn(fndef) => {
                let (view_tydefs, mut set_tydefs) = fndefs.tydefs.get_mut(fndef);
                let (view_fndefs, mut set_fndefs) = fndefs.fndefs.get_mut(fndef);
                let (view_valdefs, mut set_valdefs) = fndefs.valdefs.get_mut(fndef);
                let (view_ctxdefs, mut set_ctxdefs) = fndefs.ctxdefs.get_mut(fndef);
                let direct_tydefs = fndef_direct.tydefs.get(fndef);
                let direct_fndefs = fndef_direct.fndefs.get(fndef);
                let direct_valdefs = fndef_direct.valdefs.get(fndef);
                let direct_ctxdefs = fndef_direct.ctxdefs.get(fndef);
                set_tydefs |= direct_tydefs;
                set_fndefs |= direct_fndefs;
                set_valdefs |= direct_valdefs;
                set_ctxdefs |= direct_ctxdefs;
                for dep in direct_fndefs {
                    set_tydefs |= view_tydefs.get(dep);
                    set_fndefs |= view_fndefs.get(dep);
                    set_valdefs |= view_valdefs.get(dep);
                    set_ctxdefs |= view_ctxdefs.get(dep);
                }
                for dep in direct_valdefs {
                    set_tydefs |= valdefs.tydefs.get(dep);
                    set_fndefs |= valdefs.fndefs.get(dep);
                    set_valdefs |= valdefs.valdefs.get(dep);
                    set_ctxdefs |= valdefs.ctxdefs.get(dep);
                }
                for dep in direct_ctxdefs {
                    set_tydefs |= ctxdefs.tydefs.get(dep);
                    set_fndefs |= ctxdefs.fndefs.get(dep);
                    set_valdefs |= ctxdefs.valdefs.get(dep);
                    set_ctxdefs |= ctxdefs.ctxdefs.get(dep);
                }
            }
            DefId::Val(valdef) => {
                let (view_tydefs, mut set_tydefs) = valdefs.tydefs.get_mut(valdef);
                let (view_fndefs, mut set_fndefs) = valdefs.fndefs.get_mut(valdef);
                let (view_valdefs, mut set_valdefs) = valdefs.valdefs.get_mut(valdef);
                let (view_ctxdefs, mut set_ctxdefs) = valdefs.ctxdefs.get_mut(valdef);
                let direct_tydefs = valdef_direct.tydefs.get(valdef);
                let direct_fndefs = valdef_direct.fndefs.get(valdef);
                let direct_valdefs = valdef_direct.valdefs.get(valdef);
                let direct_ctxdefs = valdef_direct.ctxdefs.get(valdef);
                set_tydefs |= direct_tydefs;
                set_fndefs |= direct_fndefs;
                set_valdefs |= direct_valdefs;
                set_ctxdefs |= direct_ctxdefs;
                for dep in direct_fndefs {
                    set_tydefs |= fndefs.tydefs.get(dep);
                    set_fndefs |= fndefs.fndefs.get(dep);
                    set_valdefs |= fndefs.valdefs.get(dep);
                    set_ctxdefs |= fndefs.ctxdefs.get(dep);
                }
                for dep in direct_valdefs {
                    set_tydefs |= view_tydefs.get(dep);
                    set_fndefs |= view_fndefs.get(dep);
                    set_valdefs |= view_valdefs.get(dep);
                    set_ctxdefs |= view_ctxdefs.get(dep);
                }
                for dep in direct_ctxdefs {
                    set_tydefs |= ctxdefs.tydefs.get(dep);
                    set_fndefs |= ctxdefs.fndefs.get(dep);
                    set_valdefs |= ctxdefs.valdefs.get(dep);
                    set_ctxdefs |= ctxdefs.ctxdefs.get(dep);
                }
            }
            DefId::Ctx(ctxdef) => {
                let (view_tydefs, mut set_tydefs) = ctxdefs.tydefs.get_mut(ctxdef);
                let (view_fndefs, mut set_fndefs) = ctxdefs.fndefs.get_mut(ctxdef);
                let (view_valdefs, mut set_valdefs) = ctxdefs.valdefs.get_mut(ctxdef);
                let (view_ctxdefs, mut set_ctxdefs) = ctxdefs.ctxdefs.get_mut(ctxdef);
                let direct_tydefs = ctxdef_direct.tydefs.get(ctxdef);
                let direct_fndefs = ctxdef_direct.fndefs.get(ctxdef);
                let direct_valdefs = ctxdef_direct.valdefs.get(ctxdef);
                let direct_ctxdefs = ctxdef_direct.ctxdefs.get(ctxdef);
                set_tydefs |= direct_tydefs;
                set_fndefs |= direct_fndefs;
                set_valdefs |= direct_valdefs;
                set_ctxdefs |= direct_ctxdefs;
                for dep in direct_fndefs {
                    set_tydefs |= fndefs.tydefs.get(dep);
                    set_fndefs |= fndefs.fndefs.get(dep);
                    set_valdefs |= fndefs.valdefs.get(dep);
                    set_ctxdefs |= fndefs.ctxdefs.get(dep);
                }
                for dep in direct_valdefs {
                    set_tydefs |= valdefs.tydefs.get(dep);
                    set_fndefs |= valdefs.fndefs.get(dep);
                    set_valdefs |= valdefs.valdefs.get(dep);
                    set_ctxdefs |= valdefs.ctxdefs.get(dep);
                }
                for dep in direct_ctxdefs {
                    set_tydefs |= view_tydefs.get(dep);
                    set_fndefs |= view_fndefs.get(dep);
                    set_valdefs |= view_valdefs.get(dep);
                    set_ctxdefs |= view_ctxdefs.get(dep);
                }
            }
        }
    }
    (fndefs, valdefs, ctxdefs)
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
    Int32(i32),
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

    /// Bindable tydefs needed by each abstract type.
    type_tydefs: Subsets<TydefId, TypeId>,

    /// Bindable tydefs needed by each fndef.
    fndef_tydefs: Subsets<TydefId, FndefId>,

    /// Bindable fndefs needed by each fndef.
    fndef_fndefs: Subsets<FndefId, FndefId>,

    /// Bindable valdefs needed by each fndef.
    fndef_valdefs: Subsets<ValdefId, FndefId>,
}

impl<'a> Cache<'a> {
    pub fn new(ir: &'a IR) -> Self {
        let mut contexts = IndexSet::new();
        contexts.insert(Context::default());
        // TODO: Represent all these subsets and sparse graphs in a non-quadratic way.
        let mut type_tydefs = Subsets::new(ir.tydefs.len_idx());
        for &ty in &ir.types {
            let (prev, mut set) = type_tydefs.push();
            match ty {
                Type::String | Type::Bool | Type::Int32 => {}
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
        let fndefs = NeedSets::direct(
            ir,
            &type_tydefs,
            ir.fndefs.iter_enumerated().map(|(i, def)| (i, def.needs)),
        );
        let valdefs = NeedSets::direct(
            ir,
            &type_tydefs,
            ir.valdefs.iter_enumerated().map(|(i, def)| (i, def.needs)),
        );
        let ctxdefs = NeedSets::direct(
            ir,
            &type_tydefs,
            ir.ctxdefs.iter_enumerated().map(|(i, def)| (i, def.needs)),
        );
        let (fndefs, _, _) = transitive_closure(ir, &fndefs, &valdefs, &ctxdefs);
        Self {
            ir,
            contexts,
            tuples: Tuples::new(),
            tys: IndexSet::new(),
            fns: IndexSet::new(),
            vals: IndexSet::new(),
            types: HashMap::new(),
            type_tydefs,
            fndef_tydefs: fndefs.tydefs,
            fndef_fndefs: fndefs.fndefs,
            fndef_valdefs: fndefs.valdefs,
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

    pub fn fn_fndef(&mut self, ctx: ContextId, fndef: FndefId) -> FnId {
        self.make_fn(Fn::Fndef(ctx, fndef))
    }

    pub fn val_dynamic(&mut self, valdef: ValdefId, ty: TyId) -> ValId {
        self.make_val(Val::Dynamic(valdef, ty))
    }

    fn ty_prune(&mut self, ctx: ContextId, ty: TypeId) -> ContextId {
        let mut subcontext = Context::default();
        let context = &self[ctx];
        for tydef in self.type_tydefs.get(ty) {
            subcontext.set_ty(tydef, context.tydefs[&tydef]);
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
            Type::Tuple(elems) => {
                let types = self.ir.tuples[elems]
                    .iter()
                    .map(|&elem| self.ty_pruned(ctx, elem))
                    .collect::<Vec<TyId>>();
                self.ty_tuple(&types)
            }
            Type::Tydef(tydef) => match self.ir.tydefs[tydef].def {
                Some(def) => self.ty_pruned(ctx, def),
                None => self.contexts[ctx.index()].tydefs[&tydef],
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

    pub fn fndef(&mut self, _ctx: ContextId, _fndef: FndefId) -> FnId {
        todo!()
    }

    pub fn valdef(&mut self, _ctx: ContextId, _valdef: ValdefId) -> ValId {
        todo!()
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
