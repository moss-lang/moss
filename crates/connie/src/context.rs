use std::{
    hash::{Hash, Hasher},
    ops::{AddAssign, BitXorAssign, SubAssign},
};

use index_vec::define_index_type;
use indexmap::IndexSet;

use crate::{
    lower::{FndefId, IR, TydefId, TypeId, ValdefId},
    util::default_hash,
};

define_index_type! {
    pub struct ContextId = u32;
}

define_index_type! {
    pub struct TyId = u32;
}

define_index_type! {
    pub struct FnId = u32;
}

define_index_type! {
    pub struct ValId = u32;
}

#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
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

#[derive(Clone, Copy, Eq, Hash, PartialEq)]
pub enum Constant {
    Int32(i32),
}

#[derive(Clone, Default, Eq, PartialEq)]
struct Context {
    tydefs: im_rc::HashMap<TydefId, TypeId>,
    fndefs: im_rc::HashMap<FndefId, FndefId>,
    valdefs: im_rc::HashMap<ValdefId, Option<Constant>>,
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
    fn set_ty(&mut self, def: TydefId, bind: TypeId) {
        if let Some(prev) = self.tydefs.insert(def, bind) {
            self.hash_tydefs -= default_hash(&(def, prev));
        }
        self.hash_tydefs += default_hash(&(def, bind));
    }

    fn set_fn(&mut self, def: FndefId, bind: FndefId) {
        if let Some(prev) = self.fndefs.insert(def, bind) {
            self.hash_fndefs -= default_hash(&(def, prev));
        }
        self.hash_fndefs += default_hash(&(def, bind));
    }

    fn set_val(&mut self, def: ValdefId, bind: Option<Constant>) {
        if let Some(prev) = self.valdefs.insert(def, bind) {
            self.hash_valdefs -= default_hash(&(def, prev));
        }
        self.hash_valdefs += default_hash(&(def, bind));
    }
}

pub struct Cache<'a> {
    ir: &'a IR,
    contexts: IndexSet<Context>,
}

impl<'a> Cache<'a> {
    pub fn new(ir: &'a IR) -> Self {
        let mut contexts = IndexSet::new();
        contexts.insert(Context::default());
        Self { ir, contexts }
    }

    pub fn empty(&self) -> ContextId {
        ContextId::new(0)
    }

    fn bind(&mut self, ctx: ContextId, f: impl FnOnce(&mut Context)) -> ContextId {
        let mut context = self.contexts[ctx.index()].clone();
        f(&mut context);
        let (i, _) = self.contexts.insert_full(context);
        ContextId::from_usize(i)
    }

    pub fn bind_ty(&mut self, ctx: ContextId, def: TydefId, bind: TypeId) -> ContextId {
        self.bind(ctx, |context| context.set_ty(def, bind))
    }

    pub fn bind_fn(&mut self, ctx: ContextId, def: FndefId, bind: FndefId) -> ContextId {
        self.bind(ctx, |context| context.set_fn(def, bind))
    }

    pub fn bind_val(&mut self, ctx: ContextId, def: ValdefId, bind: Option<Constant>) -> ContextId {
        self.bind(ctx, |context| context.set_val(def, bind))
    }

    pub fn bound_ty(&self, _ctx: ContextId, _def: TydefId) -> TypeId {
        todo!()
    }

    pub fn bound_fn(&self, _ctx: ContextId, _def: FndefId) -> FndefId {
        todo!()
    }

    pub fn bound_val(&self, _ctx: ContextId, _def: ValdefId) -> Option<Constant> {
        todo!()
    }

    pub fn index_ty(&mut self, _ctx: ContextId, _ty: TypeId) -> TyId {
        todo!()
    }

    pub fn index_fn(&mut self, _ctx: ContextId, _fndef: FndefId) -> FnId {
        todo!()
    }

    pub fn index_val(&mut self, _ctx: ContextId, _valdef: ValdefId) -> ValId {
        todo!()
    }
}
