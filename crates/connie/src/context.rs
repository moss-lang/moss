use index_vec::define_index_type;

use crate::lower::{FndefId, IR, TydefId, TypeId, ValdefId};

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

pub struct Cache<'a> {
    ir: &'a IR,
}

impl<'a> Cache<'a> {
    pub fn new(ir: &'a IR) -> Self {
        Self { ir }
    }

    pub fn empty(&self) -> ContextId {
        todo!()
    }

    pub fn bind_ty(&mut self, ctx: ContextId, def: TydefId, bind: TypeId) -> ContextId {
        todo!()
    }

    pub fn bind_fn(&mut self, ctx: ContextId, def: FndefId, bind: FndefId) -> ContextId {
        todo!()
    }

    pub fn bind_val(&mut self, ctx: ContextId, def: ValdefId, bind: ValdefId) -> ContextId {
        todo!()
    }

    pub fn get_ty(&mut self, ctx: ContextId, ty: TypeId) -> TyId {
        todo!()
    }

    pub fn get_fn(&mut self, ctx: ContextId, fndef: FndefId) -> FnId {
        todo!()
    }

    pub fn get_val(&mut self, ctx: ContextId, valdef: ValdefId) -> ValId {
        todo!()
    }
}
