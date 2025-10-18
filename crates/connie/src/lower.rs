use index_vec::{IndexVec, define_index_type};
use indexmap::IndexMap;
use logos::Logos;

use crate::{
    intern::{StrId, Strings},
    lex::{Token, TokenId, TokenStarts},
    parse::{self, Ctx, Region, RegionId, Tree},
    util::IdRange,
};

define_index_type! {
    pub struct PathId = u32;
}

define_index_type! {
    pub struct TypeId = u32;
}

define_index_type! {
    pub struct VarId = u32;
}

define_index_type! {
    pub struct InstrId = u32;
}

define_index_type! {
    pub struct FuncId = u32;
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Unit,
    String,
}

#[derive(Clone, Copy, Debug)]
pub struct Var {
    pub ty: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub enum Instr {
    String(StrId),
    Println(InstrId),
    Provide(VarId, InstrId, IdRange<InstrId>),
}

#[derive(Clone, Copy, Debug)]
pub struct Func {
    pub param: TypeId,
    pub result: TypeId,
    pub body: IdRange<InstrId>,
}

#[derive(Debug, Default)]
pub struct IR {
    pub strings: Strings,
    pub types: IndexVec<TypeId, Type>,
    pub vars: IndexVec<VarId, Var>,
    pub vals: IndexVec<InstrId, TypeId>,
    pub instrs: IndexVec<InstrId, Instr>,
    pub funcs: IndexVec<FuncId, Func>,
    pub main: Option<FuncId>,
}

type Path = Option<(PathId, StrId)>;

#[derive(Clone, Copy, Debug)]
enum Named {
    Scope,
    Type(TypeId),
    Var(VarId),
    Val(InstrId),
    Func(InstrId),
}

pub enum LowerError {}

type LowerResult<T> = Result<T, LowerError>;

struct Lower<'a> {
    source: &'a str,
    starts: &'a TokenStarts,
    tree: &'a Tree,
    ir: IR,
    paths: IndexMap<Path, Named>,
    path: PathId,
}

impl<'a> Lower<'a> {
    fn slice(&self, token: TokenId) -> &'a str {
        let start = self.starts[token].index();
        let (_, range) = Token::lexer(&self.source[start..])
            .spanned()
            .next()
            .unwrap();
        &self.source[(range.start + start)..(range.end + start)]
    }

    fn string(&mut self, token: TokenId) -> StrId {
        self.ir.strings.make(self.slice(token))
    }

    fn set(&mut self, token: TokenId, named: Named) -> PathId {
        let name = self.string(token);
        let (i, _) = self.paths.insert_full(Some((self.path, name)), named);
        PathId::from_usize(i)
    }

    fn set_scope(&mut self, token: TokenId) -> PathId {
        self.set(token, Named::Scope)
    }

    fn set_var(&mut self, token: TokenId, id: VarId) -> PathId {
        self.set(token, Named::Var(id))
    }

    fn region(&mut self, id: RegionId) {
        let Region { ctxs, regions, .. } = self.tree.regions[id];
        for ctx in ctxs {
            let Ctx { name, vals } = self.tree.ctxs[ctx];
            let scope = self.set_scope(name);
            for val in vals {
                let ty = match self.tree.types[self.tree.vals[val].ty] {
                    parse::Type::Name(name) => match self.slice(name) {
                        "String" => Type::String,
                        _ => todo!(),
                    },
                };
                self.ir.types.push(ty);
            }
        }
        for region in regions {
            self.region(region);
        }
    }

    fn program(mut self) -> LowerResult<IR> {
        self.region(self.tree.root);
        // TODO
        Ok(self.ir)
    }
}

pub fn lower(source: &str, starts: &TokenStarts, tree: &Tree) -> LowerResult<IR> {
    let mut paths = IndexMap::new();
    let (i, _) = paths.insert_full(None, Named::Scope);
    Lower {
        source,
        starts,
        tree,
        ir: IR::default(),
        paths,
        path: PathId::from_usize(i),
    }
    .program()
}
