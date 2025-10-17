use index_vec::{IndexVec, define_index_type};
use logos::Logos;

use crate::{
    intern::{StrId, Strings},
    lex::{Token, TokenId, TokenStarts},
    parse::{Region, RegionId, Tree},
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

pub type Path = Option<(PathId, StrId)>;

#[derive(Clone, Copy, Debug)]
pub enum Type {
    String,
}

#[derive(Clone, Copy, Debug)]
pub struct Var {
    pub ty: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub enum Instr {
    String(StrId),
    Print(InstrId),
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
    pub paths: IndexVec<PathId, Path>,
    pub types: IndexVec<TypeId, Type>,
    pub vars: IndexVec<VarId, Var>,
    pub instrs: IndexVec<InstrId, Instr>,
    pub funcs: IndexVec<FuncId, Func>,
    pub main: Option<FuncId>,
}

pub enum LowerError {}

type LowerResult<T> = Result<T, LowerError>;

struct Lower<'a> {
    source: &'a str,
    starts: &'a TokenStarts,
    tree: &'a Tree,
    ir: IR,
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

    fn region(&self, id: RegionId) {
        let Region { ctxs, regions, .. } = self.tree.regions[id];
        for ctx in ctxs {
            for val in self.tree.ctxs[ctx].vals {
                println!("{:?}", self.tree.vals[val]);
            }
        }
        for region in regions {
            self.region(region);
        }
    }

    fn program(self) -> LowerResult<IR> {
        self.region(self.tree.root);
        // TODO
        Ok(self.ir)
    }
}

pub fn lower(source: &str, starts: &TokenStarts, tree: &Tree) -> LowerResult<IR> {
    Lower {
        source,
        starts,
        tree,
        ir: IR::default(),
    }
    .program()
}
