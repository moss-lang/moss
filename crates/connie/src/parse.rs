use enumset::EnumSet;
use index_vec::{IndexVec, define_index_type};

use crate::{
    lex::{
        Token::{self, *},
        TokenId, Tokens,
    },
    util::IdRange,
};

define_index_type! {
    pub struct NameId = u32;
}

define_index_type! {
    pub struct TypeId = u32;
}

define_index_type! {
    pub struct ExprId = u32;
}

define_index_type! {
    pub struct StmtId = u32;
}

define_index_type! {
    pub struct DeclId = u32;
}

define_index_type! {
    pub struct CtxId = u32;
}

define_index_type! {
    pub struct RegionId = u32;
}

define_index_type! {
    pub struct NeedId = u32;
}

define_index_type! {
    pub struct FuncId = u32;
}

#[derive(Clone, Copy, Debug)]
pub struct Path {
    pub name: TokenId,
    pub names: IdRange<NameId>,
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Name(TokenId),
}

#[derive(Clone, Copy, Debug)]
pub enum Expr {
    Path(Path),
    String(TokenId),
    Call(ExprId, IdRange<ExprId>),
}

#[derive(Clone, Copy, Debug)]
pub enum Stmt {
    Provide(Path, ExprId),
    Expr(ExprId),
}

#[derive(Clone, Copy, Debug)]
pub struct Block {
    pub stmts: IdRange<StmtId>,
    pub expr: Option<ExprId>,
}

#[derive(Clone, Copy, Debug)]
pub enum Decl {
    Val { name: TokenId, ty: TypeId },
}

#[derive(Clone, Copy, Debug)]
pub struct Ctx {
    pub name: TokenId,
    pub decls: IdRange<DeclId>,
}

#[derive(Clone, Copy, Debug)]
pub struct Func {
    pub name: TokenId,
    pub body: Block,
}

#[derive(Clone, Copy, Debug)]
pub struct Region {
    pub ctxs: IdRange<CtxId>,
    pub regions: IdRange<RegionId>,
    pub needs: IdRange<NeedId>,
    pub funcs: IdRange<FuncId>,
}

#[derive(Debug)]
pub struct Tree {
    pub names: IndexVec<NameId, TokenId>,
    pub types: IndexVec<TypeId, Type>,
    pub exprs: IndexVec<ExprId, Expr>,
    pub stmts: IndexVec<StmtId, Stmt>,
    pub decls: IndexVec<DeclId, Decl>,
    pub ctxs: IndexVec<CtxId, Ctx>,
    pub regions: IndexVec<RegionId, Region>,
    pub needs: IndexVec<NeedId, TokenId>,
    pub funcs: IndexVec<FuncId, Func>,
    pub root: RegionId,
}

#[derive(Debug)]
pub enum ParseError {
    Expected { id: TokenId, tokens: EnumSet<Token> },
}

impl ParseError {
    pub fn message(&self) -> String {
        match self {
            ParseError::Expected { id: _, tokens } => format!(
                "expected {}",
                itertools::join(tokens.into_iter().map(|token| token.to_string()), " or ")
            ),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

struct Parser<'a> {
    tokens: &'a Tokens,
    id: TokenId,

    names: IndexVec<NameId, TokenId>,
    types: IndexVec<TypeId, Type>,
    exprs: IndexVec<ExprId, Expr>,
    stmts: IndexVec<StmtId, Stmt>,
    decls: IndexVec<DeclId, Decl>,
    ctxs: IndexVec<CtxId, Ctx>,
    regions: IndexVec<RegionId, Region>,
    needs: IndexVec<NeedId, TokenId>,
    funcs: IndexVec<FuncId, Func>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Tokens) -> Self {
        let id = TokenId::from(0);
        Self {
            tokens,
            id,

            names: Default::default(),
            types: Default::default(),
            exprs: Default::default(),
            stmts: Default::default(),
            decls: Default::default(),
            ctxs: Default::default(),
            regions: Default::default(),
            needs: Default::default(),
            funcs: Default::default(),
        }
    }

    fn peek(&self) -> Token {
        self.tokens[self.id]
    }

    fn next(&mut self) -> TokenId {
        if let Eof = self.peek() {
            panic!("unexpected end of file");
        }
        let id = self.id;
        self.id += 1;
        id
    }

    fn err(&self, tokens: EnumSet<Token>) -> ParseError {
        ParseError::Expected {
            id: self.id,
            tokens,
        }
    }

    fn expect(&mut self, kind: Token) -> ParseResult<TokenId> {
        let id = self.id;
        if self.peek() == kind {
            self.next();
            Ok(id)
        } else {
            Err(self.err(EnumSet::only(kind)))
        }
    }

    fn path(&mut self) -> ParseResult<Path> {
        let name = self.expect(Name)?;
        let mut names = Vec::new();
        while let ColonColon = self.peek() {
            self.next();
            names.push(self.expect(Name)?);
        }
        Ok(Path {
            name,
            names: IdRange::new(&mut self.names, names),
        })
    }

    fn ty(&mut self) -> ParseResult<Type> {
        match self.peek() {
            Name => Ok(Type::Name(self.next())),
            _ => Err(self.err(EnumSet::only(Name))),
        }
    }

    fn ty_id(&mut self) -> ParseResult<TypeId> {
        let ty = self.ty()?;
        Ok(self.types.push(ty))
    }

    fn expr_atom(&mut self) -> ParseResult<Expr> {
        match self.peek() {
            Str => Ok(Expr::String(self.next())),
            Name => Ok(Expr::Path(self.path()?)),
            _ => Err(self.err(Str | Name)),
        }
    }

    fn expr(&mut self) -> ParseResult<Expr> {
        let expr = self.expr_atom()?;
        match self.peek() {
            LParen => {
                self.next();
                let mut args = Vec::new();
                loop {
                    if let RParen = self.peek() {
                        break;
                    } else {
                        args.push(self.expr()?);
                        if let Comma = self.peek() {
                            self.next();
                        } else {
                            break;
                        }
                    }
                }
                self.expect(RParen)?;
                let callee = self.exprs.push(expr);
                Ok(Expr::Call(callee, IdRange::new(&mut self.exprs, args)))
            }
            _ => Ok(expr),
        }
    }

    fn expr_id(&mut self) -> ParseResult<ExprId> {
        let expr = self.expr()?;
        Ok(self.exprs.push(expr))
    }

    fn block(&mut self) -> ParseResult<Block> {
        self.expect(LBrace)?;
        let mut stmts = Vec::new();
        loop {
            match self.peek() {
                RBrace => {
                    self.next();
                    return Ok(Block {
                        stmts: IdRange::new(&mut self.stmts, stmts),
                        expr: None,
                    });
                }
                Provide => {
                    self.next();
                    let path = self.path()?;
                    self.expect(Equal)?;
                    let expr = self.expr_id()?;
                    self.expect(Semi)?;
                    stmts.push(Stmt::Provide(path, expr));
                }
                _ => {
                    let expr = self.expr_id()?;
                    match self.peek() {
                        Semi => {
                            self.next();
                            stmts.push(Stmt::Expr(expr));
                        }
                        RBrace => {
                            let stmts = IdRange::new(&mut self.stmts, stmts);
                            let expr = Some(expr);
                            return Ok(Block { stmts, expr });
                        }
                        _ => return Err(self.err(Semi | RBrace)),
                    }
                }
            }
        }
    }

    fn decl(&mut self) -> ParseResult<Decl> {
        match self.peek() {
            Name => {
                let name = self.next();
                self.expect(Colon)?;
                let ty = self.ty_id()?;
                self.expect(Semi)?;
                Ok(Decl::Val { name, ty })
            }
            _ => Err(self.err(EnumSet::only(Name))),
        }
    }

    fn decls(&mut self) -> ParseResult<IdRange<DeclId>> {
        self.expect(LBrace)?;
        let mut decls = Vec::new();
        loop {
            if let RBrace = self.peek() {
                break;
            }
            decls.push(self.decl()?);
        }
        self.expect(RBrace)?;
        Ok(IdRange::new(&mut self.decls, decls))
    }

    fn need(&mut self) -> ParseResult<TokenId> {
        self.expect(Need)?;
        let name = self.expect(Name)?;
        self.expect(Semi)?;
        Ok(name)
    }

    fn func(&mut self) -> ParseResult<Func> {
        self.expect(Fn)?;
        let name = self.expect(Name)?;
        self.expect(LParen)?;
        self.expect(RParen)?;
        let body = self.block()?;
        Ok(Func { name, body })
    }

    fn region(&mut self) -> ParseResult<Region> {
        let mut ctxs = Vec::new();
        let mut regions = Vec::new();
        let mut needs = Vec::new();
        let mut funcs = Vec::new();
        loop {
            match self.peek() {
                Context => {
                    self.next();
                    match self.peek() {
                        Start => {
                            self.next();
                            self.expect(Semi)?;
                            regions.push(self.region()?);
                        }
                        End => {
                            self.next();
                            self.expect(Semi)?;
                            return Ok(Region {
                                ctxs: IdRange::new(&mut self.ctxs, ctxs),
                                regions: IdRange::new(&mut self.regions, regions),
                                needs: IdRange::new(&mut self.needs, needs),
                                funcs: IdRange::new(&mut self.funcs, funcs),
                            });
                        }
                        Name => {
                            let name = self.next();
                            let decls = self.decls()?;
                            ctxs.push(Ctx { name, decls });
                        }
                        _ => return Err(self.err(Start | End | Name)),
                    }
                }
                Fn => funcs.push(self.func()?),
                Need => needs.push(self.need()?),
                _ => return Err(self.err(Context | Fn | Need)),
            }
        }
    }

    fn tree(mut self) -> ParseResult<Tree> {
        let mut ctxs = Vec::new();
        let mut regions = Vec::new();
        let mut needs = Vec::new();
        let mut funcs = Vec::new();
        loop {
            match self.peek() {
                Context => {
                    self.next();
                    match self.peek() {
                        Start => {
                            self.next();
                            self.expect(Semi)?;
                            regions.push(self.region()?);
                        }
                        Name => {
                            let name = self.next();
                            let decls = self.decls()?;
                            ctxs.push(Ctx { name, decls });
                        }
                        _ => return Err(self.err(Start | Name)),
                    }
                }
                Fn => funcs.push(self.func()?),
                Need => needs.push(self.need()?),
                Eof => {
                    let region = Region {
                        ctxs: IdRange::new(&mut self.ctxs, ctxs),
                        regions: IdRange::new(&mut self.regions, regions),
                        needs: IdRange::new(&mut self.needs, needs),
                        funcs: IdRange::new(&mut self.funcs, funcs),
                    };
                    let root = self.regions.push(region);
                    return Ok(Tree {
                        names: self.names,
                        types: self.types,
                        exprs: self.exprs,
                        stmts: self.stmts,
                        decls: self.decls,
                        ctxs: self.ctxs,
                        regions: self.regions,
                        needs: self.needs,
                        funcs: self.funcs,
                        root,
                    });
                }
                _ => return Err(self.err(Context | Fn | Need | Eof)),
            }
        }
    }
}

pub fn parse(tokens: &Tokens) -> ParseResult<Tree> {
    Parser::new(tokens).tree()
}
