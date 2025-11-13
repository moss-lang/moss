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
    pub struct NeedId = u32;
}

define_index_type! {
    pub struct BindId = u32;
}

define_index_type! {
    pub struct ParamId = u32;
}

define_index_type! {
    pub struct FieldId = u32;
}

define_index_type! {
    pub struct ExprId = u32;
}

define_index_type! {
    pub struct StmtId = u32;
}

define_index_type! {
    pub struct ImportId = u32;
}

define_index_type! {
    pub struct TydefId = u32;
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
    pub struct StructdefId = u32;
}

#[derive(Clone, Copy, Debug)]
pub struct Path {
    pub prefix: IdRange<NameId>,
    pub last: TokenId,
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Path(Path),
}

#[derive(Clone, Copy, Debug)]
pub enum NeedKind {
    Default,
    Static,
}

#[derive(Clone, Copy, Debug)]
pub struct Need {
    pub kind: NeedKind,
    pub path: Path,
}

#[derive(Clone, Copy, Debug)]
pub struct Bind {
    pub path: Path,
    pub val: ExprId,
}

#[derive(Clone, Copy, Debug)]
pub struct Param {
    pub name: TokenId,
    pub ty: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Field {
    pub name: TokenId,
    pub val: ExprId,
}

#[derive(Clone, Copy, Debug)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Neq,
    Less,
}

#[derive(Clone, Copy, Debug)]
pub enum Expr {
    This(TokenId),
    Path(Path),
    Int(TokenId),
    String(TokenId),
    Struct(Path, IdRange<FieldId>),
    Field(ExprId, TokenId),
    Method(ExprId, TokenId, IdRange<ExprId>),
    Call(Path, IdRange<BindId>, IdRange<ExprId>),
    Binary(ExprId, Binop, ExprId),
    If(ExprId, Block, Option<Block>),
}

#[derive(Clone, Copy, Debug)]
pub enum Stmt {
    Let(TokenId, ExprId),
    Var(TokenId, ExprId),
    Assign(ExprId, ExprId),
    While(ExprId, Block),
    Expr(ExprId),
}

#[derive(Clone, Copy, Debug)]
pub struct Block {
    pub stmts: IdRange<StmtId>,
    pub expr: Option<ExprId>,
}

#[derive(Clone, Copy, Debug)]
pub struct Import {
    pub from: TokenId,
    pub name: Option<TokenId>,
    pub using: IdRange<NameId>,
}

#[derive(Clone, Copy, Debug)]
pub struct Tydef {
    pub name: TokenId,
    pub def: Option<TypeId>,
}

#[derive(Clone, Copy, Debug)]
pub struct Fndef {
    pub ty: Option<TokenId>,
    pub name: TokenId,
    pub needs: IdRange<NeedId>,
    pub params: IdRange<ParamId>,
    pub result: Option<TypeId>,
    pub def: Option<Block>,
}

#[derive(Clone, Copy, Debug)]
pub struct Valdef {
    pub name: TokenId,
    pub needs: IdRange<NeedId>,
    pub ty: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Ctxdef {
    pub name: TokenId,
    pub def: IdRange<NeedId>,
}

#[derive(Clone, Copy, Debug)]
pub struct Structdef {
    pub name: TokenId,
    pub fields: IdRange<ParamId>,
}

#[derive(Debug, Default)]
pub struct Tree {
    pub names: IndexVec<NameId, TokenId>,
    pub types: IndexVec<TypeId, Type>,
    pub needs: IndexVec<NeedId, Need>,
    pub binds: IndexVec<BindId, Bind>,
    pub params: IndexVec<ParamId, Param>,
    pub fields: IndexVec<FieldId, Field>,
    pub exprs: IndexVec<ExprId, Expr>,
    pub stmts: IndexVec<StmtId, Stmt>,
    pub imports: IndexVec<ImportId, Import>,
    pub tydefs: IndexVec<TydefId, Tydef>,
    pub fndefs: IndexVec<FndefId, Fndef>,
    pub valdefs: IndexVec<ValdefId, Valdef>,
    pub ctxdefs: IndexVec<CtxdefId, Ctxdef>,
    pub structdefs: IndexVec<StructdefId, Structdef>,
}

#[derive(Clone, Copy, Debug)]
pub enum ParseError {
    Expected { id: TokenId, tokens: EnumSet<Token> },
}

impl ParseError {
    pub fn message(self) -> String {
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
    tree: Tree,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Tokens) -> Self {
        let id = TokenId::from(0);
        Self {
            tokens,
            id,
            tree: Tree::default(),
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
        let mut prefix = Vec::new();
        let mut last = self.expect(Name)?;
        while let ColonColon = self.peek() {
            self.next();
            prefix.push(last);
            last = self.expect(Name)?;
        }
        Ok(Path {
            prefix: IdRange::new(&mut self.tree.names, prefix),
            last,
        })
    }

    fn ty(&mut self) -> ParseResult<Type> {
        match self.peek() {
            Name => Ok(Type::Path(self.path()?)),
            _ => Err(self.err(EnumSet::only(Name))),
        }
    }

    fn ty_id(&mut self) -> ParseResult<TypeId> {
        let ty = self.ty()?;
        Ok(self.tree.types.push(ty))
    }

    fn need(&mut self) -> ParseResult<Need> {
        let kind = match self.peek() {
            Static => {
                self.next();
                NeedKind::Static
            }
            _ => NeedKind::Default,
        };
        let path = self.path()?;
        Ok(Need { kind, path })
    }

    fn needs(&mut self) -> ParseResult<Vec<Need>> {
        self.expect(LBracket)?;
        let mut needs = Vec::new();
        loop {
            if let RBracket = self.peek() {
                self.next();
                return Ok(needs);
            }
            needs.push(self.need()?);
            if let Comma = self.peek() {
                self.next();
            }
        }
    }

    fn need_ids(&mut self) -> ParseResult<IdRange<NeedId>> {
        let needs = self.needs()?;
        Ok(IdRange::new(&mut self.tree.needs, needs))
    }

    fn bind(&mut self) -> ParseResult<Bind> {
        let path = self.path()?;
        self.expect(Equal)?;
        let val = self.expr_id()?;
        Ok(Bind { path, val })
    }

    fn param(&mut self) -> ParseResult<Param> {
        let name = self.expect(Name)?;
        self.expect(Colon)?;
        let ty = self.ty_id()?;
        Ok(Param { name, ty })
    }

    fn field(&mut self) -> ParseResult<Field> {
        let name = self.expect(Name)?;
        let val = match self.peek() {
            Equal => {
                self.next();
                self.expr_id()?
            }
            _ => {
                let prefix = IdRange::new(&mut self.tree.names, Vec::new());
                let last = name;
                self.tree.exprs.push(Expr::Path(Path { prefix, last }))
            }
        };
        Ok(Field { name, val })
    }

    fn args(&mut self) -> ParseResult<Vec<Expr>> {
        self.expect(LParen)?;
        let mut args = Vec::new();
        loop {
            if let RParen = self.peek() {
                self.next();
                return Ok(args);
            }
            args.push(self.expr()?);
            if let Comma = self.peek() {
                self.next();
            }
        }
    }

    fn arg_ids(&mut self) -> ParseResult<IdRange<ExprId>> {
        let args = self.args()?;
        Ok(IdRange::new(&mut self.tree.exprs, args))
    }

    fn expr_atom(&mut self) -> ParseResult<Expr> {
        match self.peek() {
            LParen => {
                self.next();
                let expr = self.expr()?;
                self.expect(RParen)?;
                Ok(expr)
            }
            If => {
                self.next();
                let cond = self.expr_id()?;
                let yes = self.block()?;
                let no = match self.peek() {
                    Else => {
                        self.next();
                        Some(self.block()?)
                    }
                    _ => None,
                };
                Ok(Expr::If(cond, yes, no))
            }
            This => Ok(Expr::This(self.next())),
            Int => Ok(Expr::Int(self.next())),
            Str => Ok(Expr::String(self.next())),
            Name => {
                let path = self.path()?;
                match self.peek() {
                    LParen => {
                        let binds = IdRange::new(&mut self.tree.binds, Vec::new());
                        let args = self.arg_ids()?;
                        Ok(Expr::Call(path, binds, args))
                    }
                    LBracket => {
                        self.next();
                        let mut binds = Vec::new();
                        loop {
                            if let RBracket = self.peek() {
                                self.next();
                                break;
                            }
                            binds.push(self.bind()?);
                            if let Comma = self.peek() {
                                self.next();
                            }
                        }
                        let binds = IdRange::new(&mut self.tree.binds, Vec::new());
                        let args = self.arg_ids()?;
                        Ok(Expr::Call(path, binds, args))
                    }
                    LBrace => {
                        self.next();
                        let mut fields = Vec::new();
                        loop {
                            if let RBrace = self.peek() {
                                self.next();
                                let fields = IdRange::new(&mut self.tree.fields, fields);
                                return Ok(Expr::Struct(path, fields));
                            }
                            fields.push(self.field()?);
                            if let Comma = self.peek() {
                                self.next();
                            }
                        }
                    }
                    _ => Ok(Expr::Path(path)),
                }
            }
            _ => Err(self.err(LParen | If | This | Int | Str | Name)),
        }
    }

    fn expr_chain(&mut self) -> ParseResult<Expr> {
        let mut expr = self.expr_atom()?;
        while let Dot = self.peek() {
            self.next();
            let object = self.tree.exprs.push(expr);
            let name = self.expect(Name)?;
            match self.peek() {
                LParen => {
                    let args = self.arg_ids()?;
                    expr = Expr::Method(object, name, args);
                }
                _ => expr = Expr::Field(object, name),
            }
        }
        Ok(expr)
    }

    fn expr_factor(&mut self) -> ParseResult<Expr> {
        self.expr_chain()
    }

    fn expr_term(&mut self) -> ParseResult<Expr> {
        let mut lhs = self.expr_factor()?;
        loop {
            let op = match self.peek() {
                Star => Binop::Mul,
                Slash => Binop::Div,
                _ => break,
            };
            self.next();
            let rhs = self.expr_factor()?;
            lhs = Expr::Binary(self.tree.exprs.push(lhs), op, self.tree.exprs.push(rhs));
        }
        Ok(lhs)
    }

    fn expr_quant(&mut self) -> ParseResult<Expr> {
        let mut lhs = self.expr_term()?;
        loop {
            let op = match self.peek() {
                Plus => Binop::Add,
                Hyphen => Binop::Sub,
                _ => break,
            };
            self.next();
            let rhs = self.expr_term()?;
            lhs = Expr::Binary(self.tree.exprs.push(lhs), op, self.tree.exprs.push(rhs));
        }
        Ok(lhs)
    }

    fn expr_comp(&mut self) -> ParseResult<Expr> {
        let lhs = self.expr_quant()?;
        let op = match self.peek() {
            ExclamEqual => Some(Binop::Neq),
            Less => Some(Binop::Less),
            _ => None,
        };
        match op {
            None => Ok(lhs),
            Some(op) => {
                self.next();
                let rhs = self.expr_quant()?;
                Ok(Expr::Binary(
                    self.tree.exprs.push(lhs),
                    op,
                    self.tree.exprs.push(rhs),
                ))
            }
        }
    }

    fn expr(&mut self) -> ParseResult<Expr> {
        self.expr_comp()
    }

    fn expr_id(&mut self) -> ParseResult<ExprId> {
        let expr = self.expr()?;
        Ok(self.tree.exprs.push(expr))
    }

    fn block(&mut self) -> ParseResult<Block> {
        self.expect(LBrace)?;
        let mut stmts = Vec::new();
        loop {
            match self.peek() {
                RBrace => {
                    self.next();
                    return Ok(Block {
                        stmts: IdRange::new(&mut self.tree.stmts, stmts),
                        expr: None,
                    });
                }
                Let => {
                    self.next();
                    let name = self.expect(Name)?;
                    self.expect(Equal)?;
                    let expr = self.expr_id()?;
                    self.expect(Semi)?;
                    stmts.push(Stmt::Let(name, expr));
                }
                Var => {
                    self.next();
                    let name = self.expect(Name)?;
                    self.expect(Equal)?;
                    let expr = self.expr_id()?;
                    self.expect(Semi)?;
                    stmts.push(Stmt::Var(name, expr));
                }
                While => {
                    self.next();
                    let cond = self.expr_id()?;
                    let body = self.block()?;
                    stmts.push(Stmt::While(cond, body))
                }
                _ => {
                    let expr = self.expr()?;
                    let expr_id = self.tree.exprs.push(expr);
                    match expr {
                        Expr::If(..) => {
                            stmts.push(Stmt::Expr(expr_id));
                            continue;
                        }
                        _ => match self.peek() {
                            Equal => {
                                self.next();
                                let rhs = self.expr_id()?;
                                self.expect(Semi)?;
                                stmts.push(Stmt::Assign(expr_id, rhs));
                            }
                            Semi => {
                                self.next();
                                stmts.push(Stmt::Expr(expr_id));
                            }
                            RBrace => {
                                self.next();
                                let stmts = IdRange::new(&mut self.tree.stmts, stmts);
                                let expr = Some(expr_id);
                                return Ok(Block { stmts, expr });
                            }
                            _ => return Err(self.err(Equal | Semi | RBrace)),
                        },
                    }
                }
            }
        }
    }

    fn import(&mut self) -> ParseResult<Import> {
        self.expect(Import)?;
        let from = self.expect(Str)?;
        let name = match self.peek() {
            As => {
                self.next();
                Some(self.expect(Name)?)
            }
            _ => None,
        };
        let mut using = Vec::new();
        if let Use = self.peek() {
            self.next();
            loop {
                if let Semi = self.peek() {
                    break;
                }
                using.push(self.expect(Name)?);
                if let Comma = self.peek() {
                    self.next();
                }
            }
        }
        self.expect(Semi)?;
        let using = IdRange::new(&mut self.tree.names, using);
        Ok(Import { from, name, using })
    }

    fn tydef(&mut self) -> ParseResult<Tydef> {
        self.expect(Type)?;
        let name = self.expect(Name)?;
        let def = match self.peek() {
            Equal => {
                self.next();
                Some(self.ty_id()?)
            }
            _ => None,
        };
        self.expect(Semi)?;
        Ok(Tydef { name, def })
    }

    fn fndef(&mut self) -> ParseResult<Fndef> {
        self.expect(Fn)?;
        let mut name = self.expect(Name)?;
        let ty = match self.peek() {
            Dot => {
                self.next();
                let ty = Some(name);
                name = self.expect(Name)?;
                ty
            }
            _ => None,
        };
        let needs = match self.peek() {
            LBracket => self.need_ids()?,
            _ => IdRange::new(&mut self.tree.needs, Vec::new()),
        };
        self.expect(LParen)?;
        let mut params = Vec::new();
        loop {
            if let RParen = self.peek() {
                self.next();
                break;
            }
            params.push(self.param()?);
            if let Comma = self.peek() {
                self.next();
            }
        }
        let params = IdRange::new(&mut self.tree.params, params);
        let result = match self.peek() {
            Colon => {
                self.next();
                Some(self.ty_id()?)
            }
            _ => None,
        };
        let def = match self.peek() {
            Semi => {
                self.next();
                None
            }
            LBrace => Some(self.block()?),
            _ => return Err(self.err(Semi | LBrace)),
        };
        Ok(Fndef {
            ty,
            name,
            needs,
            params,
            result,
            def,
        })
    }

    fn valdef(&mut self) -> ParseResult<Valdef> {
        self.expect(Val)?;
        let name = self.expect(Name)?;
        let needs = match self.peek() {
            LBracket => self.need_ids()?,
            _ => IdRange::new(&mut self.tree.needs, Vec::new()),
        };
        self.expect(Colon)?;
        let ty = self.ty_id()?;
        self.expect(Semi)?;
        Ok(Valdef { name, needs, ty })
    }

    fn ctxdef(&mut self) -> ParseResult<Ctxdef> {
        self.expect(Context)?;
        let name = self.expect(Name)?;
        self.expect(Equal)?;
        let mut def = Vec::new();
        loop {
            if let Semi = self.peek() {
                self.next();
                break;
            }
            def.push(self.need()?);
            if let Comma = self.peek() {
                self.next();
            }
        }
        let def = IdRange::new(&mut self.tree.needs, def);
        Ok(Ctxdef { name, def })
    }

    fn structdef(&mut self) -> ParseResult<Structdef> {
        self.expect(Struct)?;
        let name = self.expect(Name)?;
        self.expect(LBrace)?;
        let mut fields = Vec::new();
        loop {
            if let RBrace = self.peek() {
                self.next();
                let fields = IdRange::new(&mut self.tree.params, fields);
                return Ok(Structdef { name, fields });
            }
            fields.push(self.param()?);
            if let Comma = self.peek() {
                self.next();
            }
        }
    }

    fn tree(mut self) -> ParseResult<Tree> {
        loop {
            match self.peek() {
                Import => {
                    let import = self.import()?;
                    self.tree.imports.push(import);
                }
                Type => {
                    let tydef = self.tydef()?;
                    self.tree.tydefs.push(tydef);
                }
                Fn => {
                    let fndef = self.fndef()?;
                    self.tree.fndefs.push(fndef);
                }
                Val => {
                    let valdef = self.valdef()?;
                    self.tree.valdefs.push(valdef);
                }
                Context => {
                    let ctxdef = self.ctxdef()?;
                    self.tree.ctxdefs.push(ctxdef);
                }
                Struct => {
                    let structdef = self.structdef()?;
                    self.tree.structdefs.push(structdef);
                }
                Eof => return Ok(self.tree),
                _ => return Err(self.err(Import | Type | Fn | Val | Context | Struct | Eof)),
            }
        }
    }
}

pub fn parse(tokens: &Tokens) -> ParseResult<Tree> {
    Parser::new(tokens).tree()
}
