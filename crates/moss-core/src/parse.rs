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
    /// The index of a [`TokenId`] in the `names` field of a [`Tree`].
    pub struct NameId = u32;
}

define_index_type! {
    /// The index of a [`Member`] in the `members` field of a [`Tree`].
    pub struct MemberId = u32;
}

define_index_type! {
    /// The index of a [`Type`] in the `types` field of a [`Tree`].
    pub struct TypeId = u32;
}

define_index_type! {
    /// The index of a [`Bind`] in the `binds` field of a [`Tree`].
    pub struct BindId = u32;
}

define_index_type! {
    /// The index of a [`Need`] in the `needs` field of a [`Tree`].
    pub struct NeedId = u32;
}

define_index_type! {
    /// The index of a [`Param`] in the `params` field of a [`Tree`].
    pub struct ParamId = u32;
}

define_index_type! {
    /// The index of a [`Field`] in the `fields` field of a [`Tree`].
    pub struct FieldId = u32;
}

define_index_type! {
    /// The index of a [`Binding`] in the `bindings` field of a [`Tree`].
    pub struct BindingId = u32;
}

define_index_type! {
    /// The index of an [`Expr`] in the `exprs` field of a [`Tree`].
    pub struct ExprId = u32;
}

define_index_type! {
    /// The index of a [`Stmt`] in the `stmts` field of a [`Tree`].
    pub struct StmtId = u32;
}

define_index_type! {
    /// The index of an [`Import`] in the `imports` field of a [`Tree`].
    pub struct ImportId = u32;
}

define_index_type! {
    /// The index of an [`IdRange<BindId>`] in the `assumes` field of a [`Tree`].
    pub struct AssumeId = u32;
}

define_index_type! {
    /// The index of a [`Tydef`] in the `tydefs` field of a [`Tree`].
    pub struct TydefId = u32;
}

define_index_type! {
    /// The index of a [`Tagdef`] in the `tagdefs` field of a [`Tree`].
    pub struct TagdefId = u32;
}

define_index_type! {
    /// The index of an [`Aliasdef`] in the `aliasdefs` field of a [`Tree`].
    pub struct AliasdefId = u32;
}

define_index_type! {
    /// The index of a [`Funcdef`] in the `funcdefs` field of a [`Tree`].
    pub struct FuncdefId = u32;
}

define_index_type! {
    /// The index of an [`Attachdef`] in the `attachdefs` field of a [`Tree`].
    pub struct AttachdefId = u32;
}

define_index_type! {
    /// The index of a [`Detachdef`] in the `detachdefs` field of a [`Tree`].
    pub struct DetachdefId = u32;
}

define_index_type! {
    /// The index of a [`Valdef`] in the `valdefs` field of a [`Tree`].
    pub struct ValdefId = u32;
}

define_index_type! {
    /// The index of a [`Ctxdef`] in the `ctxdefs` field of a [`Tree`].
    pub struct CtxdefId = u32;
}

define_index_type! {
    /// The index of a [`Decl`] in the `decls` field of a [`Tree`].
    pub struct DeclId = u32;
}

#[derive(Clone, Copy, Debug)]
pub struct Path {
    pub prefix: IdRange<NameId>,
    pub last: TokenId,
}

#[derive(Clone, Copy, Debug)]
pub struct Member {
    pub name: TokenId,
    pub ty: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Spec(Spec),
    Tuple(IdRange<TypeId>),
    Record(IdRange<MemberId>),
}

#[derive(Clone, Copy, Debug)]
pub struct Spec {
    pub dot: bool,
    pub path: Path,
    pub binds: IdRange<BindId>,
}

#[derive(Clone, Copy, Debug)]
pub enum Entry {
    Lit(TokenId),
    Ref(Spec),
}

#[derive(Clone, Copy, Debug)]
pub struct Bind {
    pub key: Spec,
    pub val: Option<Entry>,
}

#[derive(Clone, Copy, Debug)]
pub enum NeedKind {
    Default,
    Static,
}

#[derive(Clone, Copy, Debug)]
pub struct Need {
    pub kind: NeedKind,
    pub bind: BindId,
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
pub enum Unop {
    Neg,
    Not,
}

#[derive(Clone, Copy, Debug)]
pub enum Binop {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    And,
    Or,
    Xor,
}

#[derive(Clone, Copy, Debug)]
pub enum Binding {
    Single(BindId),
    Composite(ExprId),
}

#[derive(Clone, Copy, Debug)]
pub enum Expr {
    Lit(TokenId),
    Path(Path),
    Tag(Path, ExprId),
    Record(TokenId, IdRange<FieldId>, TokenId),
    Field(ExprId, TokenId),
    Method(ExprId, TokenId, IdRange<ExprId>),
    Call(Path, IdRange<BindId>, IdRange<ExprId>), // Should this be `BindingId` instead of `BindId`?
    Unary(Unop, ExprId),
    Binary(ExprId, Binop, ExprId),
    If(ExprId, Block, Option<Block>),
    Bind(TokenId, IdRange<BindingId>),
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
    pub names: IdRange<NameId>,
    pub methods: IdRange<NameId>,
}

#[derive(Clone, Copy, Debug)]
pub struct Tydef {
    pub name: TokenId,
    pub needs: IdRange<NeedId>,
}

#[derive(Clone, Copy, Debug)]
pub struct Tagdef {
    pub name: TokenId,
    pub needs: IdRange<NeedId>,
    pub def: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Aliasdef {
    pub name: TokenId,
    pub needs: IdRange<NeedId>,
    pub def: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub enum Return {
    Unit,
    Type(TypeId),
    Bind(IdRange<NeedId>),
}

#[derive(Clone, Copy, Debug)]
pub struct Fndef {
    pub name: TokenId,
    pub needs: IdRange<NeedId>,
    pub params: IdRange<ParamId>,
    pub result: Return,
    pub def: Option<Block>,
}

#[derive(Clone, Copy, Debug)]
pub struct Funcdef {
    pub fndef: Fndef,
}

#[derive(Clone, Copy, Debug)]
pub struct Attachdef {
    pub ty: TokenId,
    pub fndef: Fndef,
}

#[derive(Clone, Copy, Debug)]
pub struct Detachdef {
    pub fndef: Fndef,
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
    pub needs: IdRange<NeedId>,
    pub def: IdRange<NeedId>,
}

#[derive(Clone, Copy, Debug)]
pub enum Decl {
    Tydef(TydefId),
    Tagdef(TagdefId),
    Aliasdef(AliasdefId),
    Funcdef(FuncdefId),
    Attachdef(AttachdefId),
    Detachdef(DetachdefId),
    Valdef(ValdefId),
    Ctxdef(CtxdefId),
}

#[derive(Debug, Default)]
pub struct Tree {
    pub names: IndexVec<NameId, TokenId>,
    pub members: IndexVec<MemberId, Member>,
    pub types: IndexVec<TypeId, Type>,
    pub binds: IndexVec<BindId, Bind>,
    pub needs: IndexVec<NeedId, Need>,
    pub params: IndexVec<ParamId, Param>,
    pub fields: IndexVec<FieldId, Field>,
    pub bindings: IndexVec<BindingId, Binding>,
    pub exprs: IndexVec<ExprId, Expr>,
    pub stmts: IndexVec<StmtId, Stmt>,
    pub imports: IndexVec<ImportId, Import>,
    pub assumes: IndexVec<AssumeId, IdRange<BindId>>,
    pub tydefs: IndexVec<TydefId, Tydef>,
    pub tagdefs: IndexVec<TagdefId, Tagdef>,
    pub aliasdefs: IndexVec<AliasdefId, Aliasdef>,
    pub funcdefs: IndexVec<FuncdefId, Funcdef>,
    pub attachdefs: IndexVec<AttachdefId, Attachdef>,
    pub detachdefs: IndexVec<DetachdefId, Detachdef>,
    pub valdefs: IndexVec<ValdefId, Valdef>,
    pub ctxdefs: IndexVec<CtxdefId, Ctxdef>,
    pub decls: IndexVec<DeclId, Decl>, // TODO: Don't make declaration order significant.
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

/// Whether or not struct literals are allowed when parsing this expression.
#[derive(Clone, Copy)]
enum Curly {
    Yes,
    No,
}

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

    fn member(&mut self) -> ParseResult<Member> {
        let name = self.expect(Name)?;
        self.expect(Colon)?;
        let ty = self.ty_id()?;
        Ok(Member { name, ty })
    }

    fn ty(&mut self) -> ParseResult<Type> {
        match self.peek() {
            Name => Ok(Type::Spec(self.spec()?)),
            LParen => {
                self.next();
                let mut elements = Vec::new();
                loop {
                    if let RParen = self.peek() {
                        self.next();
                        let elements = IdRange::new(&mut self.tree.types, elements);
                        return Ok(Type::Tuple(elements));
                    }
                    elements.push(self.ty()?);
                    if let Comma = self.peek() {
                        self.next();
                    }
                }
            }
            LBrace => {
                self.next();
                let mut members = Vec::new();
                loop {
                    if let RBrace = self.peek() {
                        self.next();
                        let members = IdRange::new(&mut self.tree.members, members);
                        return Ok(Type::Record(members));
                    }
                    members.push(self.member()?);
                    if let Comma = self.peek() {
                        self.next();
                    }
                }
            }
            _ => Err(self.err(Name | LBrace)),
        }
    }

    fn ty_id(&mut self) -> ParseResult<TypeId> {
        let ty = self.ty()?;
        Ok(self.tree.types.push(ty))
    }

    fn spec(&mut self) -> ParseResult<Spec> {
        let dot = match self.peek() {
            Dot => {
                self.next();
                true
            }
            _ => false,
        };
        let path = self.path()?;
        let mut binds = Vec::new();
        if let LBracket = self.peek() {
            self.next();
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
        }
        let binds = IdRange::new(&mut self.tree.binds, binds);
        Ok(Spec { dot, path, binds })
    }

    fn entry(&mut self) -> ParseResult<Entry> {
        match self.peek() {
            Name => Ok(Entry::Ref(self.spec()?)),
            Uint32 | Int32 | Uint64 | Int64 | Uint | Int | Char | Str => {
                Ok(Entry::Lit(self.next()))
            }
            _ => Err(self.err(Name | Uint32 | Int32 | Uint64 | Int64 | Uint | Int | Char | Str)),
        }
    }

    fn bind(&mut self) -> ParseResult<Bind> {
        let key = self.spec()?;
        let val = match self.peek() {
            Equal => {
                self.next();
                Some(self.entry()?)
            }
            _ => None,
        };
        Ok(Bind { key, val })
    }

    fn bind_id(&mut self) -> ParseResult<BindId> {
        let bind = self.bind()?;
        Ok(self.tree.binds.push(bind))
    }

    fn need(&mut self) -> ParseResult<Need> {
        let kind = match self.peek() {
            Static => {
                self.next();
                NeedKind::Static
            }
            _ => NeedKind::Default,
        };
        let bind = self.bind_id()?;
        Ok(Need { kind, bind })
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
                self.expr_id(Curly::Yes)?
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
            args.push(self.expr(Curly::Yes)?);
            if let Comma = self.peek() {
                self.next();
            }
        }
    }

    fn arg_ids(&mut self) -> ParseResult<IdRange<ExprId>> {
        let args = self.args()?;
        Ok(IdRange::new(&mut self.tree.exprs, args))
    }

    fn expr_if(&mut self) -> ParseResult<Expr> {
        self.expect(If)?;
        let cond = self.expr_id(Curly::No)?;
        let yes = self.block()?;
        let no = match self.peek() {
            Else => {
                self.next();
                match self.peek() {
                    If => {
                        let expr = self.expr_if()?;
                        Some(Block {
                            stmts: IdRange::new(&mut self.tree.stmts, Vec::new()),
                            expr: Some(self.tree.exprs.push(expr)),
                        })
                    }
                    LBrace => Some(self.block()?),
                    _ => return Err(self.err(If | LBrace)),
                }
            }
            _ => None,
        };
        Ok(Expr::If(cond, yes, no))
    }

    fn expr_bind(&mut self) -> ParseResult<Expr> {
        let start = self.expect(Bind)?;
        let mut bindings = Vec::new();
        loop {
            if let Semi | RBrace = self.peek() {
                let bindings = IdRange::new(&mut self.tree.bindings, bindings);
                return Ok(Expr::Bind(start, bindings));
            }
            let key = self.spec()?;
            let binding = match self.peek() {
                Comma => {
                    let val = None;
                    Binding::Single(self.tree.binds.push(Bind { key, val }))
                }
                Equal => {
                    self.next();
                    let val = Some(self.entry()?);
                    Binding::Single(self.tree.binds.push(Bind { key, val }))
                }
                LParen => {
                    if key.dot {
                        return Err(self.err(Comma | Equal));
                    } else {
                        let args = self.arg_ids()?;
                        let expr = Expr::Call(key.path, key.binds, args);
                        Binding::Composite(self.tree.exprs.push(expr))
                    }
                }
                _ => return Err(self.err(Comma | Equal | LParen)),
            };
            bindings.push(binding);
            if let Comma = self.peek() {
                self.next();
            }
        }
    }

    fn expr_atom(&mut self, curly: Curly) -> ParseResult<Expr> {
        let mut expected =
            LParen | Bind | If | Name | Uint32 | Int32 | Uint64 | Int64 | Uint | Int | Char | Str;
        if let Curly::Yes = curly {
            expected |= RBrace;
        }
        match self.peek() {
            LParen => {
                self.next();
                let expr = self.expr(Curly::Yes)?;
                self.expect(RParen)?;
                Ok(expr)
            }
            LBrace => {
                if let Curly::No = curly {
                    return Err(self.err(expected));
                }
                let lbrace = self.next();
                let mut fields = Vec::new();
                loop {
                    if let RBrace = self.peek() {
                        let rbrace = self.next();
                        let fields = IdRange::new(&mut self.tree.fields, fields);
                        return Ok(Expr::Record(lbrace, fields, rbrace));
                    }
                    fields.push(self.field()?);
                    if let Comma = self.peek() {
                        self.next();
                    }
                }
            }
            Uint32 | Int32 | Uint64 | Int64 | Uint | Int | Char | Str => Ok(Expr::Lit(self.next())),
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
                        let binds = IdRange::new(&mut self.tree.binds, binds);
                        let args = self.arg_ids()?;
                        Ok(Expr::Call(path, binds, args))
                    }
                    token => {
                        // Only treat as tag if the next token can start an atom, not a block end.
                        if expected.contains(token) && token != RBrace {
                            let inner = self.expr_atom(curly)?;
                            Ok(Expr::Tag(path, self.tree.exprs.push(inner)))
                        } else {
                            Ok(Expr::Path(path))
                        }
                    }
                }
            }
            If => self.expr_if(),
            Bind => self.expr_bind(),
            _ => Err(self.err(expected)),
        }
    }

    fn expr_chain(&mut self, curly: Curly) -> ParseResult<Expr> {
        let mut expr = self.expr_atom(curly)?;
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

    fn expr_factor(&mut self, curly: Curly) -> ParseResult<Expr> {
        self.expr_chain(curly)
    }

    fn expr_term(&mut self, curly: Curly) -> ParseResult<Expr> {
        let mut lhs = self.expr_factor(curly)?;
        loop {
            let op = match self.peek() {
                Star => Binop::Mul,
                Slash => Binop::Div,
                Percent => Binop::Rem,
                _ => break,
            };
            self.next();
            let rhs = self.expr_factor(curly)?;
            lhs = Expr::Binary(self.tree.exprs.push(lhs), op, self.tree.exprs.push(rhs));
        }
        Ok(lhs)
    }

    fn expr_quant(&mut self, curly: Curly) -> ParseResult<Expr> {
        let mut lhs = self.expr_term(curly)?;
        loop {
            let op = match self.peek() {
                Plus => Binop::Add,
                Hyphen => Binop::Sub,
                _ => break,
            };
            self.next();
            let rhs = self.expr_term(curly)?;
            lhs = Expr::Binary(self.tree.exprs.push(lhs), op, self.tree.exprs.push(rhs));
        }
        Ok(lhs)
    }

    fn expr_comp(&mut self, curly: Curly) -> ParseResult<Expr> {
        let lhs = self.expr_quant(curly)?;
        let op = match self.peek() {
            EqualEqual => Some(Binop::Eq),
            ExclamEqual => Some(Binop::Ne),
            Less => Some(Binop::Lt),
            Greater => Some(Binop::Gt),
            LessEqual => Some(Binop::Le),
            GreaterEqual => Some(Binop::Ge),
            _ => None,
        };
        match op {
            None => Ok(lhs),
            Some(op) => {
                self.next();
                let rhs = self.expr_quant(curly)?;
                Ok(Expr::Binary(
                    self.tree.exprs.push(lhs),
                    op,
                    self.tree.exprs.push(rhs),
                ))
            }
        }
    }

    fn expr(&mut self, curly: Curly) -> ParseResult<Expr> {
        // TODO: Handle all the unary and binary operators.
        self.expr_comp(curly)
    }

    fn expr_id(&mut self, curly: Curly) -> ParseResult<ExprId> {
        let expr = self.expr(curly)?;
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
                    let expr = self.expr_id(Curly::Yes)?;
                    self.expect(Semi)?;
                    stmts.push(Stmt::Let(name, expr));
                }
                Var => {
                    self.next();
                    let name = self.expect(Name)?;
                    self.expect(Equal)?;
                    let expr = self.expr_id(Curly::Yes)?;
                    self.expect(Semi)?;
                    stmts.push(Stmt::Var(name, expr));
                }
                While => {
                    self.next();
                    let cond = self.expr_id(Curly::No)?;
                    let body = self.block()?;
                    stmts.push(Stmt::While(cond, body))
                }
                _ => {
                    let expr = self.expr(Curly::Yes)?;
                    let expr_id = self.tree.exprs.push(expr);
                    match expr {
                        Expr::If(..) => {
                            stmts.push(Stmt::Expr(expr_id));
                            continue;
                        }
                        _ => match self.peek() {
                            Equal => {
                                self.next();
                                let rhs = self.expr_id(Curly::Yes)?;
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
        let mut names = Vec::new();
        let mut methods = Vec::new();
        if let Use = self.peek() {
            self.next();
            loop {
                match self.peek() {
                    Semi => break,
                    Name => {
                        let name = self.next();
                        names.push(name);
                    }
                    Dot => {
                        self.next();
                        let name = self.expect(Name)?;
                        methods.push(name);
                    }
                    _ => (),
                }
                if let Comma = self.peek() {
                    self.next();
                }
            }
        }
        self.expect(Semi)?;
        Ok(Import {
            from,
            name,
            names: IdRange::new(&mut self.tree.names, names),
            methods: IdRange::new(&mut self.tree.names, methods),
        })
    }

    fn assume(&mut self) -> ParseResult<IdRange<BindId>> {
        self.expect(Assume)?;
        let mut binds = Vec::new();
        loop {
            if let Semi = self.peek() {
                self.next();
                return Ok(IdRange::new(&mut self.tree.binds, binds));
            }
            binds.push(self.bind()?);
            if let Comma = self.peek() {
                self.next();
            }
        }
    }

    fn fndef(&mut self, name: TokenId) -> ParseResult<Fndef> {
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
                match self.peek() {
                    Bind => {
                        self.next();
                        let mut needs = Vec::new();
                        loop {
                            if let Semi | LBrace = self.peek() {
                                break;
                            }
                            needs.push(self.need()?);
                            if let Comma = self.peek() {
                                self.next();
                            }
                        }
                        Return::Bind(IdRange::new(&mut self.tree.needs, needs))
                    }
                    _ => Return::Type(self.ty_id()?),
                }
            }
            _ => Return::Unit,
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
        let needs = match self.peek() {
            LBracket => self.need_ids()?,
            _ => IdRange::new(&mut self.tree.needs, Vec::new()),
        };
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
        Ok(Ctxdef { name, needs, def })
    }

    fn decl(&mut self) -> ParseResult<Decl> {
        match self.peek() {
            Type => {
                self.next();
                let name = self.expect(Name)?;
                let needs = match self.peek() {
                    LBracket => self.need_ids()?,
                    _ => IdRange::new(&mut self.tree.needs, Vec::new()),
                };
                match self.peek() {
                    Semi => {
                        self.next();
                        let id = self.tree.tydefs.push(Tydef { name, needs });
                        Ok(Decl::Tydef(id))
                    }
                    Equal => {
                        self.next();
                        let def = self.ty_id()?;
                        self.expect(Semi)?;
                        let id = self.tree.aliasdefs.push(Aliasdef { name, needs, def });
                        Ok(Decl::Aliasdef(id))
                    }
                    _ => {
                        let def = self.ty_id()?;
                        self.expect(Semi)?;
                        let id = self.tree.tagdefs.push(Tagdef { name, needs, def });
                        Ok(Decl::Tagdef(id))
                    }
                }
            }
            Fn => {
                self.next();
                match self.peek() {
                    Name => {
                        let first = self.next();
                        match self.peek() {
                            LBracket | LParen => {
                                let name = first;
                                let fndef = self.fndef(name)?;
                                let id = self.tree.funcdefs.push(Funcdef { fndef });
                                Ok(Decl::Funcdef(id))
                            }
                            Dot => {
                                self.next();
                                let ty = first;
                                let name = self.expect(Name)?;
                                let fndef = self.fndef(name)?;
                                let id = self.tree.attachdefs.push(Attachdef { ty, fndef });
                                Ok(Decl::Attachdef(id))
                            }
                            _ => Err(self.err(LBracket | LParen | Dot)),
                        }
                    }
                    Dot => {
                        self.next();
                        let name = self.expect(Name)?;
                        let fndef = self.fndef(name)?;
                        let id = self.tree.detachdefs.push(Detachdef { fndef });
                        Ok(Decl::Detachdef(id))
                    }
                    _ => Err(self.err(Name | Dot)),
                }
            }
            Val => {
                let valdef = self.valdef()?;
                let id = self.tree.valdefs.push(valdef);
                Ok(Decl::Valdef(id))
            }
            Context => {
                let ctxdef = self.ctxdef()?;
                let id = self.tree.ctxdefs.push(ctxdef);
                Ok(Decl::Ctxdef(id))
            }
            _ => Err(self.err(Type | Fn | Val | Context)),
        }
    }

    fn tree(mut self) -> ParseResult<Tree> {
        loop {
            match self.peek() {
                Import => {
                    let import = self.import()?;
                    self.tree.imports.push(import);
                }
                Assume => {
                    let assume = self.assume()?;
                    self.tree.assumes.push(assume);
                }
                Eof => return Ok(self.tree),
                _ => {
                    let decl = self.decl()?;
                    self.tree.decls.push(decl);
                }
            }
        }
    }
}

pub fn parse(tokens: &Tokens) -> ParseResult<Tree> {
    Parser::new(tokens).tree()
}
