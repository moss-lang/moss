use enumset::EnumSet;

use crate::lex::{Token::{self, *}, TokenId, Tokens};

use super::parse::ParseError;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone)]
pub struct Path {
    pub prefix: Vec<TokenId>,
    pub last: TokenId,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub ty: TokenId,
    pub name: TokenId,
}

#[derive(Debug, Clone)]
pub enum Type {
    Path(Path),
}

#[derive(Debug, Clone)]
pub enum NeedKind {
    Default,
    Static,
}

#[derive(Debug, Clone)]
pub struct Need {
    pub kind: NeedKind,
    pub path: Path,
}

#[derive(Debug, Clone)]
pub struct Bind {
    pub path: Path,
    pub val: Expr,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: TokenId,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: TokenId,
    pub val: Expr,
}

#[derive(Debug, Clone, Copy)]
pub enum Binop {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
}

#[derive(Debug, Clone)]
pub enum Expr {
    This(TokenId),
    Path(Path),
    Int(TokenId),
    String(TokenId),
    Struct(Path, Vec<Field>),
    Field(Box<Expr>, TokenId),
    Method(Box<Expr>, TokenId, Vec<Expr>),
    Call(Path, Vec<Bind>, Vec<Expr>),
    Binary(Box<Expr>, Binop, Box<Expr>),
    If(Box<Expr>, Box<Block>, Option<Box<Block>>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let(TokenId, Expr),
    Var(TokenId, Expr),
    Assign(Expr, Expr),
    While(Expr, Block),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub from: TokenId,
    pub name: Option<TokenId>,
    pub names: Vec<TokenId>,
    pub methods: Vec<Method>,
}

#[derive(Debug, Clone)]
pub struct Tydef {
    pub name: TokenId,
    pub def: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct Fndef {
    pub ty: Option<TokenId>,
    pub name: TokenId,
    pub needs: Vec<Need>,
    pub params: Vec<Param>,
    pub result: Option<Type>,
    pub def: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct Valdef {
    pub name: TokenId,
    pub needs: Vec<Need>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Ctxdef {
    pub name: TokenId,
    pub def: Vec<Need>,
}

#[derive(Debug, Clone)]
pub struct Structdef {
    pub name: TokenId,
    pub fields: Vec<Param>,
}

#[derive(Debug, Default, Clone)]
pub struct Tree {
    pub imports: Vec<Import>,
    pub tydefs: Vec<Tydef>,
    pub fndefs: Vec<Fndef>,
    pub valdefs: Vec<Valdef>,
    pub ctxdefs: Vec<Ctxdef>,
    pub structdefs: Vec<Structdef>,
}

#[derive(Clone, Copy)]
enum Curly {
    Yes,
    No,
}

struct Parser<'a> {
    tokens: &'a Tokens,
    id: TokenId,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Tokens) -> Self {
        Self {
            tokens,
            id: TokenId::from(0),
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
        Ok(Path { prefix, last })
    }

    fn ty(&mut self) -> ParseResult<Type> {
        match self.peek() {
            Name => Ok(Type::Path(self.path()?)),
            _ => Err(self.err(EnumSet::only(Name))),
        }
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

    fn bind(&mut self) -> ParseResult<Bind> {
        let path = self.path()?;
        self.expect(Equal)?;
        let val = self.expr(Curly::Yes)?;
        Ok(Bind { path, val })
    }

    fn param(&mut self) -> ParseResult<Param> {
        let name = self.expect(Name)?;
        self.expect(Colon)?;
        let ty = self.ty()?;
        Ok(Param { name, ty })
    }

    fn field(&mut self) -> ParseResult<Field> {
        let name = self.expect(Name)?;
        let val = match self.peek() {
            Equal => {
                self.next();
                self.expr(Curly::Yes)?
            }
            _ => {
                let prefix = Vec::new();
                let last = name;
                Expr::Path(Path { prefix, last })
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

    fn expr_if(&mut self) -> ParseResult<Expr> {
        self.expect(If)?;
        let cond = self.expr(Curly::No)?;
        let then = self.block()?;
        let els = match self.peek() {
            Else => {
                self.next();
                Some(self.block()?)
            }
            _ => None,
        };
        Ok(Expr::If(
            Box::new(cond),
            Box::new(then),
            els.map(Box::new),
        ))
    }

    fn expr_atom(&mut self, curly: Curly) -> ParseResult<Expr> {
        match self.peek() {
            LParen => {
                self.next();
                let expr = self.expr(curly)?;
                self.expect(RParen)?;
                Ok(expr)
            }
            If => self.expr_if(),
            This => Ok(Expr::This(self.next())),
            Int => Ok(Expr::Int(self.next())),
            Str => Ok(Expr::String(self.next())),
            Name => {
                let path = self.path()?;
                match self.peek() {
                    LParen => {
                        let binds = Vec::new();
                        let args = self.args()?;
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
                        let args = self.args()?;
                        Ok(Expr::Call(path, binds, args))
                    }
                    LBrace => {
                        if let Curly::No = curly {
                            return Ok(Expr::Path(path));
                        }
                        self.next();
                        let mut fields = Vec::new();
                        loop {
                            if let RBrace = self.peek() {
                                self.next();
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

    fn expr_chain(&mut self, curly: Curly) -> ParseResult<Expr> {
        let mut expr = self.expr_atom(curly)?;
        while let Dot = self.peek() {
            self.next();
            let object = Box::new(expr);
            let name = self.expect(Name)?;
            expr = match self.peek() {
                LParen => {
                    let args = self.args()?;
                    Expr::Method(object, name, args)
                }
                _ => Expr::Field(object, name),
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
                Percent => Binop::Rem,
                Star => Binop::Mul,
                Slash => Binop::Div,
                _ => break,
            };
            self.next();
            let rhs = self.expr_factor(curly)?;
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
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
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
        }
        Ok(lhs)
    }

    fn expr_comp(&mut self, curly: Curly) -> ParseResult<Expr> {
        let lhs = self.expr_quant(curly)?;
        let op = match self.peek() {
            Less => Some(Binop::Lt),
            Greater => Some(Binop::Gt),
            ExclamEqual => Some(Binop::Neq),
            LessEqual => Some(Binop::Leq),
            EqualEqual => Some(Binop::Eq),
            GreaterEqual => Some(Binop::Geq),
            _ => None,
        };
        match op {
            None => Ok(lhs),
            Some(op) => {
                self.next();
                let rhs = self.expr_quant(curly)?;
                Ok(Expr::Binary(Box::new(lhs), op, Box::new(rhs)))
            }
        }
    }

    fn expr(&mut self, curly: Curly) -> ParseResult<Expr> {
        self.expr_comp(curly)
    }

    fn block(&mut self) -> ParseResult<Block> {
        self.expect(LBrace)?;
        let mut stmts = Vec::new();
        loop {
            match self.peek() {
                RBrace => {
                    self.next();
                    return Ok(Block { stmts, expr: None });
                }
                Let => {
                    self.next();
                    let name = self.expect(Name)?;
                    self.expect(Equal)?;
                    let expr = self.expr(Curly::Yes)?;
                    self.expect(Semi)?;
                    stmts.push(Stmt::Let(name, expr));
                }
                Var => {
                    self.next();
                    let name = self.expect(Name)?;
                    self.expect(Equal)?;
                    let expr = self.expr(Curly::Yes)?;
                    self.expect(Semi)?;
                    stmts.push(Stmt::Var(name, expr));
                }
                While => {
                    self.next();
                    let cond = self.expr(Curly::No)?;
                    let body = self.block()?;
                    stmts.push(Stmt::While(cond, body))
                }
                _ => {
                    let expr = self.expr(Curly::Yes)?;
                    match expr {
                        Expr::If(..) => {
                            stmts.push(Stmt::Expr(expr));
                            continue;
                        }
                        _ => match self.peek() {
                            Equal => {
                                self.next();
                                let rhs = self.expr(Curly::Yes)?;
                                self.expect(Semi)?;
                                stmts.push(Stmt::Assign(expr, rhs));
                            }
                            Semi => {
                                self.next();
                                stmts.push(Stmt::Expr(expr));
                            }
                            RBrace => {
                                self.next();
                                return Ok(Block {
                                    stmts,
                                    expr: Some(Box::new(expr)),
                                });
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
                if let Semi = self.peek() {
                    break;
                }
                let name_tok = self.expect(Name)?;
                match self.peek() {
                    Dot => {
                        self.next();
                        let ty = name_tok;
                        let name = self.expect(Name)?;
                        methods.push(Method { ty, name });
                    }
                    _ => names.push(name_tok),
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
            names,
            methods,
        })
    }

    fn tydef(&mut self) -> ParseResult<Tydef> {
        self.expect(Type)?;
        let name = self.expect(Name)?;
        let def = match self.peek() {
            Equal => {
                self.next();
                Some(self.ty()?)
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
            LBracket => self.needs()?,
            _ => Vec::new(),
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
        let result = match self.peek() {
            Colon => {
                self.next();
                Some(self.ty()?)
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
            LBracket => self.needs()?,
            _ => Vec::new(),
        };
        self.expect(Colon)?;
        let ty = self.ty()?;
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
                return Ok(Structdef { name, fields });
            }
            fields.push(self.param()?);
            if let Comma = self.peek() {
                self.next();
            }
        }
    }

    fn tree(mut self) -> ParseResult<Tree> {
        let mut tree = Tree::default();
        loop {
            match self.peek() {
                Import => {
                    let import = self.import()?;
                    tree.imports.push(import);
                }
                Type => {
                    let tydef = self.tydef()?;
                    tree.tydefs.push(tydef);
                }
                Fn => {
                    let fndef = self.fndef()?;
                    tree.fndefs.push(fndef);
                }
                Val => {
                    let valdef = self.valdef()?;
                    tree.valdefs.push(valdef);
                }
                Context => {
                    let ctxdef = self.ctxdef()?;
                    tree.ctxdefs.push(ctxdef);
                }
                Struct => {
                    let structdef = self.structdef()?;
                    tree.structdefs.push(structdef);
                }
                Eof => return Ok(tree),
                _ => return Err(self.err(Import | Type | Fn | Val | Context | Struct | Eof)),
            }
        }
    }
}

pub fn parse(tokens: &Tokens) -> ParseResult<Tree> {
    Parser::new(tokens).tree()
}
