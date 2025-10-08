use enumset::EnumSet;
use index_vec::{Idx, IndexVec, define_index_type};

use crate::lex::{
    Token::{self, *},
    TokenId, Tokens,
};

define_index_type! {
    pub struct ExprId = u32;
}

define_index_type! {
    pub struct StmtId = u32;
}

define_index_type! {
    pub struct FuncId = u32;
}

#[derive(Clone, Copy, Debug)]
pub struct IdRange<I> {
    pub start: I,
    pub end: I,
}

impl<I: Idx> IdRange<I> {
    fn new<T>(v: &mut IndexVec<I, T>, xs: Vec<T>) -> Self {
        let start = v.next_idx();
        v.append(&mut IndexVec::from_vec(xs));
        let end = v.next_idx();
        Self { start, end }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Expr {
    String(TokenId),
    Call(TokenId, ExprId),
}

#[derive(Clone, Copy, Debug)]
pub enum Stmt {
    Expr(ExprId),
}

#[derive(Clone, Copy, Debug)]
pub struct Block {
    pub stmts: IdRange<StmtId>,
    pub expr: Option<ExprId>,
}

#[derive(Clone, Copy, Debug)]
pub struct Func {
    pub name: TokenId,
    pub body: Block,
}

#[derive(Debug, Default)]
pub struct Tree {
    pub exprs: IndexVec<ExprId, Expr>,
    pub stmts: IndexVec<StmtId, Stmt>,
    pub funcs: IndexVec<FuncId, Func>,
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
    tree: Tree,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Tokens) -> Self {
        let id = TokenId::from(0);
        let tree = Tree::default();
        Self { tokens, id, tree }
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

    fn expr(&mut self) -> ParseResult<Expr> {
        match self.peek() {
            Str => Ok(Expr::String(self.next())),
            Name => {
                let name = self.next();
                self.expect(LParen)?;
                let arg = self.expr_id()?;
                self.expect(RParen)?;
                Ok(Expr::Call(name, arg))
            }
            _ => Err(self.err(Str | Name)),
        }
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
                _ => {
                    let expr = self.expr_id()?;
                    match self.peek() {
                        Semi => {
                            self.next();
                            stmts.push(Stmt::Expr(expr));
                        }
                        RBrace => {
                            let stmts = IdRange::new(&mut self.tree.stmts, stmts);
                            let expr = Some(expr);
                            return Ok(Block { stmts, expr });
                        }
                        _ => return Err(self.err(Semi | RBrace)),
                    }
                }
            }
        }
    }

    fn func(&mut self) -> ParseResult<Func> {
        self.expect(Fn)?;
        let name = self.expect(Name)?;
        self.expect(LParen)?;
        self.expect(RParen)?;
        let body = self.block()?;
        Ok(Func { name, body })
    }

    fn tree(mut self) -> ParseResult<Tree> {
        loop {
            match self.peek() {
                Fn => {
                    let func = self.func()?;
                    self.tree.funcs.push(func);
                }
                Eof => return Ok(self.tree),
                _ => return Err(self.err(Fn | Eof)),
            }
        }
    }
}

pub fn parse(tokens: &Tokens) -> ParseResult<Tree> {
    Parser::new(tokens).tree()
}
