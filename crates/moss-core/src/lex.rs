use std::{
    fmt,
    ops::{Add, Range},
};

use enumset::EnumSetType;
use index_vec::{IndexVec, define_index_type};
use logos::Logos;

define_index_type! {
    pub struct ByteIndex = u32;
    IMPL_RAW_CONVERSIONS = true;
}

define_index_type! {
    pub struct TokenId = u32;
}

impl Add<i32> for TokenId {
    type Output = Self;

    fn add(self, rhs: i32) -> Self::Output {
        Self::from_raw(if rhs < 0 {
            self.raw() - (-rhs) as u32
        } else {
            self.raw() + rhs as u32
        })
    }
}

#[derive(Debug, EnumSetType, Logos)]
#[logos(skip r"(#[^\n]+|\s)+")]
pub enum Token {
    Eof,

    #[token("!")]
    Exclam,

    #[token("%")]
    Percent,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("*")]
    Star,

    #[token("+")]
    Plus,

    #[token(",")]
    Comma,

    #[token("-")]
    Hyphen,

    #[token(".")]
    Dot,

    #[token("/")]
    Slash,

    #[token(":")]
    Colon,

    #[token(";")]
    Semi,

    #[token("<")]
    Less,

    #[token("=")]
    Equal,

    #[token(">")]
    Greater,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("!=")]
    ExclamEqual,

    #[token("::")]
    ColonColon,

    #[token("<=")]
    LessEqual,

    #[token("==")]
    EqualEqual,

    #[token(">=")]
    GreaterEqual,

    #[token("as")]
    As,

    #[token("assume")]
    Assume,

    #[token("context")]
    Context,

    #[token("else")]
    Else,

    #[token("fn")]
    Fn,

    #[token("if")]
    If,

    #[token("import")]
    Import,

    #[token("let")]
    Let,

    #[token("static")]
    Static,

    #[token("struct")]
    Struct,

    #[token("this")]
    This,

    #[token("type")]
    Type,

    #[token("use")]
    Use,

    #[token("val")]
    Val,

    #[token("var")]
    Var,

    #[token("while")]
    While,

    #[regex(r"[A-Z_a-z]\w*")]
    Name,

    #[regex(r#""([^"\\\n]|\\["\\nrt])*""#)]
    Str,

    #[regex(r"\d+")]
    Int,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Eof => write!(f, "end of file"),
            Token::Exclam => write!(f, "`!`"),
            Token::Percent => write!(f, "`%`"),
            Token::LParen => write!(f, "`(`"),
            Token::RParen => write!(f, "`)`"),
            Token::Star => write!(f, "`*`"),
            Token::Plus => write!(f, "`+`"),
            Token::Comma => write!(f, "`,`"),
            Token::Hyphen => write!(f, "`-`"),
            Token::Dot => write!(f, "`.`"),
            Token::Slash => write!(f, "`/`"),
            Token::Colon => write!(f, "`:`"),
            Token::Semi => write!(f, "`;`"),
            Token::Less => write!(f, "`<`"),
            Token::Equal => write!(f, "`=`"),
            Token::Greater => write!(f, "`>`"),
            Token::LBracket => write!(f, "`[`"),
            Token::RBracket => write!(f, "`]`"),
            Token::LBrace => write!(f, "`{{`"),
            Token::RBrace => write!(f, "`}}`"),
            Token::ExclamEqual => write!(f, "`!=`"),
            Token::ColonColon => write!(f, "`::`"),
            Token::LessEqual => write!(f, "`<=`"),
            Token::EqualEqual => write!(f, "`==`"),
            Token::GreaterEqual => write!(f, "`>=`"),
            Token::As => write!(f, "`as`"),
            Token::Assume => write!(f, "`assume`"),
            Token::Context => write!(f, "`context`"),
            Token::Else => write!(f, "`else`"),
            Token::Fn => write!(f, "`fn`"),
            Token::If => write!(f, "`if`"),
            Token::Import => write!(f, "`import`"),
            Token::Let => write!(f, "`let`"),
            Token::Static => write!(f, "`static`"),
            Token::Struct => write!(f, "`struct`"),
            Token::This => write!(f, "`this`"),
            Token::Type => write!(f, "`type`"),
            Token::Use => write!(f, "`use`"),
            Token::Val => write!(f, "`val`"),
            Token::Var => write!(f, "`var`"),
            Token::While => write!(f, "`while`"),
            Token::Name => write!(f, "name"),
            Token::Str => write!(f, "string"),
            Token::Int => write!(f, "integer"),
        }
    }
}

pub type Tokens = IndexVec<TokenId, Token>;

pub type TokenStarts = IndexVec<TokenId, ByteIndex>;

#[derive(Clone, Copy, Debug)]
pub enum LexError {
    SourceTooLong,
    InvalidToken { start: ByteIndex, end: ByteIndex },
}

impl LexError {
    pub fn byte_range(self) -> Range<usize> {
        match self {
            LexError::SourceTooLong => {
                let max = ByteIndex::from(u32::MAX).index();
                max..max
            }
            LexError::InvalidToken { start, end } => start.index()..end.index(),
        }
    }

    pub fn message(self) -> &'static str {
        match self {
            LexError::SourceTooLong => "file size exceeds 4 GiB limit",
            LexError::InvalidToken { .. } => "invalid token",
        }
    }
}

pub fn lex(source: &str) -> Result<(Tokens, TokenStarts), LexError> {
    let Ok(eof) = u32::try_from(source.len()) else {
        return Err(LexError::SourceTooLong);
    };
    let mut tokens = IndexVec::new();
    let mut starts = IndexVec::new();
    for (result, range) in Token::lexer(source).spanned() {
        let start = ByteIndex::from_usize(range.start);
        let end = ByteIndex::from_usize(range.end);
        let token = result.map_err(|_| LexError::InvalidToken { start, end })?;
        tokens.push(token);
        starts.push(start);
    }
    tokens.push(Token::Eof);
    starts.push(ByteIndex::from(eof));
    Ok((tokens, starts))
}

pub fn relex(source: &str, starts: &TokenStarts, token: TokenId) -> Range<usize> {
    let start = starts[token].index();
    let (_, range) = Token::lexer(&source[start..]).spanned().next().unwrap();
    (range.start + start)..(range.end + start)
}
