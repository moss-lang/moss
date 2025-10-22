use std::{fmt, ops::Range};

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

#[derive(Debug, EnumSetType, Logos)]
#[logos(skip r"(#[^\n]+|\s)+")]
pub enum Token {
    Eof,

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

    #[token("::")]
    ColonColon,

    #[token("context")]
    Context,

    #[token("end")]
    End,

    #[token("fn")]
    Fn,

    #[token("need")]
    Need,

    #[token("provide")]
    Provide,

    #[token("start")]
    Start,

    #[regex(r"[A-Z_a-z]\w*")]
    Name,

    #[regex(r#""[^"]*""#)]
    Str,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Eof => write!(f, "end of file"),
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
            Token::ColonColon => write!(f, "`::`"),
            Token::Context => write!(f, "`context`"),
            Token::End => write!(f, "`end`"),
            Token::Fn => write!(f, "`fn`"),
            Token::Need => write!(f, "`need`"),
            Token::Provide => write!(f, "`provide`"),
            Token::Start => write!(f, "`start`"),
            Token::Name => write!(f, "name"),
            Token::Str => write!(f, "string"),
        }
    }
}

pub type Tokens = IndexVec<TokenId, Token>;

pub type TokenStarts = IndexVec<TokenId, ByteIndex>;

#[derive(Debug)]
pub enum LexError {
    SourceTooLong,
    InvalidToken { start: ByteIndex, end: ByteIndex },
}

impl LexError {
    pub fn byte_range(&self) -> Range<usize> {
        match *self {
            LexError::SourceTooLong => {
                let max = ByteIndex::from(u32::MAX).index();
                max..max
            }
            LexError::InvalidToken { start, end } => start.index()..end.index(),
        }
    }

    pub fn message(&self) -> &str {
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
