use crate::parser;
use lspower::lsp::{
    Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, Position, Range, SemanticToken, SemanticTokenType,
};
use std::{convert::TryFrom, fmt::Debug, ptr, rc::Rc, thread};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;
use tokio::sync::{mpsc, oneshot};
use tree_sitter::{Node, Tree};
use url::Url;

#[derive(Debug)]
struct Ast(Tree);

impl PartialEq for Ast {
    fn eq(&self, other: &Self) -> bool {
        // only used by Tree-sitter after an update to try to save work if the result didn't "really
        // change", so we just do the bare minimum to make this an equivalence relation (by
        // comparing the references) rather than wasting time checking the entire tree for equality
        ptr::eq(self, other)
    }
}

impl Eq for Ast {}

#[derive(Clone, Copy, EnumIter)]
enum TokenType {
    Comment,
    String,
    Variable,
}

fn semantic_token_type(token_type: TokenType) -> SemanticTokenType {
    match token_type {
        TokenType::Comment => SemanticTokenType::COMMENT,
        TokenType::String => SemanticTokenType::STRING,
        TokenType::Variable => SemanticTokenType::VARIABLE,
    }
}

pub fn token_types() -> Vec<SemanticTokenType> {
    TokenType::iter().map(semantic_token_type).collect()
}

#[salsa::query_group(Storage)]
trait QueryGroup: salsa::Database {
    #[salsa::input]
    fn opened_files(&self) -> im::HashSet<Url>;

    #[salsa::input]
    fn source_text(&self, key: Url) -> Rc<String>;

    fn ast(&self, key: Url) -> Option<Rc<Ast>>;

    fn diagnostics(&self, key: Url) -> im::Vector<Diagnostic>;

    fn semantic_tokens(&self, key: Url) -> im::Vector<SemanticToken>;
}

fn ast(db: &dyn QueryGroup, key: Url) -> Option<Rc<Ast>> {
    if db.opened_files().contains(&key) {
        let mut parser = parser::parser();
        let text: &str = &db.source_text(key);
        let tree = parser.parse(text, None).unwrap();
        Some(Rc::new(Ast(tree)))
    } else {
        None
    }
}

// TODO: account for UTF-8 vs UTF-16
fn node_lsp_range(node: &Node) -> Range {
    let start = node.start_position();
    let end = node.end_position();
    Range {
        // TODO: figure out an alternative to unwrap here
        start: Position {
            line: u32::try_from(start.row).unwrap(),
            character: u32::try_from(start.column).unwrap(),
        },
        end: Position {
            line: u32::try_from(end.row).unwrap(),
            character: u32::try_from(end.column).unwrap(),
        },
    }
}

#[salsa::database(Storage)]
struct Database {
    storage: salsa::Storage<Self>,
}

impl Default for Database {
    fn default() -> Self {
        let mut db = Database {
            storage: salsa::Storage::default(),
        };
        db.set_opened_files(im::HashSet::new());
        db
    }
}

impl salsa::Database for Database {}

trait Processable<T> {
    fn process(self, db: &mut Database) -> T;
}

trait Request {
    fn handle(&mut self, db: &mut Database);
}

// https://stackoverflow.com/a/48066387
impl<T, U> Request for Option<(T, oneshot::Sender<U>)>
where
    T: Processable<U>,
{
    fn handle(&mut self, mut db: &mut Database) {
        // this should always match, but even if it doesn't, it just means it's already been handled
        if let Some((params, tx)) = self.take() {
            let _ = tx.send(params.process(&mut db));
        }
    }
}

type BoxedRequest = Box<dyn Request + Send>;

pub struct State {
    tx: mpsc::Sender<BoxedRequest>,
}

#[derive(Debug, thiserror::Error)]
pub enum AsyncError {
    #[error("state loop ended prematurely")]
    Send,
    #[error("failed to receive result from state loop")]
    Recv,
}

impl From<mpsc::error::SendError<BoxedRequest>> for AsyncError {
    fn from(_: mpsc::error::SendError<BoxedRequest>) -> Self {
        AsyncError::Send
    }
}

impl From<oneshot::error::RecvError> for AsyncError {
    fn from(_: oneshot::error::RecvError) -> Self {
        AsyncError::Recv
    }
}

impl State {
    pub fn new() -> Self {
        let (tx, mut rx) = mpsc::channel::<BoxedRequest>(1);
        // we do this in a non-async thread because our db isn't thread-safe
        thread::spawn(move || {
            let mut db = Database::default();
            // https://stackoverflow.com/a/52521592
            while let Some(mut request) = futures::executor::block_on(rx.recv()) {
                request.handle(&mut db);
            }
        });
        State { tx }
    }

    // confusing given that the Processable trait has a different method with the same name
    async fn process<T, U>(&self, params: T) -> Result<U, AsyncError>
    where
        T: Processable<U> + Send + 'static,
        U: Send + 'static,
    {
        let (tx, rx) = oneshot::channel();
        self.tx.send(Box::new(Some((params, tx)))).await?;
        let result = rx.await?;
        Ok(result)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum OpError<T: 'static + Debug + std::error::Error> {
    #[error(transparent)]
    Async(#[from] AsyncError),
    // TODO: figure out how to make this one transparent too
    #[error("operation-specific error")]
    Op(#[source] T),
}

#[derive(Debug, thiserror::Error)]
#[error("document was already opened")]
pub struct AlreadyOpenError {
    files: im::HashSet<Url>,
}

#[derive(Debug, thiserror::Error)]
#[error("document not yet opened")]
pub struct NotYetOpenedError {
    files: im::HashSet<Url>,
}

impl Database {
    fn open_document(&mut self, uri: Url, text: String) -> Result<(), AlreadyOpenError> {
        // we always call set_source_text, even if the file is already opened, because we want to
        // give the client the benefit of the doubt and assume that we've made a bookkeeping
        // mistake, rather than risk possibly dropping data
        self.set_source_text(uri.clone(), Rc::new(text));
        let mut files = self.opened_files();
        if let Some(_) = files.insert(uri) {
            Err(AlreadyOpenError { files })
        } else {
            self.set_opened_files(files);
            Ok(())
        }
    }
}

impl Processable<Result<(), AlreadyOpenError>> for DidOpenTextDocumentParams {
    fn process(self, db: &mut Database) -> Result<(), AlreadyOpenError> {
        let doc = self.text_document;
        db.open_document(doc.uri, doc.text)
    }
}

impl State {
    pub async fn open_document(
        &self,
        params: DidOpenTextDocumentParams,
    ) -> Result<(), OpError<AlreadyOpenError>> {
        self.process(params).await?.map_err(OpError::Op)
    }
}

impl Database {
    fn edit_document(&mut self, uri: Url, text: String) -> Result<(), NotYetOpenedError> {
        // we always call set_source_text, even if the file hadn't yet been opened, because we want
        // to give the client the benefit of the doubt and assume that we've made a bookkeeping
        // mistake, rather than risk possibly dropping data
        self.set_source_text(uri.clone(), Rc::new(text));
        let files = self.opened_files();
        if !files.contains(&uri) {
            Err(NotYetOpenedError { files })
        } else {
            Ok(())
        }
    }
}

impl Processable<Result<(), NotYetOpenedError>> for DidChangeTextDocumentParams {
    fn process(self, db: &mut Database) -> Result<(), NotYetOpenedError> {
        let uri = self.text_document.uri;
        // TODO: ensure that this is a full text sync
        let text = self
            .content_changes
            .into_iter()
            .map(|x| x.text)
            .collect::<Vec<_>>()
            .join("");
        db.edit_document(uri, text)
    }
}

impl State {
    pub async fn edit_document(
        &self,
        params: DidChangeTextDocumentParams,
    ) -> Result<(), OpError<NotYetOpenedError>> {
        self.process(params).await?.map_err(OpError::Op)
    }
}

impl Database {
    fn close_document(&mut self, uri: Url) -> Result<(), NotYetOpenedError> {
        // Salsa doesn't seem to support removing inputs https://github.com/salsa-rs/salsa/issues/37
        // so we just free most of the memory (hopefully?) by setting it to the empty string; also,
        // we always call set_source_text, even if the file hadn't yet been opened, because we want
        // to give the client the benefit of the doubt and assume that we've just made a bookkeeping
        // mistake
        self.set_source_text(uri.clone(), Rc::new(String::from("")));
        let mut files = self.opened_files();
        if let None = files.remove(&uri) {
            Err(NotYetOpenedError { files })
        } else {
            self.set_opened_files(files);
            Ok(())
        }
    }
}

impl Processable<Result<(), NotYetOpenedError>> for DidCloseTextDocumentParams {
    fn process(self, db: &mut Database) -> Result<(), NotYetOpenedError> {
        db.close_document(self.text_document.uri)
    }
}

impl State {
    pub async fn close_document(
        &self,
        params: DidCloseTextDocumentParams,
    ) -> Result<(), OpError<NotYetOpenedError>> {
        self.process(params).await?.map_err(OpError::Op)
    }
}

fn make_diagnostic(range: Range, message: String, severity: DiagnosticSeverity) -> Diagnostic {
    let mut diag = Diagnostic::new_simple(range, message);
    diag.severity = Some(severity);
    diag
}

fn diagnostics_helper(node: &Node) -> im::Vector<Diagnostic> {
    if let Some(message) = {
        if node.is_error() {
            Some("error")
        } else if node.is_missing() {
            Some("missing")
        } else {
            None
        }
    } {
        im::vector![make_diagnostic(
            node_lsp_range(node),
            format!("syntax {}", message),
            DiagnosticSeverity::Error,
        )]
    } else {
        let mut diagnostics = im::vector![];
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            diagnostics.append(diagnostics_helper(&child));
        }
        diagnostics
    }
}

fn diagnostics(db: &dyn QueryGroup, key: Url) -> im::Vector<Diagnostic> {
    match db.ast(key) {
        None => im::vector![],
        Some(tree) => diagnostics_helper(&tree.0.root_node()),
    }
}

struct DiagnosticsRequest(Url);

impl Processable<im::Vector<Diagnostic>> for DiagnosticsRequest {
    fn process(self, db: &mut Database) -> im::Vector<Diagnostic> {
        let DiagnosticsRequest(uri) = self;
        // TODO: warn if the document wasn't already opened
        db.diagnostics(uri)
    }
}

impl State {
    pub async fn get_diagnostics(&self, uri: Url) -> Result<im::Vector<Diagnostic>, AsyncError> {
        self.process(DiagnosticsRequest(uri)).await
    }
}

#[derive(Clone)]
struct AbsoluteToken {
    line: u32,
    start: u32,
    length: u32,
    token_type: TokenType,
}

fn absolute_tokens(node: &Node) -> im::Vector<AbsoluteToken> {
    if let Some(token_type) = match node.kind() {
        // TODO: make these not stringly typed
        "comment" => Some(TokenType::Comment),
        "string" => Some(TokenType::String),
        "identifier" => Some(TokenType::Variable),
        _ => None,
    } {
        let range = node_lsp_range(node);
        if range.start.line == range.end.line {
            return im::vector![AbsoluteToken {
                line: range.start.line,
                start: range.start.character,
                length: range.end.character - range.start.character,
                token_type,
            }];
        }
    }
    let mut tokens = im::vector![];
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        tokens.append(absolute_tokens(&child));
    }
    tokens
}

fn make_relative(tokens: im::Vector<AbsoluteToken>) -> im::Vector<SemanticToken> {
    let mut relative = im::vector![];
    let mut it = tokens.iter();
    match it.next() {
        None => relative,
        Some(first) => {
            relative.push_back(SemanticToken {
                delta_line: first.line,
                delta_start: first.start,
                length: first.length,
                token_type: first.token_type as u32,
                token_modifiers_bitset: 0,
            });
            let mut last_line = first.line;
            let mut last_start = first.start;
            for token in it {
                relative.push_back(SemanticToken {
                    delta_line: token.line - last_line,
                    delta_start: if token.line == last_line {
                        token.start - last_start
                    } else {
                        token.start
                    },
                    length: token.length,
                    token_type: token.token_type as u32,
                    token_modifiers_bitset: 0,
                });
                last_line = token.line;
                last_start = token.start;
            }
            relative
        }
    }
}

fn semantic_tokens(db: &dyn QueryGroup, key: Url) -> im::Vector<SemanticToken> {
    match db.ast(key) {
        None => im::vector![],
        Some(tree) => make_relative(absolute_tokens(&tree.0.root_node())),
    }
}

struct TokensRequest(Url);

impl Processable<im::Vector<SemanticToken>> for TokensRequest {
    fn process(self, db: &mut Database) -> im::Vector<SemanticToken> {
        let TokensRequest(uri) = self;
        // TODO: warn if the document wasn't already opened
        db.semantic_tokens(uri)
    }
}

impl State {
    pub async fn get_semantic_tokens(
        &self,
        uri: Url,
    ) -> Result<im::Vector<SemanticToken>, AsyncError> {
        self.process(TokensRequest(uri)).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::{fs::File, io::Read};

    fn foo_db(text: String) -> (Database, Url) {
        let mut db = Database::default();
        let uri = Url::parse("file:///tmp/foo.qn").unwrap();
        db.open_document(uri.clone(), String::from(text)).unwrap();
        (db, uri)
    }

    fn make_range(
        start_line: u32,
        start_character: u32,
        end_line: u32,
        end_character: u32,
    ) -> Range {
        Range::new(
            Position::new(start_line, start_character),
            Position::new(end_line, end_character),
        )
    }

    fn make_error(sl: u32, sc: u32, el: u32, ec: u32, message: &str) -> Diagnostic {
        make_diagnostic(
            make_range(sl, sc, el, ec),
            String::from(message),
            DiagnosticSeverity::Error,
        )
    }

    #[test]
    fn test_diagnostics_hello_world() {
        let (db, uri) = foo_db(String::from("print(\"Hello,\"\" world!\")"));
        let diagnostics = db.diagnostics(uri);
        assert_eq!(
            diagnostics,
            im::vector![
                make_error(0, 6, 0, 14, "syntax error"),
                make_error(0, 24, 0, 24, "syntax missing"),
            ],
        );
    }

    fn make_token(
        delta_line: u32,
        delta_start: u32,
        length: u32,
        token_type: TokenType,
    ) -> SemanticToken {
        SemanticToken {
            delta_line,
            delta_start,
            length,
            token_type: token_type as u32,
            token_modifiers_bitset: 0,
        }
    }

    #[test]
    fn test_tokens_hello_world() {
        let (db, uri) = foo_db({
            let mut file = File::open("examples/hello.qn").unwrap();
            let mut contents = String::new();
            file.read_to_string(&mut contents).unwrap();
            contents
        });
        let tokens = db.semantic_tokens(uri);
        assert_eq!(
            tokens,
            im::vector![
                make_token(0, 0, 21, TokenType::Comment),
                make_token(2, 0, 5, TokenType::Variable),
                make_token(0, 6, 15, TokenType::String),
            ],
        );
    }
}
