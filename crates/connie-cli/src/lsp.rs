use std::{collections::HashMap, process::ExitCode};

use anyhow::anyhow;
use crossbeam_channel::Sender;
use lsp_server::{Connection, Message, RequestId, ResponseError};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, Hover, HoverContents, HoverParams, HoverProviderCapability,
    MarkupContent, MarkupKind, MessageType, ServerCapabilities, ShowMessageParams,
    TextDocumentSyncCapability, TextDocumentSyncKind,
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument,
        Notification, ShowMessage,
    },
    request::{HoverRequest, Request},
};
use serde_json::Value;

use crate::util::err_fail;

type ResponseResult<T> = Result<T, ResponseError>;

fn notify<N: Notification>(sender: &Sender<Message>, params: N::Params) -> anyhow::Result<()> {
    sender.send(Message::Notification(lsp_server::Notification {
        method: N::METHOD.to_owned(),
        params: serde_json::to_value(params)?,
    }))?;
    Ok(())
}

struct State {
    sender: Sender<Message>,
}

impl State {
    fn did_open_text_document(&mut self, _: DidOpenTextDocumentParams) -> anyhow::Result<()> {
        Ok(())
    }

    fn did_change_text_document(&mut self, _: DidChangeTextDocumentParams) -> anyhow::Result<()> {
        Ok(())
    }

    fn did_save_text_document(&mut self, _: DidSaveTextDocumentParams) -> anyhow::Result<()> {
        Ok(())
    }

    fn did_close_text_document(&mut self, _: DidCloseTextDocumentParams) -> anyhow::Result<()> {
        Ok(())
    }

    fn hover(&self, _: HoverParams) -> ResponseResult<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::PlainText,
                value: "Hello, world!".to_string(),
            }),
            range: None,
        }))
    }
}

type RequestHandler = Box<dyn Fn(&State, RequestId, Value) -> anyhow::Result<()>>;

struct Requests {
    handlers: HashMap<&'static str, RequestHandler>,
}

impl Requests {
    fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }

    fn with<R: Request>(mut self, f: fn(&State, R::Params) -> ResponseResult<R::Result>) -> Self {
        self.handlers.insert(
            R::METHOD,
            Box::new(move |state, id, params| {
                let (result, error) = match f(state, serde_json::from_value(params)?) {
                    Ok(result) => (Some(serde_json::to_value(result)?), None),
                    Err(error) => (None, Some(error)),
                };
                state.sender.send(Message::Response(lsp_server::Response {
                    id,
                    result,
                    error,
                }))?;
                Ok(())
            }),
        );
        self
    }

    fn handle(&self, state: &State, req: lsp_server::Request) -> anyhow::Result<()> {
        let method: &str = &req.method;
        match self.handlers.get(method) {
            Some(handler) => handler(state, req.id, req.params),
            None => Err(anyhow!("unimplemented request: {method}")),
        }
    }
}

type NotificationHandler = Box<dyn Fn(&mut State, Value) -> anyhow::Result<()>>;

struct Notifications {
    handlers: HashMap<&'static str, NotificationHandler>,
}

impl Notifications {
    fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }

    fn with<N: Notification>(mut self, f: fn(&mut State, N::Params) -> anyhow::Result<()>) -> Self {
        self.handlers.insert(
            N::METHOD,
            Box::new(move |state, params| f(state, serde_json::from_value(params)?)),
        );
        self
    }

    fn handle(&self, state: &mut State, not: lsp_server::Notification) -> anyhow::Result<()> {
        let method: &str = &not.method;
        match self.handlers.get(method) {
            Some(handler) => handler(state, not.params),
            None => {
                if method.starts_with("$/") {
                    Ok(())
                } else {
                    Err(anyhow!("unimplemented notification: {method}"))
                }
            }
        }
    }
}

fn run(connection: &Connection) -> anyhow::Result<()> {
    let reqs = Requests::new().with::<HoverRequest>(State::hover);
    let nots = Notifications::new()
        .with::<DidChangeTextDocument>(State::did_change_text_document)
        .with::<DidCloseTextDocument>(State::did_close_text_document)
        .with::<DidOpenTextDocument>(State::did_open_text_document)
        .with::<DidSaveTextDocument>(State::did_save_text_document);
    connection.initialize(serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::INCREMENTAL,
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        ..Default::default()
    })?)?;
    let mut state = State {
        sender: connection.sender.clone(),
    };
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    break;
                }
                reqs.handle(&state, req)?;
            }
            Message::Response(_) => unreachable!(),
            Message::Notification(not) => nots.handle(&mut state, not)?,
        }
    }
    Ok(())
}

pub fn language_server() -> ExitCode {
    let (connection, io_threads) = Connection::stdio();
    let exit_code = match run(&connection) {
        Ok(()) => ExitCode::SUCCESS,
        Err(err) => {
            if notify::<ShowMessage>(
                &connection.sender,
                ShowMessageParams {
                    typ: MessageType::ERROR,
                    message: err.to_string(),
                },
            )
            .is_err()
            {
                err_fail(err.context("language server error"));
            }
            drop(connection); // Otherwise thread join below hangs, client thinks server's still up.
            ExitCode::FAILURE
        }
    };
    match io_threads.join() {
        Ok(()) => exit_code,
        Err(err) => err_fail(anyhow!(err).context("failed to close IO threads")),
    }
}
