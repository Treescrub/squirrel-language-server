#![allow(clippy::needless_return)]

#[cfg(test)]
mod tests;
mod document_manager;
mod analysis;
mod visitors;
mod settings;

use std::collections::HashSet;
use std::fmt::Display;

use analysis::ast::AstNode;
use analysis::ast::Identifier;
use analysis::ast::Script;
use analysis::parser::ParseError;
use analysis::source_info::SourceLocation;
use settings::DebugSettings;
use visitors::identifier_collector::IdentifierCollector;
use crate::analysis::lexer::Lexer;
use crate::analysis::parser::Parser;
use document_manager::DocumentManager;
use serde_json::Value;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use visitors::SimpleVisitorMut;
use visitors::pretty_printer::PrettyPrinter;

struct Backend {
    client: Client,
    state: Mutex<State>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            state: Mutex::new(State::new()),
        }
    }

    async fn parse_file(&self, uri: &Url, version: i32) {
        let state = self.state.lock().await;
        let mut lexer: Lexer = Lexer::new(state.doc_manager.get(uri).unwrap());
        lexer.lex();
        
        let mut parser: Parser = Parser::new(&lexer.tokens);
        drop(state);

        let parse_result = parser.parse();
        self.handle_parse_result(parse_result, uri.clone(), version).await;
    }

    async fn handle_parse_result(&self, result: std::result::Result<AstNode<Script>, ParseError>, uri: Url, version: i32) {
        match result {
            Ok(script) => {
                self.client.log_message(MessageType::INFO, "Successfully parsed").await;
                self.client.publish_diagnostics(uri, Vec::new(), Some(version)).await;

                let mut identifier_collector = IdentifierCollector::new();
                identifier_collector.visit_script(&script);

                let mut state = self.state.lock().await;
                state.identifiers = identifier_collector.identifiers;
                drop(state);

                let mut pretty_printer = PrettyPrinter::new();
                pretty_printer.visit_script(&script);

                self.print_verbose(MessageType::INFO, "Pretty printer:").await;
                self.print_verbose(MessageType::LOG, pretty_printer.text).await;
            }
            Err(error) => {
                self.client.log_message(MessageType::ERROR, format!("Failed to parse: {}", error)).await;
                self.client.log_message(MessageType::INFO, format!("start: (line: {}, column: {}), end: (line: {}, column: {})",
                        error.range.start.line, error.range.start.column, error.range.end.line, error.range.end.column)).await;
                let mut diagnostics = Vec::new();
                diagnostics.push(Diagnostic::new_simple(Range { start: error.range.start.to_position(), end: error.range.end.to_position() }, error.message));
                self.client.publish_diagnostics(uri, diagnostics, Some(version)).await;
            }
        }
    }

    async fn print_verbose<M: Display>(&self, typ: MessageType, message: M) {
        let config = self.client.configuration(vec![DebugSettings::VerbosePrint.into()]).await;
        
        if config.is_err() || config.unwrap()[0].as_bool().unwrap() {
            self.client.log_message(typ, message).await;
        }
    }

    fn build_keyword_completions() -> Vec<CompletionItem> {
        let keywords = vec!["while", "do", "if", "else", "break", "continue", "return", "null", "function", "local", "for", "foreach", 
                "in", "typeof", "base", "delete", "try", "catch", "throw", "clone", "yield", "resume", "switch", "case", "default", "this", "class",
                "extends", "constructor", "instanceof", "true", "false", "static", "enum", "const", "__LINE__", "__FILE__", "rawcall"];
        let mut items = Vec::new();

        for keyword in keywords {
            items.push(CompletionItem {
                label: String::from(keyword),
                kind: Some(CompletionItemKind::KEYWORD),
                ..CompletionItem::default()
            });
        }

        return items;
    }
}

struct State {
    doc_manager: DocumentManager,
    identifiers: Vec<AstNode<Identifier>>,
}

impl State {
    fn new() -> Self {
        Self {
            doc_manager: DocumentManager::new(),
            identifiers: Vec::new(),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    ..Default::default()
                }),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["dummy.do_something".to_string()],
                    work_done_progress_options: Default::default(),
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                ..ServerCapabilities::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
 
        let mut state = self.state.lock().await;
        state.doc_manager.open_file(&params.text_document.text, &params.text_document.uri);
        self.print_verbose(MessageType::INFO, format!("open files: {}", state.doc_manager.total_open_files())).await;
        drop(state);

        self.parse_file(&params.text_document.uri, params.text_document.version).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file changed!")
            .await;

        let mut state = self.state.lock().await;
        for content_change in &params.content_changes {
            state.doc_manager.edit_file(&content_change.text, &params.text_document.uri, content_change.range);
        }
        drop(state);

        self.parse_file(&params.text_document.uri, params.text_document.version).await;
    }

    async fn did_save(&self, _params: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;

        let mut state = self.state.lock().await;
        state.doc_manager.close_file(&params.text_document.uri);
        self.print_verbose(MessageType::INFO, format!("open files: {}", state.doc_manager.total_open_files())).await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let mut items = Self::build_keyword_completions();
        let completion_pos = params.text_document_position.position;
        let mut identifier_names: HashSet<String> = HashSet::new();

        let state = self.state.lock().await;

        for identifier in &state.identifiers {
            if identifier.range.end == SourceLocation::from_position(completion_pos) {
                continue;
            }
            if identifier_names.contains(&identifier.value.value) {
                continue;
            }

            identifier_names.insert(identifier.value.value.clone());

            items.push(CompletionItem {
                label: identifier.value.value.clone(),
                kind: Some(CompletionItemKind::VARIABLE),
                ..CompletionItem::default()
            });
        }

        Ok(Some(CompletionResponse::Array(items)))
    }
}

#[tokio::main]
async fn main() {
    #[cfg(feature = "runtime-agnostic")]
    use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

    tracing_subscriber::fmt().init();

    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    #[cfg(feature = "runtime-agnostic")]
    let (stdin, stdout) = (stdin.compat(), stdout.compat_write());

    let (service, socket) = LspService::new(Backend::new);

    Server::new(stdin, stdout, socket).serve(service).await;
}
