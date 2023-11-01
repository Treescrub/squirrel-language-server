mod lexer;
#[cfg(test)]
mod tests;
mod document_manager;
mod parser;
mod ast;
mod visitors;

use document_manager::DocumentManager;
use serde_json::Value;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use visitors::{PrettyPrinter, SimpleVisitorMut};
use crate::lexer::Lexer;
use crate::parser::Parser;

#[derive(Debug)]
struct Backend {
    client: Client,
    state: Mutex<State>,
}

#[derive(Debug)]
struct State {
    doc_manager: DocumentManager,
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
        let mut lexer: Lexer = Lexer::new(&params.text_document.text);
        lexer.lex();
        let mut tokens: String = String::from("");
        for token in &lexer.tokens {
            tokens.push_str(&token.to_string());
            tokens.push('\n');
        }
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.client
            .log_message(MessageType::INFO, format!("tokens:\n{}", tokens))
            .await;
        
        let mut parser: Parser = Parser::new(&lexer.tokens, &self.client);
        let parse_result = parser.parse();
        match parse_result {
            Ok(script) => {
                self.client.log_message(MessageType::INFO, "Successfully parsed").await;
                let mut pretty_printer = PrettyPrinter::new();

                pretty_printer.visit_script(script);
                self.client.log_message(MessageType::INFO, "Pretty printer:").await;
                self.client.log_message(MessageType::LOG, pretty_printer.text).await;
            }
            Err(message) => {
                self.client.log_message(MessageType::ERROR, format!("Failed to parse: {}", message)).await;
            }
        }
        
        let mut state = self.state.lock().await;
        state.doc_manager.open_file(&params.text_document.text, &params.text_document.uri);
        /*self.client
            .log_message(MessageType::INFO, format!("contents: {}", state.doc_manager.get(&params.text_document.uri).unwrap()))
            .await;*/
        self.client
            .log_message(MessageType::INFO, format!("open files: {}", state.doc_manager.total_open_files()))
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
        .log_message(MessageType::INFO, "file changed!")
        .await;

        let mut state = self.state.lock().await;
        for content_change in &params.content_changes {
            state.doc_manager.edit_file(&content_change.text, &params.text_document.uri, content_change.range.unwrap());
        }
        /*self.client
            .log_message(MessageType::INFO, format!("contents: {}", state.doc_manager.get(&params.text_document.uri).unwrap()))
            .await;*/
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
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
        self.client
            .log_message(MessageType::INFO, format!("open files: {}", state.doc_manager.total_open_files()))
            .await;
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Hello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
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

    let (service, socket) = LspService::new(|client| Backend { client, state: Mutex::new(State { doc_manager: DocumentManager::new() }) });
    Server::new(stdin, stdout, socket).serve(service).await;
}
