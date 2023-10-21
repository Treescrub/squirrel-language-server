use tower_lsp::Client;
use tower_lsp::lsp_types::MessageType;
use async_recursion::async_recursion;

use crate::ast::*;
use crate::lexer::*;

pub struct Parser<'a> {
    root_node: Option<Statement>,
    tokens: &'a Vec<Token>,
    token_index: usize,
    client: &'a Client,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>, client: &'a Client) -> Self {
        Self {
            root_node: None,
            tokens,
            token_index: 0,
            client,
        }
    }
    
    pub async fn parse(&mut self) {
        let client = self.client;

        client.log_message(MessageType::LOG, "Starting parse").await;
        self.statement().await;
    }

    fn next_token(&mut self) -> &'a Token {
        self.token_index += 1;

        return self.current_token();
    }
    
    fn current_token(&self) -> &'a Token {
        return self.tokens.get(self.token_index).unwrap();
    }

    fn current_token_type(&self) -> TokenType {
        return self.current_token().token_type;
    }
    
    async fn expect(&self, token_type: TokenType) {
        if self.current_token_type() != token_type {
            self.client
                .log_message(MessageType::ERROR, format!("Expected token `{}`, got `{}`", token_type, self.current_token_type()))
                .await;
        }
    }

    async fn statement(&mut self) {
        self.client.log_message(MessageType::LOG, "statement!").await;
        match self.current_token().token_type {
            TokenType::LeftCurly => {
                self.next_token();
                self.statements().await;
                self.expect(TokenType::RightCurly).await;
            }
            val => {
                self.next_token();
                self.client.log_message(MessageType::WARNING, format!("Unhandled token {} in statement", val)).await;
            }
        }
    }

    #[async_recursion]
    async fn statements(&mut self) {
        while self.current_token_type() != TokenType::RightCurly && self.current_token_type() != TokenType::Default && self.current_token_type() != TokenType::Case {
            self.statement().await;
        }
    }
}