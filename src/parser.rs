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
    
    pub async fn parse(&mut self) -> Result<Statement, String> {
        let client = self.client;

        client.log_message(MessageType::INFO, "Starting parse").await;
        let statement = self.statement().await?;

        return Ok(statement);
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
    
    async fn expect(&self, token_type: TokenType) -> Result<(), String> {
        if self.current_token_type() != token_type {
            self.client
                .log_message(MessageType::ERROR, format!("Expected token `{}`, got `{}`", token_type, self.current_token_type()))
                .await;
            
            return Err(format!("Expected token `{}`, got `{}`", token_type, self.current_token_type()));
        }

        return Ok(());
    }

    async fn statement(&mut self) -> Result<Statement, String> {
        self.client.log_message(MessageType::INFO, "statement!").await;
        match self.current_token().token_type {
            TokenType::LeftCurly => {
                self.next_token();
                let statements = self.statements().await?;
                self.expect(TokenType::RightCurly).await?;

                return Ok(Statement::Statements(statements));
            }
            val => {
                self.next_token();
                self.client.log_message(MessageType::WARNING, format!("Unhandled token `{}` in statement", val)).await;
                return Err(format!("Unhandled token '{}' in statement", val));
            }
        }
    }

    #[async_recursion]
    async fn statements(&mut self) -> Result<Statements, String> {
        let mut statements: Vec<Statement> = Vec::new();
        while self.current_token_type() != TokenType::RightCurly && self.current_token_type() != TokenType::Default && self.current_token_type() != TokenType::Case {
            let statement: Statement = self.statement().await?;
            statements.push(statement);
        }
        self.client.log_message(MessageType::INFO, "Finished statements").await;

        return Ok(Statements {statements})
    }
}