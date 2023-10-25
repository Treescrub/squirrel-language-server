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
    
    pub async fn parse(&mut self) -> Result<Script, String> {
        let client = self.client;

        client.log_message(MessageType::INFO, "Starting parse").await;

        return Ok(self.script().await?);
    }

    fn next_token(&mut self) {
        loop {
            self.token_index += 1;

            if self.is_end_of_tokens() || self.current_token_type() != TokenType::NewLine {
                break;
            }
        }
    }
    
    fn prev_token(&self) -> Option<&'a Token> {
        return self.tokens.get(self.token_index - 1);
    }

    fn current_token(&self) -> &'a Token {
        return self.tokens.get(self.token_index).unwrap();
    }

    fn current_token_type(&self) -> TokenType {
        return self.current_token().token_type;
    }
    
    fn expect(&self, token_type: TokenType) -> Result<(), String> {
        if self.current_token_type() != token_type {
            return Err(format!("Expected token `{}`, got `{}`", token_type, self.current_token_type()));
        }

        return Ok(());
    }

    async fn script(&mut self) -> Result<Script, String> {
        let mut statements: Vec<Statement> = Vec::new();
        // while !self.is_end_of_tokens() {
            statements.push(self.statement().await?);
            self.optional_semicolon()?;
        // }

        return Ok(Script { statements })
    }

    fn optional_semicolon(&mut self) -> Result<(), String> {
        if self.prev_token().is_some() && (self.prev_token().unwrap().token_type == TokenType::RightCurly || self.prev_token().unwrap().token_type == TokenType::Semicolon) {
            return Ok(());
        }

        if self.current_token_type() == TokenType::Semicolon {
            self.next_token();
            return Ok(());
        }

        if !self.is_end_of_statement() {
            return Err(String::from("expected end of statement"));
        }

        return Ok(());
    }

    fn is_end_of_tokens(&self) -> bool {
        return self.token_index == self.tokens.len();
    }

    fn is_end_of_statement(&self) -> bool {
        return (self.prev_token().is_some() && self.prev_token().unwrap().token_type == TokenType::NewLine)
                || self.is_end_of_tokens() || self.current_token_type() == TokenType::RightCurly || self.current_token_type() == TokenType::Semicolon;
    }

    async fn statement(&mut self) -> Result<Statement, String> {
        self.client.log_message(MessageType::INFO, "statement!").await;
        match self.current_token().token_type {
            TokenType::LeftCurly => {
                self.next_token();
                let statements = self.statements().await?;
                self.expect(TokenType::RightCurly)?;

                return Ok(Statement::Statements(statements));
            }
            TokenType::Break => {
                self.next_token();

                return Ok(Statement::Break);
            }
            TokenType::Continue => {
                self.next_token();

                return Ok(Statement::Continue);
            }
            TokenType::Const => {
                self.next_token();

                return self.const_statement();
            }
            val => {
                self.next_token();
                return Err(format!("Unhandled token '{}' in statement", val));
            }
        }
    }

    fn const_statement(&mut self) -> Result<Statement, String> {
        self.expect(TokenType::Identifier)?;
        let id = self.current_token().svalue.as_ref().unwrap();
        self.next_token();
        self.expect(TokenType::Assign)?;
        self.next_token();
        let scalar = self.scalar()?;
        self.next_token();
        self.optional_semicolon()?;

        return Ok(Statement::Const(Identifier {value: id.clone()}, scalar));
    }

    fn scalar(&mut self) -> Result<Scalar, String> {
        match self.current_token_type() {
            TokenType::IntegerLiteral => {
                return Ok(Scalar::Integer);
            }
            TokenType::FloatLiteral => {
                return Ok(Scalar::Float);
            }
            TokenType::StringLiteral => {
                return Ok(Scalar::StringLiteral);
            }
            TokenType::True => {
                return Ok(Scalar::True);
            }
            TokenType::False => {
                return Ok(Scalar::False);
            }
            TokenType::Minus => {
                self.next_token();
                match self.current_token_type() {
                    TokenType::IntegerLiteral => {
                        return Ok(Scalar::Integer);
                    }
                    TokenType::FloatLiteral => {
                        return Ok(Scalar::Float);
                    }
                    unhandled_type => {
                        return Err(format!("expected int or float scalar, got {}", unhandled_type));
                    }
                }
            }
            unhandled_type => {
                return Err(format!("expected int, float, or string scalar, got {}", unhandled_type));
            }
        }
    }

    #[async_recursion]
    async fn statements(&mut self) -> Result<Statements, String> {
        self.client.log_message(MessageType::INFO, "Start statements").await;
        let mut statements: Vec<Statement> = Vec::new();
        while self.current_token_type() != TokenType::RightCurly && self.current_token_type() != TokenType::Default && self.current_token_type() != TokenType::Case {
            statements.push(self.statement().await?);

            self.optional_semicolon()?;
        }
        self.client.log_message(MessageType::INFO, "Finished statements").await;

        return Ok(Statements {statements})
    }
}