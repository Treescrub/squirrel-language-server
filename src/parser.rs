use tower_lsp::Client;

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
    
    pub fn parse(&mut self) -> Result<Script, String> {
        let client = self.client;

        return Ok(self.script()?);
    }

    fn next_token(&mut self) -> &Token {
        loop {
            self.token_index += 1;

            if self.is_end_of_tokens() || self.current_token_type() != TokenType::NewLine {
                break;
            }
        }

        return self.current_token();
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

    fn current_token_and_advance(&mut self) -> &Token {
        let current = self.current_token();
        self.next_token();

        return current;
    }
    
    fn expect(&self, token_type: TokenType) -> Result<(), String> {
        if self.current_token_type() != token_type {
            return Err(format!("Expected token `{}`, got `{}`", token_type, self.current_token_type()));
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

    fn script(&mut self) -> Result<Script, String> {
        let mut statements: Vec<Statement> = Vec::new();
        // while !self.is_end_of_tokens() {
            statements.push(self.statement()?);
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

    fn statement(&mut self) -> Result<Statement, String> {
        match self.current_token().token_type {
            TokenType::LeftCurly => {
                self.next_token();
                let statements = self.statements()?;
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

    fn factor(&mut self) -> Result<Factor, String> {
        let token_type = self.current_token_type();
        let token = self.current_token();
        self.next_token();

        match token_type {
            TokenType::StringLiteral => {
                return Ok(Factor::Scalar(Scalar::StringLiteral));
            }
            TokenType::Base => {
                return Ok(Factor::Base);
            }
            TokenType::Identifier => {
                return Ok(Factor::Identifier(Identifier { value: token.svalue.as_ref().unwrap().clone() }));
            }
            TokenType::Constructor => {
                return Ok(Factor::Constructor);
            }
            TokenType::This => {
                return Ok(Factor::This);
            }
            /*TokenType::DoubleColon => {
                return Ok(Factor::DoubleColon);
            }*/
            TokenType::Null => {
                return Ok(Factor::Null);
            }
            TokenType::IntegerLiteral => {
                return Ok(Factor::Scalar(Scalar::Integer));
            }
            TokenType::FloatLiteral => {
                return Ok(Factor::Scalar(Scalar::Float));
            }
            TokenType::True => {
                return Ok(Factor::Scalar(Scalar::True));
            }
            TokenType::False => {
                return Ok(Factor::Scalar(Scalar::False));
            }
            TokenType::LeftSquare => {
                todo!();
            }
            TokenType::LeftCurly => {
                todo!();
            }
            TokenType::Function => {
                todo!();
            }
            TokenType::Class => {
                todo!();
            }
            TokenType::Minus => {
                self.next_token();
                match self.current_token_type() {
                    TokenType::IntegerLiteral => {
                        return Ok(Factor::Scalar(Scalar::Integer));
                    }
                    TokenType::FloatLiteral => {
                        return Ok(Factor::Scalar(Scalar::Float));
                    }
                    _ => {
                        todo!();
                    }
                }
            }
            TokenType::LogicalNot => {
                todo!();
            }
            token_type => {
                return Err(format!("Unexpected token for factor: {}", token_type));
            }
        }
    }

    fn statements(&mut self) -> Result<Statements, String> {
        let mut statements: Vec<Statement> = Vec::new();
        while self.current_token_type() != TokenType::RightCurly && self.current_token_type() != TokenType::Default && self.current_token_type() != TokenType::Case {
            statements.push(self.statement()?);

            self.optional_semicolon()?;
        }

        return Ok(Statements {statements})
    }

    fn expression(&mut self) -> Result<Expression, String> {
        let logical_or = self.logical_or_expression()?;
        
        match self.current_token_type() {
            TokenType::Newslot => {
                let expression = self.expression()?;

                return Ok(Expression::Newslot);
            }
            _ => todo!()
        }
    }

    fn logical_or_expression(&mut self) -> Result<LogicalOrExpression, String> {
        let left = self.logical_and_expression()?;
        let right = self.logical_or_expression()?;

        return Ok(LogicalOrExpression { left, right: Box::new(right) });
    }

    fn logical_and_expression(&mut self) -> Result<LogicalAndExpression, String> {
        let left = self.bitwise_or_expression()?;
        let right = self.logical_and_expression()?;

        return Ok(LogicalAndExpression { left, right: Box::new(right) });
    }

    fn bitwise_or_expression(&mut self) -> Result<BitwiseOrExpression, String> {
        let left = self.bitwise_xor_expression()?;
        let right = self.bitwise_xor_expression()?;
        
        return Ok(BitwiseOrExpression { left, right });
    }

    fn bitwise_xor_expression(&mut self) -> Result<BitwiseXorExpression, String> {
        let left = self.bitwise_and_expression()?;
        let right = self.bitwise_and_expression()?;
        
        return Ok(BitwiseXorExpression { left, right });
    }

    fn bitwise_and_expression(&mut self) -> Result<BitwiseAndExpression, String> {
        let left = self.equal_expression()?;
        let right = self.equal_expression()?;
        
        return Ok(BitwiseAndExpression { left, right });
    }

    fn equal_expression(&mut self) -> Result<EqualExpression, String> {
        let left = self.compare_expression()?;
        let operator = self.next_token().token_type;
        let right = self.compare_expression()?;
        
        return Ok(EqualExpression { left, operator, right });
    }

    fn compare_expression(&mut self) -> Result<CompareExpression, String> {
        let left = self.shift_expression()?;
        let operator = self.next_token().token_type;
        let right = self.shift_expression()?;

        return Ok(CompareExpression { left, operator, right });
    }

    fn shift_expression(&mut self) -> Result<ShiftExpression, String> {
        let left = self.plus_expression()?;
        let operator = self.current_token_and_advance().token_type;
        let right = self.plus_expression()?;

        return Ok(ShiftExpression { left, operator, right });
    }

    fn plus_expression(&mut self) -> Result<PlusExpression, String> {
        let left = self.multiply_expression()?;
        let operator = self.current_token_and_advance().token_type;
        let right = self.multiply_expression()?;

        return Ok(PlusExpression { left, operator, right });
    }

    fn multiply_expression(&mut self) -> Result<MultiplyExpression, String> {
        let left = self.prefixed_expression()?;
        let operator = self.current_token_and_advance().token_type;
        let right = self.prefixed_expression()?;

        return Ok(MultiplyExpression { left, operator, right });
    }

    fn prefixed_expression(&mut self) -> Result<PrefixedExpression, String> {
        let factor = self.factor()?;
        match self.current_token_type() {
            TokenType::Dot => {
                self.expect(TokenType::Identifier)?;

                return Ok(PrefixedExpression::DotAccess);
            }
            TokenType::LeftSquare => {
                let expression = self.expression()?;
                self.expect(TokenType::RightSquare)?;

                return Ok(PrefixedExpression::ArrayStyleAccess);
            }
            TokenType::PlusPlus => {
                return Ok(PrefixedExpression::PostIncrement);
            }
            TokenType::MinusMinus => {
                return Ok(PrefixedExpression::PostDecrement);
            }
            TokenType::LeftParen => {
                self.function_call_args()?;

                return Ok(PrefixedExpression::FunctionCall);
            }
            token_type => {
                todo!();
            }
        }
    }

    fn function_call_args(&mut self) -> Result<FunctionCallArgs, String> {
        todo!();
    }

    fn comma_expression(&mut self) -> Result<CommaExpression, String> {
        let mut expressions = vec![self.expression()?];
        while self.current_token_type() == TokenType::Comma {
            self.next_token();
            expressions.push(self.expression()?);
        }

        return Ok(CommaExpression { expressions });
    }

    async fn unary_op(&mut self) -> Result<UnaryOp, String> {
        todo!();
    }
}