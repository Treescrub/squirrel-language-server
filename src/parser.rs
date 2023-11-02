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
    
    fn prev_token(&self) -> &Token {
        return self.tokens.get(self.token_index - 1).unwrap();
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
    
    fn expect(&mut self, token_type: TokenType) -> Result<&Token, String> {
        if self.current_token_type() != token_type {
            return Err(format!("Expected token `{}`, got `{}`", token_type, self.current_token_type()));
        }

        let cur_token = self.current_token();
        self.next_token();

        return Ok(cur_token);
    }

    fn is_end_of_tokens(&self) -> bool {
        return self.token_index == self.tokens.len();
    }
    
    fn is_end_of_statement(&self) -> bool {
        return (self.prev_token().token_type == TokenType::NewLine) || self.is_end_of_tokens() 
                || self.current_token_type() == TokenType::RightCurly || self.current_token_type() == TokenType::Semicolon;
    }

    fn build_error(&self, mut message: String) -> String {
        message.push_str(&format!(" (on line {}, col {})", self.current_token().range.start.line, self.current_token().range.start.column));

        return message;
    }

    fn script(&mut self) -> Result<Script, String> {
        let mut statements: Vec<Statement> = Vec::new();
        // while !self.is_end_of_tokens() {
            statements.push(self.statement()?);
            if self.prev_token().token_type != TokenType::RightCurly && self.prev_token().token_type != TokenType::Semicolon {
                self.optional_semicolon()?;
            }
        // }

        return Ok(Script { statements })
    }

    fn optional_semicolon(&mut self) -> Result<(), String> {
        if self.current_token_type() == TokenType::Semicolon {
            self.next_token();
            return Ok(());
        }

        if !self.is_end_of_statement() {
            return Err(self.build_error(String::from("expected end of statement")));
        }

        return Ok(());
    }

    fn statement(&mut self) -> Result<Statement, String> {
        let token_type = self.current_token_type();

        match token_type {
            TokenType::If => {
                self.next_token();
                return self.if_statement();
            }
            TokenType::While => {
                todo!();
            }
            TokenType::Do => {
                todo!();
            }
            TokenType::For => {
                todo!();
            }
            TokenType::Foreach => {
                todo!();
            }
            TokenType::Switch => {
                todo!();
            }
            TokenType::Local => {
                todo!();
            }
            TokenType::Return => {
                self.next_token();
                if !self.is_end_of_statement() {
                    return Ok(Statement::Return(Some(self.comma_expression()?)));
                } else {
                    return Ok(Statement::Return(None));
                }
            }
            TokenType::Yield => {
                self.next_token();
                if !self.is_end_of_statement() {
                    return Ok(Statement::Yield(Some(self.comma_expression()?)));
                } else {
                    return Ok(Statement::Yield(None));
                }
            }
            TokenType::Break => {
                self.next_token();
                return Ok(Statement::Break);
            }
            TokenType::Continue => {
                self.next_token();
                return Ok(Statement::Continue);
            }
            TokenType::Function => {
                todo!();
            }
            TokenType::Class => {
                todo!();
            }
            TokenType::Enum => {
                todo!();
            }
            TokenType::LeftCurly => {
                self.next_token();
                let statements = self.statements()?;
                self.expect(TokenType::RightCurly)?;

                return Ok(Statement::Statements(statements));
            }
            TokenType::Try => {
                todo!();
            }
            TokenType::Throw => {
                self.next_token();
                return Ok(Statement::Throw(self.comma_expression()?));
            }
            TokenType::Const => {
                self.next_token();
                return self.const_statement();
            }
            val => {
                return Ok(Statement::CommaExpression(self.comma_expression()?));
                // self.next_token();
                // return Err(self.build_error(format!("Unhandled token '{}' in statement", val)));
            }
        }
    }

    fn if_statement(&mut self) -> Result<Statement, String> {
        self.expect(TokenType::LeftParen)?;
        let comma_expression = self.comma_expression()?;
        self.expect(TokenType::RightParen)?;
        
        let if_block = self.statement()?;
        let mut else_block = None;

        if self.current_token_type() == TokenType::Else {
            self.next_token();
            else_block = Some(Box::new(self.statement()?));
        }

        return Ok(Statement::If(comma_expression, Box::new(if_block), else_block));
    }

    fn const_statement(&mut self) -> Result<Statement, String> {
        let id = self.expect(TokenType::Identifier)?.svalue.as_ref().unwrap().clone();
        self.expect(TokenType::Assign)?;
        let scalar = self.scalar()?;
        self.optional_semicolon()?;

        return Ok(Statement::Const(Identifier {value: id}, scalar));
    }

    fn scalar(&mut self) -> Result<Scalar, String> {
        match self.current_token_type() {
            TokenType::IntegerLiteral => {
                self.next_token();
                return Ok(Scalar::Integer);
            }
            TokenType::FloatLiteral => {
                self.next_token();
                return Ok(Scalar::Float);
            }
            TokenType::StringLiteral => {
                self.next_token();
                return Ok(Scalar::StringLiteral);
            }
            TokenType::True => {
                self.next_token();
                return Ok(Scalar::True);
            }
            TokenType::False => {
                self.next_token();
                return Ok(Scalar::False);
            }
            TokenType::Minus => {
                self.next_token();
                match self.current_token_type() {
                    TokenType::IntegerLiteral => {
                        self.next_token();
                        return Ok(Scalar::Integer);
                    }
                    TokenType::FloatLiteral => {
                        self.next_token();
                        return Ok(Scalar::Float);
                    }
                    unhandled_type => {
                        return Err(self.build_error(format!("expected int or float scalar, got '{}'", unhandled_type)));
                    }
                }
            }
            unhandled_type => {
                return Err(self.build_error(format!("expected int, float, or string scalar, got '{}'", unhandled_type)));
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
            TokenType::DoubleColon => {
                todo!();
            }
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
                return Ok(self.array_init()?);
            }
            TokenType::LeftCurly => {
                todo!();
            }
            TokenType::Function => {
                todo!();
            }
            TokenType::At => {
                todo!();
            }
            TokenType::Class => {
                todo!();
            }
            TokenType::Minus => {
                match self.current_token_type() {
                    TokenType::IntegerLiteral => {
                        self.next_token();
                        return Ok(Factor::Scalar(Scalar::Integer));
                    }
                    TokenType::FloatLiteral => {
                        self.next_token();
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
            unhandled_type => {
                return Err(self.build_error(format!("Unexpected token for factor: '{}'", unhandled_type)));
            }
        }
    }

    fn statements(&mut self) -> Result<Statements, String> {
        let mut statements: Vec<Statement> = Vec::new();
        while self.current_token_type() != TokenType::RightCurly && self.current_token_type() != TokenType::Default && self.current_token_type() != TokenType::Case {
            statements.push(self.statement()?);

            if self.prev_token().token_type != TokenType::RightCurly && self.prev_token().token_type != TokenType::Semicolon {
                self.optional_semicolon()?;
            }
        }

        return Ok(Statements {statements})
    }

    fn expression(&mut self) -> Result<Expression, String> {
        let logical_or = Box::new(self.logical_or_expression()?);
        let mut expr_type = None;

        match self.current_token_type() {
            TokenType::Newslot => {
                expr_type = Some(ExpressionType::Newslot(Box::new(self.expression()?)));
            }
            TokenType::Assign => {
                expr_type = Some(ExpressionType::Assign(Box::new(self.expression()?)));
            }
            TokenType::MinusEqual => {
                expr_type = Some(ExpressionType::MinusEqual(Box::new(self.expression()?)));
            }
            TokenType::PlusEqual => {
                expr_type = Some(ExpressionType::PlusEqual(Box::new(self.expression()?)));
            }
            TokenType::MultiplyEqual => {
                expr_type = Some(ExpressionType::MultiplyEqual(Box::new(self.expression()?)));
            }
            TokenType::DivideEqual => {
                expr_type = Some(ExpressionType::DivideEqual(Box::new(self.expression()?)));
            }
            TokenType::Ternary => {
                let true_case = self.expression()?;
                self.expect(TokenType::Colon)?;
                let false_case = self.expression()?;

                expr_type = Some(ExpressionType::Ternary(Box::new(true_case), Box::new(false_case)));
            }
            _ => {}
        }

        return Ok(Expression { logical_or, expr_type });
    }

    fn array_init(&mut self) -> Result<Factor, String> {
        let mut expressions = Vec::new();

        while self.current_token_type() != TokenType::RightSquare {
            expressions.push(self.expression()?);

            if self.current_token_type() == TokenType::Comma {
                self.next_token();
            }
        }

        self.next_token();
        
        return Ok(Factor::ArrayInit(expressions));
    }

    fn class_expression(&mut self) -> Result<Factor, String> {
        self.next_token();

        if self.current_token_type() == TokenType::Extends {
            self.next_token();
            self.expression()?;
        }
        if self.current_token_type() == TokenType::AttributeOpen {
            self.next_token();
            todo!("ParseTableOrClass");
        }
        self.expect(TokenType::LeftCurly)?;

        todo!("ParseTableOrClass");
    }

    fn logical_or_expression(&mut self) -> Result<LogicalOrExpression, String> {
        let left = self.logical_and_expression()?;
        let mut right = None;

        if self.current_token_type() == TokenType::LogicalOr {
            self.next_token();
            right = Some(Box::new(self.logical_or_expression()?));
        }

        return Ok(LogicalOrExpression { left, right });
    }

    fn logical_and_expression(&mut self) -> Result<LogicalAndExpression, String> {
        let left = self.bitwise_or_expression()?;
        let mut right = None;

        if self.current_token_type() == TokenType::LogicalAnd {
            self.next_token();
            right = Some(Box::new(self.logical_and_expression()?));
        }

        return Ok(LogicalAndExpression { left, right });
    }

    fn bitwise_or_expression(&mut self) -> Result<BitwiseOrExpression, String> {
        let left = self.bitwise_xor_expression()?;
        let mut right = None;

        if self.current_token_type() == TokenType::BitwiseOr {
            self.next_token();
            right = Some(self.bitwise_xor_expression()?);
        }
        
        return Ok(BitwiseOrExpression { left, right });
    }

    fn bitwise_xor_expression(&mut self) -> Result<BitwiseXorExpression, String> {
        let left = self.bitwise_and_expression()?;
        let mut right = None;

        if self.current_token_type() == TokenType::BitwiseXor {
            self.next_token();
            right = Some(self.bitwise_and_expression()?);
        }
        
        return Ok(BitwiseXorExpression { left, right });
    }

    fn bitwise_and_expression(&mut self) -> Result<BitwiseAndExpression, String> {
        let left = self.equal_expression()?;
        let mut right = None;

        if self.current_token_type() == TokenType::BitwiseAnd {
            self.next_token();
            right = Some(self.equal_expression()?);
        }
        
        return Ok(BitwiseAndExpression { left, right });
    }

    fn equal_expression(&mut self) -> Result<EqualExpression, String> {
        let left = self.compare_expression()?;
        let mut operator = None;
        let mut right = None;

        match self.current_token_type() {
            TokenType::Equal | TokenType::NotEqual | TokenType::ThreeWayCompare => {
                operator = Some(self.current_token_type());
                self.next_token();
                right = Some(self.compare_expression()?);
            }
            _ => {}
        }

        return Ok(EqualExpression { left, operator, right });
    }

    fn compare_expression(&mut self) -> Result<CompareExpression, String> {
        let left = self.shift_expression()?;
        let mut operator = None;
        let mut right = None;

        match self.current_token_type() {
            TokenType::GreaterThan | TokenType::GreaterOrEqual | TokenType::LessThan 
                | TokenType::LessOrEqual | TokenType::In | TokenType::Instanceof => {
                operator = Some(self.current_token_type());
                self.next_token();
                right = Some(self.shift_expression()?);
            }
            _ => {}
        }

        return Ok(CompareExpression { left, operator, right });
    }

    fn shift_expression(&mut self) -> Result<ShiftExpression, String> {
        let left = self.plus_expression()?;
        let mut operator = None;
        let mut right = None;

        match self.current_token_type() {
            TokenType::UnsignedShiftRight | TokenType::ShiftLeft | TokenType::ShiftRight => {
                operator = Some(self.current_token_type());
                self.next_token();
                right = Some(self.plus_expression()?);
            }
            _ => {}
        }

        return Ok(ShiftExpression { left, operator, right });
    }

    fn plus_expression(&mut self) -> Result<PlusExpression, String> {
        let left = self.multiply_expression()?;
        let mut operator = None;
        let mut right = None;

        match self.current_token_type() {
            TokenType::Plus | TokenType::Minus => {
                operator = Some(self.current_token_type());
                self.next_token();
                right = Some(self.multiply_expression()?);
            }
            _ => {}
        }

        return Ok(PlusExpression { left, operator, right });
    }

    fn multiply_expression(&mut self) -> Result<MultiplyExpression, String> {
        let left = self.prefixed_expression()?;
        let mut operator = None;
        let mut right = None;

        match self.current_token_type() {
            TokenType::Multiply | TokenType::Divide | TokenType::Modulo => {
                operator = Some(self.current_token_type());
                self.next_token();
                right = Some(self.prefixed_expression()?);
            }
            _ => {}
        }

        return Ok(MultiplyExpression { left, operator, right });
    }

    fn prefixed_expression(&mut self) -> Result<PrefixedExpression, String> {
        let factor = self.factor()?;
        let mut expr_type = None;

        match self.current_token_type() {
            TokenType::Dot => {
                self.next_token();
                let id = self.expect(TokenType::Identifier)?.svalue.as_ref().unwrap().clone();

                expr_type = Some(PrefixedExpressionType::DotAccess(Identifier { value: id }));
            }
            TokenType::LeftSquare => {
                self.next_token();
                let expression = self.expression()?;
                self.expect(TokenType::RightSquare)?;

                expr_type = Some(PrefixedExpressionType::ArrayStyleAccess(expression));
            }
            TokenType::PlusPlus => {
                expr_type = Some(PrefixedExpressionType::PostIncrement);
                self.next_token();
            }
            TokenType::MinusMinus => {
                expr_type = Some(PrefixedExpressionType::PostDecrement);
                self.next_token();
            }
            TokenType::LeftParen => {
                self.next_token();
                let function_call_args = self.function_call_args()?;

                expr_type = Some(PrefixedExpressionType::FunctionCall(function_call_args));
            }
            _ => {}
        }

        return Ok(PrefixedExpression { factor, expr_type });
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

    fn unary_op(&mut self) -> Result<UnaryOp, String> {
        let operator: TokenType;
        match self.current_token_type() {
            TokenType::Minus | TokenType::BitwiseNot | TokenType::LogicalNot | TokenType::Typeof
            | TokenType::Resume | TokenType::Clone => {
                operator = self.current_token_type();
            }
            token_type => {
                return Err(self.build_error(format!("unary op with unhandled token '{}'", token_type)));
            }
        }
        let expression = self.prefixed_expression()?;

        return Ok(UnaryOp { operator, expression });
    }
}