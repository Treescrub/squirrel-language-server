use crate::ast::*;
use crate::lexer::*;

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    token_index: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens,
            token_index: 0,
        }
    }
    
    pub fn parse(&mut self) -> Result<Script, String> {
        // hacky way to prevent stack overflow :/
        return stacker::grow(100 * 1024 * 1024, || {
            return Ok(self.script()?);
        });
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
        return self.current_token_type() == TokenType::EndOfTokens;
    }
    
    fn is_end_of_statement(&self) -> bool {
        return (self.prev_token().token_type == TokenType::NewLine) || self.is_end_of_tokens() 
                || self.current_token_type() == TokenType::RightCurly || self.current_token_type() == TokenType::Semicolon;
    }

    fn build_error(&self, mut message: String) -> String {
        message.push_str(&format!(" (on line {}, col {})", self.current_token().range.start.line, self.current_token().range.start.column));

        return message;
    }

    // ---------------------------------------------------

    fn script(&mut self) -> Result<Script, String> {
        let mut statements: Vec<Statement> = Vec::new();
        while !self.is_end_of_tokens() {
            statements.push(self.statement()?);
            if self.prev_token().token_type != TokenType::RightCurly && self.prev_token().token_type != TokenType::Semicolon {
                self.optional_semicolon()?;
            }
        }

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
                return self.if_statement();
            }
            TokenType::While => {
                return self.while_statement();
            }
            TokenType::Do => {
                return self.do_while_statement();
            }
            TokenType::For => {
                return self.for_statement();
            }
            TokenType::Foreach => {
                return self.for_each_statement();
            }
            TokenType::Switch => {
                return self.switch_statement();
            }
            TokenType::Local => {
                return Ok(Statement::LocalDeclare(self.local_declare()?));
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
                return self.function_statement();
            }
            TokenType::Class => {
                todo!("Class statement not implemented");
            }
            TokenType::Enum => {
                return self.enum_statement();
            }
            TokenType::LeftCurly => {
                return self.statement_block();
            }
            TokenType::Try => {
                return self.try_statement();
            }
            TokenType::Throw => {
                self.next_token();
                return Ok(Statement::Throw(self.comma_expression()?));
            }
            TokenType::Const => {
                return self.const_statement();
            }
            _ => {
                return Ok(Statement::CommaExpression(self.comma_expression()?));
            }
        }
    }

    fn if_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
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

    fn while_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        self.expect(TokenType::LeftParen)?;
        let comma_expression = self.comma_expression()?;
        self.expect(TokenType::RightParen)?;
        let statement = Box::new(self.statement()?);

        return Ok(Statement::While(comma_expression, statement));
    }

    fn do_while_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        let statement = Box::new(self.statement()?);
        self.expect(TokenType::While)?;
        self.expect(TokenType::LeftParen)?;
        let comma_expression = self.comma_expression()?;
        self.expect(TokenType::RightParen)?;

        return Ok(Statement::DoWhile(statement, comma_expression));
    }

    fn for_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        self.expect(TokenType::LeftParen)?;

        let mut init = None;
        let mut condition = None;
        let mut post = None;

        if self.current_token_type() == TokenType::Local {
            init = Some(ForInit::LocalDeclare(self.local_declare()?));
        } else if self.current_token_type() != TokenType::Semicolon {
            init = Some(ForInit::CommaExpression(self.comma_expression()?));
        }

        self.expect(TokenType::Semicolon)?;

        if self.current_token_type() != TokenType::Semicolon {
            condition = Some(self.comma_expression()?);
        }

        self.expect(TokenType::Semicolon)?;

        if self.current_token_type() != TokenType::RightParen {
            post = Some(self.comma_expression()?);
        }

        self.expect(TokenType::RightParen)?;

        let statement = Box::new(self.statement()?);

        return Ok(Statement::For(init, condition, post, statement));
    }

    fn for_each_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        self.expect(TokenType::LeftParen)?;
        let mut value_identifier = Identifier { value: self.expect(TokenType::Identifier)?.svalue.as_ref().unwrap().clone() };
        let mut key_identifier = None;

        if self.current_token_type() == TokenType::Comma {
            self.next_token();
            key_identifier = Some(value_identifier);
            value_identifier = Identifier { value: self.expect(TokenType::Identifier)?.svalue.as_ref().unwrap().clone() };
        }

        self.expect(TokenType::In)?;
        let expression = self.expression()?;
        let statement = self.statement()?;

        return Ok(Statement::ForEach(value_identifier, key_identifier, expression, Box::new(statement)));
    }

    fn switch_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        self.expect(TokenType::LeftParen)?;
        let value = self.comma_expression()?;
        self.expect(TokenType::RightParen)?;
        self.expect(TokenType::LeftCurly)?;

        let mut switch_cases = Vec::new();

        while self.current_token_type() == TokenType::Case {
            self.next_token();
            let expression = self.expression()?;
            self.expect(TokenType::Colon)?;

            let statements = self.statements()?;

            switch_cases.push(SwitchCase { condition: expression, body: statements });
        }

        if self.current_token_type() == TokenType::Default {
            self.next_token();
            self.expect(TokenType::Colon)?;
            let statements = self.statements()?;
        }

        self.expect(TokenType::RightCurly)?;
 
        return Ok(Statement::Switch(value, switch_cases));
    }

    fn function_statement(&mut self) -> Result<Statement, String> {
        self.next_token();

        let mut identifiers = Vec::new();
        identifiers.push(Identifier::from(self.expect(TokenType::Identifier)?));

        while self.current_token_type() == TokenType::DoubleColon {
            self.next_token();
            identifiers.push(Identifier::from(self.expect(TokenType::Identifier)?));
        }

        let mut bind_env = None;

        if self.current_token_type() == TokenType::LeftSquare {
            self.next_token();
            let env_expression = self.expression()?;
            self.expect(TokenType::RightSquare)?;
            
            bind_env = Some(env_expression);
        }

        let function_identifier = FunctionIdentifier { identifiers };
        let params = self.function_params()?;
        let body = self.statement()?;

        return Ok(Statement::Function(function_identifier, bind_env, params, Box::new(body)));
    }

    fn class_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        self.prefixed_expression()?;
        self.class_expression()?;

        todo!();
    }

    fn enum_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        let name = Identifier::from(self.expect(TokenType::Identifier)?);
        self.expect(TokenType::LeftCurly)?;

        let mut entries = Vec::new();

        while self.current_token_type() != TokenType::RightCurly {
            let entry_name = Identifier::from(self.expect(TokenType::Identifier)?);
            let mut entry_value = None;

            if self.current_token_type() == TokenType::Assign {
                self.next_token();
                entry_value = Some(self.scalar()?);
            }

            entries.push(EnumEntry { key: entry_name, value: entry_value });

            if self.current_token_type() == TokenType::Comma {
                self.next_token();
            }
        }
        self.next_token();

        return Ok(Statement::Enum(name, EnumValues { values: entries }));
    }

    fn statement_block(&mut self) -> Result<Statement, String> {
        self.next_token();
        let statements = self.statements()?;
        self.expect(TokenType::RightCurly)?;

        return Ok(Statement::StatementBlock(statements));
    }

    fn try_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        let try_body = self.statement()?;
        self.expect(TokenType::Catch)?;
        self.expect(TokenType::LeftParen)?;
        let identifier = Identifier { value: self.expect(TokenType::Identifier)?.svalue.as_ref().unwrap().clone() };
        self.expect(TokenType::RightParen)?;
        let catch_body = self.statement()?;

        return Ok(Statement::TryCatch(Box::new(try_body), identifier, Box::new(catch_body)));
    }

    fn function_params(&mut self) -> Result<FunctionParams, String> {
        self.expect(TokenType::LeftParen)?;

        let mut params = Vec::new();

        while self.current_token_type() != TokenType::RightParen {
            if self.current_token_type() == TokenType::Varargs {
                self.next_token();
                
                if self.current_token_type() != TokenType::RightParen {
                    return Err(self.build_error(String::from("expected ')' after varargs")));
                }

                params.push(FunctionParam::VarParams);
            } else {
                let name = Identifier::from(self.expect(TokenType::Identifier)?);

                if self.current_token_type() == TokenType::Assign {
                    self.next_token();
                    let default_val = self.expression()?;

                    params.push(FunctionParam::Default(name, default_val));
                } else {
                    params.push(FunctionParam::Normal(name));
                }
            }

            if self.current_token_type() == TokenType::Comma {
                self.next_token();
            } else if self.current_token_type() != TokenType::RightParen {
                return Err(self.build_error(String::from("expected ')' or ','")));
            }
        }

        self.expect(TokenType::RightParen)?;

        return Ok(FunctionParams { params });
    }

    fn local_declare(&mut self) -> Result<LocalDeclare, String> {
        self.next_token();
        if self.current_token_type() == TokenType::Function {
            self.next_token();
            let identifier = Identifier::from(self.expect(TokenType::Identifier)?);
            let mut bind_env = None;

            if self.current_token_type() == TokenType::LeftSquare {
                bind_env = Some(self.expression()?);
                self.expect(TokenType::RightSquare)?;
            }

            let params = self.function_params()?;

            return Ok(LocalDeclare::Function(identifier, bind_env, params));
        }

        let mut assign_expressions = Vec::new();

        loop {
            assign_expressions.push(self.assign_expression()?);

            if self.current_token_type() == TokenType::Comma {
                self.next_token();
            } else {
                break;
            }
        }

        return Ok(LocalDeclare::Assign(assign_expressions));
    }

    fn assign_expression(&mut self) -> Result<AssignExpression, String> {
        let identifier = Identifier::from(self.expect(TokenType::Identifier)?);
        let mut expression = None;

        if self.current_token_type() == TokenType::Assign {
            self.next_token();
            expression = Some(self.expression()?);
        }

        return Ok(AssignExpression { identifier, value: expression });
    }

    fn const_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
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

        match token_type {
            TokenType::StringLiteral => {
                self.next_token();

                return Ok(Factor::Scalar(Scalar::StringLiteral));
            }
            TokenType::Base => {
                self.next_token();

                return Ok(Factor::Base);
            }
            TokenType::Identifier => {
                self.next_token();

                return Ok(Factor::Identifier(Identifier::from(token)));
            }
            TokenType::Constructor => {
                self.next_token();

                return Ok(Factor::Constructor);
            }
            TokenType::This => {
                self.next_token();

                return Ok(Factor::This);
            }
            TokenType::DoubleColon => {
                self.next_token();

                return Ok(Factor::DoubleColon(Box::new(self.prefixed_expression()?)));
            }
            TokenType::Null => {
                self.next_token();

                return Ok(Factor::Null);
            }
            TokenType::IntegerLiteral => {
                self.next_token();

                return Ok(Factor::Scalar(Scalar::Integer));
            }
            TokenType::FloatLiteral => {
                self.next_token();

                return Ok(Factor::Scalar(Scalar::Float));
            }
            TokenType::True => {
                self.next_token();

                return Ok(Factor::Scalar(Scalar::True));
            }
            TokenType::False => {
                self.next_token();

                return Ok(Factor::Scalar(Scalar::False));
            }
            TokenType::LeftSquare => {
                self.next_token();

                return Ok(self.array_init()?);
            }
            TokenType::LeftCurly => {
                todo!("Table factor not implemented");
            }
            TokenType::Function => {
                todo!("Function factor not implemented");
            }
            TokenType::At => {
                todo!("Lambda function factor not implemented");
            }
            TokenType::Class => {
                todo!("Class factor not implemented");
            }
            TokenType::Minus => {
                self.next_token();

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
                        return Ok(Factor::UnaryOp(Box::new(self.unary_op()?)));
                    }
                }
            }
            TokenType::LogicalNot => {
                return Ok(Factor::UnaryOp(Box::new(self.unary_op()?)));
            }
            TokenType::BitwiseNot => {
                return Ok(Factor::UnaryOp(Box::new(self.unary_op()?)));
            }
            TokenType::Typeof => {
                return Ok(Factor::UnaryOp(Box::new(self.unary_op()?)));
            }
            TokenType::Resume => {
                return Ok(Factor::UnaryOp(Box::new(self.unary_op()?)));
            }
            TokenType::Clone => {
                return Ok(Factor::UnaryOp(Box::new(self.unary_op()?)));
            }
            TokenType::Rawcall => {
                self.next_token();

                return Ok(Factor::RawCall(self.function_call_args()?));
            }
            TokenType::MinusMinus => {
                return Ok(Factor::UnaryOp(Box::new(self.unary_op()?)));
            }
            TokenType::PlusPlus => {
                return Ok(Factor::UnaryOp(Box::new(self.unary_op()?)));
            }
            TokenType::Delete => {
                self.next_token();
                let prefixed_expression = self.prefixed_expression()?;

                return Ok(Factor::Delete(Box::new(prefixed_expression)));
            }
            TokenType::LeftParen => {
                self.next_token();
                let comma_expression = self.comma_expression()?;
                self.expect(TokenType::RightParen)?;

                return Ok(Factor::ParenExpression(comma_expression));
            }
            TokenType::LineInfo => {
                self.next_token();

                return Ok(Factor::LineInfo);
            }
            TokenType::FileInfo => {
                self.next_token();

                return Ok(Factor::FileInfo);
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
                self.next_token();
                expr_type = Some(ExpressionType::Newslot(Box::new(self.expression()?)));
            }
            TokenType::Assign => {
                self.next_token();
                expr_type = Some(ExpressionType::Assign(Box::new(self.expression()?)));
            }
            TokenType::MinusEqual => {
                self.next_token();
                expr_type = Some(ExpressionType::MinusEqual(Box::new(self.expression()?)));
            }
            TokenType::PlusEqual => {
                self.next_token();
                expr_type = Some(ExpressionType::PlusEqual(Box::new(self.expression()?)));
            }
            TokenType::MultiplyEqual => {
                self.next_token();
                expr_type = Some(ExpressionType::MultiplyEqual(Box::new(self.expression()?)));
            }
            TokenType::DivideEqual => {
                self.next_token();
                expr_type = Some(ExpressionType::DivideEqual(Box::new(self.expression()?)));
            }
            TokenType::Ternary => {
                self.next_token();
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
                let id = Identifier::from(self.expect(TokenType::Identifier)?);

                expr_type = Some(PrefixedExpressionType::DotAccess(id));
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
                expr_type = Some(PrefixedExpressionType::FunctionCall(self.function_call_args()?));
            }
            _ => {}
        }

        return Ok(PrefixedExpression { factor, expr_type });
    }

    fn function_call_args(&mut self) -> Result<FunctionCallArgs, String> {
        self.expect(TokenType::LeftParen)?;
        let mut expressions = Vec::new();
        while self.current_token_type() != TokenType::RightParen {
            expressions.push(self.expression()?);

            if self.current_token_type() == TokenType::Comma {
                self.next_token();

                if self.current_token_type() == TokenType::RightParen {
                    return Err(self.build_error(String::from("expression expected in function call, found ')'")));
                }
            }
        }
        self.next_token();

        let mut post_call_init = None;
        // post-call initializer
        if self.current_token_type() == TokenType::LeftCurly {
            self.next_token();
            let mut entries = Vec::new();
            while self.current_token_type() != TokenType::RightCurly {
                if self.current_token_type() == TokenType::LeftSquare {
                    self.next_token();
                    let comma_expression = self.comma_expression()?;
                    self.expect(TokenType::RightSquare)?;
                    self.expect(TokenType::Assign)?;
                    let expression = self.expression()?;

                    entries.push(PostCallInitializeEntry::ArrayStyle(comma_expression, expression));
                } else {
                    let identifier = self.expect(TokenType::Identifier)?.svalue.as_ref().unwrap().clone();
                    self.expect(TokenType::Assign)?;
                    let expression = self.expression()?;

                    entries.push(PostCallInitializeEntry::TableStyle(Identifier { value: identifier }, expression))
                }

                if self.current_token_type() == TokenType::Comma {
                    self.next_token();
                }
            }
            self.next_token();
            post_call_init = Some(PostCallInitialize { entries });
        }

        return Ok(FunctionCallArgs { args: expressions, post_call_init });
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
            | TokenType::Resume | TokenType::Clone | TokenType::PlusPlus | TokenType::MinusMinus => {
                operator = self.current_token_type();
            }
            token_type => {
                return Err(self.build_error(format!("unary op with unhandled token '{}'", token_type)));
            }
        }
        self.next_token();
        let expression = self.prefixed_expression()?;

        return Ok(UnaryOp { operator, expression });
    }
}