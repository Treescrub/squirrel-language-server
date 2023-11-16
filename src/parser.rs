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
        self.ignore_newlines();

        // hacky way to prevent stack overflow :/
        return stacker::grow(100 * 1024 * 1024, || {
            return Ok(self.script()?);
        });
    }

    fn ignore_newlines(&mut self) {
        while self.current_token_type() == TokenType::NewLine {
            self.next_token();
        }
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
        message.push_str(&format!(" (on line {}, col {}, token {})", self.current_token().range.start.line, self.current_token().range.start.column, self.token_index));

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
        match self.current_token_type() {
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
                return self.class_statement();
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

        let if_block = Box::new(self.statement()?);
        let mut else_block = None;

        if self.current_token_type() == TokenType::Else {
            self.next_token();
            else_block = Some(Box::new(self.statement()?));
        }

        return Ok(Statement::If(comma_expression, if_block, else_block));
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
        let mut value_identifier = Identifier::from(self.expect(TokenType::Identifier)?);
        let mut key_identifier = None;

        if self.current_token_type() == TokenType::Comma {
            self.next_token();
            key_identifier = Some(value_identifier);
            value_identifier = Identifier::from(self.expect(TokenType::Identifier)?);
        }

        self.expect(TokenType::In)?;
        let expression = self.expression()?;
        let statement = Box::new(self.statement()?);

        return Ok(Statement::ForEach(value_identifier, key_identifier, expression, statement));
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

        let mut default = None;
        if self.current_token_type() == TokenType::Default {
            self.next_token();
            self.expect(TokenType::Colon)?;
            default = Some(self.statements()?);
        }

        self.expect(TokenType::RightCurly)?;
 
        return Ok(Statement::Switch(value, switch_cases, default));
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
        let body = Box::new(self.statement()?);

        return Ok(Statement::Function(function_identifier, bind_env, params, body));
    }

    fn class_statement(&mut self) -> Result<Statement, String> {
        self.next_token();
        let class_name = self.prefixed_expression()?;
        let class_expression = self.class_expression()?;

        return Ok(Statement::Class(class_name, class_expression));
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
        let try_body = Box::new(self.statement()?);
        self.expect(TokenType::Catch)?;
        self.expect(TokenType::LeftParen)?;
        let identifier = Identifier::from(self.expect(TokenType::Identifier)?);
        self.expect(TokenType::RightParen)?;
        let catch_body = Box::new(self.statement()?);

        return Ok(Statement::TryCatch(try_body, identifier, catch_body));
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
        let id = Identifier::from(self.expect(TokenType::Identifier)?);
        self.expect(TokenType::Assign)?;
        let scalar = self.scalar()?;
        self.optional_semicolon()?;

        return Ok(Statement::Const(id, scalar));
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
        match self.current_token_type() {
            TokenType::StringLiteral => {
                self.next_token();

                return Ok(Factor::Scalar(Scalar::StringLiteral));
            }
            TokenType::Base => {
                self.next_token();

                return Ok(Factor::Base);
            }
            TokenType::Identifier => {
                let identifier = Identifier::from(self.current_token());
                self.next_token();

                return Ok(Factor::Identifier(identifier));
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
                return Ok(Factor::TableInit(self.table()?));
            }
            TokenType::Function => {
                return self.function_expression();
            }
            TokenType::At => {
                return self.lambda_expression();
            }
            TokenType::Class => {
                self.next_token();
                
                return Ok(Factor::ClassExpression(self.class_expression()?));
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
            TokenType::LogicalNot | TokenType::BitwiseNot | TokenType::Typeof | TokenType::Resume | TokenType::Clone
                | TokenType::MinusMinus | TokenType::PlusPlus => {
                return Ok(Factor::UnaryOp(Box::new(self.unary_op()?)));
            }
            TokenType::Rawcall => {
                self.next_token();

                return Ok(Factor::RawCall(self.function_call_args()?));
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

    fn simple_table_entry(&mut self) -> Result<TableEntry, String> {
        let key = Identifier::from(self.expect(TokenType::Identifier)?);
        self.expect(TokenType::Assign)?;
        let value = self.expression()?;

        return Ok(TableEntry::Simple(key, value))
    }

    fn table_entry(&mut self, is_class: bool) -> Result<TableEntry, String> {
        match self.current_token_type() {
            TokenType::Function => {
                self.next_token();
                let key = Identifier::from(self.expect(TokenType::Identifier)?);
                let params = self.function_params()?;
                let body = self.statement()?;

                return Ok(TableEntry::Function(key, params, body));
            },
            TokenType::Constructor => {
                self.next_token();
                let params = self.function_params()?;
                let body = self.statement()?;

                return Ok(TableEntry::Constructor(params, body));
            },
            TokenType::LeftSquare => {
                self.next_token();
                let key = self.comma_expression()?;
                self.expect(TokenType::RightSquare)?;
                self.expect(TokenType::Assign)?;
                let value = self.expression()?;

                return Ok(TableEntry::DynamicAssign(key, value));
            },
            TokenType::StringLiteral => {
                if !is_class {
                    let key = self.current_token().svalue.as_ref().unwrap().clone();
                    self.next_token();
                    self.expect(TokenType::Colon)?;
                    let value = self.expression()?;

                    return Ok(TableEntry::JsonStyle(key, value));
                } else {
                    return self.simple_table_entry();
                }
            },
            _ => {
                return self.simple_table_entry();
            },
        }
    }

    fn table(&mut self) -> Result<Table, String> {
        self.next_token();

        let mut entries = Vec::new();

        while self.current_token_type() != TokenType::RightCurly {
            entries.push(self.table_entry(false)?);

            if self.current_token_type() == TokenType::Comma {
                self.next_token();
            }
        }
        self.next_token();

        return Ok(Table { entries });
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
                let true_case = Box::new(self.expression()?);
                self.expect(TokenType::Colon)?;
                let false_case = Box::new(self.expression()?);

                expr_type = Some(ExpressionType::Ternary(true_case, false_case));
            }
            _ => {}
        }

        return Ok(Expression { logical_or, expr_type });
    }

    fn function_expression(&mut self) -> Result<Factor, String> {
        self.next_token();

        let mut bind_env = None;

        if self.current_token_type() == TokenType::LeftSquare {
            self.next_token();
            bind_env = Some(self.expression()?);
            self.expect(TokenType::RightSquare)?;
        }

        let params = self.function_params()?;
        let body = self.statement()?;

        return Ok(Factor::FunctionExpression(bind_env, params, Box::new(body)))
    }

    fn lambda_expression(&mut self) -> Result<Factor, String> {
        self.next_token();

        let mut bind_env = None;

        if self.current_token_type() == TokenType::LeftSquare {
            self.next_token();
            bind_env = Some(self.expression()?);
            self.expect(TokenType::RightSquare)?;
        }

        let params = self.function_params()?;
        let body = self.expression()?;

        return Ok(Factor::LambdaExpression(bind_env, params, body))
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

    fn class_expression(&mut self) -> Result<ClassExpression, String> {
        let mut base_class = None;
        let mut attributes = None;

        if self.current_token_type() == TokenType::Extends {
            self.next_token();
            base_class = Some(self.expression()?);
        }
        if self.current_token_type() == TokenType::AttributeOpen {
            attributes = Some(self.class_attributes()?);
        }
        
        let body = self.class_table()?;

        return Ok(ClassExpression { base_class, attributes, body });
    }

    fn class_table(&mut self) -> Result<Table, String> {
        self.expect(TokenType::LeftCurly)?;

        let mut entries = Vec::new();

        while self.current_token_type() != TokenType::RightCurly {
            if self.current_token_type() == TokenType::AttributeOpen {
                entries.push(TableEntry::Attributes(self.class_attributes()?));
            }
            if self.current_token_type() == TokenType::Static {
                self.next_token();
            }

            entries.push(self.table_entry(true)?);

            if self.current_token_type() == TokenType::Semicolon {
                self.next_token();
            }
        }
        self.next_token();

        return Ok(Table { entries });
    }

    fn class_attributes(&mut self) -> Result<Table, String> {
        self.next_token();

        let mut entries = Vec::new();

        while self.current_token_type() != TokenType::AttributeClose {
            entries.push(self.table_entry(false)?);

            if self.current_token_type() == TokenType::Comma {
                self.next_token();
            }
        }
        self.next_token();

        return Ok(Table { entries });
    }

    fn logical_or_expression(&mut self) -> Result<LogicalOrExpression, String> {
        let left = self.logical_and_expression()?;
        let mut right = Vec::new();

        while self.current_token_type() == TokenType::LogicalOr {
            self.next_token();
            right.push(Box::new(self.logical_or_expression()?));
        }

        return Ok(LogicalOrExpression { left, right });
    }

    fn logical_and_expression(&mut self) -> Result<LogicalAndExpression, String> {
        let left = self.bitwise_or_expression()?;
        let mut right = Vec::new();

        while self.current_token_type() == TokenType::LogicalAnd {
            self.next_token();
            right.push(Box::new(self.logical_and_expression()?));
        }

        return Ok(LogicalAndExpression { left, right });
    }

    fn bitwise_or_expression(&mut self) -> Result<BitwiseOrExpression, String> {
        let left = self.bitwise_xor_expression()?;
        let mut right = Vec::new();

        while self.current_token_type() == TokenType::BitwiseOr {
            self.next_token();
            right.push(self.bitwise_xor_expression()?);
        }
        
        return Ok(BitwiseOrExpression { left, right });
    }

    fn bitwise_xor_expression(&mut self) -> Result<BitwiseXorExpression, String> {
        let left = self.bitwise_and_expression()?;
        let mut right = Vec::new();

        while self.current_token_type() == TokenType::BitwiseXor {
            self.next_token();
            right.push(self.bitwise_and_expression()?);
        }

        return Ok(BitwiseXorExpression { left, right });
    }

    fn bitwise_and_expression(&mut self) -> Result<BitwiseAndExpression, String> {
        let left = self.equal_expression()?;
        let mut right = Vec::new();

        while self.current_token_type() == TokenType::BitwiseAnd {
            self.next_token();
            right.push(self.equal_expression()?);
        }
        
        return Ok(BitwiseAndExpression { left, right });
    }

    fn equal_expression(&mut self) -> Result<EqualExpression, String> {
        let left = self.compare_expression()?;
        let mut slices = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::Equal | TokenType::NotEqual | TokenType::ThreeWayCompare => {
                    let operator = self.current_token_type();
                    self.next_token();
                    let right = self.compare_expression()?;

                    slices.push(BinaryOpSlice::new(operator, right));
                }
                _ => break,
            }
        }

        return Ok(EqualExpression { left, slices });
    }

    fn compare_expression(&mut self) -> Result<CompareExpression, String> {
        let left = self.shift_expression()?;
        let mut slices = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::GreaterThan | TokenType::GreaterOrEqual | TokenType::LessThan 
                    | TokenType::LessOrEqual | TokenType::In | TokenType::Instanceof => {
                    let operator = self.current_token_type();
                    self.next_token();
                    let right = self.shift_expression()?;

                    slices.push(BinaryOpSlice::new(operator, right));
                }
                _ => break,
            }
        }

        return Ok(CompareExpression { left, slices });
    }

    fn shift_expression(&mut self) -> Result<ShiftExpression, String> {
        let left = self.plus_expression()?;
        let mut slices = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::UnsignedShiftRight | TokenType::ShiftLeft | TokenType::ShiftRight => {
                    let operator = self.current_token_type();
                    self.next_token();
                    let right = self.plus_expression()?;

                    slices.push(BinaryOpSlice::new(operator, right));
                }
                _ => break,
            }
        }

        return Ok(ShiftExpression { left, slices });
    }

    fn plus_expression(&mut self) -> Result<PlusExpression, String> {
        let left = self.multiply_expression()?;
        let mut slices = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::Plus | TokenType::Minus => {
                    let operator = self.current_token_type();
                    self.next_token();
                    let right = self.multiply_expression()?;

                    slices.push(BinaryOpSlice::new(operator, right))
                }
                _ => break,
            }
        }

        return Ok(PlusExpression { left, slices });
    }

    fn multiply_expression(&mut self) -> Result<MultiplyExpression, String> {
        let left = self.prefixed_expression()?;
        let mut slices = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::Multiply | TokenType::Divide | TokenType::Modulo => {
                    let operator = self.current_token_type();
                    self.next_token();
                    let right = self.prefixed_expression()?;

                    slices.push(BinaryOpSlice::new(operator, right))
                }
                _ => break,
            }
        }

        return Ok(MultiplyExpression { left, slices });
    }

    fn prefixed_expression(&mut self) -> Result<PrefixedExpression, String> {
        let factor = self.factor()?;
        let mut expr_types = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::Dot => {
                    self.next_token();
                    let id = Identifier::from(self.expect(TokenType::Identifier)?);

                    expr_types.push(PrefixedExpressionType::DotAccess(id));
                }
                TokenType::LeftSquare => {
                    self.next_token();
                    let expression = self.expression()?;
                    self.expect(TokenType::RightSquare)?;

                    expr_types.push(PrefixedExpressionType::ArrayStyleAccess(expression));
                }
                TokenType::PlusPlus => {
                    expr_types.push(PrefixedExpressionType::PostIncrement);
                    self.next_token();
                }
                TokenType::MinusMinus => {
                    expr_types.push(PrefixedExpressionType::PostDecrement);
                    self.next_token();
                }
                TokenType::LeftParen => {
                    expr_types.push(PrefixedExpressionType::FunctionCall(self.function_call_args()?));
                }
                _ => break,
            }
        }

        return Ok(PrefixedExpression { factor, expr_types });
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
                    let identifier = Identifier::from(self.expect(TokenType::Identifier)?);
                    self.expect(TokenType::Assign)?;
                    let expression = self.expression()?;

                    entries.push(PostCallInitializeEntry::TableStyle(identifier, expression))
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