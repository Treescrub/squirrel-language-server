use std::fmt::Display;
use std::process::id;

use crate::ast::*;
use crate::lexer::*;
use crate::source_info::SourceRange;

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub range: SourceRange,
}

impl ParseError {
    pub fn new(message: String, range: SourceRange) -> Self {
        Self {
            message,
            range,
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (line {}, column {})", self.message, self.range.start.line, self.range.start.column)
    }
}

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    token_index: usize,
    node_start: Vec<usize>,
}

impl<'a> Parser<'a> {
    fn wrap_node<T>(&mut self, non_terminal: &dyn Fn(&mut Self) -> Result<T, ParseError>) -> Result<AstNode<T>, ParseError> {
        let start_index = self.token_index;

        let result = non_terminal(self)?;

        let end_index = self.token_index;
        let range = SourceRange::new(self.tokens.get(start_index).unwrap().range.start, self.tokens.get(end_index).unwrap().range.end);

        return Ok(AstNode::new(range, result));
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        Self {
            tokens,
            token_index: 0,
            node_start: Vec::new(),
        }
    }

    fn start_node(&mut self) {
        self.node_start.push(self.token_index);
    }

    fn new_node<T>(&mut self, value: T) -> AstNode<T> {
        let range = SourceRange::new(self.tokens.get(self.node_start.pop().unwrap()).unwrap().range.start, self.tokens.get(self.token_index).unwrap().range.end);

        return AstNode::new(range, value);
    }
    
    pub fn parse(&mut self) -> Result<AstNode<Script>, ParseError> {
        self.ignore_newlines();

        // hacky way to prevent stack overflow :/
        return stacker::grow(100 * 1024 * 1024, || {
            return Ok(self.script()?);
        });
    }

    pub fn reset(&mut self) {
        self.token_index = 0;
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

    fn expect(&mut self, token_type: TokenType) -> Result<&Token, ParseError> {
        if self.current_token_type() != token_type {
            return Err(ParseError::new(format!("Expected token `{}`, got `{}`", token_type, self.current_token_type()), self.current_token().range));
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

    fn build_error(&self, message: String) -> ParseError {
        return ParseError::new(message, self.current_token().range);
    }

    // ---------------------------------------------------

    fn script(&mut self) -> Result<AstNode<Script>, ParseError> {
        self.start_node();

        let mut statements: Vec<AstNode<Statement>> = Vec::new();
        while !self.is_end_of_tokens() {
            statements.push(self.statement()?);
            if self.prev_token().token_type != TokenType::RightCurly && self.prev_token().token_type != TokenType::Semicolon {
                self.optional_semicolon()?;
            }
        }

        return Ok(self.new_node(Script::new(statements)));
    }

    fn optional_semicolon(&mut self) -> Result<(), ParseError> {
        if self.current_token_type() == TokenType::Semicolon {
            self.next_token();
            return Ok(());
        }

        if !self.is_end_of_statement() {
            return Err(self.build_error(String::from("expected end of statement")));
        }

        return Ok(());
    }

    fn statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

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
                let local_declare = self.local_declare()?;

                return Ok(self.new_node(Statement::LocalDeclare(local_declare)));
            }
            TokenType::Return => {
                self.next_token();
                if !self.is_end_of_statement() {
                    let comma_expression = self.comma_expression()?;

                    return Ok(self.new_node(Statement::Return(Some(comma_expression))));
                } else {
                    return Ok(self.new_node(Statement::Return(None)));
                }
            }
            TokenType::Yield => {
                self.next_token();
                if !self.is_end_of_statement() {
                    let comma_expression = self.comma_expression()?;

                    return Ok(self.new_node(Statement::Yield(Some(comma_expression))));
                } else {
                    return Ok(self.new_node(Statement::Yield(None)));
                }
            }
            TokenType::Break => {
                self.next_token();
                return Ok(self.new_node(Statement::Break));
            }
            TokenType::Continue => {
                self.next_token();
                return Ok(self.new_node(Statement::Continue));
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
                let comma_expression = self.comma_expression()?;

                return Ok(self.new_node(Statement::Throw(comma_expression)));
            }
            TokenType::Const => {
                return self.const_statement();
            }
            _ => {
                let comma_expression = self.comma_expression()?;

                return Ok(self.new_node(Statement::CommaExpression(comma_expression)));
            }
        }
    }

    fn if_statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();
        self.expect(TokenType::LeftParen)?;
        let comma_expression = self.comma_expression()?;
        self.expect(TokenType::RightParen)?;

        let if_block = self.statement()?;
        let mut else_block = None;

        if self.current_token_type() == TokenType::Else {
            self.next_token();
            else_block = Some(self.statement()?);
        }

        return Ok(self.new_node(Statement::If(comma_expression, if_block, else_block)));
    }

    fn while_statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();
        self.expect(TokenType::LeftParen)?;
        let comma_expression = self.comma_expression()?;
        self.expect(TokenType::RightParen)?;
        let statement = self.statement()?;

        return Ok(self.new_node(Statement::While(comma_expression, statement)));
    }

    fn do_while_statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();
        let statement = self.statement()?;
        self.expect(TokenType::While)?;
        self.expect(TokenType::LeftParen)?;
        let comma_expression = self.comma_expression()?;
        self.expect(TokenType::RightParen)?;

        return Ok(self.new_node(Statement::DoWhile(statement, comma_expression)));
    }

    fn for_statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();
        self.expect(TokenType::LeftParen)?;

        let mut init = None;
        let mut condition = None;
        let mut post = None;

        if self.current_token_type() == TokenType::Local {
            self.start_node();
            let local_declare = self.local_declare()?;

            init = Some(self.new_node(ForInit::LocalDeclare(local_declare)));
        } else if self.current_token_type() != TokenType::Semicolon {
            self.start_node();
            let comma_expression = self.comma_expression()?;

            init = Some(self.new_node(ForInit::CommaExpression(comma_expression)));
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

        let statement = self.statement()?;

        return Ok(self.new_node(Statement::For(init, condition, post, statement)));
    }

    fn for_each_statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();
        self.expect(TokenType::LeftParen)?;

        self.start_node();
        let identifier = Identifier::from(self.expect(TokenType::Identifier)?);
        let mut value_identifier = self.new_node(identifier);
        let mut key_identifier = None;

        if self.current_token_type() == TokenType::Comma {
            self.next_token();
            key_identifier = Some(value_identifier);

            self.start_node();
            let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

            value_identifier = self.new_node(identifier);
        }

        self.expect(TokenType::In)?;
        let expression = self.expression()?;
        self.expect(TokenType::RightParen)?;
        let statement = self.statement()?;

        return Ok(self.new_node(Statement::ForEach(value_identifier, key_identifier, expression, statement)));
    }

    fn switch_statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();
        self.expect(TokenType::LeftParen)?;
        let value = self.comma_expression()?;
        self.expect(TokenType::RightParen)?;
        self.expect(TokenType::LeftCurly)?;

        let mut switch_cases = Vec::new();

        while self.current_token_type() == TokenType::Case {
            self.start_node();

            self.next_token();
            let expression = self.expression()?;
            self.expect(TokenType::Colon)?;

            let statements = self.statements()?;

            switch_cases.push(self.new_node(SwitchCase::new(expression, statements)));
        }

        let mut default = None;
        if self.current_token_type() == TokenType::Default {
            self.next_token();
            self.expect(TokenType::Colon)?;
            default = Some(self.statements()?);
        }

        self.expect(TokenType::RightCurly)?;
 
        return Ok(self.new_node(Statement::Switch(value, switch_cases, default)));
    }

    fn function_statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();

        let mut identifiers = Vec::new();
        self.start_node();
        self.start_node();
        let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

        identifiers.push(self.new_node(identifier));

        while self.current_token_type() == TokenType::DoubleColon {
            self.start_node();

            self.next_token();
            let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

            identifiers.push(self.new_node(identifier));
        }

        let mut bind_env = None;

        if self.current_token_type() == TokenType::LeftSquare {
            self.next_token();
            let env_expression = self.expression()?;
            self.expect(TokenType::RightSquare)?;
            
            bind_env = Some(env_expression);
        }

        let function_identifier = self.new_node(FunctionIdentifier::new(identifiers));
        let params = self.function_params()?;
        let body = self.statement()?;

        return Ok(self.new_node(Statement::Function(function_identifier, bind_env, params, body)));
    }

    fn class_statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();
        let class_name = self.prefixed_expression()?;
        let class_expression = self.class_expression()?;

        return Ok(self.new_node(Statement::Class(class_name, class_expression)));
    }

    fn enum_statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();
        self.start_node();
        let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

        let name = self.new_node(identifier);
        self.expect(TokenType::LeftCurly)?;

        let mut entries = Vec::new();

        self.start_node();
        while self.current_token_type() != TokenType::RightCurly {
            self.start_node();

            self.start_node();
            let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

            let entry_name = self.new_node(identifier);
            let mut entry_value = None;

            if self.current_token_type() == TokenType::Assign {
                self.next_token();
                entry_value = Some(self.scalar()?);
            }

            entries.push(self.new_node(EnumEntry::new(entry_name, entry_value)));

            if self.current_token_type() == TokenType::Comma {
                self.next_token();
            }
        }
        let values = self.new_node(EnumValues::new(entries));
        self.next_token();

        return Ok(self.new_node(Statement::Enum(name, values)));
    }

    fn statement_block(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();
        let statements = self.statements()?;
        self.expect(TokenType::RightCurly)?;

        return Ok(self.new_node(Statement::StatementBlock(statements)));
    }

    fn try_statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();
        let try_body = self.statement()?;
        self.expect(TokenType::Catch)?;
        self.expect(TokenType::LeftParen)?;

        self.start_node();
        let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

        let identifier = self.new_node(identifier);

        self.expect(TokenType::RightParen)?;
        let catch_body = self.statement()?;

        return Ok(self.new_node(Statement::TryCatch(try_body, identifier, catch_body)));
    }

    fn function_params(&mut self) -> Result<AstNode<FunctionParams>, ParseError> {
        self.start_node();

        self.expect(TokenType::LeftParen)?;

        let mut params = Vec::new();

        while self.current_token_type() != TokenType::RightParen {
            self.start_node();
            if self.current_token_type() == TokenType::Varargs {
                self.next_token();
                
                if self.current_token_type() != TokenType::RightParen {
                    return Err(self.build_error(String::from("expected ')' after varargs")));
                }

                params.push(self.new_node(FunctionParam::VarParams));
            } else {
                self.start_node();
                let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

                let name = self.new_node(identifier);

                if self.current_token_type() == TokenType::Assign {
                    self.next_token();
                    let default_val = self.expression()?;

                    params.push(self.new_node(FunctionParam::Default(name, default_val)));
                } else {
                    params.push(self.new_node(FunctionParam::Normal(name)));
                }
            }

            if self.current_token_type() == TokenType::Comma {
                self.next_token();
            } else if self.current_token_type() != TokenType::RightParen {
                return Err(self.build_error(String::from("expected ')' or ','")));
            }
        }

        self.expect(TokenType::RightParen)?;

        return Ok(self.new_node(FunctionParams::new(params)));
    }

    fn local_declare(&mut self) -> Result<AstNode<LocalDeclare>, ParseError> {
        self.start_node();

        self.next_token();
        if self.current_token_type() == TokenType::Function {
            self.next_token();
            self.start_node();
            let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

            let identifier = self.new_node(identifier);
            let mut bind_env = None;

            if self.current_token_type() == TokenType::LeftSquare {
                bind_env = Some(self.expression()?);
                self.expect(TokenType::RightSquare)?;
            }

            let params = self.function_params()?;
            let body = self.statement()?;

            return Ok(self.new_node(LocalDeclare::Function(identifier, bind_env, params, body)));
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

        return Ok(self.new_node(LocalDeclare::Assign(assign_expressions)));
    }

    fn assign_expression(&mut self) -> Result<AstNode<AssignExpression>, ParseError> {
        self.start_node();

        self.start_node();
        let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

        let identifier = self.new_node(identifier);
        let mut expression = None;

        if self.current_token_type() == TokenType::Assign {
            self.next_token();
            expression = Some(self.expression()?);
        }

        return Ok(self.new_node(AssignExpression::new(identifier, expression)));
    }

    fn const_statement(&mut self) -> Result<AstNode<Statement>, ParseError> {
        self.start_node();

        self.next_token();
        self.start_node();
        let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

        let id = self.new_node(identifier);
        self.expect(TokenType::Assign)?;
        let scalar = self.scalar()?;
        self.optional_semicolon()?;

        return Ok(self.new_node(Statement::Const(id, scalar)));
    }

    fn scalar(&mut self) -> Result<AstNode<Scalar>, ParseError> {
        self.start_node();

        match self.current_token_type() {
            TokenType::IntegerLiteral => {
                let value = self.current_token().nvalue.unwrap();
                self.next_token();

                return Ok(self.new_node(Scalar::Integer(value)));
            }
            TokenType::FloatLiteral => {
                let value = self.current_token().fvalue.unwrap();
                self.next_token();

                return Ok(self.new_node(Scalar::Float(value)));
            }
            TokenType::StringLiteral => {
                let value = self.current_token().svalue.as_ref().unwrap().to_string();
                self.next_token();

                return Ok(self.new_node(Scalar::StringLiteral(value)));
            }
            TokenType::True => {
                self.next_token();
                return Ok(self.new_node(Scalar::True));
            }
            TokenType::False => {
                self.next_token();
                return Ok(self.new_node(Scalar::False));
            }
            TokenType::Minus => {
                self.next_token();
                match self.current_token_type() {
                    TokenType::IntegerLiteral => {
                        let value = self.current_token().nvalue.unwrap();
                        self.next_token();

                        return Ok(self.new_node(Scalar::Integer(value)));
                    }
                    TokenType::FloatLiteral => {
                        let value = self.current_token().fvalue.unwrap();
                        self.next_token();

                        return Ok(self.new_node(Scalar::Float(value)));
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

    fn factor(&mut self) -> Result<AstNode<Factor>, ParseError> {
        self.start_node();

        match self.current_token_type() {
            TokenType::StringLiteral => {
                self.start_node();
                let value = self.current_token().svalue.as_ref().unwrap().to_string();
                self.next_token();

                let string_literal = Scalar::StringLiteral(value);
                let scalar = Factor::Scalar(self.new_node(string_literal));

                return Ok(self.new_node(scalar));
            }
            TokenType::Base => {
                self.next_token();

                return Ok(self.new_node(Factor::Base));
            }
            TokenType::Identifier => {
                self.start_node();
                let identifier = self.new_node(Identifier::from(self.current_token()));
                self.next_token();

                return Ok(self.new_node(Factor::Identifier(identifier)));
            }
            TokenType::Constructor => {
                self.next_token();

                return Ok(self.new_node(Factor::Constructor));
            }
            TokenType::This => {
                self.next_token();

                return Ok(self.new_node(Factor::This));
            }
            TokenType::DoubleColon => {
                self.next_token();

                let prefixed_expr = self.prefixed_expression()?;

                return Ok(self.new_node(Factor::DoubleColon(prefixed_expr)));
            }
            TokenType::Null => {
                self.next_token();

                return Ok(self.new_node(Factor::Null));
            }
            TokenType::IntegerLiteral => {
                self.start_node();
                let value = self.new_node(Scalar::Integer(self.current_token().nvalue.unwrap()));
                self.next_token();

                return Ok(self.new_node(Factor::Scalar(value)));
            }
            TokenType::FloatLiteral => {
                self.start_node();
                let value = self.new_node(Scalar::Float(self.current_token().fvalue.unwrap()));
                self.next_token();

                return Ok(self.new_node(Factor::Scalar(value)));
            }
            TokenType::True => {
                self.start_node();
                self.next_token();

                let true_scalar = self.new_node(Scalar::True);

                return Ok(self.new_node(Factor::Scalar(true_scalar)));
            }
            TokenType::False => {
                self.start_node();
                self.next_token();

                let false_scalar = self.new_node(Scalar::False);

                return Ok(self.new_node(Factor::Scalar(false_scalar)));
            }
            TokenType::LeftSquare => {
                self.next_token();

                return Ok(self.array_init()?);
            }
            TokenType::LeftCurly => {
                let table = self.table()?;

                return Ok(self.new_node(Factor::TableInit(table)));
            }
            TokenType::Function => {
                return self.function_expression();
            }
            TokenType::At => {
                return self.lambda_expression();
            }
            TokenType::Class => {
                self.next_token();
                
                let class_expression = self.class_expression()?;

                return Ok(self.new_node(Factor::ClassExpression(class_expression)));
            }
            TokenType::Minus => {
                self.start_node();
                self.next_token();
                let minus = self.new_node(TokenType::Minus);

                match self.current_token_type() {
                    TokenType::IntegerLiteral => {
                        self.start_node();
                        let value = self.new_node(Scalar::Integer(self.current_token().nvalue.unwrap()));
                        self.next_token();

                        return Ok(self.new_node(Factor::Scalar(value)));
                    }
                    TokenType::FloatLiteral => {
                        self.start_node();
                        let value = self.new_node(Scalar::Float(self.current_token().fvalue.unwrap()));
                        self.next_token();

                        return Ok(self.new_node(Factor::Scalar(value)));
                    }
                    _ => {
                        let unary_op = self.unary_op(minus)?;

                        return Ok(self.new_node(Factor::UnaryOp(unary_op)));
                    }
                }
            }
            TokenType::LogicalNot | TokenType::BitwiseNot | TokenType::Typeof | TokenType::Resume | TokenType::Clone
                | TokenType::MinusMinus | TokenType::PlusPlus => {
                self.start_node();
                let operator = self.new_node(self.current_token_type());
                self.next_token();

                let unary_op = self.unary_op(operator)?;

                return Ok(self.new_node(Factor::UnaryOp(unary_op)));
            }
            TokenType::Rawcall => {
                self.next_token();

                let function_call_args = self.function_call_args()?;

                return Ok(self.new_node(Factor::RawCall(function_call_args)));
            }
            TokenType::Delete => {
                self.next_token();
                let prefixed_expression = self.prefixed_expression()?;

                return Ok(self.new_node(Factor::Delete(prefixed_expression)));
            }
            TokenType::LeftParen => {
                self.next_token();
                let comma_expression = self.comma_expression()?;
                self.expect(TokenType::RightParen)?;

                return Ok(self.new_node(Factor::ParenExpression(comma_expression)));
            }
            TokenType::LineInfo => {
                self.next_token();

                return Ok(self.new_node(Factor::LineInfo));
            }
            TokenType::FileInfo => {
                self.next_token();

                return Ok(self.new_node(Factor::FileInfo));
            }
            unhandled_type => {
                return Err(self.build_error(format!("Unexpected token for factor: '{}'", unhandled_type)));
            }
        }
    }

    fn simple_table_entry(&mut self) -> Result<AstNode<TableEntry>, ParseError> {
        self.start_node();

        self.start_node();
        let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

        let key = self.new_node(identifier);
        self.expect(TokenType::Assign)?;
        let value = self.expression()?;

        return Ok(self.new_node(TableEntry::Simple(key, value)))
    }

    fn table_entry(&mut self, is_class: bool) -> Result<AstNode<TableEntry>, ParseError> {
        self.start_node();

        match self.current_token_type() {
            TokenType::Function => {
                self.next_token();

                self.start_node();
                let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

                let key = self.new_node(identifier);
                let params = self.function_params()?;
                let body = self.statement()?;

                return Ok(self.new_node(TableEntry::Function(key, params, body)));
            },
            TokenType::Constructor => {
                self.next_token();
                let params = self.function_params()?;
                let body = self.statement()?;

                return Ok(self.new_node(TableEntry::Constructor(params, body)));
            },
            TokenType::LeftSquare => {
                self.next_token();
                let key = self.comma_expression()?;
                self.expect(TokenType::RightSquare)?;
                self.expect(TokenType::Assign)?;
                let value = self.expression()?;

                return Ok(self.new_node(TableEntry::DynamicAssign(key, value)));
            },
            TokenType::StringLiteral => {
                if !is_class {
                    let key = self.current_token().svalue.as_ref().unwrap().clone();
                    self.next_token();
                    self.expect(TokenType::Colon)?;
                    let value = self.expression()?;

                    return Ok(self.new_node(TableEntry::JsonStyle(key, value)));
                } else {
                    return self.simple_table_entry();
                }
            },
            _ => {
                return self.simple_table_entry();
            },
        }
    }

    fn table(&mut self) -> Result<AstNode<Table>, ParseError> {
        self.start_node();

        self.next_token();

        let mut entries = Vec::new();

        while self.current_token_type() != TokenType::RightCurly {
            entries.push(self.table_entry(false)?);

            if self.current_token_type() == TokenType::Comma {
                self.next_token();
            }
        }
        self.next_token();

        return Ok(self.new_node(Table::new(entries)));
    }

    fn statements(&mut self) -> Result<AstNode<Statements>, ParseError> {
        self.start_node();

        let mut statements: Vec<AstNode<Statement>> = Vec::new();
        while self.current_token_type() != TokenType::RightCurly && self.current_token_type() != TokenType::Default && self.current_token_type() != TokenType::Case {
            statements.push(self.statement()?);

            if self.prev_token().token_type != TokenType::RightCurly && self.prev_token().token_type != TokenType::Semicolon {
                self.optional_semicolon()?;
            }
        }

        return Ok(self.new_node(Statements::new(statements)));
    }

    fn expression(&mut self) -> Result<AstNode<Expression>, ParseError> {
        self.start_node();

        let logical_or = self.logical_or_expression()?;
        let mut expr_type = None;

        self.start_node();
        match self.current_token_type() {
            TokenType::Newslot => {
                self.next_token();
                let expression = self.expression()?;

                expr_type = Some(self.new_node(ExpressionType::Newslot(expression)));
            }
            TokenType::Assign => {
                self.next_token();
                let expression = self.expression()?;

                expr_type = Some(self.new_node(ExpressionType::Assign(expression)));
            }
            TokenType::MinusEqual => {
                self.next_token();
                let expression = self.expression()?;

                expr_type = Some(self.new_node(ExpressionType::MinusEqual(expression)));
            }
            TokenType::PlusEqual => {
                self.next_token();
                let expression = self.expression()?;

                expr_type = Some(self.new_node(ExpressionType::PlusEqual(expression)));
            }
            TokenType::MultiplyEqual => {
                self.next_token();
                let expression = self.expression()?;

                expr_type = Some(self.new_node(ExpressionType::MultiplyEqual(expression)));
            }
            TokenType::DivideEqual => {
                self.next_token();
                let expression = self.expression()?;

                expr_type = Some(self.new_node(ExpressionType::DivideEqual(expression)));
            }
            TokenType::Ternary => {
                self.next_token();
                let true_case = self.expression()?;
                self.expect(TokenType::Colon)?;
                let false_case = self.expression()?;

                expr_type = Some(self.new_node(ExpressionType::Ternary(true_case, false_case)));
            }
            _ => {}
        }

        return Ok(self.new_node(Expression::new(logical_or, expr_type)));
    }

    fn function_expression(&mut self) -> Result<AstNode<Factor>, ParseError> {
        self.start_node();

        self.next_token();

        let mut bind_env = None;

        if self.current_token_type() == TokenType::LeftSquare {
            self.next_token();
            bind_env = Some(self.expression()?);
            self.expect(TokenType::RightSquare)?;
        }

        let params = self.function_params()?;
        let body = self.statement()?;

        return Ok(self.new_node(Factor::FunctionExpression(bind_env, params, body)));
    }

    fn lambda_expression(&mut self) -> Result<AstNode<Factor>, ParseError> {
        self.start_node();

        self.next_token();

        let mut bind_env = None;

        if self.current_token_type() == TokenType::LeftSquare {
            self.next_token();
            bind_env = Some(self.expression()?);
            self.expect(TokenType::RightSquare)?;
        }

        let params = self.function_params()?;
        let body = self.expression()?;

        return Ok(self.new_node(Factor::LambdaExpression(bind_env, params, body)));
    }

    fn array_init(&mut self) -> Result<AstNode<Factor>, ParseError> {
        self.start_node();

        let mut expressions = Vec::new();

        while self.current_token_type() != TokenType::RightSquare {
            expressions.push(self.expression()?);

            if self.current_token_type() == TokenType::Comma {
                self.next_token();
            }
        }

        self.next_token();
        
        return Ok(self.new_node(Factor::ArrayInit(expressions)));
    }

    fn class_expression(&mut self) -> Result<AstNode<ClassExpression>, ParseError> {
        self.start_node();

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

        return Ok(self.new_node(ClassExpression::new(base_class, attributes, body)));
    }

    fn class_table(&mut self) -> Result<AstNode<Table>, ParseError> {
        self.start_node();

        self.expect(TokenType::LeftCurly)?;

        let mut entries = Vec::new();

        while self.current_token_type() != TokenType::RightCurly {
            if self.current_token_type() == TokenType::AttributeOpen {
                self.start_node();
                let class_attributes = self.class_attributes()?;

                entries.push(self.new_node(TableEntry::Attributes(class_attributes)));
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

        return Ok(self.new_node(Table::new(entries)));
    }

    fn class_attributes(&mut self) -> Result<AstNode<Table>, ParseError> {
        self.start_node();

        self.next_token();

        let mut entries = Vec::new();

        while self.current_token_type() != TokenType::AttributeClose {
            entries.push(self.table_entry(false)?);

            if self.current_token_type() == TokenType::Comma {
                self.next_token();
            }
        }
        self.next_token();

        return Ok(self.new_node(Table::new(entries)));
    }

    fn logical_or_expression(&mut self) -> Result<AstNode<LogicalOrExpression>, ParseError> {
        self.start_node();

        let left = self.logical_and_expression()?;
        let mut right = Vec::new();

        while self.current_token_type() == TokenType::LogicalOr {
            self.next_token();
            right.push(self.logical_or_expression()?);
        }

        return Ok(self.new_node(LogicalOrExpression::new(left, right)));
    }

    fn logical_and_expression(&mut self) -> Result<AstNode<LogicalAndExpression>, ParseError> {
        self.start_node();

        let left = self.bitwise_or_expression()?;
        let mut right = Vec::new();

        while self.current_token_type() == TokenType::LogicalAnd {
            self.next_token();
            right.push(self.logical_and_expression()?);
        }

        return Ok(self.new_node(LogicalAndExpression::new(left, right)));
    }

    fn bitwise_or_expression(&mut self) -> Result<AstNode<BitwiseOrExpression>, ParseError> {
        self.start_node();

        let left = self.bitwise_xor_expression()?;
        let mut right = Vec::new();

        while self.current_token_type() == TokenType::BitwiseOr {
            self.next_token();
            right.push(self.bitwise_xor_expression()?);
        }
        
        return Ok(self.new_node(BitwiseOrExpression::new(left, right)));
    }

    fn bitwise_xor_expression(&mut self) -> Result<AstNode<BitwiseXorExpression>, ParseError> {
        self.start_node();

        let left = self.bitwise_and_expression()?;
        let mut right = Vec::new();

        while self.current_token_type() == TokenType::BitwiseXor {
            self.next_token();
            right.push(self.bitwise_and_expression()?);
        }

        return Ok(self.new_node(BitwiseXorExpression::new(left, right)));
    }

    fn bitwise_and_expression(&mut self) -> Result<AstNode<BitwiseAndExpression>, ParseError> {
        self.start_node();

        let left = self.equal_expression()?;
        let mut right = Vec::new();

        while self.current_token_type() == TokenType::BitwiseAnd {
            self.next_token();
            right.push(self.equal_expression()?);
        }
        
        return Ok(self.new_node(BitwiseAndExpression::new(left, right)));
    }

    fn equal_expression(&mut self) -> Result<AstNode<EqualExpression>, ParseError> {
        self.start_node();

        let left = self.compare_expression()?;
        let mut slices = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::Equal | TokenType::NotEqual | TokenType::ThreeWayCompare => {
                    self.start_node();
                    self.start_node();
                    let operator = self.new_node(self.current_token_type());
                    self.next_token();
                    let right = self.compare_expression()?;

                    slices.push(self.new_node(BinaryOpSlice::new(operator, right)));
                }
                _ => break,
            }
        }

        return Ok(self.new_node(EqualExpression::new(left, slices)));
    }

    fn compare_expression(&mut self) -> Result<AstNode<CompareExpression>, ParseError> {
        self.start_node();

        let left = self.shift_expression()?;
        let mut slices = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::GreaterThan | TokenType::GreaterOrEqual | TokenType::LessThan 
                    | TokenType::LessOrEqual | TokenType::In | TokenType::Instanceof => {
                    self.start_node();
                    self.start_node();
                    let operator = self.new_node(self.current_token_type());
                    self.next_token();
                    let right = self.shift_expression()?;

                    slices.push(self.new_node(BinaryOpSlice::new(operator, right)));
                }
                _ => break,
            }
        }

        return Ok(self.new_node(CompareExpression::new(left, slices)));
    }

    fn shift_expression(&mut self) -> Result<AstNode<ShiftExpression>, ParseError> {
        self.start_node();

        let left = self.plus_expression()?;
        let mut slices = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::UnsignedShiftRight | TokenType::ShiftLeft | TokenType::ShiftRight => {
                    self.start_node();
                    self.start_node();
                    let operator = self.new_node(self.current_token_type());
                    self.next_token();
                    let right = self.plus_expression()?;

                    slices.push(self.new_node(BinaryOpSlice::new(operator, right)));
                }
                _ => break,
            }
        }

        return Ok(self.new_node(ShiftExpression::new(left, slices)));
    }

    fn plus_expression(&mut self) -> Result<AstNode<PlusExpression>, ParseError> {
        self.start_node();

        let left = self.multiply_expression()?;
        let mut slices = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::Plus | TokenType::Minus => {
                    self.start_node();
                    self.start_node();
                    let operator = self.new_node(self.current_token_type());
                    self.next_token();
                    let right = self.multiply_expression()?;

                    slices.push(self.new_node(BinaryOpSlice::new(operator, right)));
                }
                _ => break,
            }
        }

        return Ok(self.new_node(PlusExpression::new(left, slices)));
    }

    fn multiply_expression(&mut self) -> Result<AstNode<MultiplyExpression>, ParseError> {
        self.start_node();

        let left = self.prefixed_expression()?;
        let mut slices = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::Multiply | TokenType::Divide | TokenType::Modulo => {
                    self.start_node();
                    self.start_node();
                    let operator = self.new_node(self.current_token_type());
                    self.next_token();
                    let right = self.prefixed_expression()?;

                    slices.push(self.new_node(BinaryOpSlice::new(operator, right)));
                }
                _ => break,
            }
        }

        return Ok(self.new_node(MultiplyExpression::new(left, slices)));
    }

    fn prefixed_expression(&mut self) -> Result<AstNode<PrefixedExpression>, ParseError> {
        self.start_node();

        let factor = self.factor()?;
        let mut expr_types = Vec::new();

        loop {
            match self.current_token_type() {
                TokenType::Dot => {
                    self.start_node();
                    self.next_token();
                    self.start_node();
                    let identifier = Identifier::from(self.expect(TokenType::Identifier)?);

                    let id = self.new_node(identifier);

                    expr_types.push(self.new_node(PrefixedExpressionType::DotAccess(id)));
                }
                TokenType::LeftSquare => {
                    self.start_node();
                    self.next_token();
                    let expression = self.expression()?;
                    self.expect(TokenType::RightSquare)?;

                    expr_types.push(self.new_node(PrefixedExpressionType::ArrayStyleAccess(expression)));
                }
                TokenType::PlusPlus => {
                    self.start_node();
                    expr_types.push(self.new_node(PrefixedExpressionType::PostIncrement));
                    self.next_token();
                }
                TokenType::MinusMinus => {
                    self.start_node();
                    expr_types.push(self.new_node(PrefixedExpressionType::PostDecrement));
                    self.next_token();
                }
                TokenType::LeftParen => {
                    self.start_node();
                    let function_call_args = self.function_call_args()?;

                    expr_types.push(self.new_node(PrefixedExpressionType::FunctionCall(function_call_args)));
                }
                _ => break,
            }
        }

        return Ok(self.new_node(PrefixedExpression::new(factor, expr_types)));
    }

    fn function_call_args(&mut self) -> Result<AstNode<FunctionCallArgs>, ParseError> {
        self.start_node();

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
            self.start_node();
            self.next_token();
            let mut entries = Vec::new();
            while self.current_token_type() != TokenType::RightCurly {
                self.start_node();
                if self.current_token_type() == TokenType::LeftSquare {
                    self.next_token();
                    let comma_expression = self.comma_expression()?;
                    self.expect(TokenType::RightSquare)?;
                    self.expect(TokenType::Assign)?;
                    let expression = self.expression()?;

                    entries.push(self.new_node(PostCallInitializeEntry::ArrayStyle(comma_expression, expression)));
                } else {
                    self.start_node();
                    let identifier = Identifier::from(self.expect(TokenType::Identifier)?);
                    
                    let identifier = self.new_node(identifier);
                    self.expect(TokenType::Assign)?;
                    let expression = self.expression()?;

                    entries.push(self.new_node(PostCallInitializeEntry::TableStyle(identifier, expression)));
                }

                if self.current_token_type() == TokenType::Comma {
                    self.next_token();
                }
            }
            self.next_token();
            post_call_init = Some(self.new_node(PostCallInitialize::new(entries)));
        }

        return Ok(self.new_node(FunctionCallArgs::new(expressions, post_call_init)));
    }

    fn comma_expression(&mut self) -> Result<AstNode<CommaExpression>, ParseError> {
        self.start_node();

        let mut expressions = vec![self.expression()?];
        while self.current_token_type() == TokenType::Comma {
            self.next_token();
            expressions.push(self.expression()?);
        }

        return Ok(self.new_node(CommaExpression::new(expressions)));
    }

    fn unary_op(&mut self, operator: AstNode<TokenType>) -> Result<AstNode<UnaryOp>, ParseError> {
        self.start_node();

        let expression = self.prefixed_expression()?;

        return Ok(self.new_node(UnaryOp::new(operator, expression)));
    }
}