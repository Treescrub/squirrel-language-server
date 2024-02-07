use crate::analysis::ast::*;
pub mod pretty_printer;

pub trait SimpleVisitorMut {
    fn visit_script(&mut self, script: &AstNode<Script>) {
        self.walk_script(script);
    }
    fn visit_statements(&mut self, statements: &AstNode<Statements>) {
        self.walk_statements(statements);
    }
    fn visit_statement(&mut self, statement: &AstNode<Statement>) {
        self.walk_statement(statement);
    }
    fn visit_if_statement(&mut self, condition: &AstNode<CommaExpression>, if_block: &AstNode<Statement>, else_block: &Option<AstNode<Statement>>) {
        self.walk_if_statement(condition, if_block, else_block);
    }
    fn visit_while_statement(&mut self, condition: &AstNode<CommaExpression>, loop_body: &AstNode<Statement>) {
        self.walk_while_statement(condition, loop_body);
    }
    fn visit_do_while_statement(&mut self, loop_body: &AstNode<Statement>, condition: &AstNode<CommaExpression>) {
        self.walk_do_while_statement(loop_body, condition);
    }
    fn visit_for_statement(&mut self, init: &Option<AstNode<ForInit>>, condition: &Option<AstNode<CommaExpression>>, post: &Option<AstNode<CommaExpression>>, body: &AstNode<Statement>) {
        self.walk_for_statement(init, condition, post, body);
    }
    fn visit_foreach_statement(&mut self, value: &AstNode<Identifier>, key: &Option<AstNode<Identifier>>, container: &AstNode<Expression>, body: &AstNode<Statement>) {
        self.walk_foreach_statement(value, key, container, body);
    }
    fn visit_switch_statement(&mut self, value: &AstNode<CommaExpression>, cases: &Vec<AstNode<SwitchCase>>, default: &Option<AstNode<Statements>>) {
        self.walk_switch_statement(value, cases, default);
    }
    fn visit_local_declare_statement(&mut self, local_declare: &AstNode<LocalDeclare>) {
        self.walk_local_declare_statement(local_declare);
    }
    fn visit_return_statement(&mut self, value: &Option<AstNode<CommaExpression>>) {
        self.walk_return_statement(value);
    }
    fn visit_yield_statement(&mut self, value: &Option<AstNode<CommaExpression>>) {
        self.walk_yield_statement(value);
    }
    fn visit_break_statement(&mut self) {
        self.walk_break_statement();
    }
    fn visit_continue_statement(&mut self) {
        self.walk_continue_statement();
    }
    fn visit_function_statement(&mut self, func_identifier: &AstNode<FunctionIdentifier>, env: &Option<AstNode<Expression>>, params: &AstNode<FunctionParams>, body: &AstNode<Statement>) {
        self.walk_function_statement(func_identifier, env, params, body);
    }
    fn visit_class_statement(&mut self, name: &AstNode<PrefixedExpression>, class_expr: &AstNode<ClassExpression>) {
        self.walk_class_statement(name, class_expr);
    }
    fn visit_enum_statement(&mut self, name: &AstNode<Identifier>, values: &AstNode<EnumValues>) {
        self.walk_enum_statement(name, values);
    }
    fn visit_statement_block(&mut self, statements: &AstNode<Statements>) {
        self.walk_statement_block(statements);
    }
    fn visit_trycatch_statement(&mut self, try_body: &AstNode<Statement>, exception_name: &AstNode<Identifier>, catch_body: &AstNode<Statement>) {
        self.walk_trycatch_statement(try_body, exception_name, catch_body);
    }
    fn visit_throw_statement(&mut self, exception: &AstNode<CommaExpression>) {
        self.walk_throw_statement(exception);
    }
    fn visit_const_statement(&mut self, name: &AstNode<Identifier>, value: &AstNode<Scalar>) {
        self.walk_const_statement(name, value);
    }
    fn visit_comma_expr_statement(&mut self, comma_expr: &AstNode<CommaExpression>) {
        self.walk_comma_expr_statement(comma_expr);
    }
    fn visit_local_declare(&mut self, local_declare: &AstNode<LocalDeclare>) {
        self.walk_local_declare(local_declare);
    }
    fn visit_assign_expr(&mut self, assign_expr: &AstNode<AssignExpression>) {
        self.walk_assign_expr(assign_expr);
    }
    fn visit_for_init(&mut self, for_init: &AstNode<ForInit>) {
        self.walk_for_init(for_init);
    }
    fn visit_unary_op(&mut self, unary_op: &AstNode<UnaryOp>) {
        self.walk_unary_op(unary_op);
    }
    fn visit_func_identifier(&mut self, func_identifier: &AstNode<FunctionIdentifier>) {
        self.walk_func_identifier(func_identifier);
    }
    fn visit_func_params(&mut self, params: &AstNode<FunctionParams>) {
        self.walk_func_params(params);
    }
    fn visit_func_param(&mut self, param: &AstNode<FunctionParam>) {
        self.walk_func_param(param);
    }
    fn visit_post_call_init(&mut self, post_call_init: &AstNode<PostCallInitialize>) {
        self.walk_post_call_init(post_call_init);
    }
    fn visit_table(&mut self, table: &AstNode<Table>) {
        self.walk_table(table);
    }
    fn visit_table_entry(&mut self, entry: &AstNode<TableEntry>) {
        self.walk_table_entry(entry);
    }
    fn visit_expression(&mut self, expression: &AstNode<Expression>) {
        self.walk_expression(expression);
    }
    fn visit_class_expr(&mut self, class_expression: &AstNode<ClassExpression>) {
        self.walk_class_expr(class_expression);
    }
    fn visit_comma_expr(&mut self, comma_expression: &AstNode<CommaExpression>) {
        self.walk_comma_expr(comma_expression);
    }
    fn visit_logical_or_exp(&mut self, logical_or: &AstNode<LogicalOrExpression>) {
        self.walk_logical_or_exp(logical_or);
    }
    fn visit_logical_and_exp(&mut self, logical_and: &AstNode<LogicalAndExpression>) {
        self.walk_logical_and_exp(logical_and);
    }
    fn visit_bitwise_or_exp(&mut self, bitwise_or: &AstNode<BitwiseOrExpression>) {
        self.walk_bitwise_or_exp(bitwise_or);
    }
    fn visit_bitwise_xor_exp(&mut self, bitwise_xor: &AstNode<BitwiseXorExpression>) {
        self.walk_bitwise_xor_exp(bitwise_xor);
    }
    fn visit_bitwise_and_exp(&mut self, bitwise_and: &AstNode<BitwiseAndExpression>) {
        self.walk_bitwise_and_exp(bitwise_and);
    }
    fn visit_equal_exp(&mut self, equal: &AstNode<EqualExpression>) {
        self.walk_equal_exp(equal);
    }
    fn visit_compare_exp(&mut self, compare: &AstNode<CompareExpression>) {
        self.walk_compare_exp(compare);
    }
    fn visit_shift_exp(&mut self, shift: &AstNode<ShiftExpression>) {
        self.walk_shift_exp(shift);
    }
    fn visit_plus_exp(&mut self, plus: &AstNode<PlusExpression>) {
        self.walk_plus_exp(plus);
    }
    fn visit_multiply_exp(&mut self, multiply: &AstNode<MultiplyExpression>) {
        self.walk_multiply_exp(multiply);
    }
    fn visit_prefixed_exp(&mut self, prefixed: &AstNode<PrefixedExpression>) {
        self.walk_prefixed_exp(prefixed);
    }
    fn visit_scalar(&mut self, _scalar: &AstNode<Scalar>) {}
    fn visit_factor(&mut self, _factor: &AstNode<Factor>) {}
    fn visit_identifier(&mut self, _identifier: &AstNode<Identifier>) {}


    // walk methods for default visiting behavior

    fn walk_script(&mut self, script: &AstNode<Script>) {
        self.visit_statements(&script.value.statements);
    }
    fn walk_statements(&mut self, statements: &AstNode<Statements>) {
        for statement in &statements.value.statements {
            self.visit_statement(statement);
        }
    }
    fn walk_statement(&mut self, statement: &AstNode<Statement>) {
        match statement.value.as_ref() {
            Statement::If(condition, if_block, else_block) => {
                self.visit_if_statement(condition, if_block, else_block);
            }
            Statement::While(condition, loop_body) => {
                self.visit_while_statement(condition, loop_body);
            }
            Statement::DoWhile(loop_body, condition) => {
                self.visit_do_while_statement(loop_body, condition);
            }
            Statement::For(init, condition, post, body) => {
                self.visit_for_statement(init, condition, post, body);
            },
            Statement::ForEach(value, key, container, body) => {
                self.visit_foreach_statement(value, key, container, body);
            },
            Statement::Switch(value, cases, default) => {
                self.visit_switch_statement(value, cases, default);
            },
            Statement::LocalDeclare(local_declare) => {
                self.visit_local_declare_statement(local_declare);
            },
            Statement::Return(value) => {
                self.visit_return_statement(value);
            },
            Statement::Yield(value) => {
                self.visit_yield_statement(value);
            },
            Statement::Break => {
                self.visit_break_statement();
            },
            Statement::Continue => {
                self.visit_continue_statement();
            },
            Statement::Function(func_identifier, env, params, body) => {
                self.visit_function_statement(func_identifier, env, params, body);
            },
            Statement::Class(name, class_expr) => {
                self.visit_class_statement(name, class_expr);
            },
            Statement::Enum(name, values) => {
                self.visit_enum_statement(name, values);
            },
            Statement::StatementBlock(statements) => {
                self.visit_statement_block(statements);
            },
            Statement::TryCatch(try_body, exception_name, catch_body) => {
                self.visit_trycatch_statement(try_body, exception_name, catch_body);
            },
            Statement::Throw(exception) => {
                self.visit_throw_statement(exception);
            },
            Statement::Const(name, value) => {
                self.visit_const_statement(name, value);
            },
            Statement::CommaExpression(comma_expr) => {
                self.visit_comma_expr_statement(comma_expr);
            },
        }
    }
    fn walk_if_statement(&mut self, condition: &AstNode<CommaExpression>, if_block: &AstNode<Statement>, else_block: &Option<AstNode<Statement>>) {
        self.visit_comma_expr(condition);
        self.visit_statement(if_block);
        if else_block.is_some() {
            self.visit_statement(else_block.as_ref().unwrap());
        }
    }
    fn walk_while_statement(&mut self, condition: &AstNode<CommaExpression>, loop_body: &AstNode<Statement>) {
        self.visit_comma_expr(condition);
        self.visit_statement(loop_body);
    }
    fn walk_do_while_statement(&mut self, loop_body: &AstNode<Statement>, condition: &AstNode<CommaExpression>) {
        self.visit_statement(loop_body);
        self.visit_comma_expr(condition);
    }
    fn walk_for_statement(&mut self, init: &Option<AstNode<ForInit>>, condition: &Option<AstNode<CommaExpression>>, post: &Option<AstNode<CommaExpression>>, body: &AstNode<Statement>) {
        if let Some(init) = init {
            self.visit_for_init(init);
        }

        if let Some(condition) = condition {
            self.visit_comma_expr(condition);
        }

        if let Some(post) = post {
            self.visit_comma_expr(post);
        }

        self.visit_statement(body);
    }
    fn walk_foreach_statement(&mut self, value: &AstNode<Identifier>, key: &Option<AstNode<Identifier>>, container: &AstNode<Expression>, body: &AstNode<Statement>) {
        self.visit_identifier(value);
        
        if let Some(key) = key {
            self.visit_identifier(key);
        }

        self.visit_expression(container);
        self.visit_statement(body);
    }
    fn walk_switch_statement(&mut self, value: &AstNode<CommaExpression>, cases: &Vec<AstNode<SwitchCase>>, default: &Option<AstNode<Statements>>) {
        self.visit_comma_expr(value);
        
        for case in cases {
            self.visit_expression(&case.value.condition);
            self.visit_statements(&case.value.body);
        }

        if let Some(default) = default {
            self.visit_statements(default);
        }
    }
    fn walk_local_declare_statement(&mut self, local_declare: &AstNode<LocalDeclare>) {
        self.visit_local_declare(local_declare);
    }
    fn walk_return_statement(&mut self, value: &Option<AstNode<CommaExpression>>) {
        if let Some(value) = value {
            self.visit_comma_expr(value);
        }
    }
    fn walk_yield_statement(&mut self, value: &Option<AstNode<CommaExpression>>) {
        if let Some(value) = value {
            self.visit_comma_expr(value);
        }
    }
    fn walk_break_statement(&mut self) {}
    fn walk_continue_statement(&mut self) {}
    fn walk_function_statement(&mut self, func_identifier: &AstNode<FunctionIdentifier>, env: &Option<AstNode<Expression>>, params: &AstNode<FunctionParams>, body: &AstNode<Statement>) {
        self.visit_func_identifier(func_identifier);

        if let Some(env) = env {
            self.visit_expression(env);
        }

        self.visit_func_params(params);
        self.visit_statement(body);
    }
    fn walk_class_statement(&mut self, name: &AstNode<PrefixedExpression>, class_expr: &AstNode<ClassExpression>) {
        self.visit_prefixed_exp(name);

        self.visit_class_expr(class_expr);
    }
    fn walk_enum_statement(&mut self, name: &AstNode<Identifier>, values: &AstNode<EnumValues>) {
        self.visit_identifier(name);

        for entry in &values.value.values {
            self.visit_identifier(&entry.value.key);

            if let Some(value) = &entry.value.value {
                self.visit_scalar(value);
            }
        }
    }
    fn walk_statement_block(&mut self, statements: &AstNode<Statements>) {
        self.visit_statements(statements);
    }
    fn walk_trycatch_statement(&mut self, try_body: &AstNode<Statement>, exception_name: &AstNode<Identifier>, catch_body: &AstNode<Statement>) {
        self.visit_statement(try_body);
        self.visit_identifier(exception_name);
        self.visit_statement(catch_body);
    }
    fn walk_throw_statement(&mut self, exception: &AstNode<CommaExpression>) {
        self.visit_comma_expr(exception);
    }
    fn walk_const_statement(&mut self, name: &AstNode<Identifier>, value: &AstNode<Scalar>) {
        self.visit_identifier(name);
        self.visit_scalar(value);
    }
    fn walk_comma_expr_statement(&mut self, comma_expr: &AstNode<CommaExpression>) {
        self.visit_comma_expr(comma_expr);
    }
    fn walk_local_declare(&mut self, local_declare: &AstNode<LocalDeclare>) {
        match local_declare.value.as_ref() {
            LocalDeclare::Function(identifier, bind_env, params, body) => {
                self.visit_identifier(identifier);
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
                self.visit_statement(body);
            }
            LocalDeclare::Assign(assign_exprs) => {
                for assign_expr in assign_exprs {
                    self.visit_assign_expr(assign_expr);
                }
            }
        }
    }
    fn walk_assign_expr(&mut self, assign_expr: &AstNode<AssignExpression>) {
        self.visit_identifier(&assign_expr.value.identifier);
        if let Some(expression) = &assign_expr.value.value {
            self.visit_expression(expression);
        }
    }
    fn walk_for_init(&mut self, for_init: &AstNode<ForInit>) {
        match for_init.value.as_ref() {
            ForInit::LocalDeclare(local_declare) => {
                self.visit_local_declare(local_declare);
            }
            ForInit::CommaExpression(comma_expr) => {
                self.visit_comma_expr(comma_expr);
            }
        }
    }
    fn walk_unary_op(&mut self, unary_op: &AstNode<UnaryOp>) {
        self.visit_prefixed_exp(&unary_op.value.expression);
    }
    fn walk_func_identifier(&mut self, func_identifier: &AstNode<FunctionIdentifier>) {
        for identifier in &func_identifier.value.identifiers {
            self.visit_identifier(identifier);
        }
    }
    fn walk_func_params(&mut self, params: &AstNode<FunctionParams>) {
        for param in &params.value.params {
            self.visit_func_param(param);
        }
    }
    fn walk_func_param(&mut self, param: &AstNode<FunctionParam>) {
        match param.value.as_ref() {
            FunctionParam::Normal(identifier) => self.visit_identifier(identifier),
            FunctionParam::Default(identifier, default_val) => {
                self.visit_identifier(identifier);
                self.visit_expression(default_val);
            },
            FunctionParam::VarParams => {},
        }
    }
    fn walk_post_call_init(&mut self, post_call_init: &AstNode<PostCallInitialize>) {
        for entry in &post_call_init.value.entries {
            match entry.value.as_ref() {
                PostCallInitializeEntry::ArrayStyle(key, value) => {
                    self.visit_comma_expr(key);
                    self.visit_expression(value);
                },
                PostCallInitializeEntry::TableStyle(key, value) => {
                    self.visit_identifier(key);
                    self.visit_expression(value);
                },
            }
        }
    }
    fn walk_table(&mut self, table: &AstNode<Table>) {
        for entry in &table.value.entries {
            self.visit_table_entry(entry);
        }
    }
    fn walk_table_entry(&mut self, entry: &AstNode<TableEntry>) {
        match entry.value.as_ref() {
            TableEntry::Function(name, params, body) => {
                self.visit_identifier(name);
                self.visit_func_params(params);
                self.visit_statement(body);
            },
            TableEntry::Constructor(params, body) => {
                self.visit_func_params(params);
                self.visit_statement(body);
            },
            TableEntry::DynamicAssign(key, value) => {
                self.visit_comma_expr(key);
                self.visit_expression(value);
            },
            TableEntry::JsonStyle(_key, value) => {
                self.visit_expression(value);
            },
            TableEntry::Simple(key, value) => {
                self.visit_identifier(key);
                self.visit_expression(value);
            },
            TableEntry::Attributes(table) => {
                self.visit_table(table);
            },
        }
    }
    fn walk_expression(&mut self, expression: &AstNode<Expression>) {
        self.visit_logical_or_exp(&expression.value.logical_or);

        if expression.value.expr_type.is_none() {
            return;
        }

        match expression.value.expr_type.as_ref().unwrap().value.as_ref() {
            ExpressionType::Newslot(value) => self.visit_expression(value),
            ExpressionType::Assign(value) => self.visit_expression(value),
            ExpressionType::MinusEqual(value) => self.visit_expression(value),
            ExpressionType::PlusEqual(value) => self.visit_expression(value),
            ExpressionType::MultiplyEqual(value) => self.visit_expression(value),
            ExpressionType::DivideEqual(value) => self.visit_expression(value),
            ExpressionType::Ternary(true_case, false_case) => {
                self.visit_expression(true_case);
                self.visit_expression(false_case);
            },
        }
    }
    fn walk_class_expr(&mut self, class_expression: &AstNode<ClassExpression>) {
        if let Some(base_class) = &class_expression.value.base_class {
            self.visit_expression(base_class);
        }

        if let Some(attributes) = &class_expression.value.attributes {
            self.visit_table(attributes);
        }

        self.visit_table(&class_expression.value.body);
    }
    fn walk_comma_expr(&mut self, comma_expression: &AstNode<CommaExpression>) {
        for expression in &comma_expression.value.expressions {
            self.visit_expression(expression);
        }
    }
    fn walk_logical_or_exp(&mut self, logical_or: &AstNode<LogicalOrExpression>) {
        self.visit_logical_and_exp(&logical_or.value.left);

        for expression in &logical_or.value.right {
            self.visit_logical_or_exp(expression);
        }
    }
    fn walk_logical_and_exp(&mut self, logical_and: &AstNode<LogicalAndExpression>) {
        self.visit_bitwise_or_exp(&logical_and.value.left);
        
        for expression in &logical_and.value.right {
            self.visit_logical_and_exp(expression);
        }
    }
    fn walk_bitwise_or_exp(&mut self, bitwise_or: &AstNode<BitwiseOrExpression>) {
        self.visit_bitwise_xor_exp(&bitwise_or.value.left);

        for expression in &bitwise_or.value.right {
            self.visit_bitwise_xor_exp(expression);
        }
    }
    fn walk_bitwise_xor_exp(&mut self, bitwise_xor: &AstNode<BitwiseXorExpression>) {
        self.visit_bitwise_and_exp(&bitwise_xor.value.left);

        for expression in &bitwise_xor.value.right {
            self.visit_bitwise_and_exp(expression);
        }
    }
    fn walk_bitwise_and_exp(&mut self, bitwise_and: &AstNode<BitwiseAndExpression>) {
        self.visit_equal_exp(&bitwise_and.value.left);

        for expression in &bitwise_and.value.right {
            self.visit_equal_exp(expression);
        }
    }
    fn walk_equal_exp(&mut self, equal: &AstNode<EqualExpression>) {
        self.visit_compare_exp(&equal.value.left);

        for expression in &equal.value.slices {
            self.visit_compare_exp(&expression.value.right);
        }
    }
    fn walk_compare_exp(&mut self, compare: &AstNode<CompareExpression>) {
        self.visit_shift_exp(&compare.value.left);

        for expression in &compare.value.slices {
            self.visit_shift_exp(&expression.value.right);
        }
    }
    fn walk_shift_exp(&mut self, shift: &AstNode<ShiftExpression>) {
        self.visit_plus_exp(&shift.value.left);

        for expression in &shift.value.slices {
            self.visit_plus_exp(&expression.value.right);
        }
    }
    fn walk_plus_exp(&mut self, plus: &AstNode<PlusExpression>) {
        self.visit_multiply_exp(&plus.value.left);

        for expression in &plus.value.slices {
            self.visit_multiply_exp(&expression.value.right);
        }
    }
    fn walk_multiply_exp(&mut self, multiply: &AstNode<MultiplyExpression>) {
        self.visit_prefixed_exp(&multiply.value.left);

        for expression in &multiply.value.slices {
            self.visit_prefixed_exp(&expression.value.right);
        }
    }
    fn walk_prefixed_exp(&mut self, prefixed: &AstNode<PrefixedExpression>) {
        self.visit_factor(&prefixed.value.factor);

        for expression in &prefixed.value.expr_types {
            match expression.value.as_ref() {
                PrefixedExpressionType::DotAccess(identifier) => self.visit_identifier(identifier),
                PrefixedExpressionType::ArrayStyleAccess(expression) => self.visit_expression(expression),
                PrefixedExpressionType::PostIncrement => {},
                PrefixedExpressionType::PostDecrement => {},
                PrefixedExpressionType::FunctionCall(call_args) => {
                    for arg in &call_args.value.args {
                        self.visit_expression(arg);
                    }

                    if let Some(post_call_init) = &call_args.value.post_call_init {
                        self.visit_post_call_init(post_call_init);
                    }
                },
            }
        }
    }
    fn walk_scalar(&mut self, _scalar: &AstNode<Scalar>) {}
    fn walk_factor(&mut self, _factor: &AstNode<Factor>) {}
    fn walk_identifier(&mut self, _identifier: &AstNode<Identifier>) {}
}