use crate::ast::*;
pub mod pretty_printer;

pub trait SimpleVisitor {
    fn visit_script(&self, script: Script) {
        self.visit_statements(script.statements);
    }
    fn visit_statements(&self, statements: Statements) {
        for statement in statements.statements {
            self.visit_statement(statement);
        }
    }
    fn visit_statement(&self, statement: Statement) {
        self.default_visit_statement(statement);
    }
    fn default_visit_statement(&self, statement: Statement) {
        match statement {
            Statement::If(condition, if_block, else_block) => {
                self.visit_comma_expr(condition);
                self.visit_statement(*if_block);
                if else_block.is_some() {
                    self.visit_statement(*else_block.unwrap());
                }
            }
            Statement::While(condition, loop_body) => {
                self.visit_comma_expr(condition);
                self.visit_statement(*loop_body);
            }
            Statement::DoWhile(loop_body, condition) => {
                self.visit_statement(*loop_body);
                self.visit_comma_expr(condition);
            }
            Statement::For(init, condition, post, body) => {
                if let Some(init) = init {
                    self.visit_for_init(init);
                }

                if let Some(condition) = condition {
                    self.visit_comma_expr(condition);
                }

                if let Some(post) = post {
                    self.visit_comma_expr(post);
                }

                self.visit_statement(*body);
            },
            Statement::ForEach(value, key, container, body) => {
                self.visit_identifier(value);
                
                if let Some(key) = key {
                    self.visit_identifier(key);
                }

                self.visit_expression(container);
                self.visit_statement(*body);
            },
            Statement::Switch(value, cases, default) => {
                self.visit_comma_expr(value);
                
                for case in cases {
                    self.visit_expression(case.condition);
                    self.visit_statements(case.body);
                }

                if let Some(default) = default {
                    self.visit_statements(default);
                }
            },
            Statement::LocalDeclare(local_declare) => {
                self.visit_local_declare(local_declare);
            },
            Statement::Return(value) => {
                if let Some(value) = value {
                    self.visit_comma_expr(value);
                }
            },
            Statement::Yield(value) => {
                if let Some(value) = value {
                    self.visit_comma_expr(value);
                }
            },
            Statement::Break => {},
            Statement::Continue => {},
            Statement::Function(func_identifier, env, params, body) => {
                self.visit_func_identifier(func_identifier);

                if let Some(env) = env {
                    self.visit_expression(env);
                }

                self.visit_func_params(params);
                self.visit_statement(*body);
            },
            Statement::Class(name, class_expr) => {
                self.visit_prefixed_exp(name);

                self.visit_class_expr(class_expr);
            },
            Statement::Enum(name, values) => {
                self.visit_identifier(name);

                for entry in values.values {
                    self.visit_identifier(entry.key);

                    if let Some(value) = entry.value {
                        self.visit_scalar(value);
                    }
                }
            },
            Statement::StatementBlock(statements) => {
                self.visit_statements(statements);
            },
            Statement::TryCatch(try_body, exception_name, catch_body) => {
                self.visit_statement(*try_body);
                self.visit_identifier(exception_name);
                self.visit_statement(*catch_body);
            },
            Statement::Throw(exception) => {
                self.visit_comma_expr(exception);
            },
            Statement::Const(name, value) => {
                self.visit_identifier(name);
                self.visit_scalar(value);
            },
            Statement::CommaExpression(comma_expr) => {
                self.visit_comma_expr(comma_expr);
            },
        }
    }
    fn visit_local_declare(&self, local_declare: LocalDeclare) {
        match local_declare {
            LocalDeclare::Function(identifier, bind_env, params, body) => {
                self.visit_identifier(identifier);
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
                self.visit_statement(*body);
            }
            LocalDeclare::Assign(assign_exprs) => {
                for assign_expr in assign_exprs {
                    self.visit_assign_expr(assign_expr);
                }
            }
        }
    }
    fn visit_assign_expr(&self, assign_expr: AssignExpression) {
        self.visit_identifier(assign_expr.identifier);
        if let Some(expression) = assign_expr.value {
            self.visit_expression(expression);
        }
    }
    fn visit_for_init(&self, for_init: ForInit) {
        match for_init {
            ForInit::LocalDeclare(local_declare) => {
                self.visit_local_declare(local_declare);
            }
            ForInit::CommaExpression(comma_expr) => {
                self.visit_comma_expr(comma_expr);
            }
        }
    }
    fn visit_unary_op(&self, unary_op: UnaryOp) {
        self.visit_prefixed_exp(unary_op.expression);
    }
    fn visit_func_identifier(&self, func_identifier: FunctionIdentifier) {
        for identifier in func_identifier.identifiers {
            self.visit_identifier(identifier);
        }
    }
    fn visit_func_params(&self, params: FunctionParams) {
        for param in params.params {
            self.visit_func_param(param);
        }
    }
    fn visit_func_param(&self, param: FunctionParam) {
        match param {
            FunctionParam::Normal(identifier) => self.visit_identifier(identifier),
            FunctionParam::Default(identifier, default_val) => {
                self.visit_identifier(identifier);
                self.visit_expression(default_val);
            },
            FunctionParam::VarParams => {},
        }
    }
    fn visit_post_call_init(&self, post_call_init: PostCallInitialize) {
        for entry in post_call_init.entries {
            match entry {
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
    fn visit_table(&self, table: Table) {
        for entry in table.entries {
            self.visit_table_entry(entry);
        }
    }
    fn visit_table_entry(&self, entry: TableEntry) {
        match entry {
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
    fn visit_expression(&self, expression: Expression) {
        self.visit_logical_or_exp(*expression.logical_or);

        if expression.expr_type.is_none() {
            return;
        }

        match expression.expr_type.unwrap() {
            ExpressionType::Newslot(value) => self.visit_expression(*value),
            ExpressionType::Assign(value) => self.visit_expression(*value),
            ExpressionType::MinusEqual(value) => self.visit_expression(*value),
            ExpressionType::PlusEqual(value) => self.visit_expression(*value),
            ExpressionType::MultiplyEqual(value) => self.visit_expression(*value),
            ExpressionType::DivideEqual(value) => self.visit_expression(*value),
            ExpressionType::Ternary(true_case, false_case) => {
                self.visit_expression(*true_case);
                self.visit_expression(*false_case);
            },
        }
    }
    fn visit_class_expr(&self, class_expression: ClassExpression) {
        if let Some(base_class) = class_expression.base_class {
            self.visit_expression(base_class);
        }

        if let Some(attributes) = class_expression.attributes {
            self.visit_table(attributes);
        }

        self.visit_table(class_expression.body);
    }
    fn visit_comma_expr(&self, comma_expression: CommaExpression) {
        for expression in comma_expression.expressions {
            self.visit_expression(expression);
        }
    }
    fn visit_logical_or_exp(&self, logical_or: LogicalOrExpression) {
        self.visit_logical_and_exp(logical_or.left);

        for expression in logical_or.right {
            self.visit_logical_or_exp(*expression);
        }
    }
    fn visit_logical_and_exp(&self, logical_and: LogicalAndExpression) {
        self.visit_bitwise_or_exp(logical_and.left);
        
        for expression in logical_and.right {
            self.visit_logical_and_exp(*expression);
        }
    }
    fn visit_bitwise_or_exp(&self, bitwise_or: BitwiseOrExpression) {
        self.visit_bitwise_xor_exp(bitwise_or.left);

        for expression in bitwise_or.right {
            self.visit_bitwise_xor_exp(expression);
        }
    }
    fn visit_bitwise_xor_exp(&self, bitwise_xor: BitwiseXorExpression) {
        self.visit_bitwise_and_exp(bitwise_xor.left);

        for expression in bitwise_xor.right {
            self.visit_bitwise_and_exp(expression);
        }
    }
    fn visit_bitwise_and_exp(&self, bitwise_and: BitwiseAndExpression) {
        self.visit_equal_exp(bitwise_and.left);

        for expression in bitwise_and.right {
            self.visit_equal_exp(expression);
        }
    }
    fn visit_equal_exp(&self, equal: EqualExpression) {
        self.visit_compare_exp(equal.left);

        for expression in equal.slices {
            self.visit_compare_exp(expression.right);
        }
    }
    fn visit_compare_exp(&self, compare: CompareExpression) {
        self.visit_shift_exp(compare.left);

        for expression in compare.slices {
            self.visit_shift_exp(expression.right);
        }
    }
    fn visit_shift_exp(&self, shift: ShiftExpression) {
        self.visit_plus_exp(shift.left);

        for expression in shift.slices {
            self.visit_plus_exp(expression.right);
        }
    }
    fn visit_plus_exp(&self, plus: PlusExpression) {
        self.visit_multiply_exp(plus.left);

        for expression in plus.slices {
            self.visit_multiply_exp(expression.right);
        }
    }
    fn visit_multiply_exp(&self, multiply: MultiplyExpression) {
        self.visit_prefixed_exp(multiply.left);

        for expression in multiply.slices {
            self.visit_prefixed_exp(expression.right);
        }
    }
    fn visit_prefixed_exp(&self, prefixed: PrefixedExpression) {
        self.visit_factor(prefixed.factor);

        for expression in prefixed.expr_types {
            match expression {
                PrefixedExpressionType::DotAccess(identifier) => self.visit_identifier(identifier),
                PrefixedExpressionType::ArrayStyleAccess(expression) => self.visit_expression(expression),
                PrefixedExpressionType::PostIncrement => {},
                PrefixedExpressionType::PostDecrement => {},
                PrefixedExpressionType::FunctionCall(call_args) => {
                    for arg in call_args.args {
                        self.visit_expression(arg);
                    }

                    if let Some(post_call_init) = call_args.post_call_init {
                        self.visit_post_call_init(post_call_init);
                    }
                },
            }
        }
    }
    fn visit_scalar(&self, _scalar: Scalar) {}
    fn visit_factor(&self, _factor: Factor) {}
    fn visit_identifier(&self, _identifier: Identifier) {}
}

pub trait SimpleVisitorMut {
    fn visit_script(&mut self, script: Script) {
        self.visit_statements(script.statements);
    }
    fn visit_statements(&mut self, statements: Statements) {
        for statement in statements.statements {
            self.visit_statement(statement);
        }
    }
    fn visit_statement(&mut self, statement: Statement) {
        self.default_visit_statement(statement);
    }
    fn default_visit_statement(&mut self, statement: Statement) {
        match statement {
            Statement::If(condition, if_block, else_block) => {
                self.visit_comma_expr(condition);
                self.visit_statement(*if_block);
                if else_block.is_some() {
                    self.visit_statement(*else_block.unwrap());
                }
            }
            Statement::While(condition, loop_body) => {
                self.visit_comma_expr(condition);
                self.visit_statement(*loop_body);
            }
            Statement::DoWhile(loop_body, condition) => {
                self.visit_statement(*loop_body);
                self.visit_comma_expr(condition);
            }
            Statement::For(init, condition, post, body) => {
                if let Some(init) = init {
                    self.visit_for_init(init);
                }

                if let Some(condition) = condition {
                    self.visit_comma_expr(condition);
                }

                if let Some(post) = post {
                    self.visit_comma_expr(post);
                }

                self.visit_statement(*body);
            },
            Statement::ForEach(value, key, container, body) => {
                self.visit_identifier(value);
                
                if let Some(key) = key {
                    self.visit_identifier(key);
                }

                self.visit_expression(container);
                self.visit_statement(*body);
            },
            Statement::Switch(value, cases, default) => {
                self.visit_comma_expr(value);
                
                for case in cases {
                    self.visit_expression(case.condition);
                    self.visit_statements(case.body);
                }

                if let Some(default) = default {
                    self.visit_statements(default);
                }
            },
            Statement::LocalDeclare(local_declare) => {
                self.visit_local_declare(local_declare);
            },
            Statement::Return(value) => {
                if let Some(value) = value {
                    self.visit_comma_expr(value);
                }
            },
            Statement::Yield(value) => {
                if let Some(value) = value {
                    self.visit_comma_expr(value);
                }
            },
            Statement::Break => {},
            Statement::Continue => {},
            Statement::Function(func_identifier, env, params, body) => {
                self.visit_func_identifier(func_identifier);

                if let Some(env) = env {
                    self.visit_expression(env);
                }

                self.visit_func_params(params);
                self.visit_statement(*body);
            },
            Statement::Class(name, class_expr) => {
                self.visit_prefixed_exp(name);

                self.visit_class_expr(class_expr);
            },
            Statement::Enum(name, values) => {
                self.visit_identifier(name);

                for entry in values.values {
                    self.visit_identifier(entry.key);

                    if let Some(value) = entry.value {
                        self.visit_scalar(value);
                    }
                }
            },
            Statement::StatementBlock(statements) => {
                self.visit_statements(statements);
            },
            Statement::TryCatch(try_body, exception_name, catch_body) => {
                self.visit_statement(*try_body);
                self.visit_identifier(exception_name);
                self.visit_statement(*catch_body);
            },
            Statement::Throw(exception) => {
                self.visit_comma_expr(exception);
            },
            Statement::Const(name, value) => {
                self.visit_identifier(name);
                self.visit_scalar(value);
            },
            Statement::CommaExpression(comma_expr) => {
                self.visit_comma_expr(comma_expr);
            },
        }
    }
    fn visit_local_declare(&mut self, local_declare: LocalDeclare) {
        match local_declare {
            LocalDeclare::Function(identifier, bind_env, params, body) => {
                self.visit_identifier(identifier);
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
                self.visit_statement(*body);
            }
            LocalDeclare::Assign(assign_exprs) => {
                for assign_expr in assign_exprs {
                    self.visit_assign_expr(assign_expr);
                }
            }
        }
    }
    fn visit_assign_expr(&mut self, assign_expr: AssignExpression) {
        self.visit_identifier(assign_expr.identifier);
        if let Some(expression) = assign_expr.value {
            self.visit_expression(expression);
        }
    }
    fn visit_for_init(&mut self, for_init: ForInit) {
        match for_init {
            ForInit::LocalDeclare(local_declare) => {
                self.visit_local_declare(local_declare);
            }
            ForInit::CommaExpression(comma_expr) => {
                self.visit_comma_expr(comma_expr);
            }
        }
    }
    fn visit_unary_op(&mut self, unary_op: UnaryOp) {
        self.visit_prefixed_exp(unary_op.expression);
    }
    fn visit_func_identifier(&mut self, func_identifier: FunctionIdentifier) {
        for identifier in func_identifier.identifiers {
            self.visit_identifier(identifier);
        }
    }
    fn visit_func_params(&mut self, params: FunctionParams) {
        for param in params.params {
            self.visit_func_param(param);
        }
    }
    fn visit_func_param(&mut self, param: FunctionParam) {
        match param {
            FunctionParam::Normal(identifier) => self.visit_identifier(identifier),
            FunctionParam::Default(identifier, default_val) => {
                self.visit_identifier(identifier);
                self.visit_expression(default_val);
            },
            FunctionParam::VarParams => {},
        }
    }
    fn visit_post_call_init(&mut self, post_call_init: PostCallInitialize) {
        for entry in post_call_init.entries {
            match entry {
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
    fn visit_table(&mut self, table: Table) {
        for entry in table.entries {
            self.visit_table_entry(entry);
        }
    }
    fn visit_table_entry(&mut self, entry: TableEntry) {
        match entry {
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
    fn visit_expression(&mut self, expression: Expression) {
        self.visit_logical_or_exp(*expression.logical_or);

        if expression.expr_type.is_none() {
            return;
        }

        match expression.expr_type.unwrap() {
            ExpressionType::Newslot(value) => self.visit_expression(*value),
            ExpressionType::Assign(value) => self.visit_expression(*value),
            ExpressionType::MinusEqual(value) => self.visit_expression(*value),
            ExpressionType::PlusEqual(value) => self.visit_expression(*value),
            ExpressionType::MultiplyEqual(value) => self.visit_expression(*value),
            ExpressionType::DivideEqual(value) => self.visit_expression(*value),
            ExpressionType::Ternary(true_case, false_case) => {
                self.visit_expression(*true_case);
                self.visit_expression(*false_case);
            },
        }
    }
    fn visit_class_expr(&mut self, class_expression: ClassExpression) {
        if let Some(base_class) = class_expression.base_class {
            self.visit_expression(base_class);
        }

        if let Some(attributes) = class_expression.attributes {
            self.visit_table(attributes);
        }

        self.visit_table(class_expression.body);
    }
    fn visit_comma_expr(&mut self, comma_expression: CommaExpression) {
        for expression in comma_expression.expressions {
            self.visit_expression(expression);
        }
    }
    fn visit_logical_or_exp(&mut self, logical_or: LogicalOrExpression) {
        self.visit_logical_and_exp(logical_or.left);

        for expression in logical_or.right {
            self.visit_logical_or_exp(*expression);
        }
    }
    fn visit_logical_and_exp(&mut self, logical_and: LogicalAndExpression) {
        self.visit_bitwise_or_exp(logical_and.left);
        
        for expression in logical_and.right {
            self.visit_logical_and_exp(*expression);
        }
    }
    fn visit_bitwise_or_exp(&mut self, bitwise_or: BitwiseOrExpression) {
        self.visit_bitwise_xor_exp(bitwise_or.left);

        for expression in bitwise_or.right {
            self.visit_bitwise_xor_exp(expression);
        }
    }
    fn visit_bitwise_xor_exp(&mut self, bitwise_xor: BitwiseXorExpression) {
        self.visit_bitwise_and_exp(bitwise_xor.left);

        for expression in bitwise_xor.right {
            self.visit_bitwise_and_exp(expression);
        }
    }
    fn visit_bitwise_and_exp(&mut self, bitwise_and: BitwiseAndExpression) {
        self.visit_equal_exp(bitwise_and.left);

        for expression in bitwise_and.right {
            self.visit_equal_exp(expression);
        }
    }
    fn visit_equal_exp(&mut self, equal: EqualExpression) {
        self.visit_compare_exp(equal.left);

        for expression in equal.slices {
            self.visit_compare_exp(expression.right);
        }
    }
    fn visit_compare_exp(&mut self, compare: CompareExpression) {
        self.visit_shift_exp(compare.left);

        for expression in compare.slices {
            self.visit_shift_exp(expression.right);
        }
    }
    fn visit_shift_exp(&mut self, shift: ShiftExpression) {
        self.visit_plus_exp(shift.left);

        for expression in shift.slices {
            self.visit_plus_exp(expression.right);
        }
    }
    fn visit_plus_exp(&mut self, plus: PlusExpression) {
        self.visit_multiply_exp(plus.left);

        for expression in plus.slices {
            self.visit_multiply_exp(expression.right);
        }
    }
    fn visit_multiply_exp(&mut self, multiply: MultiplyExpression) {
        self.visit_prefixed_exp(multiply.left);

        for expression in multiply.slices {
            self.visit_prefixed_exp(expression.right);
        }
    }
    fn visit_prefixed_exp(&mut self, prefixed: PrefixedExpression) {
        self.visit_factor(prefixed.factor);

        for expression in prefixed.expr_types {
            match expression {
                PrefixedExpressionType::DotAccess(identifier) => self.visit_identifier(identifier),
                PrefixedExpressionType::ArrayStyleAccess(expression) => self.visit_expression(expression),
                PrefixedExpressionType::PostIncrement => {},
                PrefixedExpressionType::PostDecrement => {},
                PrefixedExpressionType::FunctionCall(call_args) => {
                    for arg in call_args.args {
                        self.visit_expression(arg);
                    }

                    if let Some(post_call_init) = call_args.post_call_init {
                        self.visit_post_call_init(post_call_init);
                    }
                },
            }
        }
    }
    fn visit_scalar(&mut self, _scalar: Scalar) {}
    fn visit_factor(&mut self, _factor: Factor) {}
    fn visit_identifier(&mut self, _identifier: Identifier) {}
}