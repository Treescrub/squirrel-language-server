use crate::ast::*;

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
            _ => {todo!()}
        }
    }
    fn visit_local_declare(&self, local_declare: LocalDeclare) {
        match local_declare {
            LocalDeclare::Function(identifier, bind_env, params) => {
                self.visit_identifier(identifier);
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
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
                PrefixedExpressionType::FunctionCall(_) => todo!(),
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
            _ => {todo!()}
        }
    }
    fn visit_local_declare(&mut self, local_declare: LocalDeclare) {
        match local_declare {
            LocalDeclare::Function(identifier, bind_env, params) => {
                self.visit_identifier(identifier);
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
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
                PrefixedExpressionType::FunctionCall(_) => todo!(),
            }
        }
    }
    fn visit_scalar(&mut self, _scalar: Scalar) {}
    fn visit_factor(&mut self, _factor: Factor) {}
    fn visit_identifier(&mut self, _identifier: Identifier) {}
}

pub struct PrettyPrinter {
    pub text: String,
    indentation: u32,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        Self {
            text: String::new(),
            indentation: 0,
        }
    }

    fn push_level(&mut self) {
        self.indentation += 1;
    }

    fn pop_level(&mut self) {
        self.indentation -= 1;
    }

    fn add_indents(&mut self) {
        for _ in 0..self.indentation {
            self.text.push_str("  ");
        }
    }

    fn print(&mut self, text: &str) {
        self.add_indents();
        self.text.push_str(text);
        self.text.push('\n');
    }
}

impl SimpleVisitorMut for PrettyPrinter {
    fn visit_script(&mut self, script: Script) {
        self.print(&format!("SCRIPT: {} statement(s)", script.statements.statements.len()));

        self.push_level();
        self.visit_statements(script.statements);
        self.pop_level();
    }

    fn visit_statements(&mut self, statements: Statements) {
        self.print("STATEMENTS");

        for statement in statements.statements {
            self.push_level();
            self.visit_statement(statement);
            self.pop_level();
        }
    }

    fn visit_statement(&mut self, statement: Statement) {
        self.print("STATEMENT");

        self.push_level();
        match statement {
            Statement::Break => {
                self.print("BREAK");
            }
            Statement::Continue => {
                self.print("CONTINUE");
            }
            Statement::Const(id, _scalar) => {
                self.print("CONST");

                self.push_level();
                self.visit_identifier(id);
                self.pop_level();
            }
            Statement::StatementBlock(statements) => {
                self.print("STATEMENT BLOCK");

                self.push_level();
                self.visit_statements(statements);
                self.pop_level();
            }
            Statement::If(condition, if_block, else_block) => {
                self.print("IF");

                self.push_level();
                self.visit_comma_expr(condition);
                self.visit_statement(*if_block);
                if else_block.is_some() {
                    self.visit_statement(*else_block.unwrap());
                }
                self.pop_level();
            }
            Statement::CommaExpression(comma_expression) => {
                self.print("COMMA EXPRESSION");

                self.push_level();
                for expression in comma_expression.expressions {
                    self.visit_expression(expression);
                }
                self.pop_level();
            }
            Statement::Class(name, body) => {
                self.print("CLASS");

                self.push_level();
                self.visit_prefixed_exp(name);
                self.visit_class_expr(body);
                self.pop_level();
            }
            Statement::Function(identifier, bind_env, params, body) => {
                self.print("FUNCTION");

                self.push_level();
                for identifier in identifier.identifiers {
                    self.visit_identifier(identifier);
                }
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
                self.visit_statement(*body);
                self.pop_level();
            }
            Statement::While(condition, body) => {
                self.print("WHILE");

                self.push_level();
                self.visit_comma_expr(condition);
                self.visit_statement(*body);
                self.pop_level();
            },
            Statement::DoWhile(body, condition) => {
                self.print("DO WHILE");

                self.push_level();
                self.visit_statement(*body);
                self.visit_comma_expr(condition);
                self.pop_level();
            },
            Statement::For(init, condition, post, body) => {
                self.print("FOR");

                self.push_level();
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
                self.pop_level();
            },
            Statement::ForEach(value, key, container, body) => {
                self.print("FOR EACH");

                self.push_level();
                self.visit_identifier(value);
                if let Some(key) = key {
                    self.visit_identifier(key);
                }
                self.visit_expression(container);
                self.visit_statement(*body);
                self.pop_level();
            },
            Statement::Switch(value, cases, default) => {
                self.print("SWITCH");

                self.push_level();
                self.visit_comma_expr(value);
                for case in cases {
                    self.visit_expression(case.condition);
                    self.visit_statements(case.body);
                }
                if let Some(default) = default {
                    self.visit_statements(default);
                }
                self.pop_level();
            },
            Statement::LocalDeclare(local_declare) => {
                self.print("LOCAL DECLARE");

                self.push_level();
                self.visit_local_declare(local_declare);
                self.pop_level();
            },
            Statement::Return(value) => {
                self.print("RETURN");

                self.push_level();
                if let Some(value) = value {
                    self.visit_comma_expr(value);
                }
                self.pop_level();
            },
            Statement::Yield(value) => {
                self.print("YIELD");

                self.push_level();
                if let Some(value) = value {
                    self.visit_comma_expr(value);
                }
                self.pop_level();
            },
            Statement::Enum(identifier, values) => {
                self.print("ENUM");

                self.push_level();
                self.visit_identifier(identifier);
                for entry in values.values {
                    self.visit_identifier(entry.key);
                    if let Some(value) = entry.value {
                        self.visit_scalar(value);
                    }
                }
                self.pop_level();
            },
            Statement::TryCatch(try_body, exception_id, catch_body) => {
                self.print("TRY CATCH");

                self.push_level();
                self.visit_statement(*try_body);
                self.visit_identifier(exception_id);
                self.visit_statement(*catch_body);
                self.pop_level();
            },
            Statement::Throw(value) => {
                self.print("THROW");

                self.push_level();
                self.visit_comma_expr(value);
                self.pop_level();
            },
        }

        self.pop_level();
    }

    fn visit_func_params(&mut self, params: FunctionParams) {
        self.print("FUNC PARAMS");

        self.push_level();
        for param in params.params {
            self.visit_func_param(param);
        }
        self.pop_level();
    }

    fn visit_func_param(&mut self, param: FunctionParam) {
        self.print("FUNC PARAM");
        
        self.push_level();
        match param {
            FunctionParam::Normal(identifier) => {
                self.print("NORMAL");

                self.push_level();
                self.visit_identifier(identifier);
                self.pop_level();
            },
            FunctionParam::Default(identifier, default_val) => {
                self.print("DEFAULT");
                
                self.push_level();
                self.visit_identifier(identifier);
                self.visit_expression(default_val);
                self.pop_level();
            },
            FunctionParam::VarParams => {
                self.print("VARARGS");
            },
        }
        self.pop_level();
    }

    fn visit_local_declare(&mut self, local_declare: LocalDeclare) {
        match local_declare {
            LocalDeclare::Function(identifier, bind_env, params) => {
                self.print("FUNCTION");

                self.push_level();
                self.visit_identifier(identifier);
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
                self.pop_level();
            }
            LocalDeclare::Assign(assign_exprs) => {
                self.print("ASSIGN");

                self.push_level();
                for assign_expr in assign_exprs {
                    self.visit_assign_expr(assign_expr);
                }
                self.pop_level();
            }
        }
    }

    fn visit_assign_expr(&mut self, assign_expr: AssignExpression) {
        self.print("ASSIGN EXPRESSION");

        self.push_level();
        self.visit_identifier(assign_expr.identifier);
        if let Some(expression) = assign_expr.value {
            self.visit_expression(expression);
        }
        self.pop_level();
    }

    fn visit_for_init(&mut self, for_init: ForInit) {
        self.print("FOR INIT");

        self.push_level();
        match for_init {
            ForInit::LocalDeclare(local_declare) => {
                self.print("LOCAL DECLARE");

                self.visit_local_declare(local_declare);
            }
            ForInit::CommaExpression(comma_expr) => {
                self.print("COMMA EXPRESSION");

                self.visit_comma_expr(comma_expr);
            }
        }
        self.pop_level();
    }

    fn visit_unary_op(&mut self, unary_op: UnaryOp) {
        self.print("UNARY OP");

        self.push_level();
        self.visit_prefixed_exp(unary_op.expression);
        self.pop_level();
    }

    fn visit_logical_or_exp(&mut self, logical_or: LogicalOrExpression) {
        if logical_or.right.is_empty() {
            self.visit_logical_and_exp(logical_or.left);
        } else {
            self.print("LOGICAL OR");

            self.push_level();
            self.visit_logical_and_exp(logical_or.left);
            for expression in logical_or.right {
                self.visit_logical_or_exp(*expression);
            }
            self.pop_level();
        }
    }

    fn visit_logical_and_exp(&mut self, logical_and: LogicalAndExpression) {
        if logical_and.right.is_empty() {
            self.visit_bitwise_or_exp(logical_and.left);
        } else {
            self.print("LOGICAL AND");

            self.push_level();
            self.visit_bitwise_or_exp(logical_and.left);
            for expression in logical_and.right {
                self.visit_logical_and_exp(*expression);
            }
            self.pop_level();
        }
    }

    fn visit_bitwise_or_exp(&mut self, bitwise_or: BitwiseOrExpression) {
        if bitwise_or.right.is_empty() {
            self.visit_bitwise_xor_exp(bitwise_or.left);
        } else {
            self.print("BITWISE OR");

            self.push_level();
            self.visit_bitwise_xor_exp(bitwise_or.left);
            for expression in bitwise_or.right {
                self.visit_bitwise_xor_exp(expression);
            }
            self.pop_level();
        }
    }

    fn visit_bitwise_xor_exp(&mut self, bitwise_xor: BitwiseXorExpression) {
        if bitwise_xor.right.is_empty() {
            self.visit_bitwise_and_exp(bitwise_xor.left);
        } else {
            self.print("BITWISE XOR");

            self.push_level();
            self.visit_bitwise_and_exp(bitwise_xor.left);
            for expression in bitwise_xor.right {
                self.visit_bitwise_and_exp(expression);
            }
            self.pop_level();
        }
    }

    fn visit_bitwise_and_exp(&mut self, bitwise_and: BitwiseAndExpression) {
        if bitwise_and.right.is_empty() {
            self.visit_equal_exp(bitwise_and.left);
        } else {
            self.print("BITWISE AND");

            self.push_level();
            self.visit_equal_exp(bitwise_and.left);
            for expression in bitwise_and.right {
                self.visit_equal_exp(expression);
            }
            self.pop_level();
        }
    }

    fn visit_equal_exp(&mut self, equal: EqualExpression) {
        if equal.slices.is_empty() {
            self.visit_compare_exp(equal.left);
        } else {
            self.print("EQUAL");

            self.push_level();
            self.visit_compare_exp(equal.left);
            for slice in equal.slices {
                self.visit_compare_exp(slice.right);
            }
            self.pop_level();
        }
    }

    fn visit_compare_exp(&mut self, compare: CompareExpression) {
        if compare.slices.is_empty() {
            self.visit_shift_exp(compare.left);
        } else {
            self.print("COMPARE");

            self.push_level();
            self.visit_shift_exp(compare.left);
            for slice in compare.slices {
                self.visit_shift_exp(slice.right);
            }
            self.pop_level();
        }
    }

    fn visit_shift_exp(&mut self, shift: ShiftExpression) {
        if shift.slices.is_empty() {
            self.visit_plus_exp(shift.left);
        } else {
            self.print("SHIFT");

            self.push_level();
            self.visit_plus_exp(shift.left);
            for slice in shift.slices {
                self.visit_plus_exp(slice.right);
            }
            self.pop_level();
        }
    }

    fn visit_plus_exp(&mut self, plus: PlusExpression) {
        if plus.slices.is_empty() {
            self.visit_multiply_exp(plus.left);
        } else {
            self.print("PLUS");

            self.push_level();
            self.visit_multiply_exp(plus.left);
            for slice in plus.slices {
                self.visit_multiply_exp(slice.right);
            }
            self.pop_level();
        }
    }

    fn visit_multiply_exp(&mut self, multiply: MultiplyExpression) {
        if multiply.slices.is_empty() {
            self.visit_prefixed_exp(multiply.left);
        } else {
            self.print("MULTIPLY");

            self.push_level();
            self.visit_prefixed_exp(multiply.left);
            for slice in multiply.slices {
                self.visit_prefixed_exp(slice.right);
            }
            self.pop_level();
        }
    }

    fn visit_prefixed_exp(&mut self, prefixed: PrefixedExpression) {
        if prefixed.expr_types.is_empty() {
            self.visit_factor(prefixed.factor);
        } else {
            self.print("PREFIXED");

            self.push_level();
            self.visit_factor(prefixed.factor);
            self.pop_level();
            for expr_type in prefixed.expr_types {
                self.push_level();
                match expr_type {
                    PrefixedExpressionType::DotAccess(identifier) => {
                        self.print("DOT ACCESS");

                        self.push_level();
                        self.visit_identifier(identifier);
                        self.pop_level();
                    },
                    PrefixedExpressionType::ArrayStyleAccess(expression) => {
                        self.print("ARRAY ACCESS");

                        self.push_level();
                        self.visit_expression(expression);
                        self.pop_level();
                    },
                    PrefixedExpressionType::PostIncrement => {
                        self.print("POST INCREMENT");
                    },
                    PrefixedExpressionType::PostDecrement => {
                        self.print("POST DECREMENT");
                    },
                    PrefixedExpressionType::FunctionCall(args) => {
                        self.print("FUNCTION CALL");

                        self.push_level();
                        for expression in args.args {
                            self.visit_expression(expression);
                        }
                        self.pop_level();

                        // TODO: post call init
                    },
                }
                self.pop_level();
            }
        }
    }

    fn visit_table(&mut self, table: Table) {
        for entry in table.entries {
            self.visit_table_entry(entry);
        }
    }

    fn visit_table_entry(&mut self, entry: TableEntry) {
        self.print("TABLE ENTRY");

        self.push_level();
        match entry {
            TableEntry::Function(name, params, body) => {
                self.print("FUNCTION");
                
                self.push_level();
                self.visit_identifier(name);
                self.visit_func_params(params);
                self.visit_statement(body);
                self.pop_level();
            },
            TableEntry::Constructor(params, body) => {
                self.print("CONSTRUCTOR");

                self.push_level();
                self.visit_func_params(params);
                self.visit_statement(body);
                self.pop_level();
            },
            TableEntry::DynamicAssign(key, value) => {
                self.print("DYNAMIC ASSIGN");

                self.push_level();
                self.visit_comma_expr(key);
                self.visit_expression(value);
                self.pop_level();
            },
            TableEntry::JsonStyle(_key, value) => {
                self.print("JSON STYLE");

                self.push_level();
                self.visit_expression(value);
                self.pop_level();
            },
            TableEntry::Simple(key, value) => {
                self.print("SIMPLE");

                self.push_level();
                self.visit_identifier(key);
                self.visit_expression(value);
                self.pop_level();
            },
            TableEntry::Attributes(table) => {
                self.print("ATTRIBUTES");

                self.push_level();
                self.visit_table(table);
                self.pop_level();
            },
        }
        self.pop_level();
    }

    fn visit_factor(&mut self, factor: Factor) {
        self.print("FACTOR");

        self.push_level();
        match factor {
            Factor::Scalar(scalar) => {
                self.print("SCALAR");

                self.push_level();
                self.visit_scalar(scalar);
                self.pop_level();
            },
            Factor::Base => self.print("BASE"),
            Factor::Identifier(identifier) => {
                self.visit_identifier(identifier);
            },
            Factor::Constructor => self.print("CONSTRUCTOR"),
            Factor::This => self.print("THIS"),
            Factor::DoubleColon(prefixed_expression) => {
                self.print("DOUBLE_COLON");

                self.push_level();
                self.visit_prefixed_exp(*prefixed_expression);
                self.pop_level();
            },
            Factor::Null => self.print("NULL"),
            Factor::ArrayInit(entries) => {
                self.print("ARRAY");

                self.push_level();
                for entry in entries {
                    self.visit_expression(entry);
                }
                self.pop_level();
            },
            Factor::TableInit(table) => {
                self.print("TABLE");

                self.push_level();
                self.visit_table(table);
                self.pop_level();
            },
            Factor::FunctionExpression(bind_env, params, body) => {
                self.print("FUNCTION");

                self.push_level();
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }

                self.visit_func_params(params);
                self.visit_statement(*body);
                self.pop_level();
            },
            Factor::LambdaExpression(bind_env, params, body) => {
                self.print("LAMBDA EXPRESSION");

                self.push_level();
                if let Some(bind_env) = bind_env {
                    self.visit_expression(bind_env);
                }
                self.visit_func_params(params);
                self.visit_expression(body);
                self.pop_level();
            },
            Factor::ClassExpression(class_expression) => {
                self.print("CLASS EXPRESSION");

                self.push_level();
                self.visit_class_expr(class_expression);
                self.pop_level();
            },
            Factor::UnaryOp(unary_op) => {
                self.print("UNARY OP");

                self.push_level();
                self.visit_unary_op(*unary_op);
                self.pop_level();
            },
            Factor::RawCall(args) => {
                self.print("RAWCALL");

                self.push_level();
                for expression in args.args {
                    self.visit_expression(expression);
                }

                // TODO: post call init
                self.pop_level();
            },
            Factor::Delete(expression) => {
                self.print("DELETE");

                self.push_level();
                self.visit_prefixed_exp(*expression);
                self.pop_level();
            },
            Factor::ParenExpression(expression) => {
                self.print("PAREN EXPRESSION");

                self.push_level();
                self.visit_comma_expr(expression);
                self.pop_level();
            },
            Factor::LineInfo => {
                self.print("LINE INFO");
            },
            Factor::FileInfo => {
                self.print("FILE INFO");
            },
        }
        self.pop_level();
    }

    fn visit_expression(&mut self, expression: Expression) {
        self.print("EXPRESSION");

        self.push_level();
        self.visit_logical_or_exp(*expression.logical_or);
        self.pop_level();

        if let Some(expr_type) = expression.expr_type {
            match expr_type {
                ExpressionType::Newslot(expression) => {
                    self.print("NEWSLOT");

                    self.push_level();
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::Assign(expression) => {
                    self.print("ASSIGN");

                    self.push_level();
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::MinusEqual(expression) => {
                    self.print("MINUS EQUAL");

                    self.push_level();
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::PlusEqual(expression) => {
                    self.print("PLUS EQUAL");

                    self.push_level();
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::MultiplyEqual(expression) => {
                    self.print("MULTIPLY EQUAL");

                    self.push_level();
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::DivideEqual(expression) => {
                    self.print("DIVIDE EQUAL");

                    self.push_level();
                    self.visit_expression(*expression);
                    self.pop_level();
                },
                ExpressionType::Ternary(true_case, false_case) => {
                    self.print("TERNARY");

                    self.push_level();
                    self.visit_expression(*true_case);
                    self.visit_expression(*false_case);
                    self.pop_level();
                },
            }
        }
    }

    fn visit_scalar(&mut self, scalar: Scalar) {
        match scalar {
            Scalar::Integer(value) => self.print(&format!("INTEGER: {}", value)),
            Scalar::Float(value) => self.print(&format!("FLOAT: {}", value)),
            Scalar::StringLiteral(value) => self.print(&format!("STRING: {}", value)),
            Scalar::True => self.print("TRUE"),
            Scalar::False => self.print("FALSE"),
        }
    }

    fn visit_identifier(&mut self, identifier: Identifier) {
        self.print(&format!("IDENTIFIER: {}", identifier.value));
    }
}